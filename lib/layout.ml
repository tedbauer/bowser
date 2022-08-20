open Base

type rect = { x : float; y : float; width : float; height : float }
[@@deriving sexp]

type edge_sizes = { left : float; right : float; top : float; bottom : float }
[@@deriving sexp]

let expanded_by (e : edge_sizes) (r: rect )=
  {
    x = r.x -. e.left;
    y = r.y -. e.top;
    width = r.width +. e.left +. e.right;
    height = r.height +. e.top +. e.bottom;
  }

type dimensions = {
  content : rect;
  padding : edge_sizes;
  border : edge_sizes;
  margin : edge_sizes;
}
[@@deriving sexp]

let default_dimensions =
  {
    content = { x = 0.; y = 0.; width = 0.; height = 0. };
    padding = { left = 0.; right = 0.; top = 0.; bottom = 0. };
    border = { left = 0.; right = 0.; top = 0.; bottom = 0. };
    margin = { left = 0.; right = 0.; top = 0.; bottom = 0. };
  }

type box_type =
  | BlockNode of Style.styled_node
  | InlineNode of Style.styled_node
  | AnonymousBlock
[@@deriving sexp]

type layout_box = {
  d : dimensions;
  box_type_ : box_type;
  children : layout_box list;
}
[@@deriving sexp]

let get_style_node_exn (node : layout_box) =
  match node.box_type_ with
  | BlockNode s | InlineNode s -> s
  | AnonymousBlock -> failwith "Impossible"

let lookup (node : Style.styled_node) fst snd default =
  Option.first_some
    (Map.find node.specified_values fst)
    (Map.find node.specified_values snd)
  |> Option.value ~default

let rec compute_block_height (node : layout_box) =
  let style_node = get_style_node_exn node in

  let children_computed = List.map node.children ~f:compute_block_height in
  let children_height =
    children_computed
    |> List.map ~f:(fun c -> c.d.content.height)
    |> List.fold ~init:0. ~f:(fun acc e -> acc +. e)
  in
  let margin_box =
    node.d.content
    |> expanded_by node.d.padding
    |> expanded_by node.d.border
    |> expanded_by node.d.margin in
  let curr_height =
    Map.find style_node.specified_values "height"
    |> Option.value_map
         ~f:(fun l -> Stylesheet.pixels_of_exn l)
         ~default:(margin_box.height +. children_height)
  in
  {
    d = { node.d with content = { node.d.content with height = curr_height } };
    box_type_ = node.box_type_;
    children = children_computed;
  }

let rec compute_block_pos (containing_dims : dimensions) (node: layout_box )=
  let style_node = get_style_node_exn node in
  let zero = Stylesheet.Length (0., Stylesheet.Px) in

  let margin_top = lookup style_node "margin-top" "margin" zero in
  let margin_bottom = lookup style_node "margin-bottom" "margin" zero in

  let border_top = lookup style_node "border-top-width" "border-width" zero in
  let border_bottom =
    lookup style_node "border-bottom-width" "border-width" zero
  in

  let padding_top = lookup style_node "padding-top" "padding" zero in
  let padding_bottom = lookup style_node "padding-bottom" "padding" zero in

  let content_x =
    containing_dims.content.x +. node.d.margin.left +. node.d.border.left
    +. node.d.padding.left
  in
  let content_y =
    containing_dims.content.height +. containing_dims.content.y
    +. node.d.margin.top +. node.d.border.top +. node.d.padding.top
  in

  {
    node with
    children = List.map ~f:(fun c -> compute_block_pos node.d c) node.children;
    d =
      {
        margin =
          {
            node.d.margin with
            top = Stylesheet.pixels_of_exn margin_top;
            bottom = Stylesheet.pixels_of_exn margin_bottom;
          };
        border =
          {
            node.d.border with
            top = Stylesheet.pixels_of_exn border_top;
            bottom = Stylesheet.pixels_of_exn border_bottom;
          };
        padding =
          {
            node.d.padding with
            top = Stylesheet.pixels_of_exn padding_top;
            bottom = Stylesheet.pixels_of_exn padding_bottom;
          };
        content = { node.d.content with x = content_x; y = content_y };
      };
  }

let rec compute_block_width (containing_dims : dimensions) (node: layout_box)=
  let style_node = get_style_node_exn node in

  let auto = Stylesheet.Keyword "auto" in
  let width =
    Map.find style_node.specified_values "width" |> Option.value ~default:auto
  in

  let zero = Stylesheet.Length (0., Stylesheet.Px) in

  let margin_left = lookup style_node "margin-left" "margin" zero in
  let margin_right = lookup style_node "margin-right" "margin" zero in

  let border_left = lookup style_node "border-left-width" "border-width" zero in
  let border_right =
    lookup style_node "border-right-width" "border-width" zero
  in

  let padding_left = lookup style_node "padding-left" "padding" zero in
  let padding_right = lookup style_node "padding-right" "padding" zero in

  let total =
    List.map ~f:Stylesheet.pixels_of_exn
      [
        margin_left;
        margin_right;
        border_left;
        border_right;
        padding_left;
        padding_right;
      ]
    |> List.fold ~init:0. ~f:(fun acc e -> acc +. e)
  in

  let margin_left', margin_right' =
    if
      Stylesheet.equal_value width auto
      && Float.( > ) total containing_dims.content.width
    then
      let left =
        if not (Stylesheet.equal_value margin_left auto) then
          Stylesheet.Length (0., Stylesheet.Px)
        else margin_left
      in
      let right =
        if not (Stylesheet.equal_value margin_right auto) then
          Stylesheet.Length (0., Stylesheet.Px)
        else margin_right
      in
      (left, right)
    else (margin_left, margin_right)
  in

  let underflow = containing_dims.content.width -. total in

  let width_is_auto = Stylesheet.equal_value width auto in
  let margin_left_is_auto = Stylesheet.equal_value margin_left' auto in
  let margin_right_is_auto = Stylesheet.equal_value margin_right' auto in

  let used_width, used_margin_left, used_margin_right =
    match (width_is_auto, margin_left_is_auto, margin_right_is_auto) with
    | false, false, false ->
        let margin_right'' =
          Stylesheet.Length
            (Stylesheet.pixels_of_exn margin_right' +. underflow, Stylesheet.Px)
        in
        (width, margin_left', margin_right'')
    | false, false, true ->
        let margin_right'' = Stylesheet.Length (underflow, Stylesheet.Px) in
        (width, margin_left', margin_right'')
    | false, true, false ->
        let margin_left'' = Stylesheet.Length (underflow, Stylesheet.Px) in
        (width, margin_left'', margin_right')
    | true, _, _ ->
        let margin_left'' =
          if Stylesheet.equal_value margin_left' auto then
            Stylesheet.Length (0., Stylesheet.Px)
          else margin_left'
        in
        let margin_right'' =
          if Stylesheet.equal_value margin_right' auto then
            Stylesheet.Length (0., Stylesheet.Px)
          else margin_right'
        in

        if Float.( >= ) underflow 0. then
          let width' = Stylesheet.Length (underflow, Stylesheet.Px) in
          (width', margin_left'', margin_right'')
        else
          let width' = Stylesheet.Length (0., Stylesheet.Px) in
          let margin_right'' =
            Stylesheet.Length
              ( Stylesheet.pixels_of_exn margin_right' +. underflow,
                Stylesheet.Px )
          in
          (width', margin_left', margin_right'')
    | false, true, true ->
        let margin_left'' =
          Stylesheet.Length (underflow /. 2., Stylesheet.Px)
        in
        let margin_right'' =
          Stylesheet.Length (underflow /. 2., Stylesheet.Px)
        in
        (width, margin_left'', margin_right'')
  in
  {
    d =
      {
        content =
          {
            width = Stylesheet.pixels_of_exn used_width;
            x = 0.;
            y = 0.;
            height = 0.;
          };
        padding =
          {
            left = Stylesheet.pixels_of_exn padding_left;
            right = Stylesheet.pixels_of_exn padding_right;
            top = 0.;
            bottom = 0.;
          };
        border =
          {
            left = Stylesheet.pixels_of_exn border_left;
            right = Stylesheet.pixels_of_exn border_right;
            top = 0.;
            bottom = 0.;
          };
        margin =
          {
            left = Stylesheet.pixels_of_exn used_margin_left;
            right = Stylesheet.pixels_of_exn used_margin_right;
            top = 0.;
            bottom = 0.;
          };
      };
    box_type_ = node.box_type_;
    children = List.map node.children ~f:(fun c -> compute_block_width node.d c);
  }

let compute_dimensions (root : layout_box) =
  match root.box_type_ with
  | BlockNode _ -> root
  | InlineNode _ -> root (* TODO *)
  | AnonymousBlock -> root (* TODO *)

let rec build_layout_tree (root : Style.styled_node) =
  let box_type_, children =
    match Style.display_of root with
    | Block ->
        let children =
          root.children
          |> List.map ~f:(fun c -> (c, Style.display_of c))
          |> List.group ~break:(fun (_, d1) (_, d2) ->
                 Style.equal_display d1 d2)
          |> List.map ~f:(fun grp ->
                 match List.hd grp with
                 | Some (_, Style.Block) ->
                     List.map grp ~f:(fun (c, _) -> build_layout_tree c)
                 | Some (_, Style.Inline) ->
                     [
                       {
                         children =
                           List.map grp ~f:(fun (c, _) -> build_layout_tree c);
                         d = default_dimensions;
                         box_type_ = AnonymousBlock;
                       };
                     ]
                 | Some (_, Style.None_) -> []
                 | None -> failwith "Impossible")
          |> List.fold ~init:[] ~f:(fun acc e -> acc @ e)
        in
        (BlockNode root, children)
    | Inline ->
        ( InlineNode root,
          List.map root.children ~f:(fun c -> build_layout_tree c) )
    | None_ ->
        ( AnonymousBlock,
          List.map root.children ~f:(fun c -> build_layout_tree c) )
  in
  { box_type_; children; d = default_dimensions }

let build_layout_tree_with_dims (root: Style.styled_node) =
  let container: dimensions = {
    content = {x = 0.; y = 0.; height = 400.; width = 400.;};
    padding = {left = 0.; right = 0.; top = 0.; bottom = 0.; };
    border = {left = 0.; right = 0.; top = 0.; bottom = 0.; };
    margin = {left = 0.; right = 0.; top = 0.; bottom = 0.; };
  } in
  build_layout_tree root
  |> compute_block_width container
  |> compute_block_pos container
  |> compute_block_height
