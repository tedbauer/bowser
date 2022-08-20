open Base

type rect = { x : float; y : float; width : float; height : float }
[@@deriving sexp]

type edge_sizes = { left : float; right : float; top : float; bottom : float }
[@@deriving sexp]

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

let compute_block_width (node : layout_box) =
  let d = node.d in
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

  let used_margin_left, used_margin_right =
    if
      Stylesheet.equal_value width auto
      && Float.( > ) total node.d.content.width
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
      (margin_left, margin_right)
    else (margin_left, margin_right)
  in
  ()

let rec compute_dimensions (root : layout_box) =
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
