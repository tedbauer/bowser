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
                         d =
                           {
                             content =
                               { x = 0.; y = 0.; width = 0.; height = 0. };
                             padding =
                               { left = 0.; right = 0.; top = 0.; bottom = 0. };
                             border =
                               { left = 0.; right = 0.; top = 0.; bottom = 0. };
                             margin =
                               { left = 0.; right = 0.; top = 0.; bottom = 0. };
                           };
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
  {
    box_type_;
    children;
    d =
      {
        content = { x = 0.; y = 0.; width = 0.; height = 0. };
        padding = { left = 0.; right = 0.; top = 0.; bottom = 0. };
        border = { left = 0.; right = 0.; top = 0.; bottom = 0. };
        margin = { left = 0.; right = 0.; top = 0.; bottom = 0. };
      };
  }
