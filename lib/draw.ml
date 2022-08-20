open Base

type display_command = SolidColor of Stylesheet.color * Layout.rect
type display_list = display_command list

let rec build_display_list (node : Layout.layout_box) : display_command list =
  let style_node = Layout.get_style_node_exn node in

  let cmd =
    Map.find style_node.specified_values "background"
    |> Option.map ~f:(fun v ->
           match v with
           | _ ->
               SolidColor
                 ( { r = 0; g = 0; b = 0; a = 1 },
                   node.d.content
                   |> Layout.expanded_by node.d.padding
                   |> Layout.expanded_by node.d.border ))
  in

  match cmd with
  | Some c ->
      c
      :: (List.map ~f:build_display_list node.children
         |> List.fold ~init:[] ~f:(fun acc e -> e @ acc))
  | None ->
      List.map ~f:build_display_list node.children
      |> List.fold ~init:[] ~f:(fun acc e -> e @ acc)

let paint (node : Layout.layout_box) =
  build_display_list node
  |> List.iter ~f:(fun cmd ->
         match cmd with
         | SolidColor (_color, rect) ->
             Graphics.draw_rect (Int.of_float rect.x) (Int.of_float rect.y)
               (Int.of_float rect.width) (Int.of_float rect.height))
