open Base

type color = { r : int; g : int; b : int; a : int }
type unit = Px
type value = Keyword of string | Length of float * unit | ColorValue of color
type declaration = { name : string; value_ : value }

type simple_selector = {
  tag_name : string option;
  id : string option;
  classes : string list;
}

type selector = Simple of simple_selector

let specificity_of = function
  | Simple selector ->
      let id_count = Option.value_map selector.id ~f:(fun _ -> 1) ~default:0 in
      let tag_count =
        Option.value_map selector.tag_name ~f:(fun _ -> 1) ~default:0
      in
      let class_count = List.length selector.classes in
      (id_count, tag_count, class_count)

type rule = { selectors : selector list; declarations : declaration list }
type stylesheet = { rules : rule list }
