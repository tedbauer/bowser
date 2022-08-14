open Base

type color = { r : int; g : int; b : int; a : int } [@@deriving sexp]
type length_unit = Px [@@deriving sexp]
type b = A of int * length_unit | B of string [@@deriving sexp]

type value =
  | Keyword of string
  | Length of float * length_unit
  | ColorValue of color
[@@deriving sexp]

type declaration = { name : string; value_ : value } [@@deriving sexp]

type simple_selector = {
  tag_name : string option;
  id : string option;
  classes : string list;
}
[@@deriving sexp]

type selector = Simple of simple_selector [@@deriving sexp]

let specificity_of = function
  | Simple selector ->
      let id_count = Option.value_map selector.id ~f:(fun _ -> 1) ~default:0 in
      let tag_count =
        Option.value_map selector.tag_name ~f:(fun _ -> 1) ~default:0
      in
      let class_count = List.length selector.classes in
      (id_count, tag_count, class_count)

type rule = { selectors : selector list; declarations : declaration list }
[@@deriving sexp]

type stylesheet = { rules : rule list } [@@deriving sexp]
