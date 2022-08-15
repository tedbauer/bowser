open Base

type property_map = Stylesheet.value Map.M(String).t [@@deriving sexp]

type styled_node = {
  node : Dom.node;
  specified_values : property_map;
  children : styled_node list;
}
[@@deriving sexp]

type display = Inline | Block | None_ [@@deriving sexp, eq]

let value_of (node : styled_node) ~(field : string) =
  Map.find node.specified_values field

let display_of (node : styled_node) =
  match value_of node ~field:"display" with
  | Some (Stylesheet.Keyword s) ->
      if String.equal s "block" then Block
      else if String.equal s "none" then None_
      else Inline
  | Some _ | None -> Inline

(** [matches e s] is true if [s] selects [e], false otherwise. *)
let matches (elem : Dom.element_data) (selector : Stylesheet.selector) =
  match selector with
  | Simple s ->
      let type_selector_matches =
        Option.value_map s.tag_name
          ~f:(fun name -> String.equal name elem.tag_name)
          ~default:true
      in
      let id_selector_matches =
        Option.value_map s.id ~default:true ~f:(fun s_id ->
            Dom.get_id elem
            |> Option.value_map ~default:false ~f:(fun elem_id ->
                   String.equal elem_id s_id))
      in
      let class_selectors_matches =
        let elem_classes = Dom.get_classes elem in
        Set.is_subset (Set.of_list (module String) s.classes) ~of_:elem_classes
      in
      type_selector_matches && id_selector_matches && class_selectors_matches

let matching_rule (elem : Dom.element_data) (rule : Stylesheet.rule) =
  rule.selectors
  |> List.find ~f:(matches elem)
  |> Option.map ~f:(fun selector -> (rule, Stylesheet.specificity_of selector))

let specified_values_of (elem : Dom.element_data) (s : Stylesheet.stylesheet) =
  s.rules
  |> List.filter_map ~f:(matching_rule elem)
  |> List.sort ~compare:(fun (_, s1) (_, s2) ->
         [%derive.ord: int * int * int] s1 s2)
  |> List.fold ~init:[] ~f:(fun svalues ((rule : Stylesheet.rule), _) ->
         let decls_alist =
           rule.declarations
           |> List.fold ~init:[] ~f:(fun acc (decl : Stylesheet.declaration) ->
                  (decl.name, decl.value_) :: acc)
         in
         decls_alist @ svalues)
  |> List.rev
  |> Map.of_alist_reduce (module String) ~f:(fun _ new_val -> new_val)

let rec gen_style_tree (root : Dom.node) (stylesheet : Stylesheet.stylesheet) =
  {
    node = root;
    children = List.map root.children ~f:(fun c -> gen_style_tree c stylesheet);
    specified_values =
      (match root.node_typ with
      | Text _ | Comment _ -> Map.empty (module String)
      | Element edata -> specified_values_of edata stylesheet);
  }
