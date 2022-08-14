open Base

type property_map = Stylesheet.value Map.M(String).t

type styled_node = {
  node: Dom.node;
  specified_values: property_map;
  children: styled_node list
}

let matches (elem: Dom.element_data) (selector: Stylesheet.selector) =
  match selector with
  | Simple (s) ->
    Option.value_map s.tag_name ~f:(fun name -> String.equal name elem.tag_name) ~default:false &&
    Option.value_map s.id ~default:false ~f:(fun s_id ->
      Dom.get_id elem
      |> Option.value_map ~default:false ~f:(fun elem_id -> String.equal elem_id s_id))
      

(* 
let specified_values (data: element_data) (stylesheet_: stylesheet) =
  let rule_matches data rule =


  stylesheet_.rules
  |> List.filter ~f:
 *)
let rec gen_style_tree (root: Dom.node) (stylesheet: Stylesheet.stylesheet) =
  {
    node = root;
    specified_values = Map.empty (module String);
    children = List.map root.children ~f:(fun c -> gen_style_tree c stylesheet);
  }
