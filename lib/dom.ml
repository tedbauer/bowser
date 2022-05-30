open Base

type attr_map = string Map.M(String).t

type element_data = {
  tag_name: string;
  attributes: attr_map
}

type node_type =
  | Text of string
  | Element of element_data
  | Comment of string

type node = {
  node_typ : node_type;
  children : node list;
}

let text (data : string) = {
  node_typ = Text(data) ;  
  children = []
}

let elem (name : string) (attrs : attr_map) (children : node list) = {
  node_typ = Element({
    tag_name = name;
    attributes = attrs;
  });
  children = children
}

let comm (text : string) = {
  node_typ = Comment(text);
  children = []
}

let rec pprint_s (node : node) (indent : int) =
  let { node_typ; children } = node in

  let type_str = match node_typ with
  | Text(text) -> Printf.sprintf "Text[%s]" text
  | Element(e_data) -> Printf.sprintf "Element[tag_name: %s]" e_data.tag_name
  | Comment(text) -> Printf.sprintf "Comment[%s]" text in

  let child_s = fun c -> pprint_s c (indent + 1) in
  let all_child_s = 
    List.map ~f:child_s children
    |> fun c -> String.concat ~sep:"\n" c in

  let rec repeat (s : string) (n : int) =
    if n = 0 then "" else " " ^ (repeat s (n - 1)) in
  
  if not (List.is_empty children) then
    let indent_spaces = repeat " " indent in
    Printf.sprintf "%s%s(\n%s\n%s)" indent_spaces type_str all_child_s indent_spaces
  else
    Printf.sprintf "%s%s()" (repeat " " indent) type_str

let pprint (node: node) =
  Stdio.print_endline (pprint_s node 0)