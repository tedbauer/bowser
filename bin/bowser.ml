open Lib
open Dom

let () =
  let n = {
    node_typ = Text("Test text 1");
    children = [
      {
        node_typ = Text("Test text 2");
        children = []
      };
      {
        node_typ = Comment("Test comment 1");
        children = [
          {
            node_typ = Comment("Test comment 2");
            children = []
          }
        ]
      }
    ]
  } in

  Stdio.print_endline "Pretty-printed DOM tree:";
  Dom.pprint(n);
  Stdio.print_endline "Depth of DOM tree:";
  let (d : int) = Dom.depth n in
  Stdio.print_endline (string_of_int d)
