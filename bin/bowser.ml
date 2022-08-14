open Lib
open Base
open Dom

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
}

let () = With_return.with_return (fun r -> 
  Dom.pprint n;
  Graphics.open_graph "";
  while true do
    let st = Graphics.wait_next_event [ Graphics.Key_pressed ] in
    Graphics.synchronize ();
    if st.keypressed then r.return ();
  done
)

(* let () = *)
  (* let lexbuf = Lexing.from_channel stdin in
    let page = Html_parser.page Html_lexer.token lexbuf in *)
  (* Graphics.open_graph ""; *)
  (* Dom.pprint n *)
