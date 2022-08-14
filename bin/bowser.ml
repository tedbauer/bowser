open Lib
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

let () =
  (* let lexbuf = Lexing.from_channel stdin in
    let page = Html_parser.page Html_lexer.token lexbuf in *)
    Dom.pprint n
