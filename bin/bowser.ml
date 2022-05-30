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

  Dom.pprint(n)
