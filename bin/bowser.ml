open Lib
open Base

let n : Dom.node =
  {
    node_typ = Dom.Text "Test text 1";
    children =
      [
        { node_typ = Dom.Text "Test text 2"; children = [] };
        {
          node_typ = Dom.Comment "Test comment 1";
          children =
            [ { node_typ = Dom.Comment "Test comment 2"; children = [] } ];
        };
      ];
  }

(*
* { display: block; padding: 12px; }
.a { background: #ff0000; }
.b { background: #ffa500; }
.c { background: #ffff00; }
.d { background: #008000; }
.e { background: #0000ff; }
.f { background: #4b0082; }
.g { background: #800080; }
*)
let test_stylesheet : Stylesheet.stylesheet =
  {
    rules =
      [
        {
          selectors =
            [ Stylesheet.Simple { tag_name = None; id = None; classes = [] } ];
          declarations =
            [
              { name = "display"; value_ = Stylesheet.Keyword "block" };
              {
                name = "padding";
                value_ = Stylesheet.Length (12., Stylesheet.Px);
              };
            ];
        };
        {
          selectors =
            [
              Stylesheet.Simple
                { tag_name = None; id = None; classes = [ "a" ] };
            ];
          declarations =
            [ { name = "background"; value_ = Stylesheet.Keyword "#ff0000" } ];
        };
        {
          selectors =
            [
              Stylesheet.Simple
                { tag_name = None; id = None; classes = [ "b" ] };
            ];
          declarations =
            [ { name = "background"; value_ = Stylesheet.Keyword "#ffa500" } ];
        };
        {
          selectors =
            [
              Stylesheet.Simple
                { tag_name = None; id = None; classes = [ "c" ] };
            ];
          declarations =
            [ { name = "background"; value_ = Stylesheet.Keyword "#ffff00" } ];
        };
        {
          selectors =
            [
              Stylesheet.Simple
                { tag_name = None; id = None; classes = [ "d" ] };
            ];
          declarations =
            [ { name = "background"; value_ = Stylesheet.Keyword "#008000" } ];
        };
        {
          selectors =
            [
              Stylesheet.Simple
                { tag_name = None; id = None; classes = [ "e" ] };
            ];
          declarations =
            [ { name = "background"; value_ = Stylesheet.Keyword "#0000ff" } ];
        };
        {
          selectors =
            [
              Stylesheet.Simple
                { tag_name = None; id = None; classes = [ "f" ] };
            ];
          declarations =
            [ { name = "background"; value_ = Stylesheet.Keyword "#4b0082" } ];
        };
        {
          selectors =
            [
              Stylesheet.Simple
                { tag_name = None; id = None; classes = [ "g" ] };
            ];
          declarations =
            [ { name = "background"; value_ = Stylesheet.Keyword "#800080" } ];
        };
      ];
  }

(*
<div class="a">
  <div class="b">
    <div class="c">
      <div class="d">
        <div class="e">
          <div class="f">
            <div class="g">
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</div>
*)
let test_node : Dom.node =
  {
    node_typ =
      Dom.Element
        {
          tag_name = "div";
          attributes = Map.of_alist_exn (module String) [ ("class", "a") ];
        };
    children =
      [
        {
          node_typ =
            Dom.Element
              {
                tag_name = "div";
                attributes = Map.of_alist_exn (module String) [ ("class", "b") ];
              };
          children =
            [
              {
                node_typ =
                  Dom.Element
                    {
                      tag_name = "div";
                      attributes =
                        Map.of_alist_exn (module String) [ ("class", "c") ];
                    };
                children =
                  [
                    {
                      node_typ =
                        Dom.Element
                          {
                            tag_name = "div";
                            attributes =
                              Map.of_alist_exn
                                (module String)
                                [ ("class", "d") ];
                          };
                      children =
                        [
                          {
                            node_typ =
                              Dom.Element
                                {
                                  tag_name = "div";
                                  attributes =
                                    Map.of_alist_exn
                                      (module String)
                                      [ ("class", "e") ];
                                };
                            children =
                              [
                                {
                                  node_typ =
                                    Dom.Element
                                      {
                                        tag_name = "div";
                                        attributes =
                                          Map.of_alist_exn
                                            (module String)
                                            [ ("class", "f") ];
                                      };
                                  children =
                                    [
                                      {
                                        node_typ =
                                          Dom.Element
                                            {
                                              tag_name = "div";
                                              attributes =
                                                Map.of_alist_exn
                                                  (module String)
                                                  [ ("class", "g") ];
                                            };
                                        children = [];
                                      };
                                    ];
                                };
                              ];
                          };
                        ];
                    };
                  ];
              };
            ];
        };
      ];
  }

let () =
  With_return.with_return (fun r ->
      Dom.pprint test_node;

      [%sexp_of: Style.styled_node]
        (Style.gen_style_tree test_node test_stylesheet)
      |> Sexp.to_string |> Stdio.print_endline;

      ignore n;
      Graphics.open_graph "";
      while true do
        let st = Graphics.wait_next_event [ Graphics.Key_pressed ] in
        Graphics.synchronize ();
        if st.keypressed then r.return ()
      done)

(* let () = *)
(* let lexbuf = Lexing.from_channel stdin in
   let page = Html_parser.page Html_lexer.token lexbuf in *)
(* Graphics.open_graph ""; *)
(* Dom.pprint n *)
