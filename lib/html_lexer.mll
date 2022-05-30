{
  open Html_parser
}

let alpha = ['a'-'z' 'A'-'Z']

let id = (alpha) (alpha|'_')*
let ws = ['\t' ' ' '\n']

rule token =
    parse
    | ws+        { token lexbuf }
    | "<"        { LEFT_ANGLE }
    | ">"        { RIGHT_ANGLE }
    | "/"        { FORWARD_SLASH }
    | "em"        { EM }
    | ","        { COMMA }
    | id as i    { ID (i) }

    | eof        { EOF }