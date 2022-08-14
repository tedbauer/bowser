%{
  open Dom
%}

%token LEFT_ANGLE
%token RIGHT_ANGLE
%token FORWARD_SLASH
%token EM
%token COMMA
%token <string> ID
%token EOF

%start <Dom.node> page
%%

page:
  | n = node EOF
    { n }
  ;

node:
  | o = opening_tag; n = nodes; closing_tag
    {
      {
        node_typ = Text(o);
        children = n
      }
    }
  ;

nodes:
  | COMMA
    { [] }
  | hd = node; tl = nodes
    { hd :: tl }
  ;

opening_tag:
  | LEFT_ANGLE; id1 = ID; RIGHT_ANGLE
    { id1 }
  ;

closing_tag:
  | LEFT_ANGLE; FORWARD_SLASH; ID; RIGHT_ANGLE
    { () }
  ;