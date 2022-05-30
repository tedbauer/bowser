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
  | n = node
    { n }
  ;

node:
  | o = opening_tag; n = node; closing_tag
    {
      {
        node_typ = Text(o);
        children = [n]
      }
    }
  | o = opening_tag; closing_tag
    {
      {
        node_typ = Text(o);
        children = []
      }
    }
  ;

opening_tag:
  | LEFT_ANGLE; id1 = ID; RIGHT_ANGLE
    { id1 }
  ;

closing_tag:
  | LEFT_ANGLE; FORWARD_SLASH; ID; RIGHT_ANGLE
    { () }
  ;