%{
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

opening_tag:
    | LEFT_ANGLE id = ID RIGHT_ANGLE
      { id }

closing_tag:
    | LEFT_ANGLE id = ID FORWARD_SLASH RIGHT_ANGLE
      { () }

node:
    | tag_name = opening_tag; children = separated_list(COMMA, node); closing_tag
      { 
        { 
          node_typ = Element({
            tag_name = tag_name;
            attributes = []
          });
          children = children;
        }
      }