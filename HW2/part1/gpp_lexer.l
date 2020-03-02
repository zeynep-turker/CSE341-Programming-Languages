%option noyywrap
%%
";;"  {printf("\n%s\tCOMMENT",yytext);} 

"+"     {printf("\n%s\tOP_PLUS",yytext);}
"-"     {printf("\n%s\tOP_MINUS",yytext);}
"/"     {printf("\n%s\tOP_DIV",yytext);}
"*"     {printf("\n%s\tOP_MULT",yytext);}
"("     {printf("\n%s\tOP_OP",yytext);}
")"     {printf("\n%s\tOP_CP",yytext);}
"**"     {printf("\n%s\tOP_DBLMULT",yytext);}
"\""     {printf("\n%s\tOP_OC",yytext);}
","  {printf("\n%s\tOP_COMMA",yytext);}

"and"  {printf("\n%s\tKW_AND",yytext);}
"or"  {printf("\n%s\tKW_OR",yytext);}
"not" {printf("\n%s\tKW_NOT",yytext);}
"equal"  {printf("\n%s\tKW_EQUAL",yytext);}
"less" {printf("\n%s\tKW_LESS",yytext);}
"nil"  {printf("\n%s\tKW_NIL",yytext);}
"list"   {printf("\n%s\tKW_LIST",yytext);}
"append" {printf("\n%s\tKW_APPEND",yytext);}
"concat"  {printf("\n%s\tKW_CONCAT",yytext);}
"set"   {printf("\n%s\tKW_SET",yytext);}
"deffun" {printf("\n%s\tKW_DEFFUN",yytext);}
"for"  {printf("\n%s\tKW_FOR",yytext);}
"if"   {printf("\n%s\tKW_IF",yytext);}
"exit" {printf("\n%s\tKW_EXIT",yytext);}
"load"  {printf("\n%s\tKW_LOAD",yytext);}
"disp"   {printf("\n%s\tKW_DISP",yytext);}
"true"  {printf("\n%s\tKW_TRUE",yytext);}
"false"   {printf("\n%s\tKW_FALSE",yytext);}

[-+]*[0-9] {printf("\n%s\tVALUE",yytext);}
[-+]*[1-9]+ {printf("\n%s\tVALUE",yytext);}
[a-zA-Z]+([0-9]|[a-zA-Z])* {printf("\n%s\tIDENTIFIER",yytext);}
[0-9]*[a-zA-Z]* {printf("\n%s\tSYNTAX ERROR",yytext);}
[0[0-9]+]+ {printf("\n%s\tSYNTAX ERROR",yytext);}


[ \t\n]+ //ignore whitespace and newlines
%%



