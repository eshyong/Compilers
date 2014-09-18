/*

sample simple scanner

need to compile with -lfl
*/

%{
class Expr;
class Variable;

#include "sample.tab.h"

int num_lines = 0;
%}
%x COMMENT
%%
\n			{ num_lines++; }
"\\"	  	        { return(LAMBDA); }
[A-Za-z][A-Za-z0-9]*	{ yylval.lexeme = yytext; return(VAR); }
"."			{ return(DOT); }
"("			{ return(OPEN); }
")"			{ return(CLOSE); }
"+"			{ return(PLUS); }
[0-9]+			{ yylval.lexeme = yytext; return(INT); }
[ \t]+
.			{ return(INVALID); }
"//"			{ BEGIN(COMMENT); }
<COMMENT>.*		/* eat up the rest of the line */
<COMMENT>\n		{ num_lines++; BEGIN(INITIAL); }
%%
