/*
 *  compile with bison -d   to generate header file for the lexer
 *  
 *   bison -d sample.y
 *   flex sample.flex
 *   g++ sample.tab.c lex.yy.c -lfl
 *   ./a.out < inputfile
 */
%{
#include <iostream>
#include <stdlib.h>
#include <string.h>

using namespace std;
void yyerror(char *s);        /*  defined below; called for each parse error */
extern int yylex();           /*  the entry point to the lexer; must be declared in this file  */

/************************************************************************/

 void blanks(int n) {
   while (n--) {
     cout << " ";
   }
 }

 class Expr {
   public:
   virtual void print(int indent) {
     blanks(indent);
     cout << "UNIMPLEMENTED CASE\n";
   }
 };

 class Variable : public Expr {
   char *var;

 public:
   void print(int indent) {
     blanks(indent);
     cout << var << "\n";
   }

   Variable(char *s){
     var = (char *) malloc(sizeof(char) * (strlen(s) + 1));
     strcpy(var,s);
   }
 };

 class Function: public Expr{
   Variable *var;
   Expr *body;
 public:
   Function(Variable *v, Expr *e) {
     var = v;
     body = e;
   }

   void print(int indent) { 
     blanks(indent);
     cout << "function\n";
     var->print(indent+4);
     body->print(indent+4);
   }
 };

 class App : public Expr{
   Expr *fun;
   Expr *arg;

 public:
   void print(int indent) { 
     blanks(indent);
     cout << "apply\n";
     fun->print(indent+4);
     arg->print(indent+4);
   }

   App(Expr *e1, Expr *e2) {
     fun = e1;
     arg = e2;
   }
 };

 class Sum : public Expr {
   Expr *e1;
   Expr *e2;

 public:
   void print(int indent) { 
     blanks(indent);
     cout << "sum\n";
     e1->print(indent+4);
     e2->print(indent+4);
   }

   Sum(Expr *l, Expr *r) {
     e1 = l;
     e2 = r;
   }
 };

 class Integer : public Expr {
   char *num;
 public:
   void print(int indent) {
     blanks(indent);
     cout << num << "\n";
   }

   Integer(char *z){
     num = (char *) malloc(sizeof(char) * (strlen(z) + 1));
     strcpy(num,z);
   }
 };

Expr *ast_root = NULL;	      /* the result of the parse  */
%}

/* A union of all the types that can be the result of parsing actions. */
%union {
  Expr *expr;
  Variable *var;
  char *lexeme;
}

%token LAMBDA
%token PLUS
%token DOT
%token INVALID
%token OPEN
%token CLOSE
%token <lexeme> VAR
%token <lexeme> INT



/**************************************************************************/

/* Declare types for the grammar's non-terminals. */
%type <expr> program
%type <expr> exp
%type <var> var

/* Precedence declarations go here. */
%left PLUS

%%
/* 
   Save the root of the abstract syntax tree in a global variable.
*/
program	: exp	{ ast_root = $1; }
        ;

exp  : LAMBDA var DOT exp   { $$ = new Function($2,$4); }
     | OPEN exp CLOSE       { $$ = $2; }
     | exp exp              { $$ = new App($1,$2); } 
     | exp PLUS exp         { $$ = new Sum($1,$3); }
     | VAR                  { $$ = new Variable($1); }
     | INT                  { $$ = new Integer($1); }
    //     | OPEN error CLOSE     { $$ = new Integer("0"); }  /* dummy value */
var  : VAR                  { $$ = new Variable($1); }
/* end of grammar */
%%

/* This function is called automatically when Bison detects a parse error. */
void yyerror(char *s)
{
  extern int num_lines;

  cerr << " line " << num_lines << ": " << s;
  cerr << endl;
}

int main(int argc, char **argv) {
  yyparse();
  ast_root->print(0);
}  
