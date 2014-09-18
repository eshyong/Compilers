/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

static int nested_level;

%}

/*
 * Define names for regular expressions here.
 */

BLKCOMMENT      \(\*
LINECOMMENT     \-\-
INVLDCOMMENT    \*\)
DARROW          =>
LESSEQ          <=
ASSIGNMENT      <-
INTEGER         [0-9]+
STRING          \"
BOOLEAN         [t][rR][uU][eE]|[f][aA][lL][sS][eE]
ID              [a-zA-Z][a-zA-Z0-9_]*
WHITESPACE      [ \f\r\n\t\v]
ASCII           [\.(){};+\-\*/<=:\,@~]
DEFAULT         [^a-zA-Z0-9\.(){};+\-\*/<=:\,@~]

%x BLKCOMMENT
%x ESCCOMMENT
%x LINECOMMENT
%x STRING
%x ESCCHAR
%x INVALIDSTRING

%%

 /*
  *  Nested comments
  */


 /*
  *  The multiple-character operators.
  */

 /* There are two types of comments:
  * block comments (BLKCOMMENT) and line comments (LINECOMMENT)
  * A block comment is delimited by the strings "(*" and "*)".
  * A line comment is delimited by the strings "--" and "\n".
  */
{BLKCOMMENT} {
    nested_level = 1;
    BEGIN(BLKCOMMENT);
}

<BLKCOMMENT>\(\* {
    nested_level++;
}

<BLKCOMMENT>\*\) {
    /* Matches a "*)" token at the end of a comment */
    nested_level--;
    if (nested_level == 0) {
        BEGIN(INITIAL);
    }
}

<BLKCOMMENT><<EOF>> {
    cool_yylval.error_msg = "EOF in comment";
    BEGIN(INITIAL);
    return ERROR;
}

<BLKCOMMENT>\n {
    /* Increment line number at newline */
    curr_lineno++;
}

<BLKCOMMENT>. {
    /* Do nothing */
}

{LINECOMMENT} {
    BEGIN(LINECOMMENT);
}

<LINECOMMENT><<EOF>> {
    curr_lineno++;
    BEGIN(INITIAL);
}

<LINECOMMENT>\n {
    curr_lineno++;
    BEGIN(INITIAL);
}

<LINECOMMENT>.* {
    /* Do nothing for comments */
}

 /* An invalid comment. */
{INVLDCOMMENT} {
    cool_yylval.error_msg = "Unmatched \'*)\'";
    return ERROR;
}

 /* Double character operators:
  * =>, <=, and <-:
  * switch arrow, less than/equals, and assignment operators,
  * respectively.
  */
{DARROW} { 
    return DARROW; 
}

{LESSEQ} {
    return LE;
}

{ASSIGNMENT} {
    return ASSIGN;
}

 /* Integer constants:
  * [0-9]+
  */
{INTEGER} {
    cool_yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
}

 /* String constants:
  * these are strings of characters delimited by "\"" on either ends.
  * Escape characters within these strings are interpreted and replaced
  * with their ASCII value.
  */
{STRING} {
    memset(string_buf, 0, MAX_STR_CONST);
    string_buf_ptr = string_buf;
    BEGIN(STRING);
}

<STRING>\\ {
    /* Escape characters */
    BEGIN(ESCCHAR);
}

<STRING>\n {
    cool_yylval.error_msg = "Unterminated string constant";
    curr_lineno++;
    BEGIN(INITIAL);
    return ERROR;
}

<STRING><<EOF>> {
    cool_yylval.error_msg = "Unterminated string constant";
    curr_lineno++;
    BEGIN(INITIAL);
    return ERROR;
}

<STRING>\0 {
    cool_yylval.error_msg = "String contains null character";
    BEGIN(INVALIDSTRING);
}

<STRING>\" {
    /* Ending quote: null terminate and return */
    *string_buf_ptr = '\0';
    cool_yylval.symbol = stringtable.add_string(string_buf);
    BEGIN(INITIAL);
    return STR_CONST;
}

<STRING>. {
    /* All other characters */
    if (string_buf_ptr == string_buf + MAX_STR_CONST - 1) {
        cool_yylval.error_msg = "String constant too long";
        BEGIN(INVALIDSTRING);
    } else {
        *string_buf_ptr = yytext[0];
        string_buf_ptr++;
    }
}

<ESCCHAR>\n {
    curr_lineno++;
    if (string_buf_ptr - string_buf == MAX_STR_CONST - 1) {
        cool_yylval.error_msg = "String constant too long";
        BEGIN(INVALIDSTRING);
    } else {
        *string_buf_ptr = '\n';
        string_buf_ptr++;
        BEGIN(STRING);
    }
}

<ESCCHAR>\0 {
    cool_yylval.error_msg = "String contains escaped null character.";
    BEGIN(INVALIDSTRING);
}

<ESCCHAR>[^\n] {
    if (string_buf_ptr - string_buf == MAX_STR_CONST - 1) {
        cool_yylval.error_msg = "String constant too long";
        BEGIN(INVALIDSTRING);
    } else {
        if (yytext[0] == 'n') {
            *string_buf_ptr = '\n';
        } else if (yytext[0] == 'b') {
            *string_buf_ptr = '\b';
        } else if (yytext[0] == 't') {
            *string_buf_ptr = '\t';
        } else if (yytext[0] == 'f') {
            *string_buf_ptr = '\f';
        } else {
            *string_buf_ptr = yytext[0];
        }
        string_buf_ptr++;
        BEGIN(STRING);
    }
}

<INVALIDSTRING>\" {
    BEGIN(INITIAL);
    return ERROR;
}

<INVALIDSTRING>\n {
    curr_lineno++;
    BEGIN(INITIAL);
    return ERROR;
}

<INVALIDSTRING>. {
    /* Do nothing */
}

 /* Boolean constants:
  * true or false. Case insensitive.
  */
{BOOLEAN} {
    /* Check if first character is 't' or 'f' */
    if ((char)tolower(yytext[0]) == 't') {
        cool_yylval.boolean = true;
    } else {
        cool_yylval.boolean = false;
    }
    return BOOL_CONST;
}

 /* All other IDs:
  * Object IDs must start with a lower case letter. Type IDs must start
  * with an upper case letter. Valid IDs consist of alphanumeric 
  * characters/underscores. IDs cannot begin with a number.
  */
{ID} {
    /* Keywords are case insensitive */
    memset(string_buf, 0, MAX_STR_CONST);
    strncpy(string_buf, yytext, MAX_STR_CONST - 1);
    string_buf[MAX_STR_CONST - 1] = '\0';

    /* Create a lowercase version for checking */
    for (int i = 0; i < MAX_STR_CONST; i++) {
        string_buf[i] = (char)tolower(string_buf[i]);
    }

    /* BIG IF ELSE STATEMENT */
    if (strcmp(string_buf, "class") == 0) {
        return CLASS;
    } else if (strcmp(string_buf, "else") == 0) {
        return ELSE;
    } else if (strcmp(string_buf, "fi") == 0) {
        return FI;
    } else if (strcmp(string_buf, "if") == 0) {
        return IF;
    } else if (strcmp(string_buf, "in") == 0) {
        return IN;
    } else if (strcmp(string_buf, "inherits") == 0) {
        return INHERITS;
    } else if (strcmp(string_buf, "let") == 0) {
        return LET;
    } else if (strcmp(string_buf, "loop") == 0) {
        return LOOP;
    } else if (strcmp(string_buf, "pool") == 0) {
        return POOL;
    } else if (strcmp(string_buf, "then") == 0) {
        return THEN;
    } else if (strcmp(string_buf, "while") == 0) {
        return WHILE;
    } else if (strcmp(string_buf, "case") == 0) {
        return CASE;
    } else if (strcmp(string_buf, "esac") == 0) {
        return ESAC;
    } else if (strcmp(string_buf, "of") == 0) {
        return OF;
    } else if (strcmp(string_buf, "new") == 0) {
        return NEW;
    } else if (strcmp(string_buf, "isvoid") == 0) {
        return ISVOID;
    } else if (strcmp(string_buf, "not") == 0) {
        return NOT;
    }
    
    /* If the first letter is upper case, return TYPEID
     * Otherwise, return OBJECTID
     */
    if (isupper(yytext[0])) {
        cool_yylval.symbol = stringtable.add_string(yytext);
        return TYPEID;   
    }
    
    cool_yylval.symbol = idtable.add_string(yytext);
    return OBJECTID;
}

 /* Whitespace:
  * ' ', '\n', '\r', '\v', '\f', '\t'
  * Only '\n' is considered a newline, and line number is incremented
  * accordingly.
  */
{WHITESPACE} {
    /* Increment line number at newline */
    if (yytext[0] == '\n') {
        curr_lineno++;
    }
}

 /* Operators:
  * Single ASCII characters, such as '+', '-', or delimiters like '(' and '{'
  */
{ASCII} {
    /* Return its ASCII code */
    return (int)yytext[0];
}

 /* All other characters. These are invalid and are discarded.
  */
{DEFAULT} {
    /* Invalid token */
    cool_yylval.error_msg = yytext;
    return ERROR;
}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */


%%
