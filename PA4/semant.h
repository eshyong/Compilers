#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <string.h>
#include <iostream>
#include <unordered_map>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

using std::vector;
using std::unordered_map;

#define TRUE 1
#define FALSE 0
#define ERROR_MSG_LENGTH 1024

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph. You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

enum errortype {
    DUPLICATE_CLASS          =   0, 
    CYCLIC_GRAPH             =   1, 
    UNDEFINED_ANCESTOR       =   2,
    PRIMITIVE_ANCESTOR       =   3,
    DUPLICATE_ATTR           =   4, 
    DUPLICATE_METHOD         =   5, 
    ATTR_TYPE_MISMATCH       =   6,
    METHOD_TYPE_MISMATCH     =   7,
    BAD_PLUS                 =   8,
    BAD_SUB                  =   9,
    BAD_MUL                  =  10,
    BAD_DIV                  =  11,
    BAD_LT                   =  12,
    BAD_LEQ                  =  13,
    ILLEGAL_COMP             =  14,
    UNDECLARED_ID            =  15,
    UNDEFINED_NEW            =  16,
    ID_TYPE_MISMATCH         =  17,
    IF_NOT_BOOL              =  18,
    LOOP_NOT_BOOL            =  19,
    REDEFINED_ATTR           =  20,
    SELF_ATTR                =  21,
    LET_VAR_TYPE_MISMATCH    =  22,
    PARAMS_WRONG_NUM         =  23,
    UNDEFINED_PARAM_TYPE     =  24,
    REDEFINED_PARAM_TYPE     =  25,
    REDEFINED_RETURN_TYPE    =  26,
    UNDEFINED_CLASS_DISP     =  27,
    UNDEFINED_METHOD         =  28,
    ARGS_WRONG_NUM           =  29,
    ARG_TYPE_MISMATCH        =  30,
    UNDEFINED_STATIC_TYPE    =  31,
    UNDEFINED_STATIC_METHOD  =  32,
    INVOKED_ARGS_WRONG_NUM   =  33,
    STATIC_DISP_WRONG_TYPE   =  34,
    DUPLICATE_CASE           =  35,
    DUPLICATE_FORMAL         =  36,
    SELF_FORMAL              =  37,
    NO_MAIN                  =  38,
    SELF_TYPE_REDEFINED      =  39,
    SELF_LET_BINDING         =  40,
};

// Error messages
const char * const errormsgs[] = {
    "Class %s was previously defined.\n",
    "Class %s, or an ancestor of %s, is an inheritance cycle.\n",
    "Class %s inherits from undefined class %s.\n",
    "Class %s cannot inherit class %s.\n",
    "Attribute %s is multiply defined in class.\n",
    "Method %s is multiply defined.\n",
    "Inferred type %s of initialization of attribute %s does not conform to declared type %s.\n",
    "Inferred return type %s of method %s does not conform to declared return type %s.\n",
    "non-%s arguments: %s + %s.\n",
    "non-%s arguments: %s - %s.\n",
    "non-%s arguments: %s * %s.\n",
    "non-%s arguments: %s / %s.\n",
    "non-%s arguments: %s < %s.\n",
    "non-%s arguments: %s <= %s.\n",
    "Illegal comparison with a basic type.\n",
    "Undeclared identifier %s.\n",
    "\'new\' used with undefined type %s.\n",
    "Type %s of assigned expression does not conform to declared type %s of identifier %s.\n",
    "Predicate of \'if\' does not have type Bool.\n",
    "Loop condition does not have type Bool.\n",
    "Attribute %s is an attribute of an inherited class.\n",
    "\'self\' cannot be the name of an attribute.\n",
    "Inferred type %s of initialization of %s does not conform to identifier's declared type %s.\n",
    "Incompatible number of formal parameters in redefined method %s.\n",
    "Class %s of formal parameter %s is undefined.\n",
    "In redefined method %s, parameter type %s is different from original type %s.\n",
    "In redefined method %s, return type %s is different from original return type %s.\n",
    "Dispatch on undefined class %s.\n",
    "Dispatch to undefined method %s.\n",
    "Method %s called with wrong number of arguments.\n",
    "In call of method %s, type %s of parameter %s does not conform to declared type %s.\n",
    "Static dispatch to undefined class %s.\n",
    "Static dispatch to undefined method %s.\n",
    "Method %s invoked with wrong number of arguments.\n",
    "Expression type %s does not conform to declared static type %s.\n",
    "Duplicate branch %s in case statement.\n",
    "Formal parameter %s is multiply defined.\n",
    "\'self\' cannot be the name of a formal parameter.\n",
    "Class Main is not defined.\n",
    "Redefinition of basic class SELF_TYPE.\n",
    "\'self\' cannot be bound in a \'let\' expression.\n",
};

class ClassTable {
private:
    // Used by error handling function 
    const char *firstname;
    const char *secondname;
    const char *thirdname;
    const char *fourthname;

    // Default members
    int semant_errors;
    void install_basic_classes();
    ostream& error_stream;

    // Map of class names to their pointers
    SymbolTable<Symbol, Class__class> classtable;

    // Inheritance graph
    SymbolTable<Symbol, Entry> graph;

    // Map of class names to functions
    SymbolTable<Symbol, vector<method_class *> > classmethodtable;
    
    // Map of class names to attributes
    SymbolTable<Symbol, vector<attr_class *> > classattrtable;

    // Self object environment identifier table
    SymbolTable<Symbol, Entry> objtable;
public:
    // Default functions
    ClassTable(Classes classes);
    int errors() { return semant_errors; }
    ostream& semant_error();
    ostream& semant_error(Class_ c);
    ostream& semant_error(Symbol filename, tree_node *t);

    // Semantic analysis functions
    void CheckSemantics(Classes classes);
    void CheckClass(Class_ current);
    void CheckInheritanceGraph(Classes classes);
    void CheckAttr(attr_class *attr, const Class_ c);
    void CheckMethod(method_class *method, unordered_map<Symbol, method_class *> &methods, const Class_ c);
    void CheckMethodRedefs(method_class *method, unordered_map<Symbol, method_class *> &methods, const Class_ c);
    Symbol CheckExpression(Expression expr, const Class_ c);
    Symbol CheckCase(branch_class *branch, const Class_ c);
    Symbol CheckInt(Expression expr, const Class_ c);
    Symbol CheckBool(Expression expr, const Class_ c);
    Symbol CheckStr(Expression expr, const Class_ c);

    // Helper functions
    bool IsEqual(Symbol first, Symbol second) { 
        return (first != NULL && second != NULL) && (strcmp(first->get_string(), second->get_string()) == 0);
    }
    bool IsPrimitive(Symbol type);
    method_class *LookupMethod(Symbol classname, Symbol methodname);
    Symbol LeastUpperBound(Symbol class1, Symbol class2);
    void HandleError(enum errortype error, Class_ current);
};


#endif

