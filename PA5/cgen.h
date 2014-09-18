#include <assert.h>
#include <stdio.h>
#include <unordered_map>
#include <vector>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

using std::unordered_map;
using std::vector;

enum Basicness {
    Basic, NotBasic
};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable: public SymbolTable<Symbol, CgenNode> {
private:
    // Offset tables
    unordered_map<Symbol, unordered_map<Symbol, int> *> classfntable;
    unordered_map<Symbol, unordered_map<Symbol, int> *> classattrtable;
    
    // Classtag table
    unordered_map<Symbol, int> classtags;
    
    // Inheritance graph
    unordered_map<Symbol, Symbol> graph;
    
    // Class node list
    vector<CgenNode*> nodes;
    
    // Out stream and internal counters
    ostream& str;
    int labelnum;
    int numclasses;

    // The following methods emit code for
    // constants and global declarations.

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();

    // The following creates an inheritance graph from
    // a list of classes.  The graph is implemented as
    // a tree of `CgenNode', and class names are placed
    // in the base class symbol table.

    void install_basic_classes();
    void install_class(CgenNodeP nd, int classtag);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);

    // Helper functions for emitting tables
    void CodeClassnameTables();
    void CodeObjectTables();
    void CodeDispTables();
    void GetAllMethods(CgenNode *node, unordered_map<Symbol, Symbol> &classmethods, vector<Symbol> &methodnames);
    void EmitDispTable(unordered_map<Symbol, Symbol> classmethods, vector<Symbol> methodnames);
    
    // Emit prototype objects
    void CodePrototypes();

    // Initialization method 
    void CodeInit(CgenNode *node, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);

    // Expression helper function
    void CodeExpression(Expression expr, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);

    // Method helper function
    void CodeMethods(CgenNode *node, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals);
    void CodeMethod(method_class *method, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals);

    // AST node class codegen functions
    void AssignCode(assign_class *assign, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void StaticDispatchCode(static_dispatch_class *static_dispatch, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void DispatchCode(dispatch_class *dispatch, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void CondCode(cond_class *cond, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void LoopCode(loop_class *loop, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void TypcaseCode(typcase_class *typcase, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void BlockCode(block_class *block, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void LetCode(let_class *let, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void PlusCode(plus_class *plus, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void SubCode(sub_class *sub, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void MulCode(mul_class *mul, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void DivCode(divide_class *div, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void NegCode(neg_class *neg, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void LessThanCode(lt_class *lt, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void EqualCode(eq_class *eq, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void LessEqualCode(leq_class *leq, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void CompCode(comp_class *comp, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void IntConstCode(int_const_class *int_const, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void BoolConstCode(bool_const_class *int_const, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void StringConstCode(string_const_class *int_const, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void NewClassCode(new__class *new_class, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void IsVoidCode(isvoid_class *isvoid, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);
    void ObjectCode(object_class *object, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset);

    // Helper functions to emit function book-keeping code
    void EmitPrologue();
    void EmitEpilogue(int numparams);

    // Helper functions for environment tables
    void FillClassAttrs(CgenNode *node, unordered_map<Symbol, int> *attrs, int &offset);
    void FillClassMethods(CgenNode *node, unordered_map<Symbol, int> *methods, int &offset);
    void GetAttributes(CgenNode *node, vector<attr_class *> &attributes, int &size);
    
    // Helper functions for symbol types
    bool SymsEqual(Symbol first, Symbol second) {
        return (first != NULL && second != NULL) && (strcmp(first->get_string(), second->get_string()) == 0);
    }
    bool IsPrimitive(Symbol type);

public:
    CgenClassTable(Classes, ostream& str);
    void code();
    CgenNodeP root();
};


class CgenNode: public class__class {
private: 
    CgenNodeP parentnd;                        // Parent of class
    List<CgenNode> *children;                  // Children of class
    Basicness basic_status;                    // `Basic' if class is basic
                                               // `NotBasic' otherwise

public:
    CgenNode(Class_ c,
             Basicness bstatus,
             CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    List<CgenNode> *get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    bool basic() { return (basic_status == Basic); }
    Symbol get_name() { return name; }
};

class BoolConst {
private: 
    int val;
public:
    BoolConst(int);
    void code_def(ostream&, int boolclasstag);
    void code_ref(ostream&) const;
};

