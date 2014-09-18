
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <vector>

using std::make_pair;
using std::vector;

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)  {
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

    os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_comment(char *text, ostream &str) {
    str << COMMENT << text << endl;
}

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s) {
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
      << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s) {
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s) {
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s) {
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s) {
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << dest << endl; }

static void emit_jal(char *address, ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{ s << sym->get_string() << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym->get_string() << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym->get_string() << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname->get_string() << METHOD_SEP << methodname->get_string(); }

static void emit_label_def(int l, ostream &s) {
    emit_label_ref(l, s);
    s << LABEL;
}

static void emit_break(int label, ostream &s) {
    s << BRANCH;
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beqz(char *source, int label, ostream &s) {
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s) {
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s) {
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s) {
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s) {
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s) {
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s) {
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_branch(int l, ostream& s) {
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str) {
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
}

static void emit_pop(char *reg, ostream& str) {
    emit_addiu(SP, SP, 4, str);
    emit_load(reg, 0, SP, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s) {
    emit_push(ACC, s);
    emit_move(ACC, SP, s); // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s) {
    if (source != (char*)A1) emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s) {
    s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag) {
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << GCTAG << endl;

    code_ref(s); 
    s << LABEL                                                               // label
      << WORD << stringclasstag << endl                                      // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl  // size
      << WORD;


    /***** Add dispatch information for class String ******/
    emit_disptable_ref(Str, s);

    s << endl;                                              // dispatch table
    s << WORD;  lensym->code_ref(s);  s << endl;            // string length
    emit_string_constant(s, str);                            // ascii string
    s << ALIGN;                                             // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag) {  
    for (List<StringEntry> *l = tbl; l; l = l->tl()) {
        l->hd()->code_def(s, stringclasstag);
    }
}

//
// Ints
//
void IntEntry::code_ref(ostream &s) {
    s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag) {
    // Add -1 eye catcher
    s << WORD << GCTAG << endl;

    code_ref(s);
    s << LABEL                                            // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

    /***** Add dispatch information for class Int ******/
    emit_disptable_ref(Int, s);

    s << endl;                                          // dispatch table
    s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag) {
    for (List<IntEntry> *l = tbl; l; l = l->tl()) {
        l->hd()->code_def(s, intclasstag);
    }
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { 
    assert(i == 0 || i == 1);
}

void BoolConst::code_ref(ostream& s) const {
    s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag) {
    // Add -1 eye catcher
    s << WORD << GCTAG << endl;

    code_ref(s);
    s << LABEL                                              // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

    /***** Add dispatch information for class Bool ******/
    emit_disptable_ref(Bool, s);

    s << endl;                                            // dispatch table
    s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data() {
    Symbol main    = idtable.lookup_string(MAINNAME);
    Symbol string  = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc   = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL; emit_protobj_ref(main, str);    str << endl;
    str << GLOBAL; emit_protobj_ref(integer, str); str << endl;
    str << GLOBAL; emit_protobj_ref(string, str);  str << endl;
    str << GLOBAL; falsebool.code_ref(str);  str << endl;
    str << GLOBAL; truebool.code_ref(str);   str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    //
    // We also need to know the tag of the Int, String, and Bool classes
    // during code generation.
    //
    str << INTTAG << LABEL
        << WORD << INTCLASSTAG << endl;
    str << BOOLTAG << LABEL 
        << WORD << BOOLCLASSTAG << endl;
    str << STRINGTAG << LABEL 
        << WORD << STRCLASSTAG << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text() {
    str << GLOBAL << HEAP_START << endl
        << HEAP_START << LABEL 
        << WORD << 0 << endl
        << "\t.text" << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag) {
    falsebool.code_def(str, boolclasstag);
    truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc() {
    //
    // Generate GC choice constants (pointers to GC functions)
    //
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants() {
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str, STRCLASSTAG);
    inttable.code_string_table(str, INTCLASSTAG);
    code_bools(BOOLCLASSTAG);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : str(s) {
    numclasses = 0;
    labelnum = 0;

    classtags.insert(make_pair<Symbol, int>(Object, OBJCLASSTAG));
    classtags.insert(make_pair<Symbol, int>(IO,     IOCLASSTAG));
    classtags.insert(make_pair<Symbol, int>(Int,    INTCLASSTAG));
    classtags.insert(make_pair<Symbol, int>(Bool,   BOOLCLASSTAG));
    classtags.insert(make_pair<Symbol, int>(Str,    STRCLASSTAG));

    enterscope();
    if (cgen_debug) {
       cout << "Building CgenClassTable" << endl;
    }

    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

    // Sort class table by classtags
    bool sorted = false;
    while (!sorted) {
        sorted = true;
        for (int i = 0; i < numclasses; i++) {
            Symbol name = nodes[i]->get_name();
            int tag = classtags.at(name);
            if (i != tag) {
                CgenNode *temp = nodes[i];
                nodes[i] = nodes[tag];
                nodes[tag] = temp;
                sorted = false;
            }
        }
    }

    code();
    exitscope();
}

void CgenClassTable::install_basic_classes() {

// The tree package uses these globals to annotate the classes built below.
//curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
    new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                Basic, this));
  addid(SELF_TYPE,
    new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                Basic, this));
  addid(prim_slot,
    new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                Basic, this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
       No_class,
       append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
       filename),
    Basic, this), OBJCLASSTAG);

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
       filename),        
    Basic, this), IOCLASSTAG);

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
        Object,
            single_Features(attr(val, prim_slot, no_expr())),
        filename),
     Basic, this), INTCLASSTAG);

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
      Basic, this), BOOLCLASSTAG);

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
         Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
                   single_Formals(formal(arg, Str)),
                   Str, 
                   no_expr()))),
        single_Features(method(substr, 
                   append_Formals(single_Formals(formal(arg, Int)), 
                          single_Formals(formal(arg2, Int))),
                   Str, 
                   no_expr()))),
         filename),
        Basic, this), STRCLASSTAG);

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd, int classtag) {
    Symbol name = nd->get_name();

    if (probe(name)) {
        return;
    }

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nodes.push_back(nd);
    numclasses++;
    addid(name, nd);
    
    // Main class object has a special tag
    if (SymsEqual(nd->get_name(), Main)) {
        classtag = MAINCLASSTAG;
    }
    classtags.insert(make_pair<Symbol, int>(name, classtag));
}

void CgenClassTable::install_classes(Classes cs) {
    int offset = MAINCLASSTAG;
    for (int i = cs->first(); cs->more(i); i = cs->next(i)) {
        install_class(new CgenNode(cs->nth(i), NotBasic, this), offset + i + 1);
    }
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree() {
    for (int i = 0; i < numclasses; i++) {
        set_relations(nodes[i]);
    }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd) {
    Symbol classname = nd->get_name();
    CgenNode *parent_node = probe(nd->get_parent());
    Symbol parentname = parent_node->get_name();
    nd->set_parentnd(parent_node);
    graph.insert(make_pair<Symbol, Symbol>(classname, parentname));
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n) {
    children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p) {
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

// Codes the program in its entirety. Includes all class initializations and methods, as well
// as any constants, dispatch tables, prototype objects, or symbol tables required.
void CgenClassTable::code() {
    if (cgen_debug) {
        cout << "coding global data" << endl;
    }
    code_global_data();

    if (cgen_debug) {
       cout << "choosing gc" << endl;
    }
    code_select_gc();

    if (cgen_debug) {
        cout << "coding constants" << endl;
    }
    code_constants();

    CodeClassnameTables();
    CodeObjectTables();
    CodeDispTables();
    CodePrototypes();

    if (cgen_debug) {
        cout << "coding global text" << endl;
    }
    code_global_text();

    for (int i = 0; i < numclasses; i++) {
        int offset = DEFAULT_OBJFIELDS;
        Symbol classname = nodes[i]->get_name();
        
        // Populate class attribute table
        unordered_map<Symbol, int> *attrs = new unordered_map<Symbol, int>();
        classattrtable.insert(make_pair<Symbol, unordered_map<Symbol, int> *>(classname, attrs));
        FillClassAttrs(nodes[i], attrs, offset);
        
        // Populate class method table
        offset = 0;
        unordered_map<Symbol, int> *methods = new unordered_map<Symbol, int>();
        classfntable.insert(make_pair<Symbol, unordered_map<Symbol, int> *>(classname, methods));
        FillClassMethods(nodes[i], methods, offset);
    }

    SymbolTable<Symbol, int> locals;
    for (int i = 0; i < numclasses; i++) {
        int stackoffset = -1;

        // Get self environment
        Symbol classname = nodes[i]->get_name();
        unordered_map<Symbol, int> *attrs = classattrtable.at(classname);

        // Enter class scope
        locals.enterscope();

        // Code init method, then the rest of classname's methods
        CodeInit(nodes[i], attrs, locals, stackoffset);
        if (!nodes[i]->basic()) {
            CodeMethods(nodes[i], attrs, locals);
        }

        // Exit class scope
        locals.exitscope();
    }
}

// Populates a class's attribute table, including all attributes inherited from its parents.
// This attribute table is stored in the global environment table.
void CgenClassTable::FillClassAttrs(CgenNode *node, unordered_map<Symbol, int> *attrs, int &offset) {
    if (node == NULL) {
        return;
    }
    FillClassAttrs(node->get_parentnd(), attrs, offset);

    Features features = node->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        attr_class *attr = dynamic_cast<attr_class*>(features->nth(i));
        if (attr) {
            Symbol name = attr->get_name();
            attrs->insert(make_pair<Symbol, int>(name, offset));
            offset++;
        }
    }
}

// Populates a class's method table, including all methods inherited from its parents.
// This method table is stored in the global environment table.
void CgenClassTable::FillClassMethods(CgenNode *node, unordered_map<Symbol, int> *methods, int &offset) {
    if (node == NULL) {
        return;
    }
    FillClassMethods(node->get_parentnd(), methods, offset);

    Features features = node->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        method_class *method = dynamic_cast<method_class*>(features->nth(i));
        if (method) {
            Symbol name = method->get_name();
            
            // Don't add duplicates
            if (methods->find(name) == methods->end()) {
                methods->insert(make_pair<Symbol, int>(name, offset));
                offset++;
            }
        }
    }
}

// Creates a class name table, which point to the string constants for each 
// class's name.
void CgenClassTable::CodeClassnameTables() {
    // Code a class name table
    str << CLASSNAMETAB << LABEL;
    for (int i = 0; i < numclasses; i++) {
        // Lookup on class name
        char *name = nodes[i]->get_name()->get_string();
        str << WORD; 
        stringtable.lookup_string(name)->code_ref(str);
        str << endl;
    }
}

// Helper method to code all object tables. These refer to each class's
// prototype label and init label.
void CgenClassTable::CodeObjectTables() {
    // Code an object name table, containing references to prototypes and init methods
    str << CLASSOBJTAB << LABEL;
    for (int i = 0; i < numclasses; i++) {
        // Lookup on class name
        Symbol name = nodes[i]->get_name();
        str << WORD; emit_protobj_ref(name, str); str << endl;
        str << WORD; emit_init_ref(name, str); str << endl;
    }
}

// Helper method to code all dispatch tables.
void CgenClassTable::CodeDispTables() {
    // Code a dispatch table for each class
    for (int i = 0; i < numclasses; i++) {
        // Get all methods of a class
        Symbol name = nodes[i]->get_name();
        unordered_map<Symbol, Symbol> classmethods;
        vector<Symbol> methodnames;
        GetAllMethods(nodes[i], classmethods, methodnames);

        // Emit classname label and table
        emit_disptable_ref(name, str); str << LABEL;
        EmitDispTable(classmethods, methodnames);
    }
}

// Gets all methods of a class, filling information into a table and a vector. The map
// links methods to their classnames, and a vector is filled with the methods in the order that
// they are encountered. 
void CgenClassTable::GetAllMethods(CgenNode *node, unordered_map<Symbol, Symbol> &classmethods, vector<Symbol> &methodnames) {
    if (node == NULL) {
        return;
    }
    // Recursive call to parent
    GetAllMethods(node->get_parentnd(), classmethods, methodnames);

    Symbol classname = node->get_name();
    Features features = node->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        method_class *method = dynamic_cast<method_class *>(features->nth(i));
        if (method) {
            // Don't add duplicates to the vector
            Symbol methodname = method->get_name();
            if (classmethods.find(methodname) == classmethods.end()) {
                methodnames.push_back(methodname);
            }
            classmethods[methodname] = classname;
        }
    }
}

// Emits a dispatch table for a class. Includes all methods inherited
// from its parent classes, with older methods at the top and newest methods
// at the bottom.
void CgenClassTable::EmitDispTable(unordered_map<Symbol, Symbol> classmethods, vector<Symbol> methodnames) {
    for (auto it = methodnames.begin(); it != methodnames.end(); it++) {
        Symbol classname = classmethods.at(*it);
        Symbol methodname = *it;
        str << WORD; emit_method_ref(classname, methodname, str); str << endl;
    }
}

// Codes the object skeleton, or prototype, for each class. A prototype
// includes, at a minimum, its class tag, size, and dispatch table pointer. 
// A prototype contains as many attributes as its parents' and any additionally
// defined attributes in self.
void CgenClassTable::CodePrototypes() {
    for (int i = 0; i < numclasses; i++) {
        int size = DEFAULT_OBJFIELDS;
        Symbol name = nodes[i]->get_name();
        vector<attr_class *> attributes;
        
        // Get parent classes' attributes and self attributes
        GetAttributes(nodes[i], attributes, size);

        // Emit prototype skeleton
        str << WORD << GCTAG << endl
            << name << PROTOBJ_SUFFIX << LABEL
            << WORD << classtags[name] << endl
            << WORD << size << endl
            << WORD << name->get_string() << DISPTAB_SUFFIX << endl;
        
        // Emit all attributes
        for (auto attr = attributes.begin(); attr != attributes.end(); attr++) {
            Symbol type = (*attr)->get_type_decl();
            str << WORD;
            if (SymsEqual(type, Int)) {
                (inttable.lookup_string("0"))->code_ref(str);
            } else if (SymsEqual(type, Bool)) {
                falsebool.code_ref(str);
            } else if (SymsEqual(type, Str)) {
                (stringtable.lookup_string(""))->code_ref(str);
            } else {
                str << 0;
            }
            str << endl;
        }
    }
}

// Codes the initialization method for a class, which initializes attributes
// to the expression given by attr->get_init(). If initialization expression
// is of class no_expr(), nothing is stored into the attribute.
void CgenClassTable::CodeInit(CgenNode *node, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    // Emit label
    Symbol classname = node->get_name();
    Symbol filename = node->get_filename();
    emit_init_ref(classname, str); str << LABEL;
    int offset;

    // Code prologue
    EmitPrologue();

    // Save self pointer and call parent's init method
    Symbol parentname = node->get_parentnd()->get_name();
    if (!SymsEqual(No_class, parentname)) {
        str << JAL; emit_init_ref(parentname, str); str << endl;
    }

    // Emit code for attribute expressions
    if (!node->basic()) {
        Features features = node->get_features();
        for (int i = features->first(); features->more(i); i = features->next(i)) {
            attr_class *attr = dynamic_cast<attr_class*>(features->nth(i));
            if (attr) {
                // Get attribute type and offset
                Symbol attrtype = attr->get_type_decl();
                offset = attrs->at(attr->get_name());
                
                // If initialization expression is no_expr, then the value stored will be
                // 0, false, or "" for primitive types, and void otherwise
                if (!dynamic_cast<no_expr_class*>(attr->get_init())) {
                    // Attribute has initialization expression
                    CodeExpression(attr->get_init(), classname, filename, attrs, locals, stackoffset);
                    emit_store(ACC, offset, SELF, str);
                } else if (SymsEqual(attrtype, Int)) {
                    // Int without initialization
                    emit_load_int(ACC, inttable.lookup_string("0"), str);
                    emit_store(ACC, offset, SELF, str);
                } else if (SymsEqual(attrtype, Bool)) {
                    // Bool without initialization
                    emit_load_bool(ACC, falsebool, str);
                    emit_store(ACC, offset, SELF, str);
                } else if (SymsEqual(attrtype, Str)) {
                    // String without initialization
                    emit_load_string(ACC, stringtable.lookup_string(""), str);
                    emit_store(ACC, offset, SELF, str);
                }
            }
        }
    }

    // Move self into accumulator and emit epilogue
    emit_move(ACC, SELF, str);
    EmitEpilogue(0);
}

// Codes all the methods of a class. 
void CgenClassTable::CodeMethods(CgenNode *node, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals) {
    Symbol classname = node->get_name();
    Symbol filename = node->get_filename();
    Features features = node->get_features();
    
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        method_class *method = dynamic_cast<method_class*>(features->nth(i));
        if (method) {
            CodeMethod(method, classname, filename, attrs, locals);
        }
    }
}

// Emits code for a single method in a class. Adds the formal parameters to the local variable table, and then
// evaluates the expression body of method.
void CgenClassTable::CodeMethod(method_class *method, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals) {
    Expression expr = method->get_expr();
    Formals formals = method->get_formals();
    Symbol methodname = method->get_name();
    int offset = formals->len() + LASTPARAMOFFSET - 1;
    int stackoffset = -1;

    // Emit the method label and prologue
    emit_method_ref(classname, methodname, str);
    str << LABEL;
    EmitPrologue();

    // Enter method scope
    locals.enterscope();

    // Calculate parameter offsets from frame pointer, which always points to return address.
    // Parameters are pushed onto the stack in the order that they are declared, so first param
    // is at the largest address and last param is at 12($fp).
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Symbol paramname = ((formal_class*)formals->nth(i))->get_name();
        locals.addid(paramname, new int(offset));
        offset--;
    }

    // Code the expression body
    CodeExpression(expr, classname, filename, attrs, locals, stackoffset);
    
    // Restore frame, self, and return pointers and pop parameters off the stack
    EmitEpilogue(formals->len());

    // Exit method scope
    locals.exitscope();
}

CgenNodeP CgenClassTable::root() {
    return probe(Object);
}

// Emits the prologue for a function declaration. Stores the 
// frame pointer, self pointer, and return address on the stack,
// to be recovered in the epilogue.
void CgenClassTable::EmitPrologue() {
    // Store pointers and increment stack
    emit_addiu(SP, SP, -3 * WORD_SIZE, str);
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    emit_addiu(FP, SP, WORD_SIZE, str);
    emit_move(SELF, ACC, str);
}

// Emits the epilogue for a function declaration. Loads the 
// frame pointer, self pointer, and return address from the stack,
// which were saved in the prologue, then returns from the function.
void CgenClassTable::EmitEpilogue(int numparams) {
    // Load pointers, decrement stack, pop parameters off stack, and return
    emit_load(FP, 3, SP, str);
    emit_load(SELF, 2, SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, (3 + numparams) * WORD_SIZE, str);
    str << RET << endl;
}

// Gets a list of a class's attributes, including attributes inherited from
// its parents.
void CgenClassTable::GetAttributes(CgenNode *node, vector<attr_class *> &attributes, int &size) {
    if (node == NULL) {
        return;
    }
    // Recursive call to parent class
    GetAttributes(node->get_parentnd(), attributes, size);

    // Add all attributes
    Features features = node->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        attr_class *attr = dynamic_cast<attr_class *>(features->nth(i));
        if (attr != NULL) {
            attributes.push_back(attr);
            size++;
        }
    }
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
    class__class((const class__class &) *nd),
    parentnd(NULL),
    children(NULL),
    basic_status(bstatus) 
{
    stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

// Determines the type of expression passed in and dispatches the appropriate
// code emission function at each node. 
void CgenClassTable::CodeExpression(Expression expr, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    // Why doesn't C++ have reflection...
    assign_class *assign = dynamic_cast<assign_class*>(expr);
    static_dispatch_class *static_dispatch = dynamic_cast<static_dispatch_class*>(expr);
    dispatch_class *dispatch = dynamic_cast<dispatch_class*>(expr);
    cond_class *cond = dynamic_cast<cond_class*>(expr);
    loop_class *loop = dynamic_cast<loop_class*>(expr);
    typcase_class *typcase = dynamic_cast<typcase_class*>(expr);
    block_class *block = dynamic_cast<block_class*>(expr);
    let_class *let = dynamic_cast<let_class*>(expr);
    plus_class *plus = dynamic_cast<plus_class*>(expr);
    sub_class *sub = dynamic_cast<sub_class*>(expr);
    mul_class *mul = dynamic_cast<mul_class*>(expr);
    divide_class *div = dynamic_cast<divide_class*>(expr);
    comp_class *comp = dynamic_cast<comp_class*>(expr);
    lt_class *lt = dynamic_cast<lt_class*>(expr);
    leq_class *leq = dynamic_cast<leq_class*>(expr);
    eq_class *eq = dynamic_cast<eq_class*>(expr);
    neg_class *neg = dynamic_cast<neg_class*>(expr);
    int_const_class *int_const = dynamic_cast<int_const_class*>(expr);
    bool_const_class *bool_const = dynamic_cast<bool_const_class*>(expr);
    string_const_class *string_const = dynamic_cast<string_const_class*>(expr);
    new__class *new_ = dynamic_cast<new__class*>(expr);
    isvoid_class *isvoid = dynamic_cast<isvoid_class*>(expr);
    no_expr_class *no_expr = dynamic_cast<no_expr_class*>(expr);
    object_class *object = dynamic_cast<object_class*>(expr);

    // Big ugly if statement
    if (assign) {
        AssignCode(assign, classname, filename, attrs, locals, stackoffset);
    } else if (static_dispatch) {
        StaticDispatchCode(static_dispatch, classname, filename, attrs, locals, stackoffset);
    } else if (dispatch) {
        DispatchCode(dispatch, classname, filename, attrs, locals, stackoffset);
    } else if (cond) {
        CondCode(cond, classname, filename, attrs, locals, stackoffset);   
    } else if (loop) {
        LoopCode(loop, classname, filename, attrs, locals, stackoffset);
    } else if (typcase) {
        TypcaseCode(typcase, classname, filename, attrs, locals, stackoffset);
    } else if (block) {
        BlockCode(block, classname, filename, attrs, locals, stackoffset);
    } else if (let) {
        LetCode(let, classname, filename, attrs, locals, stackoffset);
    } else if (plus) {
        PlusCode(plus, classname, filename, attrs, locals, stackoffset);
    } else if (sub) {
        SubCode(sub, classname, filename, attrs, locals, stackoffset);
    } else if (mul) {
        MulCode(mul, classname, filename, attrs, locals, stackoffset);
    } else if (div) {
        DivCode(div, classname, filename, attrs, locals, stackoffset);
    } else if (comp) {
        CompCode(comp, classname, filename, attrs, locals, stackoffset);
    } else if (lt) {
        LessThanCode(lt, classname, filename, attrs, locals, stackoffset);
    } else if (leq) {
        LessEqualCode(leq, classname, filename, attrs, locals, stackoffset);
    } else if (eq) {
        EqualCode(eq, classname, filename, attrs, locals, stackoffset);
    } else if (neg) {
        NegCode(neg, classname, filename, attrs, locals, stackoffset);
    } else if (int_const) {
        IntConstCode(int_const, classname, filename, attrs, locals, stackoffset);
    } else if (bool_const) {
        BoolConstCode(bool_const, classname, filename, attrs, locals, stackoffset);
    } else if (string_const) {
        StringConstCode(string_const, classname, filename, attrs, locals, stackoffset);
    } else if (new_) {
        NewClassCode(new_, classname, filename, attrs, locals, stackoffset);
    } else if (isvoid) {
        IsVoidCode(isvoid, classname, filename, attrs, locals, stackoffset);
    } else if (no_expr) {
        // Do nothing
    } else if (object) {
        ObjectCode(object, classname, filename, attrs, locals, stackoffset);
    } else {
        cout << "Not implemented yet!" << endl;
    }
}

void CgenClassTable::AssignCode(assign_class *assign, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("assignment", str);

    // Evaluate expression
    CodeExpression(assign->get_expr(), classname, filename, attrs, locals, stackoffset);
    
    // Store result in correct attribute or variable
    Symbol name = assign->get_name();
    if (locals.lookup(name)) {
        // Variable is in a local scope
        // Search top scope first, then global scope
        int *offset = locals.probe(name);
        if (offset == NULL) {
            offset = locals.lookup(name);
        }
        emit_store(ACC, *offset, FP, str);
    } else {
        // Variable is in attributes
        int offset = attrs->at(name);
        emit_store(ACC, offset, SELF, str);
    }

    emit_comment("end assignment", str);
}

// Code for a static method dispatch. Creates an object of type given in the expression, and calls 
// the method specified by its parent, using its parent's dispatch table.
void CgenClassTable::StaticDispatchCode(static_dispatch_class *static_dispatch, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("static dispatch", str);

    // Information about class, method, filename, and object type
    Symbol methodname = static_dispatch->get_name();
    Expression expr = static_dispatch->get_expr();
    Symbol caller = static_dispatch->get_type_name();
    Expressions args = static_dispatch->get_actual();
    int offset;

    // Arguments are pushed to stack in the order they're given
    for (int i = args->first(); args->more(i); i++) {
        CodeExpression(args->nth(i), classname, filename, attrs, locals, stackoffset);
        emit_push(ACC, str);
    }

    // Evaluate expression to create object
    CodeExpression(expr, classname, filename, attrs, locals, stackoffset);

    // Do a void check on object
    emit_bne(ACC, ZERO, labelnum, str);

    // "Object = void" branch
    // Load filename and line number, then call _dispatch_abort
    emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), str);
    emit_load_imm(T1, 1, str);
    emit_jal(DISPABORT, str);

    // Emit false branch label and increment labelnum
    emit_label_def(labelnum, str);
    labelnum++;

    // Call method from caller prototype, not self
    offset = (classfntable.at(caller))->at(methodname);
    emit_partial_load_address(T1, str); emit_protobj_ref(caller, str); str << endl;
    emit_load(T1, DISPTABLE_OFFSET, T1, str);
    emit_load(T1, offset, T1, str);
    emit_jalr(T1, str);

    emit_comment("end static dispatch", str);
}

void CgenClassTable::DispatchCode(dispatch_class *dispatch, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("dispatch", str);

    // Information about class, method, filename, and object type
    Symbol methodname = dispatch->get_name();
    Expression expr = dispatch->get_expr();
    Symbol caller = expr->get_type();
    Expressions args = dispatch->get_actual();
    int offset;

    // Arguments are pushed to stack in the order they're given
    for (int i = args->first(); args->more(i); i++) {
        CodeExpression(args->nth(i), classname, filename, attrs, locals, stackoffset);
        emit_push(ACC, str);
    }

    // Evaluate expression to create object
    CodeExpression(expr, classname, filename, attrs, locals, stackoffset);

    // Do a void check on object
    emit_bne(ACC, ZERO, labelnum, str);

    // "Object = void" branch
    // Load filename and line number, then call _dispatch_abort
    emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), str);
    emit_load_imm(T1, 1, str);
    emit_jal(DISPABORT, str);

    // Emit false branch label and increment labelnum
    emit_label_def(labelnum, str);
    labelnum++;
    
    // Get object type; if SELF_TYPE, type is classname
    if (SymsEqual(caller, SELF_TYPE)) {
        caller = classname;
    }

    // Load offset for dispatch table, then offset for method into temp register, and jump
    offset = (classfntable.at(caller))->at(methodname);
    emit_load(T1, DISPTABLE_OFFSET, ACC, str);
    emit_load(T1, offset, T1, str);
    emit_jalr(T1, str);

    emit_comment("end dispatch", str);
}

void CgenClassTable::CondCode(cond_class *cond, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("if statement", str);
    int truelabel;
    int falselabel;

    // Evaluate predicate expression
    CodeExpression(cond->get_pred(), classname, filename, attrs, locals, stackoffset);

    // Emit true and false branches
    emit_load_bool(T1, truebool, str);
    emit_beq(ACC, T1, labelnum, str);
    truelabel = labelnum; 
    labelnum++;

    // False branch (break to falselabel)
    CodeExpression(cond->get_else(), classname, filename, attrs, locals, stackoffset);
    falselabel = labelnum;
    emit_break(falselabel, str);
    labelnum++;
    
    // True branch (truelabel)
    emit_label_def(truelabel, str);
    CodeExpression(cond->get_then(), classname, filename, attrs, locals, stackoffset);
    emit_label_def(falselabel, str);

    emit_comment("end if statement", str);
}

void CgenClassTable::LoopCode(loop_class *loop, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("loop", str);

    // Emit loop label
    int looplabel = labelnum;
    int predlabel = labelnum + 1;
    emit_break(predlabel, str);
    emit_label_def(looplabel, str);
    labelnum += 2;

    // Code body expression and emit loop jump
    CodeExpression(loop->get_body(), classname, filename, attrs, locals, stackoffset);
    emit_label_def(predlabel, str);

    // Code predicate and check if condition holds
    CodeExpression(loop->get_pred(), classname, filename, attrs, locals, stackoffset);
    emit_load_bool(T1, truebool, str);
    emit_beq(ACC, T1, looplabel, str);
    
    // Emit a void object into accumulator
    emit_load_imm(ACC, 0, str);

    emit_comment("end loop", str);
}

void CgenClassTable::TypcaseCode(typcase_class *typcase, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("case", str);

    // There are multiple labels to keep track of: the first branch label, two panic labels,
    // and an exit label. The exit label is the default exit point, the first panic label
    // is jumped to if there is no match, and the second panic label is jumped to if case is called
    // on a void object.
    Cases cases = typcase->get_cases();
    Symbol casetype = typcase->get_expr()->get_type();
    Symbol temp = casetype;
    int firstlabel = labelnum;
    int nomatchlabel = labelnum + cases->len();
    int voidlabel = nomatchlabel + 1;
    int exitlabel = voidlabel + 1;

    // Code expression and create an object
    CodeExpression(typcase->get_expr(), classname, filename, attrs, locals, stackoffset);
    emit_beqz(ACC, voidlabel, str);
    emit_load(T1, TAG_OFFSET, ACC, str);
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        // Load protobj classtag into T2 and check against T1, with corresponding label
        Symbol type = ((branch_class*)cases->nth(i))->get_type_decl();
        emit_partial_load_address(T2, str); emit_protobj_ref(type, str); str << endl;
        emit_load(T2, TAG_OFFSET, T2, str);
        emit_beq(T1, T2, labelnum + i, str);
    }

    // Check class and each of its base classes
    while (!SymsEqual(temp, Object) && graph.find(temp) != graph.end()) {
        // Check type of object in accumulator using classtags
        emit_partial_load_address(T1, str); emit_protobj_ref(temp, str); str << endl;
        emit_load(T1, TAG_OFFSET, T1, str);
        
        for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
            // Load protobj classtag into T2 and check against T1, with corresponding label
            Symbol type = ((branch_class*)cases->nth(i))->get_type_decl();
            emit_partial_load_address(T2, str); emit_protobj_ref(type, str); str << endl;
            emit_load(T2, TAG_OFFSET, T2, str);
            emit_beq(T1, T2, labelnum + i, str);
        }
        temp = graph.at(temp);
    }
    // No match, jump to panic label
    emit_break(nomatchlabel, str);

    // Label number should be one greater than exit
    labelnum = exitlabel + 1;

    // Create multiple labels for each branch
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        branch_class *branch = (branch_class*)cases->nth(i);
        Symbol name = branch->get_name();
        
        // Emit branch label
        emit_label_def(firstlabel + i, str);
        
        // Each branch has its own scope
        locals.enterscope();

        // Push id onto the stack
        locals.addid(name, new int(stackoffset));
        emit_push(ACC, str);
        stackoffset--;

        // Code the expression
        CodeExpression(branch->get_expr(), classname, filename, attrs, locals, stackoffset);

        // Decrement stack and exit scope
        emit_addiu(SP, SP, 4, str);
        stackoffset++;
        locals.exitscope();

        // Jump to exit label, stored in labelnum
        emit_break(exitlabel, str);
    }
    // Emit panic code
    emit_label_def(nomatchlabel, str);
    str << JAL << CASEABORT1 << endl;
    emit_label_def(voidlabel, str);
    emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), str);
    emit_load_imm(T1, 1, str);
    str << JAL << CASEABORT2 << endl;

    // Emit exit label
    emit_label_def(exitlabel, str);

    emit_comment("end case", str);
}

void CgenClassTable::BlockCode(block_class *block, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("block", str);

    // Enter block scope, and emit code for every line in block
    Expressions body = block->get_body();
    locals.enterscope();
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        CodeExpression(body->nth(i), classname, filename, attrs, locals, stackoffset);
    }

    // Exit block scope
    locals.exitscope();

    emit_comment("end block", str);
}

void CgenClassTable::LetCode(let_class *let, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("let", str);

    Symbol id = let->get_identifier();
    Symbol type = let->get_type_decl();

    // Enter let scope
    locals.enterscope();

    // Emit initialization expression, and store on the stack
    if (!dynamic_cast<no_expr_class*>(let->get_init())) {
        // Attribute has initialization expression
        CodeExpression(let->get_init(), classname, filename, attrs, locals, stackoffset);
        emit_push(ACC, str);
    } else if (SymsEqual(type, Int)) {
        // Int without initialization
        emit_load_int(ACC, inttable.lookup_string("0"), str);
        emit_push(ACC, str);
    } else if (SymsEqual(type, Bool)) {
        // Bool without initialization
        emit_load_bool(ACC, falsebool, str);
        emit_push(ACC, str);
    } else if (SymsEqual(type, Str)) {
        // String without initialization
        emit_load_string(ACC, stringtable.lookup_string(""), str);
        emit_push(ACC, str);
    } else {
        // Otherwise store a void object
        emit_load_imm(ACC, 0, str);
        emit_push(ACC, str);
    }
    // Store ID after coding expression
    locals.addid(id, new int(stackoffset));
    stackoffset--;

    // Code let expression body
    CodeExpression(let->get_body(), classname, filename, attrs, locals, stackoffset);

    // Pop stack
    emit_addiu(SP, SP, 4, str);
    stackoffset++;

    // Exit let scope
    locals.exitscope();

    emit_comment("end let", str);
}

void CgenClassTable::PlusCode(plus_class *plus, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("add", str);
    
    // Evaluate left and right expressions, then subtract and store in accumulator
    CodeExpression(plus->get_left(), classname, filename, attrs, locals, stackoffset);
    emit_push(ACC, str);
    stackoffset--;
    CodeExpression(plus->get_right(), classname, filename, attrs, locals, stackoffset);
    
    // Move the value of the right expression into temp register
    emit_move(T2, ACC, str);

    // Pop left expression off stack and create a copy
    emit_pop(ACC, str);
    stackoffset++;
    str << JAL; emit_method_ref(Object, copy, str); str << endl;

    // Load both integer values into T1 and T2
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, str);
    emit_load(T2, DEFAULT_OBJFIELDS, T2, str);

    // Evaluate and move into accumulator
    emit_add(T1, T1, T2, str);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, str);

    emit_comment("end add", str);
}

void CgenClassTable::SubCode(sub_class *sub, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("subtract", str);
    
    // Evaluate left and right expressions, then subtract and store in accumulator
    CodeExpression(sub->get_left(), classname, filename, attrs, locals, stackoffset);
    emit_push(ACC, str);
    stackoffset--;
    CodeExpression(sub->get_right(), classname, filename, attrs, locals, stackoffset);
    
    // Move the value of the right expression into temp register
    emit_move(T2, ACC, str);

    // Pop left expression off stack and create a copy
    emit_pop(ACC, str);
    stackoffset++;
    str << JAL; emit_method_ref(Object, copy, str); str << endl;

    // Load both integer values into T1 and T2
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, str);
    emit_load(T2, DEFAULT_OBJFIELDS, T2, str);

    // Evaluate and move into accumulator
    emit_sub(T1, T1, T2, str);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, str);

    emit_comment("end sub", str);
}

void CgenClassTable::MulCode(mul_class *mul, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("mul", str);
    
    // Evaluate left and right expressions, then subtract and store in accumulator
    CodeExpression(mul->get_left(), classname, filename, attrs, locals, stackoffset);
    emit_push(ACC, str);
    stackoffset--;
    CodeExpression(mul->get_right(), classname, filename, attrs, locals, stackoffset);
    
    // Move the value of the right expression into temp register
    emit_move(T2, ACC, str);

    // Pop left expression off stack and create a copy
    emit_pop(ACC, str);
    stackoffset++;
    str << JAL; emit_method_ref(Object, copy, str); str << endl;

    // Load both integer values into T1 and T2
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, str);
    emit_load(T2, DEFAULT_OBJFIELDS, T2, str);

    // Evaluate and move into accumulator
    emit_mul(T1, T1, T2, str);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, str);

    emit_comment("end mul", str);
}

void CgenClassTable::DivCode(divide_class *div, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("div", str);
    
    // Evaluate left and right expressions, then subtract and store in accumulator
    CodeExpression(div->get_left(), classname, filename, attrs, locals, stackoffset);
    emit_push(ACC, str);
    stackoffset--;
    CodeExpression(div->get_right(), classname, filename, attrs, locals, stackoffset);
    
    // Move the value of the right expression into temp register
    emit_move(T2, ACC, str);

    // Pop left expression off stack and create a copy
    emit_pop(ACC, str);
    stackoffset++;
    str << JAL; emit_method_ref(Object, copy, str); str << endl;

    // Load both integer values into T1 and T2
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, str);
    emit_load(T2, DEFAULT_OBJFIELDS, T2, str);

    // Evaluate and move into accumulator
    emit_div(T1, T1, T2, str);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, str);

    emit_comment("end div", str);
}

void CgenClassTable::NegCode(neg_class *neg, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("neg", str);
    
    // Evaluate the expression
    CodeExpression(neg->get_expr(), classname, filename, attrs, locals, stackoffset);

    // Create a copy of the value in the accumulator
    str << JAL; emit_method_ref(Object, copy, str); str << endl;
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, str);

    // Negate the value and store back into objfield of Int in accumulator
    emit_neg(T1, T1, str);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, str);

    emit_comment("end neg", str);
}

void CgenClassTable::LessThanCode(lt_class *lt, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("lt", str);
    
    // Evaluate left and right expressions
    CodeExpression(lt->get_left(), classname, filename, attrs, locals, stackoffset);
    emit_push(ACC, str);
    stackoffset--;
    CodeExpression(lt->get_right(), classname, filename, attrs, locals, stackoffset);

    // Store values of left and right expressions into T1 and T2, respectively
    emit_pop(T1, str);
    stackoffset++;
    emit_load(T1, DEFAULT_OBJFIELDS, T1, str);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, str);
    
    // True branch
    emit_load_bool(ACC, truebool, str);
    emit_blt(T1, T2, labelnum, str);

    // Start of false branch
    emit_load_bool(ACC, falsebool, str);
    
    // True branch skips to this label
    emit_label_def(labelnum, str);
    labelnum++;

    emit_comment("end lt", str);
}

void CgenClassTable::EqualCode(eq_class *eq, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("eq", str);
    
    Symbol inferredtype = eq->get_left()->get_type();

    // Evaluate left and right expressions
    CodeExpression(eq->get_left(), classname, filename, attrs, locals, stackoffset);
    emit_push(ACC, str);
    stackoffset--;
    CodeExpression(eq->get_right(), classname, filename, attrs, locals, stackoffset);

    // Store values of left and right expressions into T1 and T2, respectively
    emit_pop(T1, str);
    stackoffset++;
    emit_move(T2, ACC, str);

    // If the left hand side and right hand side are primitives, do an equality check.
    // Otherwise, compare their addresses.
    if (IsPrimitive(inferredtype)) {
        // Load truebool and falsebool into A0 and A1
        emit_load_bool(ACC, truebool, str);
        emit_load_bool(A1, falsebool, str);

        // Jump to equality test
        emit_jal(EQUALTEST, str);
    } else {
        // True branch
        emit_load_bool(ACC, truebool, str);
        emit_beq(T1, T2, labelnum, str);

        // Start of false branch
        emit_load_bool(ACC, falsebool, str);
        
        // True branch skips to this label
        emit_label_def(labelnum, str);
        labelnum++;
    }

    emit_comment("end eq", str);
}

void CgenClassTable::LessEqualCode(leq_class *leq, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("leq", str);
    
    // Evaluate left and right expressions
    CodeExpression(leq->get_left(), classname, filename, attrs, locals, stackoffset);
    emit_push(ACC, str);
    stackoffset--;
    CodeExpression(leq->get_right(), classname, filename, attrs, locals, stackoffset);

    // Store values of left and right expressions into T1 and T2, respectively
    emit_pop(T1, str);
    stackoffset++;
    emit_load(T1, DEFAULT_OBJFIELDS, T1, str);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, str);
    
    // True branch
    emit_load_bool(ACC, truebool, str);
    emit_bleq(T1, T2, labelnum, str);

    // Start of false branch
    emit_load_bool(ACC, falsebool, str);
    
    // True branch skips to this label
    emit_label_def(labelnum, str);
    labelnum++;

    emit_comment("end leq", str);
}

void CgenClassTable::CompCode(comp_class *comp, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("not", str);
    
    int truelabel;

    // Code expression, then reverse the (boolean) result
    CodeExpression(comp->get_expr(), classname, filename, attrs, locals, stackoffset);
    emit_load_bool(T1, truebool, str);
    emit_load_bool(T2, falsebool, str);

    // If false, load true into accumulator, otherwise jump
    emit_beq(ACC, T1, labelnum, str);
    emit_move(ACC, T1, str);
    truelabel = labelnum;
    labelnum++;
    emit_break(labelnum, str);

    // If true, load false into accumulator and proceed
    emit_label_def(truelabel, str);
    emit_move(ACC, T2, str);
    emit_label_def(labelnum, str);
    labelnum++;

    emit_comment("end not", str);
}

void CgenClassTable::IntConstCode(int_const_class *int_const, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    // Emit load int
    emit_load_int(ACC, inttable.lookup_string(int_const->get_token()->get_string()), str);
}

void CgenClassTable::BoolConstCode(bool_const_class *bool_const, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    // Emit load bool
    emit_load_bool(ACC, BoolConst(bool_const->get_val()), str);
}

void CgenClassTable::StringConstCode(string_const_class *string_const, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    // Emit load string
    emit_load_string(ACC, stringtable.lookup_string(string_const->get_token()->get_string()), str);
}

void CgenClassTable::NewClassCode(new__class *new_, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("new", str);
    
    // Load address of prototype object, then call Object.copy and type_name_init
    Symbol type = new_->get_type_name();
    if (SymsEqual(type, SELF_TYPE)) {
        // Find protobj from class object table by looping with classtag
        int looplabel = labelnum;
        int predlabel = labelnum + 1;
        labelnum += 2;

        // Get classtag
        emit_move(ACC, SELF, str);
        emit_load(T1, TAG_OFFSET, ACC, str);

        // Multiply classtag by 8 (word size * 2) to get correct prototype offset
        emit_load_imm(T2, WORD_SIZE * 2, str);
        emit_mul(T1, T1, T2, str);

        // Load object table address, then add offset
        emit_load_address(ACC, CLASSOBJTAB, str);
        emit_add(ACC, ACC, T1, str);

        // Store the method init on the stack
        emit_load(T1, 1, ACC, str);
        emit_push(T1, str);

        // Load prototype object address
        emit_load(ACC, 0, ACC, str);

        // Copy and init
        str << JAL; emit_method_ref(Object, copy, str); str << endl;
        emit_pop(T1, str);
        str << JAL << T1 << endl;
    } else {
        // Load the desired class, copy and init
        emit_partial_load_address(ACC, str); emit_protobj_ref(type, str); str << endl;
        str << JAL; emit_method_ref(Object, copy, str); str << endl;
        str << JAL; emit_init_ref(type, str); str << endl;
    }

    emit_comment("end new", str);
}

void CgenClassTable::IsVoidCode(isvoid_class *isvoid, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    emit_comment("isvoid", str);
    
    // Code expression, then check if accumulator value is zero
    CodeExpression(isvoid->get_expr(), classname, filename, attrs, locals, stackoffset);
    emit_move(T1, ACC, str);
    
    // Load true
    emit_load_bool(ACC, truebool, str);
    
    // Break if equal to zero, then emit false branch
    emit_beqz(T1, labelnum, str);
    emit_load_bool(ACC, falsebool, str);
    emit_label_def(labelnum, str);
    labelnum++;

    emit_comment("end isvoid", str);
}

void CgenClassTable::ObjectCode(object_class *object, Symbol classname, Symbol filename, const unordered_map<Symbol, int> *attrs, SymbolTable<Symbol, int> &locals, int &stackoffset) {
    // Local variables hide parameters, which in turn hide attributes
    Symbol idname = object->get_name();
    
    if (SymsEqual(idname, self)) {
        // Self object
        emit_move(ACC, SELF, str);
    } else if (locals.lookup(idname)) {
        // Local variable; get from stack
        int *offset = locals.probe(idname);
        if (offset == NULL) {
            offset = locals.lookup(idname);
        }
        emit_load(ACC, *offset, FP, str);
    } else {
        // Attribute; get from self object
        int offset = attrs->at(idname);
        emit_load(ACC, offset, SELF, str);
    }
}

// Helper function to determine if type is a primitive.
bool CgenClassTable::IsPrimitive(Symbol type) {
    return (SymsEqual(type, Int) || SymsEqual(type, Bool) || SymsEqual(type, Str));
}

// End of file
