#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <unordered_map>
#include "semant.h"
#include "utilities.h"

using std::make_pair;

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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

ClassTable::ClassTable(Classes classes) : firstname(NULL), secondname(NULL), thirdname(NULL), semant_errors(0) , error_stream(cerr) {
    // Enter global scope
    // Since the tables below only use one scope, we can
    // use the "probe" function instead of "lookup"
    classtable.enterscope();
    graph.enterscope();
    classmethodtable.enterscope();
    classattrtable.enterscope();
    
    // Install and add basic classes to table
    install_basic_classes();

    // Perform semantic analysis
    CheckSemantics(classes);
}

void ClassTable::HandleError(enum errortype error, Class_ current) {
    // Our error log and unformatted message
    char log[ERROR_MSG_LENGTH];
    const char *message = errormsgs[(int)error];

    // Determine the number of string arguments to format into error log
    switch (error) {
    case DUPLICATE_CLASS:
    case DUPLICATE_ATTR:
    case DUPLICATE_METHOD:
    case UNDECLARED_ID:
    case UNDEFINED_NEW:
    case REDEFINED_ATTR:
    case PARAMS_WRONG_NUM:
    case UNDEFINED_CLASS_DISP:
    case UNDEFINED_METHOD:
    case ARGS_WRONG_NUM:
    case UNDEFINED_STATIC_TYPE:
    case UNDEFINED_STATIC_METHOD:
    case INVOKED_ARGS_WRONG_NUM:
    case DUPLICATE_FORMAL:
    case DUPLICATE_CASE:
        snprintf(log, ERROR_MSG_LENGTH, message, firstname);
        break;
    case CYCLIC_GRAPH:
        snprintf(log, ERROR_MSG_LENGTH, message, firstname, firstname);
        break;
    case UNDEFINED_ANCESTOR:
    case PRIMITIVE_ANCESTOR:
    case STATIC_DISP_WRONG_TYPE:
        snprintf(log, ERROR_MSG_LENGTH, message, firstname, secondname);
        break;
    case ATTR_TYPE_MISMATCH:
    case METHOD_TYPE_MISMATCH:
    case BAD_PLUS:
    case BAD_SUB:
    case BAD_MUL:
    case BAD_DIV:
    case BAD_LT:
    case BAD_LEQ:
    case ID_TYPE_MISMATCH:
    case LET_VAR_TYPE_MISMATCH:
    case UNDEFINED_PARAM_TYPE:
    case REDEFINED_PARAM_TYPE:
    case REDEFINED_RETURN_TYPE:
        snprintf(log, ERROR_MSG_LENGTH, message, firstname, secondname, thirdname);
        break;
    case ARG_TYPE_MISMATCH:
        snprintf(log, ERROR_MSG_LENGTH, message, firstname, secondname, thirdname, fourthname);
        break;
    default:
        // No formatting necessary
        strncpy(log, message, ERROR_MSG_LENGTH);
        break;
    }

    // Print to stream and add to error count
    if (current == NULL) {
        semant_error() << log;
    } else {
        semant_error(current) << log;
    }
}

void ClassTable::CheckSemantics(Classes classes) {
    // File must have main defined
    bool main = false;
    bool mainmeth = false;

    // First pass: add all class names and parent names
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ current = classes->nth(i);
        Symbol name = ((class__class *) current)->get_name();
        if (IsEqual(name, Main)) {
            main = true;
        }

        if (IsEqual(name, SELF_TYPE)) {
            HandleError(SELF_TYPE_REDEFINED, current);
            return;
        }
        Symbol parent = ((class__class *) current)->get_parent();

        if (classtable.probe(name) == NULL) {
            // Add classes to class table and their parents to the inheritance graph
            classtable.addid(name, classes->nth(i));
            graph.addid(name, parent);

            // Add all class methods and attributes to the environment
            vector<method_class *> *methods = new vector<method_class *>();
            vector<attr_class *> *attrs = new vector<attr_class *>();
            Features features = ((class__class *)current)->get_features();
            for (int i = features->first(); features->more(i); i = features->next(i)) {
                method_class *method = dynamic_cast<method_class *>(features->nth(i));
                attr_class *attr = dynamic_cast<attr_class *>(features->nth(i));

                // Add either a method or an attribute
                if (method != NULL) {
                    if (main && IsEqual(main_meth, method->get_name())) {
                        mainmeth = true;
                    }
                    methods->push_back(method);
                } else if (attr != NULL) {
                    attrs->push_back(attr);
                }
            }

            // Add class map to attributes and methods in global tables
            classmethodtable.addid(name, methods);
            classattrtable.addid(name, attrs);
        } else {
            // Duplicate class definition, we can exit
            Class_ current = classes->nth(i);
            Symbol name = ((class__class *) current)->get_name();
            firstname = name->get_string();
            HandleError(DUPLICATE_CLASS, current);
            return;
        }
    }
    
    // Check for Main class definition
    if (!main) {
        HandleError(NO_MAIN, NULL);
        return;
    }

    // Check class inheritance graph for errors
    CheckInheritanceGraph(classes);
    if (semant_errors) {
        return;
    }

    // Second pass: perform semantic analysis on each class
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ current = classes->nth(i);
        CheckClass(current);
    }
}

// Checks the inheritance graph for any errors. For example, it will tell you if 
// the parent class inherited is missing in the class table, or if any of the classes
// is involved in an inheritance cycle.
void ClassTable::CheckInheritanceGraph(Classes classes) {
    // Query the inheritance graph to check if hierarchy is broken
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        // The query class and its name
        Class_ current = classes->nth(i);
        Symbol name = ((class__class *) current)->get_name();
        Symbol parent = graph.probe(name);

        // Check if the query class's parent is in the classtable
        if (!IsEqual(parent, Object) && classtable.probe(parent) == NULL) {
            firstname = name->get_string();
            secondname = parent->get_string();
            HandleError(UNDEFINED_ANCESTOR, current);
            continue;
        }

        // Classes may not inherit from primitives
        if (IsEqual(parent, Int) || IsEqual(parent, Bool) || IsEqual(parent, Str)) {
            firstname = name->get_string();
            secondname = parent->get_string();
            HandleError(PRIMITIVE_ANCESTOR, current);
            continue;
        }

        // First will advance one at a time, while second will advance two at a time
        Symbol first = name;
        Symbol second = name;
        do {
            first = graph.probe(first);
            second = graph.probe(second);

            // A lookup on a null pointer will cause a crash
            if (second == NULL) {
                break;
            }
            second = graph.probe(second);

            // If first and second meet, they have entered a cycle
            if (first != NULL && second != NULL && IsEqual(first, second)) {
                firstname = name->get_string();
                HandleError(CYCLIC_GRAPH, current);
                break;
            }
        } while (first != NULL);
    }
}

// Semantic analysis on a class. Calls analysis functions on each of its features.
void ClassTable::CheckClass(Class_ c) {
    // Enter inherited class(es') scope
    objtable.enterscope();
    
    // Add inherited attributes and methods to self object scope
    Symbol classname = ((class__class *)c)->get_name();
    Symbol parent = graph.probe(classname); 
    unordered_map<Symbol, method_class *> methods;
    while (!IsEqual(parent, Object)) {
        // Add parent attributes
        vector<attr_class *> *attrs = classattrtable.probe(parent);
        if (attrs != NULL) {
            for (auto obj = attrs->begin(); obj != attrs->end(); obj++) {
                objtable.addid((*obj)->get_name(), (*obj)->get_type_decl());
            }
        }
        
        // Add parent methods, or update redefined methods
        vector<method_class *> *temp = classmethodtable.probe(parent);
        for (auto fn = temp->begin(); fn != temp->end(); fn++) {
            if (methods.find((*fn)->get_name()) != methods.end()) {
                methods.erase((*fn)->get_name());
            }
            methods.insert(make_pair<Symbol, method_class *>((*fn)->get_name(), *fn));
        }
        // Move inheritance node towards Object
        parent = graph.probe(parent);
    }

    // Enter self scope
    objtable.enterscope();

    Features features = ((class__class *)c)->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        // Determine the type of feature using dynamic casting
        Feature current = features->nth(i);
        attr_class *attr = dynamic_cast<attr_class*>(current);
        method_class *method = dynamic_cast<method_class*>(current);

        if (attr != NULL) {
            // Semantic check on attributes
            CheckAttr(attr, c);
        } else if (method != NULL) {
            // Semantic check on methods
            CheckMethod(method, methods, c);
        }
    }

    // Exit self scope
    objtable.exitscope();

    // Exit inherited class(es') scope
    objtable.exitscope();
}

// Semantic analysis on an attribute. Checks that the return type of its
// initialization expression is the same as its declared type.
void ClassTable::CheckAttr(attr_class *attr, const Class_ c) {
    // Get name and return type
    Symbol name = attr->get_name();
    Symbol type = attr->get_type_decl();
    Expression expr = attr->get_init();
    
    // Check for redefined attributes
    if (objtable.lookup(name) != NULL) {
        firstname = name->get_string();
        HandleError(REDEFINED_ATTR, c);
    }

    // Check if attempting to rebind "self"
    if (IsEqual(name, self)) {
        HandleError(SELF_ATTR, c);
    }

    // Check for duplicate attributes
    if (objtable.probe(name) == NULL) {
        objtable.addid(name, type);
    } else {
        firstname = name->get_string();
        HandleError(DUPLICATE_ATTR, c);
    }

    // Check expression type
    Symbol exprtype = CheckExpression(expr, c);
    if (IsEqual(exprtype, self)) {
        exprtype = ((class__class *)c)->get_name();
    }

    // If expression type doesn't have SELF_TYPE(attr_type), then throw an error
    if (!IsEqual(exprtype, No_type)) {
        Symbol ancestor = LeastUpperBound(exprtype, type);
        if (!IsEqual(ancestor, type)) {
            firstname = exprtype->get_string();
            secondname = name->get_string();
            thirdname = type->get_string();
            HandleError(ATTR_TYPE_MISMATCH, c);
        }
    }
}

// Semantic analysis on a method. Adds the method to a symbol table, then performs 
// a series of checks that ensure its return type matches with the return type of 
// its enclosing expression.
void ClassTable::CheckMethod(method_class *method, unordered_map<Symbol, method_class *> &methods, const Class_ c) {
    Symbol classname = ((class__class *)c)->get_name();
    Symbol methodname = method->get_name();
    Symbol returntype = method->get_return_type();
    
    // Check for redefinitions
    CheckMethodRedefs(method, methods, c);
    
    // Enter method scope
    objtable.enterscope();
    
    // Check for undefined class types in formal parameters
    Formals formals = method->get_formals();
    Symbol duplicate = NULL;
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        formal_class *param = (formal_class *)formals->nth(i);
        Symbol name = param->get_name();
        
        // Duplicate formal
        if (IsEqual(name, duplicate)) {
            firstname = name->get_string();
            HandleError(DUPLICATE_FORMAL, c);
        }

        // Cannot redefine self as a parameter
        if (IsEqual(name, self)) {
            HandleError(SELF_FORMAL, c);
        }
        duplicate = name;
        Symbol type = param->get_type_decl();

        if (classtable.probe(type) == NULL) {
            // Parameter has undefined class
            firstname = type->get_string();
            secondname = name->get_string();
            HandleError(UNDEFINED_PARAM_TYPE, c);
        }

        // Add to object table
        objtable.addid(name, type);
    }

    // Check for match between expression type and return type
    Expression expr = method->get_expr();
    Symbol exprtype = CheckExpression(expr, c);

    // If expression type does not have SELF_TYPE(returntype), throw an error
    if (IsEqual(returntype, SELF_TYPE)) {
        if (!IsEqual(exprtype, SELF_TYPE) && !IsEqual(exprtype, self)) {
            firstname = exprtype->get_string();
            secondname = methodname->get_string();
            thirdname = returntype->get_string();
            HandleError(METHOD_TYPE_MISMATCH, c);
        }
    } else {
        if (IsEqual(exprtype, self)) {
            exprtype = ((class__class *)c)->get_name();
        }
        Symbol ancestor = LeastUpperBound(returntype, exprtype);
        if (!IsEqual(returntype, ancestor)) {
            firstname = exprtype->get_string();
            secondname = methodname->get_string();
            thirdname = returntype->get_string();
            HandleError(METHOD_TYPE_MISMATCH, c);
        }
    }

    // Exit method scope
    objtable.exitscope();
}

// Checks for method redefinitions, and ensures they have the same definition.
// In particular, this function makes sure that return types and parameters
// are the same.
void ClassTable::CheckMethodRedefs(method_class *method, unordered_map<Symbol, method_class *> &methods, const Class_ c) {
    // Do a hash map lookup
    Symbol methodname = method->get_name();
    auto fn = methods.find(methodname);
    if (fn != methods.end() && IsEqual(methodname, fn->first)) {
        // We found a match, check formals list
        method_class *inherited = fn->second;
        Formals formals = inherited->get_formals();
        Formals newformals = method->get_formals();

        // Redefined return type
        Symbol oldret = inherited->get_return_type();
        Symbol newret = method->get_return_type(); 
        if (!IsEqual(oldret, newret)) {
            firstname = methodname->get_string();
            secondname = newret->get_string();
            thirdname = oldret->get_string();
            HandleError(REDEFINED_RETURN_TYPE, c);
        }

        if (formals->len() != newformals->len()) {
            // Different number of formals declared
            firstname = methodname->get_string();
            HandleError(PARAMS_WRONG_NUM, c);
        } else {
            // Check each formal for correct type 
            int i = formals->first();
            for (; formals->more(i); i = formals->next(i)) {
                Symbol oldtype = ((formal_class *)formals->nth(i))->get_type_decl();
                Symbol newtype = ((formal_class *)newformals->nth(i))->get_type_decl();
                if (!IsEqual(oldtype, newtype)) {
                    firstname = methodname->get_string();
                    secondname = newtype->get_string();
                    thirdname = oldtype->get_string();
                    HandleError(REDEFINED_PARAM_TYPE, c);
                }
            }
        }
    }
}

// Performs semantic analysis on an expression. From the class type, infers
// the expected type, and dispatches the appropriate function. For example,
// if the incoming class type is a plus_class, it will expect all leaf nodes
// to have type Int, and dispatches CheckInt to enforce this.
Symbol ClassTable::CheckExpression(Expression expr, const Class_ c) {
    Symbol rettype;
    // Big switch case using dynamic casts
    if (dynamic_cast<string_const_class *>(expr)) {
        // String constant
        rettype = Str;
    } else if (dynamic_cast<int_const_class *>(expr)) {
        // Int constant
        rettype = Int;
    } else if (dynamic_cast<bool_const_class *>(expr)) {
        // Bool constant 
        rettype = Bool;
    } else if (dynamic_cast<plus_class *>(expr) ||
               dynamic_cast<sub_class *>(expr)  ||
               dynamic_cast<mul_class *>(expr)  ||
               dynamic_cast<divide_class *>(expr)) {
        // Int expression
        rettype = CheckInt(expr, c);
    } else if (dynamic_cast<lt_class *>(expr) ||
               dynamic_cast<eq_class *>(expr) ||
               dynamic_cast<leq_class *>(expr)) {
        // Boolean expression
        rettype = CheckBool(expr, c);   
    } else if (dynamic_cast<neg_class *>(expr)) {
        // Two's complement expression
        neg_class *neg = dynamic_cast<neg_class *>(expr);
        rettype = CheckInt(neg->get_expr(), c);
    } else if (dynamic_cast<comp_class *>(expr)) {
        // Not expression
        comp_class *comp = dynamic_cast<comp_class *>(expr);
        rettype = CheckBool(comp->get_expr(), c);
    } else if (dynamic_cast<object_class *>(expr)) {
        // Object identifier expression
        object_class *obj = dynamic_cast<object_class *>(expr);
        if (IsEqual(obj->get_name(), self)) {
            rettype = self;
        } else {
            // Check top scope before traversing all scopes
            rettype = objtable.probe(obj->get_name());
            if (rettype == NULL) {
                rettype = objtable.lookup(obj->get_name());
            }

            // Obj isn't defined anywhere
            if (rettype == NULL) {
                firstname = obj->get_name()->get_string();
                HandleError(UNDECLARED_ID, c);
                goto error;
            }
        }
    } else if (dynamic_cast<new__class *>(expr)) {
        // New expression
        new__class *new_ = dynamic_cast<new__class *>(expr);
        rettype = new_->get_typename();
        if (!IsEqual(rettype, SELF_TYPE) && classtable.probe(rettype) == NULL) {
            firstname = rettype->get_string();
            HandleError(UNDEFINED_NEW, c);
            goto error;
        }
    } else if (dynamic_cast<assign_class *>(expr)) {
        // Assign expression
        assign_class *assign = dynamic_cast<assign_class *>(expr);
        Symbol name = assign->get_name();
        Symbol expected = objtable.lookup(name);
        
        // Undeclared identifier
        if (expected == NULL) {
            firstname = name->get_string();
            HandleError(UNDECLARED_ID, c);
            goto error;
        }
        
        // Check that expression type matches declared type, or a derived type that inherits
        // declared type
        rettype = CheckExpression(assign->get_expr(), c);
        Symbol ancestor = LeastUpperBound(rettype, expected);
        if (!IsEqual(ancestor, expected)) {
            firstname = rettype->get_string();
            secondname = expected->get_string();
            thirdname = name->get_string();
            HandleError(ID_TYPE_MISMATCH, c);
        }
    } else if (dynamic_cast<cond_class *>(expr)) {
        // If statement expression
        cond_class *cond = dynamic_cast<cond_class *>(expr);

        // Check that predicate has type boolean
        Expression pred = cond->get_pred();
        Symbol predtype = CheckExpression(pred, c);
        if (!IsEqual(predtype, Bool)) {
            HandleError(IF_NOT_BOOL, c);
        }
        
        // Return the LUB of then and else expressions
        Symbol then = CheckExpression(cond->get_then(), c);
        Symbol else_ = CheckExpression(cond->get_else(), c);
        rettype = LeastUpperBound(then, else_);
    } else if (dynamic_cast<loop_class *>(expr)) {
        // Loop expression
        loop_class *loop = dynamic_cast<loop_class *>(expr);

        // Check that predicate has type boolean
        Expression pred = loop->get_pred();
        Symbol predtype = CheckExpression(pred, c);
        if (!IsEqual(predtype, Bool)) {
            HandleError(LOOP_NOT_BOOL, c);
        }

        // Return the type of loop body
        CheckExpression(loop->get_body(), c);
        rettype = Object;
    } else if (dynamic_cast<typcase_class *>(expr)) {
        // Case expression
        typcase_class *typcase = dynamic_cast<typcase_class *>(expr);

        // Get type of expression
        Symbol exprtype = CheckExpression(typcase->get_expr(), c);
        if (IsEqual(exprtype, self) || IsEqual(exprtype, SELF_TYPE)) {
            exprtype = ((class__class *)c)->get_name();
        }
        Symbol branchtype;
        Symbol duplicate = NULL;
    
        // Enter case scope
        objtable.enterscope();

        // Check each case
        Cases cases = typcase->get_cases();
        for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
            branchtype = ((branch_class *)cases->nth(i))->get_type_decl();
            for (int j = cases->next(i); cases->more(j); j = cases->next(j)) {
                // Check for duplicates
                duplicate = ((branch_class *)cases->nth(j))->get_type_decl();
                if (IsEqual(duplicate, branchtype)) {
                    firstname = branchtype->get_string();
                    HandleError(DUPLICATE_CASE, c);
                }
            }
        }
        rettype = CheckCase(((branch_class *)cases->nth(0)), c);
        for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
            branchtype = CheckCase(((branch_class *)cases->nth(i)), c);
            rettype = LeastUpperBound(branchtype, rettype);
        }

        // Exit scope and return
        objtable.exitscope();
    } else if (dynamic_cast<block_class *>(expr)) {
        // Block expression
        block_class *block = dynamic_cast<block_class *>(expr);
        Expressions body = block->get_body();

        // Enter block scope
        objtable.enterscope();

        // Return type of last expression in body
        for (int i = body->first(); body->more(i); i = body->next(i)) {
            rettype = CheckExpression(body->nth(i), c);
        }

        // Exit block scope and return
        objtable.exitscope();
    } else if (dynamic_cast<let_class *>(expr)) {
        // Let expression
        let_class *let = dynamic_cast<let_class *>(expr);

        // Enter let scope
        objtable.enterscope();
        
        // Get identifier initialization info
        Symbol id = let->get_identifier();
        if (IsEqual(id, self)) {
            HandleError(SELF_LET_BINDING, c);
            goto error;
        }
        Symbol type = let->get_type_decl();
        
        // Initialization doesn't match declared type
        Symbol actual = CheckExpression(let->get_init(), c);
        if (!IsEqual(actual, No_type) && !IsEqual(type, actual)) {
            firstname = type->get_string();
            secondname = id->get_string();
            thirdname = actual->get_string();
            HandleError(LET_VAR_TYPE_MISMATCH, c);
        }
        
        // Add identifier to table after semantic check
        objtable.addid(id, type);
        
        // Get the type of the body, then exit let scope
        Symbol last = CheckExpression(let->get_body(), c);
        objtable.exitscope();
        rettype = last;
    } else if (dynamic_cast<dispatch_class *>(expr)) {
        // Dispatch expression 
        dispatch_class *dispatch = dynamic_cast<dispatch_class *>(expr);
        Symbol classname = CheckExpression(dispatch->get_expr(), c);
        Symbol parent = classname;
        Symbol methodname = dispatch->get_name();
        Expressions actual = dispatch->get_actual();
        Formals formals;
        method_class *method;

        // Lookup method using class environment
        if (IsEqual(classname, self) || IsEqual(classname, SELF_TYPE)) {
            classname = ((class__class *)c)->get_name();
            parent = classname;
        }

        // Check if classname is defined
        if (classtable.probe(classname) == NULL) {
            firstname = classname->get_string();
            HandleError(UNDEFINED_CLASS_DISP, c);
            goto error;
        }

        while (parent != NULL) {
            method = LookupMethod(parent, methodname);
            if (method != NULL) {
                break;
            }
            parent = graph.probe(parent);
        }

        // Method not defined
        if (method == NULL) {
            firstname = methodname->get_string();
            HandleError(UNDEFINED_METHOD, c);
            goto error;
        }
        
        // Confirm that length of expressions list conforms to parameter list length
        actual = dispatch->get_actual();
        formals = method->get_formals();
        if (actual->len() != formals->len()) {
            firstname = methodname->get_string();
            HandleError(ARGS_WRONG_NUM, c);
        }
        
        // Check each method argument for semantic correctness
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            Symbol exprtype = CheckExpression(actual->nth(i), c);
            if (IsEqual(exprtype, SELF_TYPE) || IsEqual(exprtype, self)) {
                exprtype = ((class__class *)c)->get_name();
            }
            Symbol expected = ((formal_class *)formals->nth(i))->get_type_decl();
            Symbol paramname = ((formal_class *)formals->nth(i))->get_name();
            
            // Check that subexpressions have SELF_TYPE of expected
            Symbol ancestor = LeastUpperBound(exprtype, expected);
            if (!IsEqual(ancestor, expected)) {
                firstname = methodname->get_string();
                secondname = exprtype->get_string();
                thirdname = paramname->get_string();
                fourthname = expected->get_string();
                HandleError(ARG_TYPE_MISMATCH, c);
            }
        }

        // Get return type, which is class C if SELF_TYPE(C)
        rettype = method->get_return_type();
        if (!IsEqual(CheckExpression(dispatch->get_expr(), c), self) && IsEqual(rettype, SELF_TYPE)) {
            rettype = classname;
        }
    } else if (dynamic_cast<static_dispatch_class *>(expr)) {
        // Static dispatch expression 
        static_dispatch_class *dispatch = dynamic_cast<static_dispatch_class *>(expr);
        Symbol classname = CheckExpression(dispatch->get_expr(), c);
        Symbol callername = dispatch->get_type_name();
        Symbol methodname = dispatch->get_name();
        method_class *method;

        // Check if calling classname is defined
        if (classtable.probe(callername) == NULL) {
            firstname = callername->get_string();
            HandleError(UNDEFINED_STATIC_TYPE, c);
            goto error;
        }

        // Check that self object inherits from classname
        Symbol parent = classname;
        if (IsEqual(parent, self)) {
            parent = ((class__class *)c)->get_name();
            classname = SELF_TYPE;
        }
        while (!IsEqual(parent, Object) && !IsEqual(parent, callername)) {
            parent = graph.probe(parent);
        }

        if (!IsEqual(parent, callername)) {
            firstname = classname->get_string();
            secondname = callername->get_string();
            HandleError(STATIC_DISP_WRONG_TYPE, c);
        }

        // Lookup method using class environment
        parent = callername;
        while (parent != NULL) {
            method = LookupMethod(parent, methodname);
            if (method != NULL) {
                break;
            }
            parent = graph.probe(parent);
        }

        // Method not defined
        if (method == NULL) {
            firstname = methodname->get_string();
            HandleError(UNDEFINED_STATIC_METHOD, c);
            goto error;
        }
        
        // Confirm that length of expressions list conforms to parameter list length
        Expressions actual = dispatch->get_actual();
        Formals formals = method->get_formals();
        if (actual->len() != formals->len()) {
            firstname = methodname->get_string();
            HandleError(INVOKED_ARGS_WRONG_NUM, c);
        }
        
        // Check each method argument for semantic correctness
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            Symbol exprtype = CheckExpression(actual->nth(i), c);
            Symbol expected = ((formal_class *)formals->nth(i))->get_type_decl();
            Symbol paramname = ((formal_class *)formals->nth(i))->get_name();
            
            // Check that subexpressions have SELF_TYPE of expected
            Symbol ancestor = LeastUpperBound(exprtype, expected);
            if (!IsEqual(ancestor, expected)) {
                firstname = methodname->get_string();
                secondname = exprtype->get_string();
                thirdname = paramname->get_string();
                fourthname = expected->get_string();
                HandleError(ARG_TYPE_MISMATCH, c);
            }
        }

        // Get return type, which is class C if SELF_TYPE(C)
        rettype = method->get_return_type();
        if (IsEqual(methodname, copy)) {
            rettype = classname;
        }
    } else if (dynamic_cast<isvoid_class *>(expr)) {
        // Isvoid expression
        isvoid_class *isvoid = dynamic_cast<isvoid_class *>(expr);
        CheckExpression(isvoid->get_expr(), c);
        rettype = Bool;
    } else if (dynamic_cast<no_expr_class *>(expr)) {
        rettype = No_type;
    }
    if (IsEqual(rettype, self) || IsEqual(rettype, SELF_TYPE)) {
        expr->set_type(SELF_TYPE);
    } else {
        expr->set_type(rettype);
    }
    return rettype;
// Error handling: return Object
error:
    expr->set_type(Object);
    return Object;
}

// Checks a single branch of a 
Symbol ClassTable::CheckCase(branch_class *branch, const Class_ c) {
    // Check name and type information
    Symbol name = branch->get_name();
    Symbol type = branch->get_type_decl();
    objtable.addid(name, type);

    // Return inferred expression type
    Symbol exprtype = CheckExpression(branch->get_expr(), c);
    return exprtype;
}

// Checks an inferred Int expression for correctness. For example,
// plus, sub, mul, and divide trees must have subtrees with return
// type Int. To do this we traverse the tree and make sure that leaf nodes
// are of type int_const_class.
Symbol ClassTable::CheckInt(Expression expr, const Class_ c) {
    Symbol left;
    Symbol right;
    enum errortype status;
 
    if (dynamic_cast<plus_class *>(expr)) {
        // Recursive case: plus class
        plus_class *plus = dynamic_cast<plus_class *>(expr);
        left = CheckInt(plus->get_left(), c);
        right = CheckInt(plus->get_right(), c);
        if (!IsEqual(left, Int) || !IsEqual(right, Int)) {
            status = BAD_PLUS;
            goto badargs;
        }
    } else if (dynamic_cast<sub_class *>(expr)) {
        // Recursive case: sub class
        sub_class *sub = dynamic_cast<sub_class *>(expr);
        left = CheckInt(sub->get_left(), c);
        right = CheckInt(sub->get_right(), c);
        if (!IsEqual(left, Int) || !IsEqual(right, Int)) {
            status = BAD_SUB;
            goto badargs;
        }
    } else if (dynamic_cast<mul_class *>(expr)) {
        // Recursive case: mul class
        mul_class *mul = dynamic_cast<mul_class *>(expr);
        left = CheckInt(mul->get_left(), c);
        right = CheckInt(mul->get_right(), c);
        if (!IsEqual(left, Int) || !IsEqual(right, Int)) {
            status = BAD_MUL;
            goto badargs;
        }
    } else if (dynamic_cast<divide_class *>(expr)) {
        // Recursive case: div class
        divide_class *div = dynamic_cast<divide_class *>(expr);
        left = CheckInt(div->get_left(), c);
        right = CheckInt(div->get_right(), c);
        if (!IsEqual(left, Int) || !IsEqual(right, Int)) {
            status = BAD_DIV;
            goto badargs;
        }
    } else {
        // Base case: check leaf node type
        return CheckExpression(expr, c);
    }
    // Inferred return value for PLUS/SUB/MUL/DIVIDE classes
    expr->set_type(Int);
    return Int;

badargs:
    // Expression has a non-Int operand
    firstname = Int->get_string();
    secondname = left->get_string();
    thirdname = right->get_string();
    HandleError(status, c);
    return Int;
}

// Checks a Bool expression for correctness. This involves inferring
// the return type of both sides and making sure they match, followed by
// performing a check on both sides to make sure all subtrees are semantically
// correct.
Symbol ClassTable::CheckBool(Expression expr, const Class_ c) {
    Symbol left;
    Symbol right;
    enum errortype status;
    if (dynamic_cast<lt_class *>(expr)) {
        // Recursive case: less than expression
        lt_class *lt = dynamic_cast<lt_class *>(expr);
        left = CheckExpression(lt->get_left(), c);
        right = CheckExpression(lt->get_right(), c);
        
        // Both left and right expressions must have type Int
        if (!IsEqual(left, Int) || !IsEqual(right, Int)) {
            status = BAD_LT;
            goto badlteq;
        }

        // Check subtrees
        left = CheckInt(lt->get_left(), c);
        right = CheckInt(lt->get_right(), c);
    } else if (dynamic_cast<leq_class *>(expr)) {
        // Recursive case: less than or equal expression
        leq_class *leq = dynamic_cast<leq_class *>(expr);
        left = CheckExpression(leq->get_left(), c);
        right = CheckExpression(leq->get_right(), c);

        // Both left and right expressions must have type Int
        if (!IsEqual(left, Int) || !IsEqual(right, Int)) {
            status = BAD_LEQ;
            goto badlteq;
        }

        // Check subtrees
        left = CheckInt(leq->get_left(), c);
        right = CheckInt(leq->get_right(), c);
    } else if (dynamic_cast<eq_class *>(expr)) {
        // Recursive case: equal expression
        eq_class *eq = dynamic_cast<eq_class *>(expr);
        left = CheckExpression(eq->get_left(), c);
        right = CheckExpression(eq->get_right(), c);

        // Left and right types must match
        if ((IsPrimitive(left) || IsPrimitive(right)) && !IsEqual(left, right)) {
            status = ILLEGAL_COMP;
            goto illegalcomp;
        }

        // Check subtrees
        left = CheckExpression(eq->get_left(), c);
        right = CheckExpression(eq->get_right(), c);
    } else {
        // Leaf case: infer the type
        return CheckExpression(expr, c);
    }
    // Inferred return value for LT/LEQ/EQ cases
    expr->set_type(Bool);
    return Bool;

badlteq:
    // LT or LEQ expression has a non-Int expression
    firstname = Int->get_string();
    secondname = left->get_string();
    thirdname = right->get_string();
    HandleError(status, c);
    return Bool;

illegalcomp:
    // EQ expression has non-matching left and right expressions
    HandleError(status, c);
    return Bool;
}

// Checks if type is a primitive type.
bool ClassTable::IsPrimitive(Symbol type) {
    return (IsEqual(type, Int) || IsEqual(type, Bool) || IsEqual(type, Str));
}

// Looks up a method from the method symbol table. If class name can't be found
// or the looked up list has no such method, NULL is returned.
method_class *ClassTable::LookupMethod(Symbol classname, Symbol methodname) {
    vector<method_class *> *methods = classmethodtable.probe(classname);
    if (methods != NULL) {
        for (auto fn = methods->begin(); fn != methods->end(); fn++) {
            if (IsEqual((*fn)->get_name(), methodname)) {
                return (*fn);
            }
        }
    }
    return NULL;
}

// Finds the first common ancestor in the inheritance tree.
// Implementation: have to nodes follow the inheritance graph to the root (Object).
// If they meet in the middle, return that node. Otherwise calculate the offset in
// inheritance level, and rerun the loop to Object, but give the deeper class
// a head start equal to the offset.
Symbol ClassTable::LeastUpperBound(Symbol class1, Symbol class2) {
    // Start with the two nodes
    Symbol first = class1;
    Symbol second = class2;
    
    // Level offset of class1 from class2 or vice versa
    int count = 0;

    // Flag for which class is at a higher level on the graph
    bool one;

    while (!IsEqual(first, Object) && !IsEqual(second, Object)) {
        // If classes are equal, we can exit early
        if (IsEqual(first, second)) {
            return first;
        }

        // Move each pointer up one parent node in the graph, until root is reached
        first = graph.probe(first);
        second = graph.probe(second);
    }

    if (IsEqual(first, Object)) {
        // Class1 reached the root first
        one = true;
        while (!IsEqual(second, Object)) {
            if (second == NULL) {
                return Object;
            }
            second = graph.probe(second);
            count++;
        }
    } else if (IsEqual(second, Object)) {
        // Class 2 reached the root first
        one = false;
        while (!IsEqual(first, Object)) {
            if (first == NULL) {
                return Object;
            }
            first = graph.probe(first);
            count++;
        }
    }

    first = class1;
    second = class2;
    
    // Give more derived class a head start
    for (int i = 0; i < count; i++) {
        if (one) {
            second = graph.probe(second);
        } else {
            first = graph.probe(first);
        }
    }
    
    // Move each pointer up until LUB is reached
    while (!IsEqual(first, second)) {
        first = graph.probe(first);
        second = graph.probe(second);
    }
    return first;
}

void ClassTable::install_basic_classes() { 
    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // Temporary variables used to store each class's methods 
    Features features;
    vector<method_class *> *methods;

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
    class_(Object, 
           No_class,
           append_Features(
                   append_Features(
                           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                   single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
           filename);
    
    // Add Object methods to class method table
    methods = new vector<method_class *>();
    features = ((class__class *)Object_class)->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        method_class *method = dynamic_cast<method_class *>(features->nth(i));
        if (method != NULL) {
            methods->push_back(method);
        }
    }
    classmethodtable.addid(Object, methods);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
           filename);

    // Add IO methods to class method table
    features = ((class__class *)IO_class)->get_features();
    methods = new vector<method_class *>();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        method_class *method = dynamic_cast<method_class *>(features->nth(i));
        if (method != NULL) {
            methods->push_back(method);
        }
    }
    classmethodtable.addid(IO, methods);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
    class_(Int, 
           Object,
           single_Features(attr(val, prim_slot, no_expr())),
           filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
    class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
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
           filename);

    // Add String methods to class method table
    features = ((class__class *)Str_class)->get_features();
    methods = new vector<method_class *>();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        method_class *method = dynamic_cast<method_class *>(features->nth(i));
        if (method != NULL) {
            methods->push_back(method);
        }
    }
    classmethodtable.addid(Str, methods);
    
    // Add primitive classes to symbol table
    classtable.addid(Object, Object_class);
    classtable.addid(IO, IO_class);
    classtable.addid(Int, Int_class);
    classtable.addid(Bool, Bool_class);
    classtable.addid(Str, Str_class);

    // Create an inheritance graph with Object at the end
    graph.addid(IO, Object);
    graph.addid(Int, Object);
    graph.addid(Bool, Object);
    graph.addid(Str, Object);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c) {
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t) {
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error() {
    semant_errors++;                            
    return error_stream;
} 

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the 'type' field in each Expression node.
        (see 'tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    // Unit tests
}
