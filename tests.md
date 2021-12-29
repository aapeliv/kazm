# Kazm Tests 

Categorized by functionality. 
Cases that are meant to pass are named test-\*.kazm.

Cases that are meant to fail are named fail-\*.kazm.

### Syntax

**fail-failed_test_name.kazm** What is the test doing wrong  

**test-passed_test_name.kazm** what the test is doing


### Arrays -- Fail Cases 
**fail-arr_assign_out_of_bounds.kazm** assign to negative array index 

**fail-arr_index.kazm** assign to array index exceeding array length 

**fail-arr_out_of_bounds.kazm** access array index exceeding array length 

**fail-arr_wrong_type.kazm** declare bool array init to int array 

**fail-arr_wrong_length.kazm** declare array of length 4 and initialize with array of length 3 

**fail-array_mult_types.kazm** declare int array and initialize with non-int array elements 

**fail-bad_type_arr.kazm** declare bool array and initialize to string 


### Arrays -- Pass Cases
**test-arr_assign.kazm** Print out elements of array before/after reassign 

**test-arr_decl.kazm** Arr decl without init 

**test-arr_init_decl.kazm** Arr decl with init 

**test-arr_length.kazm** Test arr.length 

**test-arr_lit.kazm** Array init after decl 

**test-arr_lit2.kazm** just array literal 

**test-arr_print.kazm** iterate over array and print 

**test-arr_read_elem.kazm** read elements of array 

**test-reassign_whole_array.kazm** reassign to new arr 

**test-char_arr2.kazm** char array and print elems and length 

**test-sorting_arr.kazm** sorting algo 

**test-string_arr.kazm** string arr


### Classes -- Fail Cases 
**fail-undefined_class.kazm** Using class type not created 

**fail-member_fxn_me_access.kazm** Try to access class data member without 'me' 


### Classes -- Pass Cases 
**test-class_arg.kazm** access data member of class passed in as fxn arg  

**test-class_assign.kazm** assign to class data member in main  

**test-class_constructors.kazm** class xtor

**test-class_member_access.kazm** use 'me' in class member fxn 

**test-class_method_param.kazm** class member fxn takes in class as param 

**test-class_name_mangling.kazm** diff classes, same member fxn name

**test-modify_class_in_func.kazm** pass class member as param and change member 

**test-mult_classes.kazm** mult classes 

**test-sample_method.kazm** class member fxn

**test-simple_methods.kazm** class and me

**test-team_project.kazm** first class 

### Literals -- Fail Cases
**fail-divby0.kazm** divide integer by 0 and print result 

**fail-divby0double.kazm** divide double by 0 and print result 

**fail-divbyzero.kazm** divide integer by 0 

**fail-int_type.kazm** declare int and initialize with double 

**fail-mod.kazm** modular arithmetic with double 

**fail-modby0.kazm** modular arithmetic with 0 as divisor  

**fail-type_check.kazm** declare string and initialize with int 

**fail-int_length.kazm** declare int and attempt to access its length 

### Literals -- Pass Cases
**test-add_double.kazm** Add double var to double literal 

**test-add_double2.kazm** Add positive and negative double vars 

**test-add_double3.kazm** assign double to previous double var 

**test-add_int.kazm** Add pos and neg int vars 

**test-add_int2.kazm** Add ints

**test-bool_EQ.kazm** Check boolean ==  

**test-bool_init_decl.kazm** Check boolean == 

**test-bool_NEQ.kazm** check boolean != 

**test-bool_not.kazm** check boolean !

**test-bool.kazm** check boolean !(!expr)

**test-char.kazm** char literal 

**test-char2.kazm** reassign char var 

**test-default_char.kazm** default of char  

**test-default_string.kazm** default of string 

**test-default_bool.kazm** default of bool 

**test-default_double.kazm** default of double 

**test-default_int.kazm** default of int

**test-double_init_decl.kazm** decl + init of double 

**test-double_subtract.kazm** subtraction with double

**test-int_div.kazm** division without remainder 

**test-int_divide.kazm** division with remainder (9/4) --> 2

**test-int_eq.kazm** int == 

**test-int_eq_neq.kazm** int == and != 

**test-int_GEQ.kazm** int GEQ

**test-int_greater.kazm** int > 

**test-int_init_decl.kazm** print neg num

**test-int_LEQ.kazm** int LEQ

**test-int_LT.kazm** int LT 

**test-int_mult.kazm** int * 

**test-int_neq.kazm** int NEQ

**test-int_subtract.kazm** int subtract negative num

**test-math_w_parenth.kazm** arithmetic expr with () 

**test-mod.kazm** mod 

**test-neg_double.kazm**  neg double 

**test-neg_int.kazm** neg int 

**test-simple_assign.kazm** simple assign to double 

**test-string_lit.kazm** early test hello world string lit

**test-string_var.kazm** reassign string var

**test-string.kazm** string var and lit  

### Functions -- Fail Cases
**fail-no_main.kazm** File with no 'main' fxn 

**fail-dup_fxn.kazm** Functions with same name twice 

**fail-function_type_args.kazm** call fxn w/ wrong type of param 

**fail-num_fxn_args.kazm** define function with 2 arguments and pass 3 

**fail-function_sig_mismatch.kazm** call fxn w/ wrong type of param 

**fail-fxn-param_type.kazm** use int_println with double 

**fail-wrong_return_type.kazm** declare function return type int and return double  

**fail-missing_return_type.kazm** int fxn missing return 

**fail-main_fxn_type.kazm** main declared with type string 

**fail-unknown_var_class.kazm** print undeclared int 

**fail-void_main.kazm** main declared with void return type 

**fail-fxn_in_main.kazm** fxn in fxn 


### Functions -- Pass Cases
**test-function_call_expr.kazm** fxn call with expr as int 

**test-fxn_argless.kazm** calling other func in main 

**test-fxn_int_arg.kazm** fxn with int arg called in main 

**test-hello_world.kazm** hello world

**test-no_return_main.kazm** main w no return int 

**test-simple_int_arg.kazm** early test - int param in arg

**test-void_return.kazm** call void fxn in main 

### Operators -- Fail Cases
**fail-stmts_after_return.kazm** statements after return

**fail-int_double_eq.kazm** comparing int to double 



### Operators & Control Flows -- Pass Cases
**test-and.kazm**  Boolean logic with bools 

**test-and2.kazm** Boolean logic with bools 

**test-arithmetic.kazm**  Longer math expression 

**test-empty_if.kazm** if (true) 

**test-empty_return.kazm** return;

**test-if_scope.kazm** scope of var in 'if' stmt

**test-if_scope3.kazm** scope var in 'if' stmt

**test-NEQ.kazm** int bool != 

**test-nested_if_else.kazm** nested if/else  

**test-nested_ifs.kazm** nested 'if' with next_int()

**test-nested_while.kazm** nested while 

**test-nested_ifs.kazm** nested ifs

**test-or.kazm** boolean or 

**test-or2.kazm** boolean or 

**test-or3.kazm** boolean or 

**test-return_in_if.kazm** return in if 

**test-unary_not.kazm** !

**test-while_return.kazm** return in while 

**test-while_with_var.kazm** while with var 

**test-while.kazm** while 

**test-nest_if.kazm** test nested ifs 

### Parse - Fail Cases
**fail-parse.kazm** While instead of while 

**fail-parse2.kazm** Main instead of main

**fail-comment_eof.kazm** multiline comment through eof

**fail-bad_var_name.kazm** var name uppercase

**fail-bad_class_name.kazm** class naming convention 

**fail-bad_var_name.kazm** bad var name 

**fail-comment_eof.kazm** comment through eof 

**fail-fxn_bad_name.kazm** bad fxn name

**fail-mult_decl.kazm** char a, b, c


### Scope -- Fail Cases
**fail-if_scope2.kazm** declare variable in if and use in else 

**fail-scope.kazm** declare variable in inner {} and try to use outside of scope

**fail-stmts_after_return.kazm** statements after return 


### Scope -- Pass Cases
**test-change_in_other_scope.kazm** change variable and new scope and check result

**test-extra_scope.kazm** { inside {} for new scope }

**test-scope_checking.kazm** scope check 

**test-scopes.kazm** scope check 

**fail-twice_scope_var.kazm** int a in nested scope and parent scope

### General -- Fail Cases

### General -- Pass Cases
**test-a_bit_of_everything.kazm** Generic Tester w/ classes and arrays 

**test-codegen.kazm** test codegen works 

**test-fib.kazm** fib recursion in kazm 

**test-for_loop.kazm** for loop 

**test-for.kazm** for loop with array 

**test-multi_comment.kazm** /**/ over lines 

**test-reassign.kazm** reassign 

**test-recursion.kazm** exponent recursion 

**test-sast.kazm** early testing 

**test-single_comment.kazm** single comment 

**test-whitespace.kazm** whitespace 
