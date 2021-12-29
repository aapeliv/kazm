# Kazm Tests 

Categorized by functionality. 
Cases that are meant to pass are named test-\*.kazm.\
Cases that are meant to fail are named fail-\*.kazm.

### Syntax

**fail-failed_test_name.kazm** What is the test doing wrong  
**test-passed_test_name.kazm** what the test is doing

### Parse - Fail Cases
**fail-parse.kazm** While instead of while
**fail-parse2.kazm** Main instead of main



### Arrays -- Fail Cases 
**fail-arr_assign_out_of_bounds.kazm** assign to negative array index
**fail-arr_index.kazm** assign to array index exceeding array length
**fail-arr_out_of_bounds.kazm** access array index exceeding array length
**fail-arr_wrong_length.kazm** declare array of length 4 and initialize with array of length 3
**fail-array_parser_stuff.kazm** declare int array and initialize with non-int array elements
**fail-bad_type_arr.kazm** declare bool array and initialize with int array  

### Arrays -- Pass Cases
**test-arr_assign.kazm**
**test-arr_decl.kazm**
**test-arr_init_decl.kazm**
**test-arr_length.kazm**
**test-arr_lit.kazm**
**test-arr_lit2.kazm**
**test-arr_print.kazm**
**test-arr_read_elem.kazm** 
**test-array_init_decl.kazm**
**test-char_arr.kazm**
**test-sorting_arr.kazm**
**test-string_arr.kazm**

### Classes -- Fail Cases 
**fail-unknown_var_class.kazm** use uninitialized class attribute 
**fail-undefined_class.kazm** 

### Classes -- Pass Cases 
**test-class_arg.kazm**
**test-class_assign.kazm**
**test-class_constructors.kazm**
**test-class_member_access.kazm**
**test-class_method_param.kazm**
**test-class_name_mangling.kazm**
**test-modify_class_in_func.kazm**
**test-mult_classes.kazm**
**test-sample_method.kazm**
**test-simple_methods.kazm**
**test-team_project.kazm**

### Literals -- Fail Cases
**fail-divby0.kazm** divide integer by 0 and print result
**fail-divby0double.kazm** divide double by 0 and print result
**fail-divbyzero.kazm** divide integer by 0
**fail-int_type.kazm** declare int and initialize with double 
**fail-mod.kazm** modular arithmetic with double
**fail-modby0.kazm** modular arithmetic with 0 
**fail-type_check.kazm** declare string and initialize with int
**fail-int_length.kazm** declare int and attempt to access its length 

### Literals -- Pass Cases
**test-add_double.kazm** 
**test-add_double2.kazm** 
**test-add_double3.kazm** 
**test-add_int.kazm** 
**test-add_int2.kazm** 
**test-bool_EQ.kazm**
**test-bool_init_decl.kazm**
**test-bool_NEQ.kazm**
**test-bool_not.kazm**
**test-bool.kazm**
**test-char.kazm**
**test-char2.kazm**
**test-char3.kazm**
**test-char_default.kazm** test default of char is 'm'
**test-string_default.kazm** test default of string is ""
**test-default_bool.kazm**
**test-default_double.kazm**
**test-default_bool.kazm**
**test-default_int.kazm**
**test-default_bool.kazm**
**test-double_init_decl.kazm**
**test-double_subtract.kazm**
**test-int_div.kazm** division without remainder
**test-int_divide.kazm** division with remainder
**test-int_eq.kazm**
**test-int_equals.kazm** 
**test-int_GEQ.kazm**
**test-int_greater.kazm**
**test-int_init_decl.kazm** 
**test-int_LEQ.kazm**
**test-int_LT.kazm**
**test-int_mult.kazm**
**test-int_neq.kazm**
**test-int_subtract.kazm**
**test-math_w_parenth.kazm** 
**test-mod.kazm**
**test-neg_double.kazm** 
**test-neg_int.kazm**
**test-simple_assign.kazm**
**test-simple_double.kazm**
**test-string_init_decl.kazm**
**test-string_var.kazm**
**test-string.kazm** 


### Functions -- Fail Cases
**fail-dup_fxn.kazm** 
**fail-function_sig_mismatch.kazm**
**fail-num_fxn_args.kazm** define function with 2 arguments and pass 3
**fail-function_sig_mismatch.kazm**
**fail-type_mismatch.kazm** use int_println with double
**fail-unknown_var.kazm** use int_println with nonexistent variable
**fail-wrong_return_type.kazm** declare function return type int and return double 
**test-missing_return_type.kazm**

### Functions -- Pass Cases
**test-function_call_expr.kazm** 
**test-fxn_argless.kazm**
**test-fxn_int_arg.kazm**
**test-hello_world.kazm**
**test-no_return_main.kazm**
**test-simple_int_arg.kazm**
**test-void_return.kazm** 

### Operators -- Fail Cases
**test-stmts_after_return.kazm**

### Operators & Control Flows -- Pass Cases
**test-and.kazm** 
**test-and2.kazm** 
**test-arithmetic.kazm** 
**test-empty_if.kazm**
**test-empty_return.kazm**
**test-if_scope.kazm**
**test-if_scope3.kazm**
**test-NEQ.kazm** 
**test-nested_if_else.kazm** 
**test-nested_ifs.kazm**
**test-nested_while.kazm**
**test-nested_ifs.kazm**
**test-or.kazm** 
**test-or2.kazm** 
**test-or3.kazm** 
**test-return_in_if.kazm**
**test-unary_not.kazm**
**test-while_return.kazm**
**test-while_with_var.kazm**
**test-while.kazm**

### Scope -- Fail Cases
**fail-if_scope2.kazm** declare variable in if and use in else 
**fail-scope.kazm** declare variable in inner {} and try to use outside of scope

### Scope -- Pass Cases
**test-change_in_other_scope.kazm** change variable and new scope and check result
**test-extra_scope.kazm**
**test-scope_checking.kazm**
**test-scopes.kazm**

### General -- Fail Cases

### General -- Pass Cases
**test-a_bit_of_everything.kazm** 
**test-codegen.kazm**
**test-fib.kazm** recursion with Fibonacci
**test-for_loop.kazm**
**test-for.kazm**
**test-multi_comment.kazm**
**test-reassign.kazm**
**test-recursion.kazm** recursion with exponential function
**test-sast.kazm**
**test-single_comment.kazm**
**test-whitespace.kazm**