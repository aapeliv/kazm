# Kazm Tests 

Categorized by functionality. 
Cases that are meant to pass are named test-\*.kazm. 
Cases that are meant to fail are named fail-\*.kazm.

## Syntax
**fail-failed_test_name.kazm** what the test is doing wrong
**test-passed_test_name.kazm** what the test is doing

## Arrays -- Fail Cases 

**fail-arr_assign_out_of_bounds.kazm** assign to negative array index
**fail-arr_index.kazm** assign to array index exceeding array length
**fail-arr_out_of_bounds.kazm** access array index exceeding array length
**fail-arr_wrong_length.kazm** declare array of length 4 and initialize with array of length 3
**fail-array_parser_stuff.kazm** declare int array and initialize with non-int array elements

## Arrays -- Pass Cases

## Classes -- Fail Cases 
**fail-unknown_var_class.kazm** use uninitialized class attribute 

## Classes -- Pass Cases 

## Literals -- Fail Cases
**fail-divby0.kazm** divide integer by 0 and print result
**fail-divbyzero.kazm** divide integer by 0
**fail-int_type.kazm** declare int and initialize with double 
**fail-mod.kazm** modular arithmetic with double
**fail-modby0.kazm** modular arithmetic with 0 
**fail-type_check.kazm** declare string and initialize with int

## Literals -- Pass Cases

## Functions -- Fail Cases
**fail-dup_fxn.kazm** 
**fail-num_fxn_args.kazm** define function with 2 arguments and pass 3
**fail-type_mismatch.kazm** use int_println with double
**fail-unknown_var.kazm** use int_println with nonexistent variable
**fail-wrong_return_type.kazm** declare function return type int and return double 

## Functions -- Pass Cases

## Scope -- Fail Cases
**fail-if_scope2.kazm** declare variable in if and use in else 

## Scope -- Pass Cases



**fail-parser.kazm**