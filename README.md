# OctoScript
Compiling:
    `make all`

Executing:
    `./toplevel.native`

Cleaning:
    `make clean`

Running Test Script:
    `./testall.sh`
    
Validation of Tests:
   The program compiles and runs appropriately if every test name, followed by "OK," is printed to stdout.


Description of tests:
* test-assign tests variable declaration and assignment
* test-if-1 tests that all branches of if statement are properly reached and that they can be nested
* test-if-2 tests that the dangling else problem is handled correctly
* test-ifexpr tests that if expressions (which return values) work correclty
* test-list tests that lists can be properly created, passed to functions, and printed out
* test-print tests that printing works correctly for all primitive types
* test-while tests that while loops work correctly
* fail-assign-1 tests that a variable declared as one type cannot then be assigned to another in a different statement
* fail-assign-2 tests that a variable declared as one type cannot then be assigned to another in the same statement
* fail-binop tests that the operands to a binary expression must be of the same type
* fail-function-1 tests that what is returned by a funciton must be of the same type as the given return type
* fail-function-2 tests that parameters must be called with the correct type and number of parameters
* fail-function-3 tests that only previously defined functions can be called
* fail-ifexpr tests that both branches of an if expression (which returns a value) must return values of the same type
* fail-list tests that list contents must be of the same type
* fail-return tests that returns can only be inside of functions
* fail-unop tests that unary operators can only be called on values of the proper types
    

Members:
* Manish Aryal, manish.aryal@tufts.edu
* Conor Gourley, conor.gourley@tufts.edu
* Danielle Lan, hao-wei.lan@tufts.edu
* Sinan Unan, sinan.unan@tufts.edu
