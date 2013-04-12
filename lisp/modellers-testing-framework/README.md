# Modellers Testing Framework #

## About this software ##

The functions in this file support use of a testing framework for 
scientific theories.  Tests are separated into three classes, as 
described in [1,2]:

1. Unit tests - for implementational details
2. Process tests - for the main processes within the theory
3. Canonical results - for the experimental results achieved by the theory

Publications describing the use of the framework include:

[1] P.C.R. Lane and F. Gobet, 'Developing reproducible and comprehensible
    computational models', _Artificial Intelligence_, 144:251--63, 2003.

[2] P.C.R. Lane and F. Gobet, 'A theory-driven testing methodology for
    developing scientific software', _Journal of Experimental and Theoretical
    Artificial Intelligence_, 24:421-56, 2012

## Instructions for use ##

Individual tests are written in one of the following forms:

- `(test THING-TO-CHECK [MESSAGE])`
- `(assert-= EXPECTED ACTUAL [MESSAGE])`
- `(assert-eq EXPECTED ACTUAL [MESSAGE])`
- `(assert-equalp EXPECTED ACTUAL [MESSAGE])`
- `(assert-string= EXPECTED ACTUAL [MESSAGE])`
- `(assert-true THING-TO-CHECK [MESSAGE])`
- `(assert-false THING-TO-CHECK [MESSAGE])`
- `(assert-null THING-TO-CHECK [MESSAGE])`

where: 

  - `THING-TO-CHECK` is a test, returning a boolean value. The test should return T if the test passed.
  - `MESSAGE` is optional, and is reported if the test is NIL or fails.
  - `EXPECTED` is the predicted value.
  - `ACTUAL` is the computed value.

To create a group of tests, use either of: 

- `(def-unit-tests NAME () ...)`
- `(def-process-tests NAME () ...)`
- `(def-canonical-result-tests NAME () ...)`

All three do the same job, which is to define a function called `NAME` which
takes no arguments, containing arbitrary code and one or more calls to `(test
.. ..)`.  The framework stores the functions in one of the three categories of
tests.

Tests can be run using either of:

- `(run-unit-tests)` evaluates every function defined using `def-unit-tests`
- `(run-process-tests)` evaluates every function defined using `def-process-tests`
- `(run-canonical-result-tests)` evaluates every function defined using `def-canonical-result-tests`
- `(run-all-tests)` evaluates all three of the above, running all tests

## License ##

Copyright (c) 2007-13, Peter Lane.

Released under Open Works License 0.9.2: <http://owl.apotheon.org>

