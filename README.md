# Modellers Testing Framework

This is a simple testing framework to support computational modellers in
separating out the scientific value of different aspects of their code.  The
code is assumed to provide an architecture (or framework), supporting the
development of different models (or simulations).  

Tests are divided into three groups:

1. *Unit tests* for low-level, implementational details of the architecture.
2. *Process tests* for functional aspects, usually linked to terms used within 
   the implemented scientific theory.
3. *Canonical-result tests* for the implemented models based on the architecture.

Further information about the testing framework may be found in:

1. P.C.R. Lane and F. Gobet, 'A theory-driven testing methodology for developing scientific software', _Journal of Experimental and Theoretical Artificial Intelligence_, 24:421-56, 2012.

2. P.C.R. Lane and F. Gobet, 'A methodology for developing computational implementations of scientific theories', _Proceedings of the Tenth International Conference on Computer Modelling & Simulation_ (IEEE Computer Society), pp.392-7, 2008.

## Implementations

This project defines implementations of the testing framework in:

1. Ruby
2. Lisp

## License

modellers\_testing\_framework is distributed under the terms of the 
[Open Works License](http://owl.apotheon.org/)

Copyright 2012, Peter Lane.

