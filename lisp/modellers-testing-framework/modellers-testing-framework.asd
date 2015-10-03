;;;; modellers-testing-framework.asd

(asdf:defsystem #:modellers-testing-framework
  :serial t
  :description "Modellers-testing-framework provides three levels of tests: unit tests, process tests and canonical-results tests.  These are used for unit and integration testing of a computational model, making explicit the distinction between theoretically important behaviours and implementation details.  For more information see P.C.R. Lane and F. Gobet, 'A theory-driven testing methodology for developing scientific software', Journal of Experimental and Theoretical Artificial Intelligence, 24:421-56, 2012."
  :author "Peter Lane <peter.lane@bcs.org.uk>"
  :license "Open Works License 0.9.2: http://owl.apotheon.org"
  :components ((:file "package")
               (:file "modellers-testing-framework")))

