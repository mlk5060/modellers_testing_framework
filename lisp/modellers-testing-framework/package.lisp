;;;; package.lisp

(defpackage #:modellers-testing-framework
  (:use #:cl)
  (:export #:test
           #:assert-eq
           #:assert-equalp
           #:assert=
           #:assert-string=
           #:assert-true
           #:assert-false
           #:assert-null
           #:def-unit-tests
           #:def-process-tests
           #:def-canonical-result-tests
           #:run-unit-tests
           #:run-process-tests
           #:run-canonical-result-tests
           #:run-all-tests
           )
  )

