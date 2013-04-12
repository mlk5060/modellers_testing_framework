;;;;
;;;; modellers-testing-framework.lisp
;;;;
;;;; Copyright 2007-13, Peter Lane.
;;;; Released under the Open Works License, http://owl.apotheon.org/
;;;;

(in-package #:modellers-testing-framework)

(let ((error-count 0)
      (total-tests 0)
      (unit-tests ())
      (process-tests ())
      (canonical-results-tests ()))
  (defun reset-error-count ()
	 (setf error-count 0
	       total-tests 0))
  (defun error-feedback ()
	 (format t "~%=== DONE: There ~a ~a error~a in ~a test~a~%" 
		 (if (= 1 error-count) "was" "were")
		 error-count 
		 (if (= 1 error-count) "" "s")
		 total-tests
		 (if (= 1 total-tests) "" "s")))

  ;; the basic test function
  (defun test (bool &optional (msg ""))
	 "If bool is true, display a dot, else the message"
	 (incf total-tests)
	 (if bool
	     (format t ".")
	     (format t "~&Error ~a: ~a~&" (incf error-count) msg)))
  
  ;; some customised test functions, to provide a more direct test syntax
  (flet ((test-compare (comp-fn expected actual &optional (msg ""))
		       (test (funcall comp-fn expected actual)
			     (format nil "Expected ~a got ~a. ~a" expected actual msg))))
	(defun assert-eq (expected actual &optional (msg ""))
		(test-compare #'eq expected actual msg))
	(defun assert-equalp (expected actual &optional (msg ""))
	       (test-compare #'equalp expected actual msg))
	(defun assert= (expected actual &optional (msg ""))
	       (test-compare #'= expected actual msg))
	(defun assert-string= (expected actual &optional (msg ""))
	       (test-compare #'string= expected actual msg)))
  (defun assert-true (bool &optional (msg ""))
	 (test bool msg))
  (defun assert-false (bool &optional (msg ""))
	 (test (not bool) msg))
  (defun assert-null (item &optional (msg ""))
	 (test (null item) msg))
  
  ;; macros for creating functions containing tests, placing the tests into groups
  (defmacro def-unit-tests (name &rest body)
	    (push name unit-tests)
	    `(defun ,name ,@body))
  (defmacro def-process-tests (name &rest body)
	    (push name process-tests)
	    `(defun ,name ,@body))
  (defmacro def-canonical-result-tests (name &rest body)
	    (push name canonical-results-tests)
	    `(defun ,name ,@body))

  ;; functions for running the tests
  (flet ((run-tests (name test-list)
		    (format t "Running ~a: " name)
		    (reset-error-count)
		    (dolist (test test-list) (funcall test))
		    (error-feedback)))
	(defun run-unit-tests () (run-tests "Unit tests" unit-tests))
	(defun run-process-tests () (run-tests "Process tests" process-tests))
	(defun run-canonical-result-tests () 
	       (run-tests "Canonical results" canonical-results-tests)))
  (defun run-all-tests ()
	 (run-unit-tests)
	 (run-process-tests)
	 (run-canonical-result-tests)))

