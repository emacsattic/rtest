;;; rtest-probe.el --- Code dealing with probe expressions for rtest

;; Copyright (C) 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: tools, extensions, lisp, maint
;; Version: 3.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See the rtest docs

;;; Code:

(require 'rtest-compat)
(require 'rtest-visible)
(require 'rtest-error)
(require 'rtest-parse)
(require 'rtest-grade)



;;;;;;;;;;;;;;;;;;;;;
;;;; Helper functions


(defun rtest-multiples-expected (simple-test-list)
  ""
  (some 
    #'rtest-test-uses-multiple-values-p
    simple-test-list))


(defun rtest-remove-if-not-member (the-members the-list &rest keys)
  ""

  (apply
    'remove-if-not
    #'(lambda (el)
	(memq el the-members))
    the-list
    keys))

(eval-when-compile
  (setf
    (get 'rtest-remove-if-not-member 'rtest-suite)
    '("rtest-remove-if-not-member"
       (
	 (rtest-remove-if-not-member '(A B C) '(A C E) )
	 '(A C))
       
       )))

(defun rtest-get-only-tests-in-set (set test-list)
  ""
  
  (rtest-remove-if-not-member set test-list
    :key #'rtest-clause-key))


(eval-when-compile
  (setf
    (get 'rtest-get-only-tests-in-set 'rtest-setup)
    '(progn
       (defconst rtest-canon-test-12
	 (make-rtest-clause :comparand 12)
	 "")

       (defconst rtest-canon-test-13
	 (make-rtest-clause :comparand 13)
	 "")

       (defconst rtest-canon-test-consp
	 (make-rtest-clause :predicate #'consp)
	 "")))

  (setf
    (get 'rtest-get-only-tests-in-set 'rtest-suite)
    '("rtest-get-only-tests-in-set"
       (
	 (rtest-get-only-tests-in-set rtest-grader-keys 
	   (list
	     rtest-canon-test-12
	     rtest-canon-test-consp))

	 (list
	     rtest-canon-test-12
	     rtest-canon-test-consp))
       )))





;;;;;;;;;;;;;;;;;;;;;
;;Evaluation helpers

(defun rtest-make-binding-lambda (expr bound-syms)
  ""
  (eval `(lambda(,@bound-syms) ,expr)))


(defun rtest-eval-probe-simply
  (probe bound-syms bound-vals cheat-multiples)
  ""

  (let*
    (
      (usable-probe 
	(rtest-make-binding-lambda probe bound-syms))
      
      (probe-result 
	(condition-case err 
	  (rtest-result-to-list 
	    (apply usable-probe bound-vals) 
	    cheat-multiples)

	  ;;If probe made an error, take its value to be that
	  ;;error.  The grader may be interested in exactly what
	  ;;the error is.
	  (error (list err)))))

    probe-result))

(eval-when-compile
  (setf
    (get 'rtest-eval-probe-simply 'rtest-suite)
    '("rtest-eval-probe-simply-rtest"
       ( "Simple probe without multiple-value cheating"
	 (rtest-eval-probe-simply
	   12 '() '()
	   nil)
	 '(12))

       ("Simple probe, with multiple-value cheating for Elisp"
	 (rtest-eval-probe-simply
	   12 '() '()
	   t)
	 ;;This does the rite thing for both Elisp and Common Lisp 
	 (multiple-value-list 12))
       
       ( "More complex probe expression"
	 (rtest-eval-probe-simply
	   '(+ 4 8) '() '()
	   nil)
	 '(12))
       

       ("Probe expression that uses bindings"
	 (rtest-eval-probe-simply
	   '(+ 4 a) '(a) '(8)
	   nil)
	 '(12))

       ("A probe that gets an error gives the error as its result."
	 (rtest-eval-probe-simply
	   '(car) '() '()
	   nil)
	 :map ((:predicate rtest-error-p)))

       )))


(defun rtest-one-probe-3 
  (probe-report probe test-list bound-syms bound-vals)
  "Do the probe expression and all graders.

PROBE is the test expression.
TEST-LIST should be only simple-tests.
BOUND-SYMS is a list of symbols to be bound in the probe and graders.
BOUND-VALS is a list of those symbols respective values."
  
  (let*
    (
      (simple-test-list
	(rtest-get-only-tests-in-set 
	  rtest-grader-keys test-list))
      
      (multiples-expected
	(rtest-multiples-expected simple-test-list))

      (probe-result
	(rtest-eval-probe-simply
	  probe bound-syms bound-vals multiples-expected))
      
      (failures
	(if (null test-list)
	  ;;Whether this is an error could be configurable?
	  (list
	    (rtest-make-single-failure-by-list 
	      "No tests were supplied." nil))

	  (rtest-run-grader-list  
	    probe-result simple-test-list bound-syms bound-vals))))
      
    (setf 
      (rtest-probe-failure-result-list  probe-report) 
      probe-result)
    (setf 
      (rtest-probe-failure-results-are-multiple probe-report) 
      multiples-expected)

    failures))



(eval-when-compile
  (setf
    (get 'rtest-one-probe-3 'rtest-setup)
    '(progn
       ;;Make rtest-one-probe-3 result into a grader result
       (defalias 'rtest-one-probe-3^1 'car)))

  (setf
    (get 'rtest-one-probe-3 'rtest-suite)
    '("rtest-one-probe-3"
       (rtest rtest-borrow-setup 'rtest-get-only-tests-in-set)

       ( "Simple test succeeds"
	 (rtest-one-probe-3 
	   (make-rtest-probe-failure)
	   12
	   (list
	     rtest-canon-test-12) 
	   '() '())
	 
	 nil)

       ( "Simple test fails"
	 (rtest-one-probe-3
	   (make-rtest-probe-failure)
	   12
	   (list
	     rtest-canon-test-13) 
	   '() '())
	 
	 :every 
	 (:type rtest-some-failure))

       ( "Multiple test succeeds"
	 (rtest-one-probe-3
	   (make-rtest-probe-failure)
	   12
	   (list
	     rtest-canon-test-12
	     (make-rtest-clause :predicate #'numberp)) 

	   '() '())
	 nil)

       ( "Multiple test fails in one branch"
	 (rtest-one-probe-3
	   (make-rtest-probe-failure)
	   12
	   (list
	     rtest-canon-test-13
	     (make-rtest-clause :predicate #'numberp)) 

	   '() '())

	 :test (= (length RESULT) 1)
	 :every
	 (:type rtest-some-bad-grade))

       ( "Multiple test fails in both branches"
	 (rtest-one-probe-3
	   (make-rtest-probe-failure)
	   12
	   (list
	     rtest-canon-test-13
	     rtest-canon-test-consp) 

	   '() '())

	 :test (= (length RESULT) 2)
	 :every
	 (:type rtest-some-bad-grade))

       (rtest
	 rtest-inherit
	 :inherit-from  'rtest-one-grader-1
	 :do-call
	 #'(lambda 
	     (probe-result test bound-syms bound-vals grader-func-alist)
	     (rtest-one-probe-3
	       (make-rtest-probe-failure)

	       probe-result
	       (list test)
	       bound-syms
	       bound-vals))
	 
	 :alter-output
	 #'rtest-one-probe-3^1)

       ("rtest continues even if the grader dies."
	 (rtest-one-probe-3
	   (make-rtest-probe-failure)
	   1 
	   '((:comparand . car)) '() '() )
	 :every
	 (:type rtest-some-failure))

       ("rtest doesn't throw an error out even if the probe dies."
	 (condition-case err
	   (progn
	     (rtest-one-probe-3
	       (make-rtest-probe-failure)
	       '(car) '() '() '() )
	     t)
	   (error nil))
	 t)
       
       )))




;;;;;;;;;;;;;;;;;;;;;

(defun rtest-calc-bindings (let-form)
  ""
  
  (loop
    for (symbol value-form) in let-form
    ;;Errors here will stop the single probe, appropriately.
    for value = (eval value-form)

    collect symbol into bound-syms 
    collect value  into bound-vals

    finally return (list bound-syms bound-vals)))


(eval-when-compile
  (setf
    (get 'rtest-calc-bindings 'rtest-suite)
    '("rtest-calc-bindings"

       ;;These all have to be multiples.
       ( "An empty list gives no bindings"
	 (rtest-calc-bindings '())
	 :comparand
	 '(() ()))
       
       ( "A pair is split onto the bound-syms and bound-vals lists"
	 (rtest-calc-bindings '((a 12)))
	 :comparand
	 '((a) (12)))
       
       ( "The value is evaluated, not literal"
	 (rtest-calc-bindings '((a (+ 4 8))))
	 :comparand
	 '((a) (12)))
       
       ( "Multiple pairs accumulate in order"
	 (rtest-calc-bindings '((a 12) (b 13)))
	 :comparand
	 '((a b) (12 13)))
       
       )))



(defun rtest-calc-all-bindings (binder-list)
  ""

  (loop
    for el in binder-list
    for (new-bound-syms new-bound-vals) =
    (rtest-calc-bindings (rtest-clause-value el))

    append new-bound-syms into bound-syms 
    append new-bound-vals into bound-vals
    
    finally return (list bound-syms bound-vals)))



(eval-when-compile
  (setf
    (get 'rtest-calc-all-bindings 'rtest-suite)
    '("rtest-calc-all-bindings"
       ;;These all have to be multiples
       ( "Empty lists return nils"
	 (rtest-calc-all-bindings '())
	 :comparand
	 '(nil nil))

       (rtest
	 rtest-inherit
	 :docstring    "Basically does rtest-calc-bindings on members"
	 :inherit-from 'rtest-calc-bindings
	 :do-call 
	 #'(lambda (let-form)
	     (rtest-calc-all-bindings
	       (list
		 (make-rtest-clause :let let-form)))))

       ( "Let forms with multiple elements return proper bindings"
	 (rtest-calc-all-bindings
	   (list
	     (make-rtest-clause :let '((a 12) (b 13)))))
	 :comparand
	 '((a b) (12 13)))
       
       )))


;; Do bindings at this level
(defun rtest-one-probe-2 (probe-report probe clause-list)
  ""
  
  (let
    (
      (binder-list
	(rtest-get-only-tests-in-set 
	  rtest-let-keys clause-list))
      
      (test-list
	(rtest-get-only-tests-in-set 
	  rtest-grader-keys clause-list)))

    (destructuring-bind
      (bound-syms bound-vals)
      (rtest-calc-all-bindings binder-list)

      (rtest-one-probe-3
	probe-report
	probe test-list bound-syms bound-vals))))



(eval-when-compile
  (setf
    (get 'rtest-one-probe-2 'rtest-suite)
    '("rtest-one-probe-2"
       (rtest rtest-borrow-setup 'rtest-get-only-tests-in-set)

       ( "A successful ordinary test"
	 (rtest-one-probe-2
	   (make-rtest-probe-failure)
	   12 
	   (list
	     rtest-canon-test-12))
	 
	 nil)

       ( "A failing ordinary test"
	 (rtest-one-probe-2
	   (make-rtest-probe-failure)
	   12 
	   (list
	     rtest-canon-test-13))
	 :every
	 (:type rtest-some-failure))
       

       ( "A probe with bindings that do nothing and no test"
	 (rtest-one-probe-2
	   (make-rtest-probe-failure)
	   12 
	   (list
	     (make-rtest-clause :let '((a 343)))))

	 :every
	 (:type rtest-some-failure))
       
       
       ( "A probe with bindings that do nothing"
	 (rtest-one-probe-2
	   (make-rtest-probe-failure)
	   12 
	   (list
	     (make-rtest-clause :let '((a 343)))
	     rtest-canon-test-12))
	 nil)


       ( "A let clause can bind the probe."
	 (rtest-one-probe-2
	   (make-rtest-probe-failure)
	   'a 
	   (list
	     (make-rtest-clause :let '((a 12)))
	     (make-rtest-clause :comparand 12)))
	 nil)

       ( "A let clause can bind symbols within a probe."
	 (rtest-one-probe-2
	   (make-rtest-probe-failure)
	   '(+ a 8)
	   (list
	     (make-rtest-clause :let '((a 4)))
	     (make-rtest-clause :comparand 12)))
	 nil)

       ( "A let clause can bind a grader."
	 (rtest-one-probe-2
	   (make-rtest-probe-failure)
	   12 
	   (list
	     (make-rtest-clause :let '((a 12)))
	     (make-rtest-clause :comparand 'a)))
	 nil)

       ( "A let clause can bind symbols within a grader."
	 (rtest-one-probe-2
	   (make-rtest-probe-failure)
	   12 
	   (list
	     (make-rtest-clause :let '((a 4)))
	     (make-rtest-clause :comparand '(+ a 8))))
	 nil)

       )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function to handle `:around' forms.

;;Args inside rtest-build-call-with-arounds, so the arg names don't
;;get trampled on by user code.  These can be safely re-used, even
;;nested, because they just name symbols in let forms.

(defconst rtest-call-syms        
  (list (gensym) (gensym) (gensym)) "" )


(defun rtest-build-call-with-arounds (around-forms)

  "Return a lambda form that calls rtest-one-probe-2 nested
inside all of AROUND-FORMS."
  
  (let
    (
      (final-form 
	`(rtest-one-probe-2 ,@rtest-call-syms)))
    
    (dolist (form around-forms)
      (let
	((list-form (rtest-clause-value form)))
	(assert-in-rtest (consp list-form)
	  "Form must be a list"
	  list-form)

	(setq final-form
	  (append list-form (list final-form)))))
  
    (eval `(lambda(,@rtest-call-syms) ,final-form))))


(eval-when-compile
  (setf
    (get 'rtest-build-call-with-arounds 'rtest-suite)
    '("rtest-build-call-with-arounds"

       ( "An empty list gives a do-nothing-special form."
	 (rtest-build-call-with-arounds '())

 	 :pattern
 	 (t `(lambda ,A (rtest-one-probe-2 ,@A)) t))

       ( "A typical around clauses parses OK."
	 (rtest-build-call-with-arounds
	   (rtest-parse-clauses 
	     '(:around (catch 'grade-something))
	     rtest-all-keys))
	 
	 :pattern
	 (t
	   `(lambda ,A 
	      (catch 
		(quote grade-something) 
		(rtest-one-probe-2 . ,A)))))


       ( "The resulting form calls rtest-one-probe-2 with the given
arguments."
	 (let-defalias
	   rtest-one-probe-2 list
	   (funcall
	     (rtest-build-call-with-arounds '()) 'A 'B 'C))
	 
	 '(A B C))

       

       ( "The call is nested inside any :around forms passed."
	 (rtest-build-call-with-arounds 
	   '((:around save-excursion)))

	 :pattern
	 ( t
	   `(lambda ,A 
	      (save-excursion (rtest-one-probe-2 ,@A)))
	   t))


       ;;This exceeds the normal maximum evaluation depth when called
       ;;via rtest-rtest.
       ( "The first :around forms are most deeply nested."
	 (rtest-build-call-with-arounds 
	   '( (:around save-excursion) 
	      (:around with-current-buffer buf)))

	 :pattern
	 (t
	   `(lambda ,A 
	      (with-current-buffer buf
		(save-excursion
		  (rtest-one-probe-2 ,@A))))))

       )))


(defun rtest-one-probe-1 (probe-report probe clause-list)
  ""
  
  (let
    ((around-forms
       (rtest-get-only-tests-in-set 
	   rtest-around-keys clause-list)))

    (funcall
      (rtest-build-call-with-arounds around-forms)
      probe-report probe clause-list)))


(eval-when-compile
  (setf
    (get 'rtest-one-probe-1 'rtest-suite)
    '("rtest-one-probe-1"
       (rtest rtest-borrow-setup 'rtest-get-only-tests-in-set)

       ( "A succeeding ordinary test"
	 (rtest-one-probe-1
	   (make-rtest-probe-failure)
	   12 
	   (list
	     rtest-canon-test-12))
	 nil)

       ( "A failing ordinary test"
	 (rtest-one-probe-1
	   (make-rtest-probe-failure)
	   12 
	   (list
	     rtest-canon-test-13))
	 :every 
	 (:type rtest-some-failure))


       ("Around forms work within rtest"
	 1 1 
	 :around (catch 'something))
       
         
       )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rtest-get-clauses (test-form)
  "Parse TEST-FORM into clauses and include `rtest-permanent-init-forms'"
  
  (declare (special rtest-permanent-init-forms))
  (append 
    (rtest-parse-form test-form) 
    (rtest-parse-form rtest-permanent-init-forms)))


;;;;;;;;;;;;;;;;;;;;;;



(defun rtest-parse-docstring
  (test-form &optional default)
  "Return a list: (docstring, remainder of TEST-FORM)
If there is no docstring, DEFAULT is used."

  (if 
    (and
      test-form
      (stringp
	(car test-form)))
    (list (car test-form) (cdr test-form))
    (list default test-form)))

;;No test.


(defun rtest-parse-probe
  (test-form probe-report)
  ""

  ;;The test-form must be a list.  This should be already guaranteed,
  ;;so we don't use `assert-in-rtest' becuse if it fails we should
  ;;get a real, full-strength error.
  (check-type test-form cons)

  (destructuring-bind
    (name useable-form)
    (rtest-parse-docstring test-form)
    (setf (rtest-probe-failure-docstring  probe-report) name)

    (assert-in-rtest
      (>= (length useable-form)	1)
      "The test form must have at least one element." useable-form)
    (let
      ((probe-sexp (car useable-form)))
      (setf (rtest-probe-failure-probe-sexp probe-report) probe-sexp)

      (let
	((clauses 
	   (rtest-get-clauses
	     (cdr useable-form))))
	;;probe-sexp could be carried in probe-sexp too.
	(list probe-sexp clauses)))))



(defun rtest-probe-failure-mask
  (probe-report failures)
  "Return PROBE-REPORT with failures set."
  (setf
    (rtest-probe-failure-failures probe-report)
    failures)
  probe-report)


(defun rtest-one-probe-4 (test-form tester &rest args)
  "Interpret TEST-FORM, returning a rtest-probe-failure or nil.
A nil return indicates that the test was successful.

TEST-FORM must be a list in proper form.
TESTER is a function taking 
    probe-report probe-sexp clauses &rest ...

ARGS are any additional argument to pass to TESTER." 

  (let
    ;;We fill in this structure as we learn more.  That's so that
    ;;unexpected errors can retain as much information as possible.
    ((probe-report 
       (make-rtest-probe-failure
	 :docstring   "Never got that far"
	 :probe-sexp  "Never got that far"
	 :result-list '("Never got that far")
	 :failures    '())))

    ;;If there's a problem, report it and don't die.  
    (rtest-wrap-errors
      (lambda (failures)
	(rtest-probe-failure-mask probe-report failures))
      list
      (lambda (err)
	(rtest-make-single-failure-by-list
	  "Problem evaluating the test expression itself"
	  (test-form err)))

      (apply 
	tester
	probe-report
	(append
	  (rtest-parse-probe test-form probe-report)
	  args)))))



(defun rtest-one-probe-0 (test-form)
    "Interpret TEST-FORM, returning a rtest-probe-failure or nil.
A nil return indicates that the test was successful.

TEST-FORM must be a list in proper form."

  (rtest-one-probe-4 test-form #'rtest-one-probe-1))


(eval-when-compile
  (setf
    (get 'rtest-one-probe-0 'rtest-suite)
    '("rtest-one-probe-0"
       ( "A simple successful test"
	 (rtest-one-probe-0 '(12 12))
	 nil)
       
       ( "A simple failing test"
	 (rtest-one-probe-0 '(12 13))

	 :type rtest-probe-failure)

       ("A test that won't parse gives a failure"
	 (rtest-one-probe-0 '())
	 :type rtest-probe-failure)
       
       ("Docstring is used and skipped if it exists."
	 (rtest-one-probe-0 '("A docstring" 1 2))

	 :field (:docstring "A docstring"))

       ("If docstring doesn't exist, `nil' is used."
	 (rtest-one-probe-0 '(1 2))

	 :field (:docstring nil))

       ("Bad test expressions return test-failure, they don't throw an error."
	 (rtest-one-probe-0 '(1 1 :test))
	 :type rtest-probe-failure)

       ("rtest continues even if the probe dies."
	 (rtest-one-probe-0 '("" (car) 1))
	 :type rtest-probe-failure)

       ("rtest continues even if the grader dies."
	 (rtest-one-probe-0 '("" 1 (car)))
	 :type rtest-probe-failure)


       )))

(eval-when-compile
  (setf
    (get 'rtest-probe 'rtest-suite)
    '("rtest-probe"
       rtest-eval-probe-simply
       rtest-one-probe-3

       rtest-calc-bindings
       rtest-calc-all-bindings
       rtest-one-probe-2
       
       rtest-build-call-with-arounds
       rtest-one-probe-1
       rtest-one-probe-0

       )))

(provide 'rtest-probe)

;;; rtest-probe.el ends here