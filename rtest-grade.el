;;; rtest-grade.el --- Grader code for rtest

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
(require 'rtest-visible)
(require 'rtest-error)
(require 'rtest-parse)
(require 'rtest-field)
(require 'pattern-destructure)


;;;;;;;;;;;;;;;;
;;Grader helpers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rtest-build-basic-grader-form (test fail-string fail-object)
    "Build a form to execute a single test.

FAIL-STRING evals to a string describing the failure.
FAIL-OBJECT evals to the object that caused it."
  
    `(if
       ,test
       nil
       (rtest-make-single-failure ,fail-string ,fail-object)))

  (defmacro rtest-maybe-make-single-failure (test fail-string fail-object)
    "Execute a single test.

FAIL-STRING is a string describing the failure.
FAIL-OBJECT is the object that caused it.  For now it still has to be
a single object"
  
    (rtest-build-basic-grader-form test fail-string fail-object)))

(defun rtest-test-uses-multiple-values-p (test)
  ""
  (assoc 
    (rtest-clause-key test)
    rtest-multi-val-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Grader functions.


(defun rtest-grade-by-comparand (result grader bound-syms bound-vals)
  ""
  (let
    ((the-predicate 
       (eval `(lambda(RESULT ,@bound-syms) (equal ,grader RESULT)))))
    
    (rtest-maybe-make-single-failure
      (apply the-predicate result bound-vals)
      "Didn't match"
      grader)))

(eval-when-compile
  (setf
    (get 'rtest-grade-by-comparand 'rtest-setup)
    '(progn
       (defconst rtest-no-bound-syms
	 '(:around 
	    (let
	      ((*bound-syms* '()) (*bound-vals '()))))
	 "" )))

  (setf
    (get 'rtest-grade-by-comparand 'rtest-suite)
    '("rtest-grade-by-comparand-1"

       ( "A comparand returns nil (succeeds) if it matches"
	 (rtest-grade-by-comparand 12 12 '() '())
	 nil)

       ( "Non-matches return a failure report"
	 (rtest-grade-by-comparand 12 13 '() '())
	 :type rtest-some-bad-grade)


       ( "Bindings inform the comparand expression"
	 (rtest-grade-by-comparand 12 'a '(a) '(12))
	 nil)


       ( "Bindings inform the comparand expression"
	 (rtest-grade-by-comparand 12 'a '(a) '(13))
	 :type rtest-some-bad-grade)

       ("`:comparand' works in rtest"
	 12 :comparand 12)
       
       ("implicit `:comparand' works in rtest"
	 12 12)
       
       )))



(defun rtest-grade-by-predicate (result grader bound-syms bound-vals)
  ""
  (declare (ignore bound-syms bound-vals))
  (rtest-maybe-make-single-failure
    (funcall grader result)
    "Predicate failed"
    grader))

(eval-when-compile
  (setf
    (get 'rtest-grade-by-predicate 'rtest-suite)
    '("rtest-grade-by-predicate"
       ( "Matching predicates return nil"
	 (rtest-grade-by-predicate 12 #'numberp '() '())
	 nil)

       ( "Non-matching predicates return a failure report"
	 (rtest-grade-by-predicate 12 #'consp '() '())
	 :type rtest-some-bad-grade)

       ( "Bindings are ignored without error"
	 (rtest-grade-by-predicate 12 #'numberp '(a) '(11))
	 nil)

       ( "`:predicate' works in rtest"
	 12
	 :predicate numberp)
       
       )))


(defun rtest-grade-by-test (result grader bound-syms bound-vals)
  ""
  (let
    ((the-predicate 
       (eval `(lambda(RESULT ,@bound-syms) ,grader))))
    (rtest-maybe-make-single-failure
      (apply the-predicate result bound-vals)
      "Test failed"
      grader)))


(eval-when-compile
  (setf
    (get 'rtest-grade-by-test 'rtest-suite)
    '("rtest-grade-by-test"
       ( "A test that succeeds returns nil (success)"
	 (rtest-grade-by-test 12 '(numberp RESULT) '() '())
	 nil)
       

       ( "A test that fails returns a failure report"
	 (rtest-grade-by-test 12 '(consp RESULT) '() '())
	 :type rtest-some-bad-grade)
       

       ( "A test pays attention to variable bindings, thus succeeding here"
	 (rtest-grade-by-test 12 '(equal a RESULT) '(a) '(12))
	 nil)

       ("A test pays attention to variable bindings, thus failing here"
	 (rtest-grade-by-test 12 '(equal a RESULT) '(a) '(13))
	 :type rtest-some-bad-grade)

       ( "Multiple bindings are all used"
	 (rtest-grade-by-test 12 '(equal (+ a b) RESULT) 
	   '(a b)
	   '(4 8))
	 nil)

       ("`:test' works in rtest"
	 12
	 :test (= RESULT 12))

       )))


(defun rtest-grade-by-type (result grader bound-syms bound-vals)
  ""
  (declare (ignore bound-syms bound-vals))

  (rtest-maybe-make-single-failure
    (typep result grader)
    "Result is not that type"
    grader))
 


(eval-when-compile
  (setf
    (get 'rtest-grade-by-type 'rtest-suite)
    '("rtest-grade-by-type"

       ( "Return success (nil) if the object is of that type"
	 (rtest-grade-by-type 12 'number '() '())
	 nil)
       
       ( "Return a failure report if the object is not of that type"
	 (rtest-grade-by-type 12 'cons '() '())
	 :type rtest-some-bad-grade)
       
       ;;It's important that we can grade our own types, even be they
       ;;warmed-over lists

       ( "Can grade rtest-single-failure"
	 (rtest-grade-by-type 
	   (make-rtest-single-failure)
	   'rtest-single-failure '() '())
	 nil)

       ( "Can grade rtest-probe-failure"
	 (rtest-grade-by-type 
	   (make-rtest-probe-failure)
	   'rtest-probe-failure '() '())
	 nil)

       ( "Can grade rtest-suite-results"
	 (rtest-grade-by-type 
	   (make-rtest-suite-results)
	   'rtest-suite-results '() '())
	 nil)

       ( "Can grade rtest-nested-test-failures"
	 (rtest-grade-by-type 
	   (make-rtest-nested-test-failures)
	   'rtest-nested-test-failures '() '())
	 nil)

       ( "Can grade rtest-field-test-failures"
	 (rtest-grade-by-type 
	   (make-rtest-field-test-failures)
	   'rtest-field-test-failures '() '())
	 nil)

       ("`:type' works within rtest"
	 1 :type number)

       )))


(defun rtest-map-graders (result grader bound-syms bound-vals)
  ""

  (cond
    ((not (listp result)) 
      (rtest-make-single-failure "Result is not a list" result))


    ((not (listp grader))
      (rtest-make-single-failure "Grader is not a list" grader))

    ;;The grader list may be shorter than the result list, but never
    ;;longer. 
    ((< (length result) (length grader))
      (rtest-make-single-failure-by-list
	"There's aren't enuff results to satisfy grader"
	(result grader)))
    

    (t
      (let*
	;;Parse it now.  
	((real-tests 
	   (mapcar #'rtest-parse-tests grader))
	  
	  (failures
	    (loop
	      for el        in   result
	      for n         from 0
	      for test-list in   real-tests
	      
	      collect
	      (rtest-grade-one-field 
		n el test-list bound-syms bound-vals))))
	  
	
	(rtest-maybe-make-rtest-nested-test-failures failures)))))



(eval-when-compile
  (setf
    (get 'rtest-map-graders 'rtest-suite)
    '("rtest-map-graders"
       ("Rejects non-list results"
	 (rtest-map-graders 45
	   '( (:comparand 1)
	      (:comparand 2))
	   nil 
	   nil)
	 :type rtest-some-bad-grade)
       
       
       ( "Reject non-list graders"
	 (rtest-map-graders '(1 2)  52
	   nil 
	   nil)
	 
	 :type rtest-some-bad-grade)
       
       ( "Reject cases where there are more graders than results"
	 (rtest-map-graders '(1 2)  '(1 2 3 4)
	   nil 
	   nil)
	 
	 :type rtest-some-bad-grade)
       

       ( "Return nil when all tests succeed."
	 (rtest-map-graders '(1 2)  
	   '( (:comparand 1)
	      (:comparand 2)
	      )
	   nil 
	   nil)
	 nil)
       
       ( "Return a failure when any test fails"
	 (rtest-map-graders '(1 1)  
	 '( (:comparand 1)
	    (:comparand 2)
	    )
	 nil nil)

	 :type rtest-some-bad-grade)
       
       ("`:map' works in rtest"
	 '(1 2)
	 :map
	 ( (:comparand 1)
	   (:comparand 2)))

       )))


(defun rtest-grade-every (result grader bound-syms bound-vals)
  ""

  (cond
    ((not (listp result)) 
      (rtest-make-single-failure "Result is not a list" result))

    (t
      (let*
	;;Parse the grader now.  
	((test-list (rtest-parse-tests grader))

	  (failures
	    (loop
	      for el        in   result
	      for n         from 0
	      
	      collect 
	      (rtest-grade-one-field 
		n el test-list bound-syms bound-vals))))
	  

	(rtest-maybe-make-rtest-nested-test-failures failures)))))



(eval-when-compile
  (setf
    (get 'rtest-grade-every 'rtest-suite)
    '("rtest-grade-every"
       ("Result must be a list"
	 (rtest-grade-every 1 '(:comparand 1) nil nil)
	 :type rtest-some-bad-grade)

       ("If the test succeeds for every element, succeed"
	 (rtest-grade-every '(1 2) '(:predicate numberp) nil nil)
	 nil)

       ("If the test fails for any element, fail"
	 (rtest-grade-every '(1 2) '(:comparand 1) nil nil)

	 :type rtest-nested-test-failures
	 :field 
	 (:failures
	   :every 
	   (:type rtest-field-test-failures)
	   :test (= (length RESULT) 1)))
       
       
       ("`:every' works in rtest"
	 '(1 2)
	 :every
	 (:type number))
       
       )))


(defun rtest-grade-field (result grader bound-syms bound-vals)
  ""

  (let*
    (
      (test-list (rtest-parse-tests (cdr grader)))
      (field-key (car grader))
      (field 
	(rtest-access-field result field-key)))
      
    (rtest-grade-one-field 
      field-key field test-list bound-syms bound-vals)))

(eval-when-compile
  (setf
    (get 'rtest-grade-field 'rtest-setup)
    '(progn
       (defconst rtest-grade-field^1
	 (make-rtest-struct 
	   :my-field        '(A B C)
	   :my-second-field '(1 2 3))
	 "" )))

  (setf
    (get 'rtest-grade-field 'rtest-suite)
    '("rtest-grade-field"
       ( "If there are no tests for that field, give success"
	 (rtest-grade-field 
	   rtest-grade-field^1 
	   '(:my-field) 
	   nil 
	   nil)
	 nil)

       ( "If that field doesn't exist, it's an error (handled elsewhere)"
	 (rtest-grade-field 
	   rtest-grade-field^1 
	   '(:my-third-field) 
	   nil 
	   nil)
	 :predicate rtest-error-p)
       
       

       ( "If all tests succeed, give success"
	 (rtest-grade-field 
	   rtest-grade-field^1 
	   '(:my-field '(A B C)) 
	   nil 
	   nil)
	 nil)

       ( "If any tests fails, give a failure"
	 (rtest-grade-field 
	   rtest-grade-field^1 
	   '(:my-field '(A 2 C)) 
	   nil 
	   nil)
	 :type rtest-field-test-failures)

       ("`:field' with a blank test works in rtest"
	 rtest-grade-field^1
	 :field (:my-field))

       ("`:field' with a trivial test works in rtest"
	 rtest-grade-field^1
	 :field (:my-field :test t))

       ("`:field' works in rtest"
	 rtest-grade-field^1
	 :field (:my-field '(A B C))
	 :field (:my-second-field '(1 2 3))
	 )

       
       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun pattern-destr-build-call-to-grader 
  (body pat-syms pat-vals)
  ""
  (declare (special *bound-syms* *bound-vals*))

  (let*
    (
      ;;Append outside and pattern syms, vals, keeping them in
      ;;consistent order.  
      (final-syms 
	(append *bound-syms* pat-syms))
      (final-vals
	(append *bound-vals* pat-vals))
	
      ;;Build a predicate to execute body with bindings.
      (call
	(pattern-destr-build-call-to-body body
	  final-syms final-vals)))
    

    (rtest-build-basic-grader-form  
      call
      "Test failed: "

      `',body)))

(eval-when-compile
  
  (setf
    (get 'pattern-destr-build-call-to-grader 'rtest-suite)
    '("pattern-destr-build-call-to-grader"
       (rtest rtest-borrow-setup 'rtest-grade-by-comparand)

       ( 
	 (eval
	   (pattern-destr-build-call-to-grader
	     '547 '(A B) '(1 2)))
	 nil
	 :include rtest-no-bound-syms)

       ( 
	 (eval
	   (pattern-destr-build-call-to-grader
	     '(= A 12) '(A) '(12)))
	 nil
	 :include rtest-no-bound-syms)
	 
       (
	 (eval
	   (pattern-destr-build-call-to-grader
	     '(= A 13) '(A) '(12) ))
	 :type rtest-some-bad-grade
	 :include rtest-no-bound-syms)

       )))


(defun rtest-grade-by-pattern 
  (result grader bound-syms bound-vals)
  ""

  (let
    ( (*bound-syms* bound-syms)
      (*bound-vals* bound-vals)
      (object  result)
      ;; (first grader) is ignored because Emacs-Lisp's backquote is
      ;; messed up and can't be used for it.  
      (pattern (second  grader))

      ;;A nil body is pointless and usually comes about because the
      ;;grader form was too short.  So we change it from nil (always
      ;;fail) to t (always succeed)

      (body    (or (third grader) t)))
      
      
    (declare (special *bound-syms* *bound-vals*))

    (funcall
      (pattern-build-lambda pattern body nil)
      object)))


(eval-when-compile
  (setf
    (get 'rtest-grade-by-pattern 'rtest-setup)
    '(progn
       (defmacro rtest-grade-by-pattern^1 (object pattern body)
	 ""
    
	 `(rtest-grade-by-pattern ',object (list t ',pattern ',body) 
	    nil nil))))


  (setf
    (get 'rtest-grade-by-pattern 'rtest-suite)
    '("rtest-grade-by-pattern"


       ((rtest-grade-by-pattern^1 2 2 568)
	 nil)

       ((rtest-grade-by-pattern^1
	  (2) (list 2) 568)
	 nil)

       ((rtest-grade-by-pattern^1
	  (2) '(2) 568) 
	 nil)

       ((rtest-grade-by-pattern^1
	  (2) `(2) 568) 
	 nil)

       ((rtest-grade-by-pattern^1
	  (2) (list 3) 568) 
	 :type rtest-some-bad-grade)

       ((rtest-grade-by-pattern^1
	  (2) '(3) 568)
	 :type rtest-some-bad-grade)

       ((rtest-grade-by-pattern^1
	  (2) `(3) 568)
	 :type rtest-some-bad-grade)

       ((rtest-grade-by-pattern^1
	  (t) (list A) 1) 
	 nil)

       ( "Bind `A' to t, and use `A' as guard; it should succeed."
	 (rtest-grade-by-pattern^1
	  (t) (list A) A) 
	 nil)

       ( "A nil grader, usually a mistaken omission, won't fail by itself"
	 (rtest-grade-by-pattern^1
	  (1) (list A) nil) 
	 nil)

       ( "A nil grader won't keep the pattern from failing"
	 (rtest-grade-by-pattern^1
	  (1) (list A B C) nil) 
	 :type rtest-some-bad-grade)

       ((rtest-grade-by-pattern^1
	  (2 2) (list A A) A) nil)

       ( "We use 3 variables so that the equal-failed report won't
look like the original object."
	 (rtest-grade-by-pattern^1
	  (2 2 3) (list A A A) A) 
	 :type rtest-some-bad-grade)

       ;;Dotted patterns.
       ( "Dotted patterns work"
	 (rtest-grade-by-pattern^1
	   (4 . 5) (cons 4 5) t)

	 nil)

       ( "Backquote works with dotted patterns"
	 (rtest-grade-by-pattern^1
	   (4 . 5) `(4 . 5) t)
	 nil)

       ( "Backquote works with complex patterns"
	 (rtest-grade-by-pattern^1
	  (2 3 (4 . 5) 2) `(,A 3 (4 . 5) ,A) A)
	 nil)


       ( "Backquote works with complex patterns"
	 (rtest-grade-by-pattern^1
	  (2 3 (4 . 5) 2) `(,A 3 (,A . 5) ,A) A) 
	 :type rtest-some-bad-grade)

       ( "`:pattern' works in rtest"
	  '(1 2 . 3)
	  :pattern 
	  ( t
	    `(,A ,B . ,C)
	    (and 
	      (= A 1)
	      (= B 2)
	      (= C 3))))

       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst rtest-grader-func-alist-1
  (list
     (cons
       :comparand
       #'rtest-grade-by-comparand)
    
     (cons
       :comparand-multi
       #'rtest-grade-by-comparand)
         
     (cons
       :predicate
       #'rtest-grade-by-predicate)

    (cons
      :predicate-multi
      #'rtest-grade-by-predicate)
    
     (cons
       :test
       #'rtest-grade-by-test)

    (cons
      :test-multi
      #'rtest-grade-by-test)

    (cons
      :type
      #'rtest-grade-by-type)

    (cons
      :map
      #'rtest-map-graders)

    (cons
      :map-multi
      #'rtest-map-graders)

    (cons
      :every
      #'rtest-grade-every)

    (cons
      :every-multi
      #'rtest-grade-every)

    (cons
      :field
      #'rtest-grade-field)

    (cons
      :pattern
      #'rtest-grade-by-pattern)
    
    (cons
      :pattern-multi
      #'rtest-grade-by-pattern)

    ;;Add new graders here
    )
  
  "List of graders." )


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;General grader functions

;;Mutually recursive with rtest-run-grader-list
(defun rtest-grade-one-field
  (n el test-list bound-syms bound-vals)
  ""
  
  (rtest-wrap-errors
    (lambda (X)
      (rtest-make-field-test-failures n el X))
    list
    (lambda (err)
      (rtest-make-single-failure-by-list 
	"Field grader died" (err)))
      
    (rtest-run-grader-list
      (list el)
      test-list bound-syms bound-vals)))



(eval-when-compile
  (setf
    (get 'rtest-grade-one-field 'rtest-suite)
    '("rtest-grade-one-field"
       (rtest rtest-borrow-setup 'rtest-get-only-tests-in-set)

       ( "If there are no tests, return no failure"
	 (rtest-grade-one-field
	   0 12 '() nil nil)
	 nil)

       ( "If all tests are successful, return no failure"
	 (rtest-grade-one-field
	   0 12 (list rtest-canon-test-12) nil nil)
	 nil)
       

       ( "If any test fails, return failure"
	 (rtest-grade-one-field
	   0 12 (list rtest-canon-test-13) nil nil)
	 :type rtest-field-test-failures)

       ("We build a valid structure even when field-indicator is our
own key"
	 (rtest-grade-one-field
	   :actual-value 12 (list rtest-canon-test-13) nil nil)
	 :type rtest-field-test-failures
	 :field (:field-indicator :actual-value)
	 :field (:actual-value 12))
       
       ("We build a valid structure even when field-indicator is our
own key"
	 (rtest-grade-one-field
	   :field-indicator 12 (list rtest-canon-test-13) nil nil)
	 :type rtest-field-test-failures
	 :field (:field-indicator :field-indicator)
	 :field (:actual-value 12))

       ("We build a valid structure even when field-indicator is our
own key"
	 (rtest-grade-one-field
	   :failures 12 (list rtest-canon-test-13) nil nil)
	 :type rtest-field-test-failures
	 :field (:field-indicator :failures)
	 :field (:actual-value 12))

       ("We build a valid structure even when actual-value is our
own key"
	 (rtest-grade-one-field
	   0 :actual-value (list rtest-canon-test-13) nil nil)
	 :type rtest-field-test-failures
	 :field (:field-indicator 0)
	 :field (:actual-value :actual-value))

       ("We build a valid structure even when actual-value is our
own key"
	 (rtest-grade-one-field
	   0 :field-indicator (list rtest-canon-test-13) nil nil)
	 :type rtest-field-test-failures
	 :field (:field-indicator 0)
	 :field (:actual-value :field-indicator))

       ("We build a valid structure even when actual-value is our
own key"
	 (rtest-grade-one-field
	   0 :failures (list rtest-canon-test-13) nil nil)
	 :type rtest-field-test-failures
	 :field (:field-indicator 0)
	 :field (:actual-value :failures))

       )))



(defun rtest-one-grader-1 
  (probe-result test bound-syms bound-vals grader-func-alist)
  "Grade PROBE-RESULT according to GRADER of GRADER-TYPE.
Returns nil if successful, an error report if not."
  
  (let*
    ( 
      (grader-type (rtest-clause-key   test))
      (grader      (rtest-clause-value test))
      (found (assoc grader-type grader-func-alist)))

    (if found

      (rtest-wrap-errors
	identity
	identity
	(lambda (err)
	  (rtest-make-single-failure-by-list 
	    "Grader died" (grader err)))
      
	(funcall 
	  (cdr found) probe-result grader bound-syms bound-vals))

      ;;If there was no test, that's an error too.
      (progn
	(rtest-make-single-failure "There's no such test" grader-type)))))

(eval-when-compile
  (setf
    (get 'rtest-one-grader-1 'rtest-suite)
    '("rtest-one-grader-1"
       (rtest rtest-borrow-setup 'rtest-get-only-tests-in-set)
       
       ( "True tests give nil"
	 (rtest-one-grader-1 
	   12
	   rtest-canon-test-12
	   nil nil
	   rtest-grader-func-alist-1)
	 nil)
       


       ( "Failed tests give a report"
	 (rtest-one-grader-1 
	   12
	   (make-rtest-clause :comparand 45)
	   nil nil
	   rtest-grader-func-alist-1)
	 :type rtest-single-failure)
       

       
       )))


(defun rtest-one-grader-0 (probe-result test bound-syms bound-vals)
  ""

  (rtest-one-grader-1
    (if 
      (rtest-test-uses-multiple-values-p test)
      probe-result
      (car probe-result))
    test
    bound-syms 
    bound-vals
    rtest-grader-func-alist-1))


(eval-when-compile
  (setf
    (get 'rtest-one-grader-0 'rtest-suite)
    '("rtest-one-grader-0"

       ("Normal tests get only the first value."
	 (rtest-one-grader-0
	  '(12 12)
	  (make-rtest-clause 
	    :test 
	    '(equal RESULT 12))
	  '() '())

	 nil)
       

       ("Multiple-value type tests get a list of all the values"
	 (rtest-one-grader-0
	  '(12 12)
	  (make-rtest-clause 
	    :test-multi 
	    '(equal (first RESULT) (second RESULT)))
	  '() '())

	 nil)

       )))


;;This feeds into both probe-result and nested-result construction, so
;;we can't skip chopping out the nils.
(defun rtest-run-grader-list 
  (probe-result simple-tests bound-syms bound-vals)
  ""
  (check-type simple-tests list)
  (rtest-chop-out-nils
    (loop
      for test in simple-tests

      collect
      (rtest-one-grader-0 probe-result test bound-syms bound-vals))))


(eval-when-compile
  (setf
    (get 'rtest-run-grader-list 'rtest-suite)
    '("rtest-run-grader-list"

       (rtest rtest-borrow-setup 'rtest-get-only-tests-in-set)

       ((rtest-run-grader-list
	  '(12)
	  (list
	    rtest-canon-test-12
	    (make-rtest-clause :predicate #'numberp))
	  '() '())

	 nil)

       ((rtest-run-grader-list
	  '(12)
	  (list
	    rtest-canon-test-12
	    rtest-canon-test-consp)
	  '() '())
	 
	 ;;Must be a list
	 :predicate listp
	 :every
	 (:type rtest-single-failure))

       )))


(eval-when-compile
  (setf
    (get 'rtest-grade 'rtest-suite)
    '("rtest-grade"

       rtest-grade-by-comparand
       rtest-grade-by-predicate
       rtest-grade-by-test
       rtest-grade-by-type
       rtest-map-graders
       rtest-grade-every
       rtest-grade-field
       pattern-destr-build-call-to-grader
       rtest-grade-by-pattern
       
       ;;General grader work
       rtest-grade-one-field
       rtest-one-grader-1
       rtest-one-grader-0
       rtest-run-grader-list

       )))

(provide 'rtest-grade)


;;; rtest-grade.el ends here




