;;; rtest-suite.el --- Handle symbols and test-suites for rtest

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
(require 'rtest-probe)


;;;;;;;;;;;;;;;;;;
;;Gathering reports


(defstruct (rtest-fail-summary (:type list))
  ""
  test-count fail-count failure)

(declaim (inline rtest-make-fail-summary rtest-make-skip-summary))
(defun rtest-make-fail-summary (failure)
  "Construct a fail-summary for a single possible failure."
  (list 1 (if failure 1 0) failure))

(defun rtest-make-skip-summary ()
  "Construct a fail-summary for a test that was skipped."
  (list 0 0 nil))

(defun* rtest-combine-fail-summaries 
  (summaries 
    &optional 
    (conversion-func #'rtest-maybe-make-rtest-nested-test-failures))
  "Collect SUMMARIES into one object.
SUMMARIES must be a list whose elements are type rtest-fail-summary.
This calls CONVERSION-FUNC to convert a list of non-nil reports, ie
failure reports, to a single object

If CONVERSION-FUNC is not given, defaults to making
rtest-nested-test-failures."

  (loop
    for raw-summary in summaries
    for summary = (rtest-coerce-to-fail-summary raw-summary)

    collect (rtest-fail-summary-failure    summary) into all-failures
    sum     (rtest-fail-summary-test-count summary) into test-count
    sum     (rtest-fail-summary-fail-count summary) into fail-count
    
    finally return 
    (list 
      test-count 
      fail-count 
      (funcall 
	conversion-func 
	(rtest-chop-out-nils all-failures)))))

(eval-when-compile
  (setf
    (get 'rtest-combine-fail-summaries 'rtest-suite)
    '("rtest-combine-fail-summaries"
       (
	 (rtest-combine-fail-summaries
	   (list
	     (rtest-make-fail-summary A)))
	 `(1 1 ,(make-rtest-nested-test-failures :failures (list A)))
	 :let ((A (make-rtest-single-failure))))
       

       (
	 (rtest-combine-fail-summaries
	   (list))
	 '(0 0 nil))
       
       (
	 (rtest-combine-fail-summaries
	   (list
	     (rtest-make-fail-summary A)
	     (rtest-make-skip-summary)))
	 `(1 1 ,(make-rtest-nested-test-failures :failures (list A)))
	 :let ((A (make-rtest-single-failure))))
       

       )))

(defun rtest-fail-summary-p (obj)
  ""
  (and
    (listp obj)
    (>= (length obj) 3)
    (integerp
      (rtest-fail-summary-test-count obj))
    (integerp
      (rtest-fail-summary-fail-count obj))
    (typep
      (rtest-fail-summary-failure    obj)
      'rtest-any-valid-return)))


(defun rtest-coerce-to-fail-summary (obj)
  ""
  (if
    (rtest-fail-summary-p obj)
    obj
    (rtest-make-fail-summary
      (rtest-make-single-failure-by-list
	"Return value was not a valid rtest-fail-summary"
	(obj)))))


;;Gathering reports
;;;;;;;;;;;;;;;;;;
;;Single element execution

(defun rtest-one-probe (test-form)
  ""

  (rtest-make-fail-summary (rtest-one-probe-0 test-form)))

(defun rtest-one-test-object (test-object)
  "Interpret a test object.

Valid types are:
  * List beginning with `rtest', which run a user-specified test.
  * List beginning with `quote'/quoted object, which is skipped.
  * Other lists, a direct test object
  * Symbol, which runs tests associated to that symbol's
    rtest-function property.

Other types return a test failure."

  (typecase test-object
    (cons
      (case (car test-object)
	;;Skip quoted objects
	(quote (rtest-make-skip-summary))

	;;Objects beginning with rtest are explicit tests.
	(rtest
	  (condition-case err
	    (eval (cdr test-object))
	    (error
	      (rtest-make-fail-summary
		(rtest-make-single-failure-by-list
		  "Problem with the user-supplied test"
		  (test-object err))))))
	
	;;Other list objects are simple tests
	(t
	  (let
	    ((failure (rtest-one-probe-0 test-object)))
	    (rtest-make-fail-summary failure)))))

    (symbol
      (rtest-one-symbol test-object))
    
    
    ;;Other types are not valid tests
    (t
      (rtest-make-fail-summary
	(rtest-make-single-failure
	  "The test expression is not even a suitable object" 
	  test-object)))))


(eval-when-compile
  (setf
    (get 'rtest-one-test-object 'rtest-setup)
    '(progn
       (defvar rtest-circular-test nil "" )
       (setf
	 (get 'rtest-circular-test 'rtest-suite)
	 '(rtest-circular-test))))

  (setf
    (get 'rtest-one-test-object 'rtest-suite)
    '("rtest-one-test-object"

       ("Starting with (quote) makes a test punt, giving automatic
success no matter how meaningless the test-form is."
	 (rtest-one-test-object ''kazoo)
	 :map
	 ((0) (0) (nil)))

       ("When the test doesn't make sense, show a simple failure"
	 (rtest-one-test-object 345)
	 	 
	 :map
	 ( 
	   (1) (1) (:type rtest-some-bad-grade) ))

       ("A test will not cycle infinitely."
	 (rtest-one-test-object 'rtest-circular-test)
	 :map ((0) (0)))
       
       ( rtest
	 progn
	 "User-supplied tests work in context"
	 (rtest-make-fail-summary nil))


       )))


;;Single-element execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Suite execution

(defun* rtest-one-suite 
  (suite &optional (func #'rtest-one-test-object))
  ""

  (destructuring-bind
    (description suite)
    (rtest-parse-docstring suite "Untitled test suite")

    (loop
      for item in suite
      collect (funcall func item) into summaries 

      finally return
      (rtest-combine-fail-summaries
	summaries
	#'(lambda (lis)
	    (make-rtest-suite-results
	      :docstring description
	      :nodes     lis))))))

(eval-when-compile
  (setf
    (get 'rtest-one-suite 'rtest-suite)
    '("rtest-one-suite"

       ( "Run each suite"
	 (let-defalias
	   rtest-one-test-object
	   rtest-make-fail-summary

	   (rtest-one-suite (list A B)))

	 :map
	 ( (2)
	   (2)
	   (:type rtest-suite-results
	     :field (:nodes (list A B))))

	 :let 
	 ( (A (make-rtest-single-failure))
	   (B (make-rtest-single-failure))))
       

       ( rtest
	 rtest-inherit
	 :docstring "Test inheritance"
	 :inherit-from 'rtest-one-test-object
	 :do-call 
	 #'(lambda (arg)
	     (rtest-one-suite (list arg)))
	 :alter-output
	 #'(lambda (arg)
	     (list
	       (rtest-fail-summary-test-count arg)
	       (rtest-fail-summary-fail-count arg)
	       (let
		 ((fails (rtest-fail-summary-failure arg)))
		 (if (rtest-suite-results-p fails)
		   (car 
		     (rtest-suite-results-nodes fails)))))))

       ( "Test that it compensates for a docstring."
	 (rtest-one-suite '("A docstring abcde"))
	 :map
	 ( () 
	   ()
	   (:field (:docstring "A docstring abcde"))))
       
       
       )))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Managing symbols.

;;Utility.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro without-reentry (flag default &rest body)
    "Run BODY, but don't let it call itself recursively.
If it was tried to be called recursively, return DEFAULT instead.
FLAG is used as the flag to guard against re-entry."
  
    `(if
       ,flag
       (progn ,default)
       (progn
	 (setf ,flag t)
	 (unwind-protect
	   (progn
	     ,@body)
	   (setf ,flag nil))))))


;;;;;;;;;;;;;;;;
;;Fix re-entry flags if they got messed up.

(defun rtest-flag-symbol-recurse (test-sym new-polarity)
  ""
  (setf
    (get test-sym 'rtest-in-progress)
    new-polarity)
  (let* 
    ((suite (rtest-symbol-to-suite test-sym)))
    (1+
      (loop
	for element in suite
	sum (rtest-maybe-flag-element element new-polarity)))))

(defun rtest-maybe-flag-symbol-recurse (test-sym new-polarity)
  ""
  
  (let
    ((current-polarity
       (get test-sym 'rtest-in-progress)))

    (if
      ;;Convert both the polarities to booleans.  It doesn't matter
      ;;that we invert both, they still match.
      (eq (not new-polarity) (not current-polarity))
      ;;Same polarity, don't revisit or we may loop.
      0
      (rtest-flag-symbol-recurse test-sym new-polarity))))

(defun rtest-maybe-flag-element (test-object new-polarity)
  ""
  
  (typecase test-object

    (symbol
      (rtest-maybe-flag-symbol-recurse test-object new-polarity))
    
    (t 0)))

(defun rtest-unflag-symbol-one-pass (sym)
  ""

  (let
    (
      (t-count   (rtest-maybe-flag-symbol-recurse sym t))
      (nil-count (rtest-maybe-flag-symbol-recurse sym nil)))
    
    (not (= t-count nil-count))))

(defun rtest-unflag-symbol (sym)
  ""
  (while (rtest-unflag-symbol-one-pass sym)))



(defun rtest-symbol-setup (sym)
  "Eval a symbol's test setup list-form, if any.
Don't eval it if it's already been evalled.
Return non-nil if the setup list-form was ever evalled."
  (let
    ((setup-form (get sym 'rtest-setup)))
    (if setup-form
      (progn
	(unless
	  (get sym 'rtest-already-set-up)
	  (eval setup-form)
	  (setf
	    (get sym 'rtest-already-set-up)
	    t))
	t)
      nil)))

(eval-when-compile
  (setf
    (get 'rtest-symbol-setup 'rtest-setup)
    '(progn
       (defun rtest-symbol-setup^2 ()
	 "Set up rtest-symbol-setup^1 as if we were seeing it for the
first time"
	 (defvar A 1 "" )
	 (setf
	   (get 'rtest-symbol-setup^1 'rtest-already-set-up)
	   nil)
	 (setf
	   (get 'rtest-symbol-setup^1 'rtest-setup)
	   '(setq A 35)))))

  (setf
    (get 'rtest-symbol-setup 'rtest-suite)
    '("rtest-symbol-setup"
       ( "Takes effect the first time"
	 (progn
	  (setq A 1)
	  (rtest-symbol-setup^2)
	  (rtest-symbol-setup 'rtest-symbol-setup^1)
	  A)

	 35)
       
       ( "Doesn't ordinarily fire twice"
	 (progn
	  (rtest-symbol-setup^2)
	  (rtest-symbol-setup 'rtest-symbol-setup^1)
	  (setq A 1)
	  (rtest-symbol-setup 'rtest-symbol-setup^1)
	  A)
	 1)
       

       )))


(defun rtest-borrow-setup (sym)
  "Ensure that another test's setup has been called.
Useful a pseudo-test, eg \(rtest rtest-borrow-setup 'other-test-sym\)"

  (if
    (rtest-symbol-setup sym)
    (rtest-make-skip-summary)
    (rtest-make-fail-summary
      (rtest-make-single-failure-by-list 
	"Couldn't find the required setup" (sym)))))


(defun rtest-one-symbol
  (test-sym)
  "Run the tests for a given symbol"

  (without-reentry
    (get test-sym 'rtest-in-progress)
    (rtest-make-skip-summary)

    (condition-case err
      (progn
	(rtest-symbol-setup test-sym)
	(rtest-one-suite (rtest-symbol-to-suite test-sym)))
      
      (error
	(rtest-make-fail-summary
	  (rtest-make-single-failure-by-list 
	    "Miscellaneous error" (err)))))))




(defun rtest-symbol-list (symbols)
  ""

  (loop
    for symbol in symbols 

    collect (rtest-one-symbol symbol) into summaries
    finally return 
    (rtest-combine-fail-summaries summaries)))

(defalias 'rtest-run-all-suites      'rtest-symbol-list)
(defalias 'rtest-run-all-syms-suites 'rtest-symbol-list)

(eval-when-compile
  (setf
    (get 'rtest-symbol-list 'rtest-suite)
    '("rtest-symbol-list"
              
       ((let-defalias
	  rtest-one-symbol
	  rtest-make-fail-summary

	  (rtest-symbol-list (list A B)))
	 
	 :map
	 ((2) (2) 
	   (:type rtest-nested-test-failures
	     :field
	     (:failures
	       (list A B))))
	 :let 
	 ( (A (make-rtest-single-failure))
	   (B (make-rtest-single-failure))))

         
       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions to figure out what tests to run

(defun rtest-nice-suite-value (value)
  ""

  ;;If the suite property is a list, it looks like a good test.
  (if
    (consp value)
    value
    nil))

;;No test

(defun rtest-symbol-to-suite (sym)
  "Get the symbol SYM's suite-object.

That will be either (preferred) SYM's rtest-suite property, or
failing that, its value."
  
  ;;If it's not even a symbol, complain.
  (check-type sym symbol)

  (or
    (rtest-nice-suite-value
      (get sym 'rtest-suite))

    ;;Backward compatibility with regress
    (if (boundp sym)
      (rtest-nice-suite-value
	(symbol-value sym)))

    ;;Hack:  Bogus suite to force a well-behaved failure
    (list (symbol-name sym)
       '("No suite of that name was found." nil :test 'nil))))

(eval-when-compile

  (setf
    (get 'rtest-symbol-to-suite 'rtest-setup)
    '(progn
       ;;;;;;;;;;;
       ;;Dummy test suites to demonstrate proper suite picking.
       (defvar rtest-common-rtest-test-0)
       (defvar rtest-common-rtest-test-1)
       (defvar rtest-common-rtest-test-2)
       ;;Has only a property
       (setf
	 (get 'rtest-common-rtest-test-0 'rtest-suite)
	 '("Picked"))

       ;;Has a value, and property is unsuitable (atomic)
       (setf
	 (get 'rtest-common-rtest-test-1 'rtest-suite)
	 t)
       (setq 
	 rtest-common-rtest-test-1
	 '("Picked"))

       ;;Has a value and a property, both OK.
       (setf
	 (get 'rtest-common-rtest-test-2 'rtest-suite)
	 '("Picked"))
       (setq 
	 rtest-common-rtest-test-2
	 '("Not picked" ("Bad test")))))


  (setf
    (get 'rtest-symbol-to-suite 'rtest-suite)
    '("rtest-symbol-to-suite"

       (
	 (rtest-symbol-to-suite 'rtest-common-rtest-test-0)
	 '("Picked"))

       ((rtest-symbol-to-suite 'rtest-common-rtest-test-1)
	 '("Picked"))

       ((rtest-symbol-to-suite 'rtest-common-rtest-test-2)
	 '("Picked"))

       ((rtest-symbol-to-suite 32)
	 :predicate rtest-error-p)

       )))


(eval-when-compile
  (setf
    (get 'rtest-suite 'rtest-suite)
    '("rtest-suite"
       rtest-combine-fail-summaries
       rtest-one-test-object
       rtest-one-suite
       rtest-symbol-list
       rtest-symbol-to-suite

       )))

(provide 'rtest-suite)

;;; rtest-suite.el ends here