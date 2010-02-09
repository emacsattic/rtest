;;; pattern-destructure.el --- Pattern-matching/destructuring code for rtest

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

;;; Commentary:  See the rtest docs

;;; Code:


(require 'rtest-visible)
(require 'pattern-walkbind)
(require 'pattern-deval)

(when (boundp 'emacs-version)
  (eval-and-compile
    (require 'cl)
    (require 'tehom-cl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Helpers
(defstruct (pattern-data (:type list))
  "Things that describe a pattern-binding we built."
  walker
  equality-tests
  preassignments
  temp-symbols
  syms
  vals)



;;Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Walker-builders


(defun pattern-split-by-car (sym binding-list)
  ""
  (loop
    for cell in binding-list

    if (eq (pattern-postbind-outside cell) sym)
    collect cell into matching-cells
    else
    collect cell into unmatching-cells

    finally return (list matching-cells unmatching-cells)))


(eval-when-compile
  (setf
    (get 'pattern-split-by-car 'rtest-setup)
    '(progn
      ;;Testing constants.
      (defconst pattern-split-testlist
	(list
	  (make-pattern-postbind :outside 'A :inside 'G12)
	  (make-pattern-postbind :outside 'A :inside 'G13)
	  (make-pattern-postbind :outside 'B :inside 'G14)
	  (make-pattern-postbind :outside 'A :inside 'G15)
	  (make-pattern-postbind :outside 'B :inside 'G16)
	  (make-pattern-postbind :outside 'C :inside 'G17))
	"")

      (defconst pattern-split-testlist-A
	(list
	  (make-pattern-postbind :outside 'A :inside 'G12)
	  (make-pattern-postbind :outside 'A :inside 'G13)
	  (make-pattern-postbind :outside 'A :inside 'G15))
	"")

      (defconst pattern-split-testlist-not-A
	(list
	  (make-pattern-postbind :outside 'B :inside 'G14)
	  (make-pattern-postbind :outside 'B :inside 'G16)
	  (make-pattern-postbind :outside 'C :inside 'G17))
	"")))

  (setf
    (get 'pattern-split-by-car 'rtest-suite)
    '("pattern-split-by-car"
       (
	 (pattern-split-by-car 'A 
	   pattern-split-testlist)

	 :comparand
	 (list
	   pattern-split-testlist-A
	   pattern-split-testlist-not-A))
       
       )))



(defun pattern-process-duplicates (cell binding-list)
  "Extract one set of sublists that all start with the same symbol.

Return a list of 2 elements: 
  combined-similar-sublists, 
  unprocessed sublists." 

  (destructuring-bind
    (equivalents remainder)
    (pattern-split-by-car (pattern-postbind-outside cell) binding-list)

    (let

      ;;List all the equivalent syms together, with the original head
      ;;as head.  NB, the result is an undotted list.
      ((equiv-list
 	 (cons
 	   (pattern-postbind-outside cell)
 	   (mapcar
 	     #'pattern-postbind-inside
 	     (cons cell equivalents)))))
      
      (list equiv-list remainder))))

(eval-when-compile
  (setf
    (get 'pattern-process-duplicates 'rtest-setup)
    '(progn
       (defconst pattern-split-^A-G11 
	 (make-pattern-postbind :outside 'A :inside 'G11)
	 "" )))

  (setf
    (get 'pattern-process-duplicates 'rtest-suite)
    '("pattern-process-duplicates"
       (rtest rtest-borrow-setup 'pattern-split-by-car)
       (
	 (pattern-process-duplicates
	   pattern-split-^A-G11
	   pattern-split-testlist)
	 (list
	   '(A G11 G12 G13 G15) 
	   pattern-split-testlist-not-A))
       
       )))


(defun pattern-join-duplicates (binding-list)
  "Combine all sublists headed by the same symbol."

  (loop
    while binding-list
    
    for (equivalents remainder) =
    (let
      ( (head (car binding-list))
	(tail (cdr binding-list)))

      (pattern-process-duplicates head tail))

    do (setq binding-list remainder)
    collect equivalents))

(eval-when-compile
  (setf
    (get 'pattern-join-duplicates 'rtest-suite)
    '("pattern-join-duplicates"
       (rtest rtest-borrow-setup 'pattern-split-by-car)
       (
	 (pattern-join-duplicates 
	   pattern-split-testlist)
	 '((A G12 G13 G15) (B G14 G16) (C G17)))
       
       )))



;;;;;;;;;;;;;;;;;;;;;;;;;
;;Build equivalence tests

;;Helper
(defun pattern-map-over-equivalence-list
  (func equivalence-list)
  "Map func over an equivalence-list"
  (apply 
    #'append
    (mapcar
      #'(lambda
	  (lis)
	  (declare (special lis))
	  (mapcar 
	    #'(lambda (sym)
		(declare (special lis))
		(funcall func (second lis) sym))
	    (nthcdr 2 lis)))
      equivalence-list)))

(defun ptest-build-one-equality-test
  (sym0 sym1)
  `(equal ,sym0 ,sym1))

(defun pat-p-build-equality-tests (unique-binding-list)
  ""
  (cons
    'and
    (pattern-map-over-equivalence-list 
      #'ptest-build-one-equality-test 
      unique-binding-list)))


(eval-when-compile
  (setf
    (get 'pat-p-build-equality-tests 'rtest-suite)
    '("pat-p-build-equality-tests"
       (
	 (pat-p-build-equality-tests
	   '((A G12 G13 G15) (B G14 G16) (C G17)))
	 '(and (equal G12 G13) (equal G12 G15) (equal G14 G16)))
       
       )))


(defun pat-r-run-basic-comparison (obj comparand)
  ""
  (rtest-maybe-make-single-failure
    (equal obj comparand) "Mismatch" (list obj comparand)))

(eval-when-compile
  (setf
    (get 'pat-r-run-basic-comparison 'rtest-suite)
    '("pat-r-run-basic-comparison"
       ( (pat-r-run-basic-comparison 1 1)
	 nil)
       
       ( (pat-r-run-basic-comparison 1 2)
	 :type rtest-some-bad-grade)
       )))



(defun pat-r-run-equality-tests (equality-list)
  ""

  (rtest-maybe-make-rtest-nested-test-failures 
    (loop
      for el in equality-list
      for report = (pat-r-run-basic-comparison (first el) (second el))
      
      if report
      collect report)))

(eval-when-compile
  (setf
    (get 'pat-r-run-equality-tests 'rtest-suite)
    '("pat-r-run-equality-tests"
       (
	 (pat-r-run-equality-tests
	   '( (1 1) (2 2)))
	 nil)
       
       ( (pat-r-run-equality-tests
	   '( (1 2) (2 1)))
	 :type rtest-some-bad-grade)
       )))


(defun pat-r-build-one-equality-pair
  (sym0 sym1)
  `(list ,sym0 ,sym1))

(defun pat-r-build-equality-tests (out-binding-list)
  "Build a form to compare items that should be equivalent according to
OUT-BINDING-LIST.

Form will return nil if successful, otherwise return a report"
  
  (let
    (
      (equality-list
	(pattern-map-over-equivalence-list 
	  #'pat-r-build-one-equality-pair
	  out-binding-list)))
    
    `
    (pat-r-run-equality-tests
      (list ,@equality-list))))

(eval-when-compile
  (setf
    (get 'pat-r-build-equality-tests 'rtest-setup)
    '(progn
       (defconst pattern-no-equality-tests^0
	 '(pat-r-run-equality-tests
	    (list))
	 "")))

  (setf
    (get 'pat-r-build-equality-tests 'rtest-suite)
    '("pat-r-build-equality-tests"
       (
	 (pat-r-build-equality-tests
	   '((A G12 G13 G15) (B G14 G16) (C G17)))

	 '(pat-r-run-equality-tests 
	   (list 
	     (list G12 G13) 
	     (list G12 G15) 
	     (list G14 G16))))

       ( "No tests = automatic success = nil"
	 (eval
	   (pat-r-build-equality-tests
	     '((A G12))))
	 nil)

       ( 
	 (let
	   ((G12 1) (G13 1))
	   (eval
	     (pat-r-build-equality-tests
	       '((A G12 G13)))))
	 nil)
       
       ( 
	 (let
	   ((G12 1) (G13 2))
	   (eval
	     (pat-r-build-equality-tests
	       '((A G12 G13)))))
	 :type rtest-some-bad-grade)
       

       )))


(defun pat-r-build-equality-tests-by-style
  (out-binding-list style)
  ""

  (if style
    (pat-p-build-equality-tests out-binding-list)
    (pat-r-build-equality-tests out-binding-list)))

;;Build equality tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Build pre-assignments


(defun pattern-build-preassignment-form (preassignment-list)
  ""
  
  (let*
    ((assign-forms
       (mapcar
	 #'(lambda (el)
	     `(setq
		,(pattern-prebind-inside el) 
		,(pattern-prebind-form el)))
	 preassignment-list)))

    `(progn ,@assign-forms)))

(eval-when-compile
  (setf
    (get 'pattern-build-preassignment-form 'rtest-setup)
    '(progn
       (defmacro pattern-eval^1
	 (form obj)
	 ""
	 `(let*
	    ((wal
	       (pattern-eval ,form)))
	    (eval
	      (pattern-build-preassignment-form
		(pattern-deval-return-vars wal)))
	    (pattern-apply-walker ,obj
	      (pattern-deval-return-walker wal))))

       (defconst pattern-no-preassignments^0
	 '(progn)
	 "")))

  (setf
    (get 'pattern-build-preassignment-form 'rtest-suite)
    '("pattern-build-preassignment-form"

       ("Doesn't do anything much if the list is empty"
	 (pattern-build-preassignment-form '())
	  pattern-no-preassignments^0)

       ( "Sets a symbol according to the given expression"
	 (let
	   ((A 0))
	   (eval
	     (pattern-build-preassignment-form
	       (list
		 (make-pattern-prebind :inside 'A :form '(+ 1 2)))))
	   A)
	 3)
       
       ( "The given form sees bindings"
	 (let
	   ((A 0) (B 2))
	   (eval
	     (pattern-build-preassignment-form
	       (list
		 (make-pattern-prebind :inside 'A :form '(+ 1 B)))))
	   A)
	 3)

       ( "It can set multiple symbols at once."
	 (let
	   ((A 0) (B 0) (C 0))
	   (eval
	     (pattern-build-preassignment-form
	       (list
		 (make-pattern-prebind :inside 'A :form '(+ 1 2))
		 (make-pattern-prebind :inside 'B :form '(+ 1 4))
		 (make-pattern-prebind :inside 'C :form '(* 1 7)))))
	   (list A B C))

	 '(3 5 7))

       ( "Runs what pattern-eval produces"
	 (pattern-build-preassignment-form
	   (pattern-deval-return-vars
	     (pattern-eval '(+ 2 3))))
	 :test (not (rtest-error-p RESULT)))

       ( "Preassignment lets the walker succeed if the respective
object matches."
	 (pattern-eval^1 '(+ 2 3) 5)
	 nil)
       

       ( "Preassignment makes a failure if the respective part of the
object doesn't match."
	 (pattern-eval^1 '(+ 2 3) 6)
	 :type rtest-some-bad-grade)
       
       
       )))


;;Build pre-assignments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Calculate all the binding information


(defun pattern-build-walker-call (pattern object-sym style)
  ""

  (destructuring-bind
    (walk-and-bind binding-list)
    (pattern-deval pattern)

    (let* 
      ((walk-report-raw-call
	 `(pattern-apply-walker ,object-sym ',walk-and-bind))

	;;Switch according to style.
	(walk-report-call
	  (if
	    style
	    `(not ,walk-report-raw-call)
	    walk-report-raw-call)))

      (list walk-report-call binding-list))))


(eval-when-compile
  (setf
    (get 'pattern-build-walker-call 'rtest-suite)
    '("pattern-build-walker-call"
       (
	 (pattern-build-walker-call '(list 2) 'G1 nil)
	 
	 :comparand
	 '((pattern-apply-walker 
	     G1 
	     (quote 
	       (pat-r-walkbind-list
		 ((pat-r-walkbind-match 2))
		 nil))) nil))
       
       
       )))


(defun pattern-build-binder (pattern object-sym &optional style)
  ""

  (destructuring-bind
    (walk-and-bind binding-list)
    (pattern-build-walker-call pattern object-sym style)

    (let*
      (	
	(temp-symbols
	  (mapcar 
	    #'pattern-somebind-inside
	    binding-list))

	(out-binding-list
	  (pattern-join-duplicates 
	    (remove-if-not #'pattern-postbind-p binding-list)))

	(preassign-form
	  (pattern-build-preassignment-form 
	    (remove-if-not #'pattern-prebind-p binding-list)))
	
	(pat-syms
	  (mapcar 
	    #'first
	    out-binding-list))

	(pat-vals
	  (mapcar 
	    #'second
	    out-binding-list))

	(equality-tests 
	  (pat-r-build-equality-tests-by-style out-binding-list style))

	(pat
	  (make-pattern-data
	    :walker         walk-and-bind
	    :equality-tests equality-tests
	    :preassignments preassign-form
	    :temp-symbols   temp-symbols
	    :syms           pat-syms
	    :vals           pat-vals
	    )))
      
      pat)))



(eval-when-compile
  (setf
    (get 'pattern-build-binder 'rtest-setup)
    '(progn
       (defmacro pattern-build-binder^1 (pattern)
	 ""
    
	 `(pattern-build-binder ',pattern 'G1 nil))))
  

  (setf
    (get 'pattern-build-binder 'rtest-suite)
    '("pattern-build-binder"
       (rtest rtest-borrow-setup 'pat-r-build-equality-tests)
       (rtest rtest-borrow-setup 'pattern-build-preassignment-form)
       ( (pattern-build-binder^1 (list 2))

	 (make-pattern-data
	   :walker
	   '(pattern-apply-walker G1 
	      (quote 
		(pat-r-walkbind-list
		  ((pat-r-walkbind-match 2))
		  nil)))
	   :equality-tests  pattern-no-equality-tests^0 
	   :preassignments  pattern-no-preassignments^0
	   :temp-symbols   nil
	   :syms           nil
	   :vals           nil))
       
       ( (pattern-build-binder^1 '(2))

	 (make-pattern-data
	   :walker
	   '(pattern-apply-walker G1 (quote (pat-r-walkbind-match (2))))
	   :equality-tests  pattern-no-equality-tests^0 
	   :preassignments  pattern-no-preassignments^0
	   :temp-symbols   nil
	   :syms           nil
	   :vals           nil))


       
       )))


;;Walker-builders
;;;;;;;;;;;;;;;;;
;;Body-call builders


(defun pattern-destr-build-call-to-body 
  (body final-syms final-vals)
  "Build a call to a body, lambda-style"
  
  (let*
    (
      ;;Build a predicate to execute body with bindings.  
      (predicate
	(eval
	  `(lambda(,@final-syms) 
	     ,body))))

    `(funcall ,predicate ,@final-vals)))

(eval-when-compile
  (setf
    (get 'pattern-destr-build-call-to-body 'rtest-suite)
    '("pattern-destr-build-call-to-body"


       ( "Body gets called with a lambda that binds pat-syms to pat-vals"
	 (eval (pattern-destr-build-call-to-body 'A '(A) '(12)))
	 12)
       
       ( "Body gets evalled OK."
	 (eval
	   (pattern-destr-build-call-to-body '(+ A 100) '(A) '(12)))
	 112)
       
       ( "Body gets evalled OK."
	 (eval 
	  (pattern-destr-build-call-to-body
	    '547 '(A B) '(1 2)))
	 547)
       

       ( "Body is informed by bindings"
	 (eval
	  (pattern-destr-build-call-to-body 
	    '(= A 12) '(A) '(12)))
	 :test RESULT)
       

       ( "Body is informed by bindings"
	 (eval
	   (pattern-destr-build-call-to-body 
	     '(= A 13) '(A) '(12)))
	 nil)
       
       )))

;;;;
;;The same thing let-style

(defun pattern-destr-build-let-binding (body final-syms final-vals)
  ""

  (let
    ((binding-list
       (mapcar* #'list final-syms final-vals)))
    
    `(let ,binding-list ,body)))

(eval-when-compile
  (setf
    (get 'pattern-destr-build-let-binding 'rtest-suite)
    '("pattern-destr-build-let-binding"
       ( "Body gets called with a lambda that binds pat-syms to pat-vals"
	 (pattern-destr-build-let-binding '43 '(A) '(12))
	 '(let ((A 12)) 43))
       
       ( "Body gets evalled OK."
	 (eval
	   (pattern-destr-build-let-binding '(+ A 100) '(A) '(12)))
	 112)
       
       ( "Body gets evalled OK."
	 (eval 
	  (pattern-destr-build-let-binding
	    '547 '(A B) '(1 2)))
	 547)

       ( "Body is informed by bindings"
	 (eval
	  (pattern-destr-build-let-binding 
	    '(= A 12) '(A) '(12)))
	 :test RESULT)
       

       ( "Body is informed by bindings"
	 (eval
	   (pattern-destr-build-let-binding 
	     '(= A 13) '(A) '(12)))
	 nil)
       )))

;;;;;;;;;;;;
;;setq-style

(defun pattern-build-setq-call (body final-syms final-vals)
  "Build a setq-like form for pattern-binding."

  (declare (ignore body))

  (let
    ((body-forms
       (mapcar* 
	 #'(lambda (S V)
	     `(setq ,S ,V)) 
	 final-syms 
	 final-vals)))
    
    `(progn ,@body-forms)))


(eval-when-compile
  (setf
    (get 'pattern-build-setq-call 'rtest-suite)
    '("pattern-build-setq-call"
       (
	 (let
	   ((A 1))
	   (eval
	     (pattern-build-setq-call nil '(A) '(12)))
	   A)
	 12)
       
       (
	 (let
	   ((A 1)(B 2))
	   (eval
	     (pattern-build-setq-call nil '(A B) '(12 13)))
	   (list A B))
	 '(12 13))

       )))

;;Untested.

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Form builders

(defun pattern-combine-0
  (pat fm)
  ""
  `(let 
     ,(pattern-data-temp-symbols pat)
     ,(pattern-data-preassignments pat)
     ,fm))

(eval-when-compile
  (setf
    (get 'pattern-combine-0 'rtest-suite)
    '("pattern-combine-0"
       ;;No good tests.

       )))


(defun pat-r-combine-patternforms (call-form pat)
  ""


  (pattern-combine-0 pat 
    `(or
       ,(pattern-data-walker pat)
       ,(pattern-data-equality-tests pat)
       ,call-form)))



(eval-when-compile
  (setf
    (get 'pat-r-combine-patternforms 'rtest-suite)
    '("pat-r-combine-patternforms"

       ( "Body executes normally, evals to my-result."
	 (eval
	   (pat-r-combine-patternforms '''my-result 
	     (make-pattern-data 
	       :walker         'nil
	       :equality-tests 'nil
	       :temp-symbols   '())))
	 
	 ''my-result)



       ( "Failure of equality-tests gives an error"
	 (eval
	   (pat-r-combine-patternforms '''my-result 
	     (make-pattern-data 
	       :walker         'nil
	       :equality-tests '(make-rtest-single-failure)
	       :temp-symbols   '())))

	 :type rtest-some-bad-grade  )



       ( "`:walker' binds syms to parts of object, all such
symbols being in `temp-symbols'.  call-form is informed by the
temp-symbols' bindings.  "
	 (eval
	   (pat-r-combine-patternforms 'A
	     (make-pattern-data 
	       :walker         '(setq A 532)
	       :equality-tests 'nil
	       :temp-symbols   '(A))))
	 532)    



       ("Failure of pattern-matching gives an error too."
	 (eval
	   (pat-r-combine-patternforms
	     '''my-result 
	     (make-pattern-data 
	       :walker         '(make-rtest-single-failure)
	       :equality-tests 'nil
	       :temp-symbols   '())))

	 :type rtest-some-bad-grade)

       )))


(defun pat-r-combine-patternforms-maybe-throw (call-form pat)
  ""

  (pattern-combine-0 pat 
    `(let
       ((failures
	  (or
	    ,(pattern-data-walker pat)
	    ,(pattern-data-equality-tests pat))))

       (if
	 failures
	 (rtest-signal failures)
	 ,call-form))))
;;Untested

(defun pat-p-combine-patternforms (call-form pat)
  ""
  
  (pattern-combine-0 pat 
    `(and
       ,(pattern-data-walker pat)
       ,(pattern-data-equality-tests pat)
       ,call-form)))


;;This works, but is not formally tested.


(defun pattern-combine-forms-by-style (call-form pat &optional style)
  ""
  
  (if
    style
    (pat-p-combine-patternforms call-form pat)
    (pat-r-combine-patternforms call-form pat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Overall builder functions:


(defun pattern-build-form (build-func pattern object-sym body style)
  ""
  
  (let*
    (      
      (pat
	(pattern-build-binder pattern object-sym style))
      (call-form
	(funcall build-func
	  body 
	  (pattern-data-syms pat)
	  (pattern-data-vals pat)))
      (combined-form
	(pattern-combine-forms-by-style call-form pat style)))

    combined-form))


(defun pattern-build-lambda (pattern body &optional style)
  "Build a lambda form that does pattern-matching"
  
  (let* 
    (
      (object-sym (gensym))
      (combined-form 
	(pattern-build-form 
	  #'pattern-destr-build-call-to-grader 
	  pattern object-sym body style)))

    ;;$$Retest this.
    (eval `(lambda(,object-sym) ,combined-form))))


(eval-when-compile
  (setf
    (get 'pattern-build-lambda 'rtest-suite)
    '("pattern-build-lambda"

       ( (funcall (pattern-build-lambda 2 t) 2)
	 nil)
       
       )))
;;Tested as part of rtest-grade-by-pattern


(defun pattern-build-bind (pattern object body)
  ""

  (pattern-build-form 
    #'pattern-destr-build-let-binding 
    pattern object body t))
;;Tested as part of pattern-bind


;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (setf
    (get 'pattern-destructure 'rtest-suite)
    '("pattern-destructure"
       pat-r-combine-patternforms
       pattern-destr-build-let-binding
       pattern-destr-build-call-to-body
       pattern-build-binder
       pattern-build-walker-call
       pat-r-build-equality-tests
       pat-p-build-equality-tests
       pat-r-run-equality-tests
       pat-r-run-basic-comparison
       pattern-join-duplicates
       pattern-process-duplicates
       pattern-split-by-car
       )))

(provide 'pattern-destructure)

;;; pattern-destructure.el ends here

