;;; rtest-visible.el --- Code visible to most modules in rtest

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

(when
  (boundp 'emacs-version)
  (eval-and-compile
    (require 'cl)
    (require 'tehom-cl)))


;;; Structures

(defstruct (rtest-single-failure )
  "Data describing a single test failure.  No sub-tests here.
OBJECT is always a list whose elements are \(object-name . object-value\)"
  text
  object)

(defstruct (rtest-field-test-failures )
  "Data describing test failures for a field rather than for a complete
probe."
  field-indicator
  actual-value
  failures)

(defstruct (rtest-nested-test-failures )
  "Data describing one or more failures of nested tests."
  failures)

(defstruct (rtest-probe-failure )
  "Data describing a probe's failure."
  docstring
  probe-sexp
  result-list
  failures
  results-are-multiple)

(defstruct (rtest-suite-results )
  "The results of running a suite."
  docstring
  nodes)


;;;;;;;;;;;;;;;
;;Derived types

(deftype rtest-some-bad-grade ()
  '(or
     rtest-single-failure
     rtest-field-test-failures
     rtest-nested-test-failures))

(deftype rtest-some-failure ()
  '(or
     rtest-single-failure
     rtest-field-test-failures
     rtest-nested-test-failures
     rtest-probe-failure
     rtest-suite-results))

(deftype rtest-any-valid-return ()
  '(or
     null
     rtest-single-failure
     rtest-field-test-failures
     rtest-nested-test-failures
     rtest-probe-failure
     rtest-suite-results))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rtest-make-single-failure (text object)
  "Build a failure report using a single anonymous object."
  
  (make-rtest-single-failure
    :text   text
    :object (list (cons "object" object))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro rtest-make-single-failure-by-list (text object-list)
    "Build a failure report using a list of object names."
  
    (let
      ((name-list
	 (mapcar #'symbol-name object-list)))

      `
      (make-rtest-single-failure
	:text   ,text
	:object 
	(mapcar*
	  #'cons
	  ',name-list
	  (list ,@object-list))))))


(eval-when-compile
  (setf
    (get 'rtest-make-single-failure-by-list 'rtest-suite)
    '("rtest-make-single-failure-by-list"
       ( 
	 (let* 
	   ((A 12) (B 13)) ;Need to be upper-case for Common Lisp
	   (rtest-make-single-failure-by-list "" (A B)))

	 :type rtest-single-failure
	 :field
	 (:object '(("A" . 12) ("B" . 13))))
       
       )))


(defun rtest-make-probe-failure
  (docstring probe-sexp result-list failures results-are-multiple)
  ""
  
  (make-rtest-probe-failure
    :docstring            docstring
    :probe-sexp           probe-sexp
    :result-list          result-list
    :failures             failures
    :results-are-multiple results-are-multiple))


(defun rtest-make-field-test-failures 
  (field-indicator actual-value failures)

  ""
  (let
    ((obj
       (make-rtest-field-test-failures
	 :actual-value    actual-value
	 :failures        failures)))
    ;;Set field-indicator specially - its prone to being a keyword and
    ;;we don't want to accidentally capture it.
    (setf
      (rtest-field-test-failures-field-indicator obj)
      field-indicator)
    obj))


(declaim (inline rtest-chop-out-nils))
(defun rtest-chop-out-nils (lis)
  ""
  (remove* nil lis))

(eval-when-compile
  (setf
    (get 'rtest-chop-out-nils 'rtest-suite)
    '("rtest-chop-out-nils"
       ((rtest-chop-out-nils '(1 nil 2 nil 3))
	 '(1 2 3))
       )))


(defun rtest-maybe-make-rtest-nested-test-failures
  (&rest failure-lists)

  "If FAILURES contains real failures, return an object of type
rtest-nested-test-failures, otherwise return nil."

  (let*
    (
      (real-failures 
	(rtest-chop-out-nils 
	  (apply #'append failure-lists))))
    (if real-failures
      (make-rtest-nested-test-failures :failures real-failures)
      nil)))

(eval-when-compile
  (setf
    (get 'rtest-maybe-make-rtest-nested-test-failures 'rtest-suite)
    '("rtest-maybe-make-rtest-nested-test-failures"
       (
	 (rtest-maybe-make-rtest-nested-test-failures '(1 2))
	 (make-rtest-nested-test-failures :failures '(1 2)))
       
       ((rtest-maybe-make-rtest-nested-test-failures '())
	 nil)

       ((rtest-maybe-make-rtest-nested-test-failures '(1 2) '(3 4))
	 (make-rtest-nested-test-failures :failures '(1 2 3 4)))
       
       )))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;CLISP warns even tho this is perfect, so we'll just live with the
;;warning.

;;This must *only* signal with rtest-some-failure objects, which
;;neither Elisp nor CLISP guarantee.  So always use it thru
;;rtest-signal, never use it directly in signal.

(define-condition
  rtest-general-error (error) 
  ((report 
     :initarg :report 
     :type rtest-some-failure))
  (:documentation 
    "The type of all rtest errors.
Its data must be an rtest report.")
  (:report
    "rtest non-specific error"))


;;;;;;;;;;;
;;Constants

(defconst rtest-success          nil  
  "Returned when a test was successful." )

;;;;
;;Lists of keys

(defconst rtest-1-val-test-keys
  '( 
     :comparand
     :test
     :predicate
     :type
     :map
     :every
     :field
     :pattern
     )
  "" )

(defconst rtest-multi-val-test-keys
  '(
     :comparand-multi
     :test-multi
     :predicate-multi
     :map-multi
     :every-multi
     :pattern-multi
     )
  "" )


(defconst rtest-multi-val-alist 
  (mapcar #'list rtest-multi-val-test-keys)

  "" )

(defconst rtest-grader-keys
  (append rtest-1-val-test-keys rtest-multi-val-test-keys)
  "" )

(defconst rtest-let-keys 
  '(:let)
  "" )

(defconst rtest-around-keys 
  '(:around)
  "" )


(defconst rtest-include-key 
  :include 
  "Key that indicates a clause is an object to be included" )


(defconst rtest-all-keys
  (append rtest-grader-keys rtest-let-keys rtest-around-keys
    (list rtest-include-key)) 
  "" )


;;;;;;;;;
;;Special variables (Unused)

(defvar *bound-syms* nil "" )
(defvar *bound-vals* nil "" )



;;;;
;;Convert vector to list

(declaim (inline rtest-vec->list))
(defun rtest-vec->list
  (vec)
  ""
  (append vec 'nil))


;;;;
(defun true-list-p (term)
  "Returns t if the term is a non-dotted list. Note that nil is a true list."
  (declare (optimize (speed 3) (safety 0)))
  
  (while (consp term)
    (setq term (cdr term)))
  
  (not term))

(eval-when-compile
  (setf
    (get 'true-list-p 'rtest-suite)
    '("true-list-p"

       ( "Atoms are not true lists"
	 (true-list-p 1)
	 nil)

       ( "nil is a true list"
	 (true-list-p '())
	 t)

       ( "Ordinary lists are true lists"
	 (true-list-p '(1 2 3))
	 t)

       ( "Dotted lists aren't true lists"
	 (true-list-p '(1 2 . 3))
	 nil)


       )))


(eval-when-compile
  (setf
    (get 'rtest-visible 'rtest-suite)
    '("rtest-visible"
       rtest-make-single-failure-by-list
       rtest-maybe-make-rtest-nested-test-failures
       rtest-chop-out-nils
       true-list-p
       )))

(provide 'rtest-visible)

;;; rtest-visible.el ends here