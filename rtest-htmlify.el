;;; rtest-htmlify.el --- HTML-building functions for rtest

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

;;
(require 'rtest-visible)
(require 'rtest-field)



;;;;;;;;;
;;Configuration

(defvar rtest-nice-tables)

;;;;;;;;;
;;Helpers

(defconst rtest-newline         "<BR>
" )
(defconst rtest-horizontal-rule "<HR>
" )
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro rtest-html-in-element (element &rest rest)
    ""

    (check-type element string)
  
    (let
      ( (start-tag (concat "<"  element ">
"))
	(end-tag   (concat "</" element ">
")))
    
      `(list
	 ,start-tag
	 ,@rest
	 ,end-tag))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;HTMLization functions:  

;;Each one returns a string-tree.  We'll flatten it before printing
;;it.
(declaim
  (ftype
    (function (t)
      (satisfies rtest-stringtree-p))
    rtest-htmlize-object
    rtest-htmlize-poor-table
    rtest-htmlize-table
    rtest-htmlize-single-failure
    rtest-htmlize-nested-test-failures
    rtest-htmlize-field-failures
    rtest-htmlize-probe-failure
    rtest-htmlize-suite-results
    rtest-htmlize-report-recursive
    rtest-htmlize-failure-list))



(defun rtest-htmlize-object (obj)
  "Print a general object in HTML"

  (rtest-html-in-element "pre"
    (rtest-print-to-string obj)))

(eval-when-compile
  (setf
    (get 'rtest-htmlize-object 'rtest-setup)
    '(progn
       (defconst rtest-printify-built-OK 
	 '(	 :test (not (rtest-error-p RESULT))
	    :predicate rtest-stringtree-p)
	 "Conditions indicating that a function didn't die and built a
rtest-stringtree-p." )))

  
  (setf
    (get 'rtest-htmlize-object 'rtest-suite)
    '("rtest-htmlize-object"
       ((rtest-htmlize-object '(1))
	 :include rtest-printify-built-OK)
       
       )))

(defun rtest-htmlize-poor-table
  (object)
  ""
  (if
    (not (consp object))
    "(Object table should be a list)"
    (list
      (mapcar
	#'(lambda
	    (pair)
	    (if
	      (not
		(consp pair))
	      "(Bad entry, should be a cons)"
	      (list 
		"
"
		(car pair)
		" = "
		(rtest-htmlize-object
		  (cdr pair))
		"
")))
	object))))

(defun rtest-htmlize-table
  (object)
  ""
  (if
    (not
      (consp object))
    "(Object table should be a list)"
    (rtest-html-in-element "table"
      (list
	(rtest-html-in-element "tr"
	  (rtest-html-in-element "th" "name")
	  (rtest-html-in-element "th" "value"))
	(mapcar
	  #'(lambda
	      (pair)
	      (if
		(not
		  (consp pair))
		"(Bad entry, should be a cons)"
		(rtest-html-in-element "tr"
		  (rtest-html-in-element "td"
		    (car pair))
		  (rtest-html-in-element "td"
		    (rtest-htmlize-object
		      (cdr pair))))))
	  object)))))

;;;; Dispatch on various structures

(defun rtest-htmlize-single-failure (this)
  ""

  (check-type this rtest-single-failure)
  
  (tehom-with-struct-slots
    (text object) this rtest-single-failure
    
    (list
      (rtest-html-in-element "strong" "Failure")
      rtest-newline
      text
      (when 
	object
	(if rtest-nice-tables
	  (rtest-htmlize-table      object)
	  (rtest-htmlize-poor-table object))))))


(defun rtest-htmlize-nested-test-failures (this)
  ""
   
  (rtest-htmlize-failure-list
    (rtest-nested-test-failures-failures this)))


(defun rtest-htmlize-field-failures (this)
  ""

  (tehom-with-struct-slots
    (field-indicator actual-value failures)
    this rtest-field-test-failures

    (list

      "Field: "
      (rtest-htmlize-object field-indicator)
      rtest-newline

      "Actual value: "
      (rtest-htmlize-object actual-value)
      rtest-newline

      "Failures: "
      (rtest-htmlize-failure-list failures)
      rtest-newline)))

;;We just assure that there is no an error and result is a
;;string-tree. 
(eval-when-compile
  (setf
    (get 'rtest-htmlize-field-failures 'rtest-suite)
    '("rtest-htmlize-field-failures"
       (rtest rtest-borrow-setup 'rtest-htmlize-object)

       ("Succeeds on an object whose list isn't a list"
	 (rtest-htmlize-field-failures
	   (make-rtest-field-test-failures 
	     :field-indicator "a" 
	     :actual-value    "b" 
	     :failures        52)
	   
	   ;;'(rtest-field-test-failures "a" "b" 52)
	   )
	 :include rtest-printify-built-OK)
       
       
       ( (rtest-htmlize-field-failures
	   (make-rtest-field-test-failures
	     :field-indicator 1
	     :actual-value    12))
	 :include rtest-printify-built-OK)

       )))



(defun rtest-htmlize-probe-failure (this)
  ""

  (assert (rtest-probe-failure-p this))

   (tehom-with-struct-slots
     (docstring probe-sexp result-list failures results-are-multiple)
     this rtest-probe-failure

     (list
       docstring  ;;It's OK if docstring is nil
       rtest-newline

       "Probe: "
       (rtest-htmlize-object probe-sexp)
       rtest-newline
       
       (if results-are-multiple
	 (list
	   "Actual values: "
	   (mapcar
	     #'rtest-htmlize-object
	     result-list))
	 
	 (list
	   "Actual value: "
	   (rtest-htmlize-object (car result-list))))
       rtest-newline

       (rtest-htmlize-failure-list failures)
       rtest-newline)))


(defun rtest-htmlize-suite-results (this)
  ""
  
  (let
    ((nodes (rtest-suite-results-nodes this)))

    (list
      (rtest-html-in-element "H3" 
	(rtest-suite-results-docstring this))

      (if nodes
	(rtest-htmlize-failure-list nodes)
	(list
	  "All tests passed"
	  rtest-newline)))))


;;;;
;;Less specific dispatchers.

(defun rtest-htmlize-report-recursive (this)
  ""

  ;;Dispatch the various types.  
  (typecase this

    (rtest-single-failure
      (rtest-htmlize-single-failure this))

    (rtest-nested-test-failures
      (rtest-htmlize-nested-test-failures this))

    (rtest-field-test-failures
      (rtest-htmlize-field-failures this))
    
    (rtest-probe-failure
      (rtest-htmlize-probe-failure this))

    (rtest-suite-results
      (rtest-htmlize-suite-results this))
    
    (t
      (list
	"Bad report object:"
	(rtest-htmlize-object this)))))


(defun rtest-htmlize-failure-list
  (this)
  ""

  (cond
    ;;If it's not a list, report a complaint.
    ((not (listp this))
      (rtest-html-in-element "strong"
	"Report object should have been a list"
	(rtest-htmlize-object this)))
    
    ;;If it's empty, report the blank string.
    ((null this)
      "")
    
    ;;And normally, do the list recursively
    (t
      (rtest-html-in-element "ol"
	(mapcar 
	  #'(lambda (element)
	      (rtest-html-in-element "li"
		(rtest-htmlize-report-recursive element)))
	  this)))))



;;Report HTML dispatch functions
;;;;;;;;;;;;;
;;HTML management functions

(defun rtest-htmlize-summary (num-tests num-fails)
  ""

  (list
    (rtest-html-in-element "H1"
      "Test Report")

    "Generated by: "
    user-mail-address 
    rtest-newline
    (current-time-string)
    rtest-newline
      
    "Tests: "
    (number-to-string num-tests)
    rtest-newline
    "Failures: "
    (number-to-string num-fails)
    rtest-newline
    "Score: "
    (number-to-string
      (truncate (/ (float (* 100 (- num-tests num-fails)))
		  num-tests)))
    "%"
    rtest-newline))

(eval-when-compile
  (setf
    (get 'rtest-htmlize-summary 'rtest-suite)
    '("rtest-htmlize-summary"
       (rtest rtest-borrow-setup 'rtest-htmlize-object)
       ((rtest-htmlize-summary 4 2)
	 :include rtest-printify-built-OK)
       
       )))


(defun rtest-htmlize-report-body (num-tests num-fails report)
  ""
  
  (list
    (rtest-htmlize-summary num-tests num-fails)
    rtest-horizontal-rule
    (rtest-html-in-element "H2" "Individual failures")
    (rtest-htmlize-report-recursive report)))


(defun rtest-htmlize-report-head ()
  ""
  (rtest-html-in-element "head"
    (rtest-html-in-element "title"
    "Test Report generated by rtest")))

(defun rtest-htmlize-report-top (num-tests num-fails report)
  ""
  
  (list
    "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">"
    "
"
    (rtest-html-in-element "html"
      (rtest-htmlize-report-head)
      (rtest-htmlize-report-body num-tests num-fails report))))


(defun rtest-wrap-results (results)
  ""
  (tehom-with-struct-slots
    (test-count fail-count failure)
    results rtest-fail-summary
    
    (let
      ((html 
	 (rtest-htmlize-report-top test-count fail-count failure)))
      (list test-count fail-count html))))

;;;;;;;;;;;;;;

(eval-when-compile
  (setf
    (get 'rtest-htmlify 'rtest-suite)
    '("rtest-htmlify"
       rtest-htmlize-object
       rtest-htmlize-field-failures
       rtest-htmlize-summary
       )))

(provide 'rtest-htmlify)

;;; rtest-htmlify.el ends here