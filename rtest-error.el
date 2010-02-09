;;; rtest-error.el --- Error-handling for rtest

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
(require 'rtest-format)


(defun* rtest-safe-wrap-rtest-error
  (err &optional (wrap-function #'identity))
  ""
  (let
    ((report
       (rtest-err->typeable err)))
    (cond
      ((typep report 'rtest-some-failure)
	(funcall wrap-function report))

      (t
	(funcall 
	  wrap-function
	  (rtest-make-single-failure-by-list 
	    "Bad object signalled.  Should be a rtest report"
	    (err)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro rtest-wrap-errors 
    (wrap-report single-to-wrappable wrap-raw &rest body)

    "Protect an rtest-report generator against errors.

The three functions must be unquoted.

WRAP-RAW tells how to wrap a non-rtest error into an rtest report.

SINGLE-TO-WRAPPABLE tells how to transform an rtest report into
something WRAP-REPORT can wrap.  This effectively means it's either
`identity' or `list'.

WRAP-REPORT tells how to wrap an rtest report into the particular type
of report we actually return.

BODY generates either nil or something that WRAP-REPORT can return.
NB SINGLE-TO-WRAPPABLE is not called on the result of BODY."

    ;;Takes symbols or lambda list-forms.  We want to call the functions
    ;;the symbols stand for, but we don't know at this point whether
    ;;it's a list-form or a symbol.  We'll find that out when we eval
    ;;them, which we can only do after we return.  So we use `labels',
    ;;preferred over flet because it's lexical in Elisp (no relevant
    ;;difference in CL)

    (let
      (
	(full-wrap-sym (gensym)))

      `(labels
	 (
	   (,full-wrap-sym (X)
	     (funcall #',wrap-report
	       (funcall #',single-to-wrappable X))))
       
	 (condition-case err
      
	   (let
	     ((failure
		(progn
		  ,@body)))

	     (if
	       failure
	       (funcall #',wrap-report failure)
	       nil))

	   (rtest-general-error
	     (rtest-safe-wrap-rtest-error err #',full-wrap-sym))

	   (error
	     (funcall #',full-wrap-sym
	       (funcall #',wrap-raw 
		 err))))))))

;;;;;;;;
;;Error reporting, 

(defun rtest-error (&rest pieces)
  ""
  
  (error (apply #'rtest-format-list pieces)))


(eval-when-compile
  (setf
    (get 'rtest-error 'rtest-suite)
    '("rtest-error"
       ( "Makes an error"
	 (rtest-error "Values: "'rtest-newline" == "rtest-newline)
	 :predicate rtest-error-p)

       )))


;;;;;;;;;;;;;;;;;;;;
;;Assert-like macros

;;The args don't correspond to assert, because it expects to be part
;;of a test, not something whose values the user will want to change
;;in mid-test.  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro assert-in-rtest (invariant fail-string &rest fail-objects)
    ""
  
    `(when
       (not ,invariant)
       (rtest-signal
	 (rtest-make-single-failure-by-list 
	   ,fail-string 
	   ,fail-objects)))))


(provide 'rtest-error)

;;; rtest-error.el ends here