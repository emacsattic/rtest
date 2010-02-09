;;; rtest-plus.el --- Extra entry points for rtest

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
(require 'rtest-probe)
(require 'rtest-suite)
(require 'caselet)

;;;;;;;;;;;;;;;;;;;;;
;;Use rtest clauses in a sort of case statement.

(defun rtest-single-case (sym catch-sym clauses &rest body)
  ""
  
  (let
    ((simple-tests 
       (rtest-parse-form clauses)))
    
    `(if
       ;;rtest is report-style so `nil' == succeeded,
       ;;anything else == failed.
       (null
	 (rtest-run-grader-list (list ,sym) ',simple-tests () ()))
       (throw ,catch-sym
	 (progn ,@body)))))


;;;###autoload
(defmacro rtest-case (probe &rest cases)
  "Like `case', but uses rtest grader clauses."

  (caselet-worker 
    #'rtest-single-case nil probe cases))

(eval-when-compile
  (setf
    (get 'rtest-case 'rtest-suite)
    '("rtest-case"

       ( "It selects the appropriate case"
	 (rtest-case (+ 1 2)
	  ((:comparand 1) 'A)
	  ((:comparand 3) 'B))
	 'B)
       
       ( "If there is no match, we get nil"
	 (rtest-case (+ 1 2)
	  ((:comparand 1) 'A))
	 nil)
       

       )))

;;;;;;;;;

;;;###autoload
(defmacro rtest-assert (form &rest dummy)
  "Like assert, but FORM must be an rtest.
This is never continuable and ignores further arguments."
  
  `(let ((failure (rtest-one-probe-0 ',form)))
     (when failure
       (rtest-signal failure))))

;;Untested

(eval-when-compile
  (setf
    (get 'rtest-plus 'rtest-suite)
    '("rtest-plus"
       rtest-case
       )))

(provide 'rtest-plus)

;;; rtest-plus.el ends here