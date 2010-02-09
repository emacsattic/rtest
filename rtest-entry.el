;;; rtest-entry.el --- Part of the rtest package

;; Copyright (C) 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;;         Wayne Mesard <wmesard@sgi.com>
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


;;;;;;;;;;;;;;;;
;;; Requirements

(require 'cl)

;;;;;
;;Set up load paths
(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-list
    'load-path
    default-directory)
  ;;Make the ancillary files available.
  (add-to-list
    'load-path
    (expand-file-name "ancillary" default-directory)))

(require 'pp)
(require 'tehom-font-lock)

;;Compatibility
(require 'rtest-compat)

;;Common to Elisp and Common Lisp
(require 'rtest-visible)
(require 'rtest-common)
(require 'rtest-field)
(require 'rtest-htmlify)
(require 'rtest-edit)
(require 'rtest-tools)

;;Elisp-only
(require 'rtest-report)
(require 'rtest-knownbuf)


;;;;;;;;;;;;;;;;;;;;
;;Configuration

;;Duplicates the `defgroup' in rtest-cl-entry
(defgroup rtest nil
  "The rtest testing system"
  :group 'lisp)


(defcustom rtest-nice-tables nil 
  "Does the HTMLifier use HTML tables?"
  :type  'boolean
  :group 'rtest)


;;;;;;;;;;;;;;;;;;;;
;;Internal variables

(defvar rtest-top-suites '() 
  "List of active suites." )

;;;;;;;;;;;;;;;;;;;;;;;
;;Helper

(defun rtest-elisp-report-results (results)
  ""

  (apply #'rtest-report-html (rtest-wrap-results results)))

(defun rtest-prompt-for-suites (verb)
  "Pick one or more test suites from all test suite symbols."

  (rtest-prompt-for-suites-aux-2 verb #'rtest-get-testable-functions))

(defun rtest-prompt-for-active-suites (verb)
  ""

  (rtest-prompt-for-active-suites-aux-2 
    verb
    'rtest-top-suites
    #'rtest-prompt-for-suites))


;;;;;;;;;;;;;;
;;Entry points

;;;###autoload
(defun rtest-next-sexp (p)
  ""
  
  (interactive "d")
  
  (save-excursion
    (let
      ( (form 
	  (read (current-buffer))))

      (rtest-elisp-report-results
	(rtest-one-test-object form)))))

;;;###autoload
(defun rtest-run-raw-suites (&rest suite-syms)
  "Run one or more test suites, from all possible suites."
  (interactive (rtest-prompt-for-suites "Run" ))

  (apply #'rtest-run-suites suite-syms))

;;;;;;;;;;;;;;;;;;;;;;;;
;;Managing active suites.

;;;###autoload
(defun rtest-run-suites (&rest suite-syms)
  "Run some of the active suites.
If there are none, make some suites active and run them."
  
  (interactive (rtest-prompt-for-active-suites "Run"))
  
  (rtest-elisp-report-results
    (rtest-symbol-list suite-syms)))


;;;###autoload
(defun rtest-run-all ()
  "Run all the active suites"
  
  (interactive)

  (apply #'rtest-run-suites rtest-top-suites))


;;;###autoload
(defun rtest-add-suites (&rest suite-syms)
  "Add suites to the list of active suites"
  (interactive (rtest-prompt-for-suites "Add" ))

  (dolist (sym suite-syms)
    (add-to-list 'rtest-top-suites sym)))


;;;###autoload
(defun rtest-arrange-suites (&rest suites)
  "Arrange the list of active suites"
  
  (interactive (rtest-prompt-for-active-suites "Keep"))
  (setq rtest-top-suites suites))


;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rtest-insert-call (&rest suites)
  "Inserts code to run one or more test SUITES.
The idea is that the programmer would put this at the end of the .el file."
  (interactive (rtest-prompt-for-suites "Insert" ))

  (insert 
    "\n;; Run diagnostics when this module is evaluated or compiled"
    "\n;; if and only if the rtest package is already loaded."
    "\n;; This code will not appear in the compiled (.elc) file"
    "\n"
    (pp-to-string
      `(eval-when-compile
	 (autoload 'rtest-suites "rtest-entry" "run test suites" t)
	 (if (featurep 'rtest-entry)
	   (rtest-suites
	     ,@suites))))))

;;Tests

(eval-when-compile
  (setf
    (get 'rtest-insert-call 'rtest-suite)

    '("rtest-insert-call"

       ( 
	 "Create the call site"
	 (progn
	   (rtest-insert-call 'foobar 'biz)
	   (goto-char (point-min))
	   (read (current-buffer)))
	 
	 '(eval-when-compile
	    (autoload 'rtest-suites "rtest-entry" "run test suites" t)
	    (if
	      (featurep 'rtest-entry)
	      (rtest-suites foobar biz)))
	 :around (with-temp-buffer))

       ( "Insert a new suite and get it added to the call site."
	 (progn
	   (rtest-insert-call 'foobar 'biz)
	   (goto-char (point-min))
	   (rtest-insert-suite "testme" "testmedoc" t)
	   (goto-char (point-min))
	   (read (current-buffer)))
	 '(eval-when-compile
	    (autoload 'rtest-suites "rtest-entry" "run test suites" t)
	    (if
	      (featurep 'rtest-entry)
	      (rtest-suites foobar biz)))
	 :around (with-temp-buffer))

       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Fontlock the various control structures we've defined here.  
(tehom-add-font-lock-to-all-lisp
  (eval-when-compile
    (list 

      (tehom-build-font-lock-expression  
	'(
	   "pattern-case"
	   "pattern-ecase"
	   "rtest-case"
	   "condlet*"
	   "condlet"
	   "pattern-bind"
	   "tehom-with-struct-slots"))

      ;;Basic parts of rtest.
      (list
	(concat
	  "'"
	  (regexp-opt
	    '("rtest-suite"
	       "rtest-setup")
	    t)
	  "\\>")
	1
	font-lock-constant-face)

      (tehom-build-font-lock-expression
	'( "rtest-error"
	   "rtest-signal"
	   "rtest-assert"
	   "assert-in-rtest")
	font-lock-warning-face))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'rtest-entry)

;;; rtest-entry.el ends here
