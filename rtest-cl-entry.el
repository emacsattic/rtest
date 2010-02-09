;;; rtest-cl-entry.el --- Emacs-lisp support for tests in Common Lisp

;; Copyright (C) 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: extensions, lisp

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

;; This file is the top level of the emacs interface to the Common
;; Lisp rtest.  That is, it is loaded in emacs to do rtest in an
;; inferior lisp process.

;; For the entry points to Emacs Lisp rtest, see rtest-entry.el

;; Setup:

;; It is IMPORTANT to have your inferior lisp set up properly.  If you
;; already use inferior lisp mode (`run-lisp'), it prolly already is
;; set up, but read on just in case.

;; inferior-lisp-program must name your Common Lisp interpreter, with
;; appropriate command line arguments.

;; inferior-lisp-prompt must match every Lisp prompt, including error
;; prompts like CLISP's "1. Break [17]>" and package-saying prompts
;; like "rtest[8]>" and "1. Break rtest[61]>".  NB, this variable
;; can't be customized (at least in emacs <= 20.4) but has to be set
;; in .emacs or similar.

;; If it's not already set up, I recommend playing with
;; isearch-forward-regexp in the *inferior-lisp* buffer, and then
;; verifying the regexp with search-forward-regexp.

;; This sets Common Lisp's `*print-case*' to `:downcase', otherwise
;; there would be case clashes between Common Lisp and Elisp.  If this
;; bothers you, I'm sorry, but a workaround would be far more complex.

;; Troubleshooting:

;; Make *SURE* emacs' inferior Lisp prompt regexp is set properly.
;; That's almost always the problem.

;; If a command freezes, and after you quit the command, the *inferior
;; lisp* buffer won't respond, the inferior-lisp-prompt variable is
;; prolly set wrong.  See Setup.

;; The easiest way to get out of that is to abort the command
;; (of course), kill your Lisp process, and restart it.  Then fix that
;; prompt.  

;; If you get error messages that symbols are unrecognized or are not
;; lists, and those symbols are in all upper case, eg `NIL' or `Nil',
;; you may have accidentally changed Common Lisp `*print-case*' to
;; something other than `:downcase'.  Fix: Change it back.

;;; Code:

;;; Requirements

(require 'cl)

;;Set up load paths
(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-list
    'load-path
    default-directory)
  ;;Make the ancillary files available.
  (add-to-list
    'load-path
    (expand-file-name "ancillary" default-directory)))

(require 'rtest-visible)
(require 'rtest-report)
(require 'rtest-edit)
(require 'tehom-inflisp)


;;;;;;;;;;;;;;;;;;;
;;Configuration

;;Duplicates the `defgroup' in rtest-entry
(defgroup rtest nil
  "The rtest testing system"
  :group 'lisp)

(defcustom rtest-lisp-directory "~/projects/rtest-3.0" 
  "The directory where rtest-lisp.lisp and other source files live" 
  :type  'directory
  :group 'rtest)


(defcustom rtest-nice-tables nil 
  "Does the HTMLifier use HTML tables?"
  :type  'boolean
  :group 'rtest)



;;;;;;;;;;;;;;;;;;
;;Helper functions



(defvar rtest-loaded-inferior-lisp-p nil "" )

(defun rtest-load-inferior-lisp ()
  "Start the rtest package in an inferior lisp buffer.

Starts inferior lisp if none is running."

  (interactive)
  (run-lisp inferior-lisp-program)
  (setq rtest-loaded-inferior-lisp-p t)

  (lisp-load-file 
    (expand-file-name "rtest-lisp.lisp" rtest-lisp-directory))

  (tehom-inflisp-eval-void 
    '(in-package "rtest"))

  (tehom-inflisp-eval-void
    '(setq 
       ;;Don't set *print-readably*, which often causes problems by
       ;;throwing errors when it just can't print readably.
       *print-case*     :downcase
       *print-level*    nil
       *print-length*   nil))

  (tehom-inflisp-eval-void
    `(defconstant user-mail-address ,user-mail-address)))
 

(defun rtest-ensure-inferior-lisp ()
  ""
  
  (unless rtest-loaded-inferior-lisp-p (rtest-load-inferior-lisp)))

(defun rtest-get-all-suite-names-inferior-lisp ()
  "Collect a list of suites in inferior lisp."

  (rtest-ensure-inferior-lisp)
  (let
    ((symbols 
       (tehom-inflisp-eval '(rtest-get-testable-functions))))
    
    (mapcar #'symbol-name symbols)))

(eval-when-compile
  (setf
    (get 'rtest-get-all-suite-names-inferior-lisp 'rtest-suite)
    '("rtest-get-all-suite-names-inferior-lisp"
       ( "Return a list of the symbols' names, each as a list of 1 string"
	 (let-defalias
	   tehom-inflisp-eval
	   (lambda (&rest dummy) '(Alpha Beta))
  
	   (rtest-get-all-suite-names-inferior-lisp))

	 '("Alpha" "Beta"))
       )))


(defun rtest-inferior-prompt-for-suites (verb)
  "Return a list of symbols naming the selected test suites.

The symbols mean nothing in Elisp, but we conform to the Elisp/Elisp
convention of using symbols, not strings."
  
  (rtest-prompt-for-suites-aux-2
    verb
    #'rtest-get-all-suite-names-inferior-lisp))



;;;;;;;;;;;;;;;;
;;User functions, paralleling the rtest-entry functions.


(defun rtest-inferior-eval-and-report (form)
  ""
  (rtest-ensure-inferior-lisp)
  (apply #'rtest-report-html 
    (tehom-inflisp-eval
      `(rtest-wrap-results ,form))))


;;;###autoload
(defun rtest-inferior-next-sexp (p)
  ""
  
  (interactive "d")
  
  (save-excursion
    (let
      ( (form 
	  (read (current-buffer))))

      (rtest-inferior-eval-and-report
	`(rtest-one-test-object ,form)))))


;;;###autoload
(defun rtest-inferior-run-raw-suites (&rest suite-syms)
  "Run one or more test suites, from all possible suites."

  (interactive 
    (rtest-inferior-prompt-for-suites "Run" ))

  (rtest-ensure-inferior-lisp)

  (rtest-inferior-eval-and-report 
    `(rtest-symbol-list ',suite-syms)))

(eval-when-compile
  (setf
    (get 'rtest-inferior-run-raw-suites 'rtest-suite)
    '("rtest-inferior-run-raw-suites"
       (
	 (let-defalias
	    tehom-inflisp-eval  list
	    (rtest-inferior-run-raw-suites 
	      '(MY-VAR)))
	 '((rtest-wrap-results (rtest-symbol-list '(MY-VAR)))))
       
       (
	 (let-defalias
	   tehom-inflisp-eval  list
	   (rtest-inferior-run-raw-suites 
	     '(MY-VAR THIS-VAR THAT-VAR)))

	 '((rtest-wrap-results 
	    (rtest-symbol-list '(MY-VAR THIS-VAR THAT-VAR)))))
       )))



;;;;;;;;;;;;;;;;;;;;;;;;
;;Managing active suites.

(defvar rtest-inferior-active-suites nil "" )

(defun rtest-prompt-for-inferior-active-suites (verb)
  ""

  (rtest-prompt-for-active-suites-aux-2
    verb
    'rtest-inferior-active-suites
    #'rtest-inferior-prompt-for-suites))



;;;###autoload
(defun rtest-inferior-run-suites (&rest suite-syms)
  "Run some of the active suites.
If there are none, make some suites active and run them."
  
  (interactive (rtest-prompt-for-inferior-active-suites "Run"))

   (apply #'rtest-inferior-run-raw-suites suite-syms))


;;;###autoload
(defun rtest-inferior-run-all ()
  "Run all the active suites"
  
  (interactive)

  (apply #'rtest-inferior-run-raw-suites rtest-inferior-active-suites))

;;;###autoload
(defun rtest-inferior-add-suites (&rest suite-syms)
  "Add suites to the list of active suites"
  (interactive (rtest-inferior-prompt-for-suites "Add" ))

  (dolist (sym suite-syms)
    (add-to-list 'rtest-inferior-active-suites sym)))


;;;###autoload
(defun rtest-inferior-arrange-suites (&rest suites)
  "Arrange the list of active suites"
  
  (interactive (rtest-prompt-for-inferior-active-suites "Keep"))
  (setq rtest-inferior-active-suites suites))

;;;;;;;;;;;;;;;

 
(eval-when-compile
  (setf
    (get 'rtest-cl-entry-rtest 'rtest-suite)
    '("rtest-cl-entry-rtest
These tests require an inferior lisp process to be running, and have
loaded the rtest package." 
       
       rtest-get-all-suite-names-inferior-lisp
       rtest-inferior-run-raw-suites

       
       )))


;;This is tested by hand because it is so interactive.
'
(progn
  (rtest-ensure-inferior-lisp)
  ;;Create dummy test suites.
  (tehom-inflisp-eval-void
    '(progn
       (defvar my-good-suite nil)
       (setf (get 'my-good-suite 'rtest-suite)
	 '((1 1)))

       (defvar my-bad-suite nil)
       (setf (get 'my-bad-suite 'rtest-suite)
	 '((1 2))))))

;;Run these tests and observe the results.
'
(rtest-inferior-run-raw-suites 'my-good-suite)

'
(rtest-inferior-run-raw-suites 'my-bad-suite)


(provide 'rtest-cl-entry)

;;; rtest-cl-entry.el ends here
