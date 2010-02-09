;;; rtest-edit.el --- Emacs interaction for the rtest package

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

;;; Requirements

(require 'cl)
(require 'hideshow)
(require 'arrange)


;;; Configuration

(defcustom rtest-expert nil
  "If nil, \\[rtest-insert-suite] will insert some helpful comments.
If not nil and not t, it will insert a one-line helpful comment."
  :type 'boolean
  :group 'rtest)

(defcustom rtest-default-variable-name t
  "If non-nil, \\[rtest-insert-suite] will attempt to supply a default name
for the new test suite."
  :type 'boolean
  :group 'rtest)



;;;;;;;;;;;;;
;;; Constants

;;;;;;;;;
;;Version

(defconst rtest-version "3.0")
(defconst rtest-maintainer-address "tob@world.std.com")

;;;;;;;;;
;;Other constants

(defconst rtest-defun-types 
  '(defun defun* defsubst defsubst* defmacro defmacro* defmethod) 
  "List of defun-variant symbols we mite see" )

(defconst rtest-lisp-mode-alist
  '(
     (emacs-lisp-mode       . rtest-run-suites)
     (lisp-interaction-mode . rtest-run-suites)
     (lisp-mode             . rtest-inferior-run-raw-suites))

  "Map from the various lisp modes to a suitable rtest function." )

(defconst rtest-properties-that-are-tests 
  '(rtest-suite regression-suite)
  "Properties that denote rtest suites" )


;;; Variables


(defvar rtest-history '() "" )

;;;;;;;;;;;;;
;;; Functions

;;;;;;;;;;;
;;symbol<->promptable

(defun rtest-map-strings-to-symbols (strings)
  ""
  
  (mapcar #'intern-soft strings))


(defun rtest-prompt-for-suites-aux-2 (verb acquire-func)
  "Pick one or more test suites from all test suite symbols."
  (let*
    ( 
      (all-suite-names-alist (funcall acquire-func))
      
      (suite-name-list
	(rtest-prompt-for-suites-aux verb
	  all-suite-names-alist)))

    (rtest-map-strings-to-symbols suite-name-list)))
 
(defun rtest-prompt-for-active-suites-aux-2 
  (verb active-suites-sym acquire-func)
  ""

  (if
    (null (symbol-value active-suites-sym))
    
    ;;If there are no active suites, choose from all possible suites
    ;;and make those ones active.
    (let
      ((new-suites (funcall acquire-func verb)))
      (set active-suites-sym new-suites)
      new-suites)

    (let*
      ( 
	(all-suite-names-alist 
	  (mapcar #'symbol-name (symbol-value active-suites-sym)))

	(suite-name-list
	  (rtest-prompt-for-suites-aux verb
	    all-suite-names-alist)))

      (rtest-map-strings-to-symbols suite-name-list))))

;;Old version that doesn't use arrange.el.  This is kept around just
;;in case we ever want to offer a choice between the 2 selectors.

'
(defun rtest-prompt-for-suites-aux (verb all-suite-names)
  (let
    ( (all-suite-names-alist (mapcar #'list all-suite-names))
      (suites-to-use '())
      (done nil))
    
    (while (not done)
      (let* 
	(
	  (prompt
	    (concat verb " test suite "
	      (if suites-to-use
		"(Return when done)"
		"(Return for all)")
	      ": "))
	  (nam
	    (completing-read 
	      prompt all-suite-names-alist nil t nil 'rtest-history)))
	
	(if (string= nam "")
	  (setq done t)

	  (push nam suites-to-use))))

    (or suites-to-use
      (mapcar #'car all-suite-names-alist))))

(defun rtest-prompt-for-suites-aux (verb all-suite-names)
  ""
  (arrange-strings-other-window 
    all-suite-names 
    (concat verb " which test suites? ")))



(defun rtest-get-defun-name ()
  ""
  (save-excursion
    (beginning-of-defun)
    (down-list 1)
    
    (let
      ;;Get the head of the list-form
      ((type (read (current-buffer))))
      
      (cond 
	;;If we're looking at a lisp-type function, get its name
	( 
	  (memq type rtest-defun-types)
	  (if
	    (looking-at "[\n\t ]*\\([^\n\t ]+\\)")
	    (buffer-substring-no-properties
	      (match-beginning 1) (match-end 1))
	    nil))
	
	;;Otherwise nil
	(t nil)))))


(defun rtest-get-buffer-name ()
  ""
  
  (when 
    buffer-file-name
    (file-name-nondirectory
      (file-name-sans-extension buffer-file-name))))

(defun rtest-default-variable-name ()
  (if rtest-default-variable-name
    (let 
      ((str 
	 (cond
	   ((rtest-get-defun-name))
	   ((rtest-get-buffer-name)))))

      (if str
	(cons str 0)))))



(defun rtest-interactively-get-suite-name ()
  ""

  (let* ((var (read-from-minibuffer
                "Variable name for this test suite: "
                (rtest-default-variable-name)
                nil t))
          (vname (symbol-name var))
          (doc (read-string
		 (concat "Documentation: (default \"" vname "\"): ")))
          )
    (list vname
      (prin1-to-string (if (zerop (length doc)) vname doc))
      (let ((loc (rtest-call-site)))
	(if (and loc
	      (or current-prefix-arg
		(y-or-n-p "Add to the rtest call site below? ")))
	  loc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun rtest-insert-suite (name docstring update-call-site)
  "Insert a template for a new test suite.
Prompts for the NAME of the variable to use, and for a short DOCSTRING
describing the purpose of the test suite.
  If rtest-expert (which see) is non-nil, some helpful comments are also
inserted."
  (interactive (rtest-interactively-get-suite-name))

  (end-of-defun)
  (if (eq t update-call-site)
    ;; for non-interactive calls (otherwise the caller would have to call
    ;; rtest-call-site itself).
    (setq update-call-site (rtest-call-site)))

  (insert 
    "\n(eval\-when-compile"
    )
  (unless rtest-expert
    (insert
      "\n  ;; This code will not appear in the compiled (.elc) file"))

  (insert
    "\n  (setf"
    "\n    (get '" name " 'rtest-suite)"
    "\n    '(" docstring
    )
  (cond
    ((eq t rtest-expert))

    (rtest-expert
      (insert "\n     ;; ([description] probe grader)"))
    
    (t
      (insert
	"\n     ;; The simplest form for a test is:"
	"\n     ;;   ([description] probe grader)"
	"\n     ;;   DESCRIPTION - string"
	"\n     ;;   PROBE -  a sexp which runs the actual test"
	"\n     ;;   GRADER - the desired result or a sexp which determines"
	"\n     ;;   how we did"
	)))


  (save-excursion
    (insert 
      "\n       )))"
      "\n"
      )
    (if update-call-site
      (progn
	(goto-char update-call-site)
	(insert " " name)))))


;;;;;;;;;;;;;;;;;;;;;;;

(defun rtest-call-site ()
  (save-excursion
    (let ((regexp "(if (featurep 'rtest)[\n\t ]*(rtest-suites"))
      (goto-char (point-min))
      (when
	(and (re-search-forward regexp nil t)
	  (not (re-search-forward regexp nil t)))
	(point-marker)))))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; Recognizing and running test suites from a point in a buffer



(defun rtest-access-suite-name (sexp)
  ""

  ;;Don't die when destructuring-bind fails.
  (condition-case err

    ;;Poor-man's pattern-matching.  
    (destructuring-bind
      (sb-setf 
	(sb-get (sb-quote-0 name) (sb-quote-1 property)) &rest anything)
      sexp
      (if
	(and
	  (eq sb-setf    'setf)
	  (eq sb-get     'get)
	  (eq sb-quote-0 'quote)
	  (eq sb-quote-1 'quote)
	  (memq property rtest-properties-that-are-tests))
	name))
    (error nil)))


(eval-when-compile
  (setf
    (get 'rtest-access-suite-name 'rtest-suite)
    '("rtest-access-suite-name"

       ( "Return the suite's name defined by the expression."
	 (rtest-access-suite-name 
	  '(setf (get 'nameless 'rtest-suite)))

	 'nameless)

       ( "If no suite's name is defined, return nil."
	 (rtest-access-suite-name 
	  '(setf (get 'nameless 'something-else)))

	 nil))))



(defun rtest-read-suite-name ()
  "Read the following sexps until a suite-name is seen.

When called, it must be looking at a possible list of tests such as
rtest-insert-suite produces."

  (let
    ((found nil)
      sexp)
    ;;read each sexp and stop when one of them works.  Error if we
    ;;read too far.  
    (while (not found)
      (setq sexp (read (current-buffer)))
      
      (setq found (rtest-access-suite-name sexp)))

    found))
;;Tested by hand.  

;;;###autoload
(defun rtest-defun (arg)
  "Run tests on the function or suite under point.

If prefix ARG is non-nil, eval it first.

Does nothing if the buffer is not in a known lisp mode."

  (interactive "P")

  (block test-defun
    (let*
      ( (rtest-mode-data (assoc major-mode rtest-lisp-mode-alist))
	(rtest-func (cdr rtest-mode-data)))

      ;;If there's no way to run tests we find, stop now.
      (if
	(not rtest-mode-data)
	(return-from test-defun))
    
      (when arg (eval-defun nil))

      (save-excursion
      
	;;Find position
	(beginning-of-defun)

	;;Now read stuff.
	(down-list 1)
  
	(let
	  ;;Get the head of the list-form
	  ((type (read (current-buffer))))
      
	  ;;Now point is immediately after the first element.
	  (cond 
	
	    ;;We're looking at a lisp-type function
	    ( 
	      (memq type rtest-defun-types)
	      (let
		((suite-name (read (current-buffer))))
		(funcall rtest-func suite-name)))
	
	    ;;We're prolly looking at a rtest test form.  Later, return
	    ;;value may be a list of suites instead.  
	    ( 
	      (eq type 'eval-when-compile)
	      (let
		((suite-name (rtest-read-suite-name)))
		(funcall rtest-func suite-name)))))))))
;;Tested by hand.  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun rtest-hide-tests ()
  "Hide all the \(eval-when-compile ... \) forms.

hide-show mode must be active for this to work, see `hs-minor-mode'."
  
  (interactive)
  (when hs-minor-mode
    (while
      (re-search-forward "^([ \t]*eval\-when-compile" nil t)
      (goto-char (match-beginning 0))
      (hs-hide-block-at-point))))


;;;###autoload
(defun rtest-load-only-suites ()
  "Eval the blocks protected by eval-when-compile."
  
  (interactive)
  (goto-char 1)
  (while
    (re-search-forward "^([ \t]*eval\-when-compile" nil t)
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)))))

(eval-when-compile
  (setf
    (get 'rtest-load-only-suites 'rtest-suite)
    '("rtest-load-only-suites"

       ( "Evaluates everything inside eval-when-compile"
	 (with-buffer-containing 
	   "(eval-when-compile (setq a 56))"
	   (let ((a 13))
	     (rtest-load-only-suites)
	     a))
	 56)

       ( "Evaluates nothing outside eval-when-compile"
	 (with-buffer-containing 
	   "(setq a 56)"
	   (let ((a 13))
	     (rtest-load-only-suites)
	     a))
	 13)
       
       )))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submitting bug reports


(defun rtest-submit-bug-report ()
  "Submit via mail a bug report on rtest"
  (interactive)
  (require 'reporter)
  (let
    (
      (has-arrange         (featurep 'arrange))
      (has-tehom-cl        (featurep 'tehom-cl))
      (has-comint-redirect (featurep 'comint-redirect))
      (has-local-vars      (featurep 'local-vars)))
    
    (reporter-submit-bug-report
      rtest-maintainer-address
      (concat "rtest " rtest-version)
      (list
	'major-mode;;ie, is it Elisp or Common Lisp.
	'rtest-permanent-init-forms
	;;Do we have all the support packages loaded?  NB, not all are
	;;needed for both Elisp and Common Lisp
	'has-arrange
	'has-tehom-cl
	'has-comint-redirect
	'has-local-vars
	))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile
  (setf
    (get 'rtest-edit 'rtest-suite)
    '("rtest-edit"
       rtest-access-suite-name
       )))

(provide 'rtest-edit)

;;; rtest-edit.el ends here