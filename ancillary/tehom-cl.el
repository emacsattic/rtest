;;; tehom-cl.el --- Extra support for the cl package structures

;; Copyright (C) 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: extensions, tools, lisp

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

;; These functions give the cl package's structures a little more of
;; the Common Lisp functionality.

;;; Code:

(require 'cl)
(require 'tehom-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tehom-get-field-sym-name (field-key)
  "Get the name of a field symbol.
`symbol-name' does this in Common Lisp."
  
  (let*
    ((raw-name (symbol-name field-key)))
    
    (if
      (eq (aref ":" 0) (aref raw-name 0))
      (substring raw-name 1)
      raw-name)))


(eval-when-compile
  (setf
    (get 'tehom-get-field-sym-name 'rtest-suite)
    '("tehom-get-field-sym-name"

       ( "Gets the concat-able name of a field from a symbol starting
with `:'"
	 (tehom-get-field-sym-name :my-field)
	 "my-field")
	 
       ( "Gets the concat-able name of a field from its bare symbol"
	 (tehom-get-field-sym-name 'my-field)
	 "my-field")
         
       )))


(defun tehom-get-struct-sym-name (struct-sym)
  "Get the name of a vector-like defstruct symbol.
`symbol-name' does this in Common Lisp."

  
  (using-local-vars
    (local-var raw-name (symbol-name struct-sym))
    
    (string-match "cl-struct-" raw-name)
    (local-var start-of-struct-name 
      (match-end 0))
    (local-var struct-name 
      (substring raw-name start-of-struct-name))

    struct-name))

(eval-when-compile
  (setf
    (get 'tehom-get-struct-sym-name 'rtest-suite)
    '("tehom-get-struct-sym-name"
       ;;These tests use structs from rtest-tools
       ( "Gets the concat-able name of a structure from its symbol"
	 (tehom-get-struct-sym-name 'cl-struct-rtest-struct)
	 "rtest-struct")
	 
       ( "Resists non-struct symbols"
	 (tehom-get-struct-sym-name 'vector)
	 :predicate rtest-error-p)
         
       )))


(defun tehom-cl-sym-in-tags-p
  (struct-sym tags-sym-name)
  ""

  (memq struct-sym
    (symbol-value
      (intern tags-sym-name))))


(defun tehom-cl-list-struct-sym
  (obj)
  ""
  (let
    ((struct-sym (car obj)))
      
    (if
      (and
	(symbolp struct-sym)
	(tehom-cl-sym-in-tags-p 
	  struct-sym 
	  (concat "cl-struct-" (symbol-name struct-sym) "-tags")))
      struct-sym
      nil)))


(defun tehom-cl-vector-struct-sym
  (obj)
  ""
  (let
    ((struct-sym (aref obj 0)))
      
    (if
      (and
	(symbolp struct-sym)
	(tehom-cl-sym-in-tags-p 
	  struct-sym 
	  (concat (symbol-name struct-sym) "-tags")))

      (intern
	(tehom-get-struct-sym-name struct-sym))
      
      nil)))

(defun tehom-type-of (obj)
  "Return the type of OBJ, even if it's a structure.
`type-of' does this in Common Lisp."

  (let
    ((type (type-of obj)))
  
    (case type
      ('vector
	(or 
	  (tehom-cl-vector-struct-sym obj)
	  type))
	
      ('cons
	(or 
	  (tehom-cl-list-struct-sym obj)
	  type))
	
      (t type))))

(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (setf
    (get 'tehom-type-of 'rtest-suite)
    '("tehom-type-of"
       ( "Gets the type of a given structure, as a symbol"
	 (tehom-type-of
	   (make-rtest-struct :my-field '(A B C)))

	 'rtest-struct)

       ( "Gets the type of a list-like structure, as a symbol"
	 (tehom-type-of
	   (make-rtest-struct-list :my-field '(A B C)))

	 'rtest-struct-list)

       )))



;;Not possible in Common Lisp, but useful.
(defun tehom-get-struct-slots (struct-sym)
  ""
  (remove* 'cl-tag-slot
    (mapcar
      #'car
      (get struct-sym 'cl-struct-slots))))

(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (setf
    (get 'tehom-get-struct-slots 'rtest-suite)
    '("tehom-get-struct-slots"
       ( (tehom-get-struct-slots 'rtest-struct)
	   
	 '(my-field my-second-field))
       
       )))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;constantp, or a reasonable facsimile thereof.

;;We don't have real constants in Elisp.  In theory we could test some
;;property of a symbol, call it `tehom-i-am-constant'?

;;These symbols should be treated as literal, even tho they are
;;unbound.  So we simply make them self-evaluating.
(defconst rtest-literal-symbols 
  '( &rest &body &optional &keys &aux)
  "" )
(dolist (sym rtest-literal-symbols)
  (set sym sym))


;;This plays loosely with the difference between "constant" and
;;"literal".
(defun constantp (form)
  "non-nil if FORM is a literal.
FORM must be an atom."

  (cond
    ;;A vector is constant if all its parts are constant
    ((vectorp form)
      (every #'constantp form))

    ;;A symbol is considered constant if bound and self-evaluating.
    ((symbolp form)
      (and
	(boundp form)
	(eq
	  (symbol-value form)
	  form)
	;;`form' is self-evaluating, but it's an artifact of the fact
	;;that it's bound *here*, and it still isn't a literal.
	(not (eq form 'form))))

    ;;;;;;;;;;;
    ;;Do similarly for any other atoms that should be considered carefully.

    ;;All other atoms are considered constant.
    ((atom form)
      t)

    ;;Lists are only considered constant if quoted, otherwise they be
    ;;function calls.
    (t
      (eq
	(car form)
	'quote))))



(eval-when-compile
  (setf
    (get 'constantp 'rtest-suite)
    '("constantp"
       ( "Normal literals give non-nil"
	 (constantp 12)
	 t)
       
       ( "Normal literals give non-nil"
	 (constantp "abc")
	 t)

       ( "Self-evaluating symbols are considered literals here"
	 (constantp t)
	 t)
       

       ( "Non self-evaluating symbols give nil"
	 (let (foo)
	   (constantp 'foo))
	 nil)

       ( "Unbound symbols give nil"
	 (let (foo)
	   (makunbound 'foo)
	   (constantp 'foo))

	 nil)
       
       ( "`form', defined within the call itself, doesn't falsely
appear constant." 
	 (constantp 'form)
	 nil)

       ( "Unquoted lists give nil.  NB, the single quote is stripped
before constantp sees it"
	 (constantp '(2))
	 nil)
       
       ( "Vectors with all constants give t"
	 (constantp [1 2 3])
	 t)
       
       ( "Vectors with some variables give nil"
	 (let (foo)
	   (constantp [1 foo foo 4]))
	 nil)
       

       )))

;;;;;;;;;;;;;;;;;;;;;;;;;

;;Elisp cannot fully emulate `multiple-value-call' because it can't
;;distinguish multiple values from lists.  The cl package aliases it
;;to `apply' which treats only the last argument as returning multiple
;;values.  `tehom-multiple-value-call' covers some more of the
;;situation by treating all arguments as multiple values.  In any
;;case, the user must use some judgement.

(defun tehom-multiple-value-call (function &rest args)
  ""
  
  (apply function (apply #'append args)))

(eval-when-compile
  (setf
    (get 'tehom-multiple-value-call 'rtest-suite)
    '("tehom-multiple-value-call"
       ((tehom-multiple-value-call #'list '(1 2) '(3 4))
	 '(1 2 3 4))
       )))

;;;;;;;;;;;;;

(defmacro define-condition (name parent-types slot-specs &rest options)
  "Define a condition in Elisp.
Unlike Common Lisp, option `:report' may only be a string.
This ignores slot-specs."
  (declare (ignore slot-specs options))
  (let*
    ((use-parent-types
       (remove-duplicates 
	 (append 
	   (list 'error) 
	   parent-types
	   (list name))))
      
      (report-cell
	(assoc
	  :report
	  options))
      (use-error-message
	(if report-cell
	  (second report-cell)
	  (symbol-name name))))
    
  `(progn
     (defvar ,name)
     ',report-cell
     (put ',name
       'error-conditions
       ',use-parent-types) 
     (put ',name 'error-message ,use-error-message))))


(eval-when-compile
  (setf
    (get 'define-condition 'rtest-setup)
    '(progn
      (unintern 'condition-foo)
      (define-condition
	condition-foo (testing-error) ()
	(:documentation 
	  "A condition only used to test define-condition")
	(:report
	  "Too few pizzas"))))

  (setf
    (get 'define-condition 'rtest-suite)
    '("define-condition"
       ( "condition-case uses the new error properly"

	 (condition-case err
	   (signal 'condition-foo t)
	   (testing-error 345)
	   (error 54))
	 345)

       ( "The `:report' string is used for the error message"
	 (condition-case err
	   (signal 'condition-foo t)
	   (error (error-message-string err)))
	 "Too few pizzas")
       

       )))

;;Where it's error-message-string in Elisp, it's print-object in
;;Common Lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Font-lock some new things font-lock forgot about.

(tehom-add-font-lock-to-all-lisp
  (eval-when-compile
    (list 
      (tehom-build-font-lock-expression  
	'(
	   "multiple-value-call"
	   "tehom-multiple-value-call")))))

;;Overall test.
(eval-when-compile
  (setf
    (get 'tehom-cl 'rtest-suite)
    '("tehom-cl tests"
       tehom-get-field-sym-name
       tehom-get-struct-sym-name
       tehom-type-of
       tehom-get-struct-slots
       constantp
       tehom-multiple-value-call
       define-condition
       )))


(provide 'tehom-cl)

;;; tehom-cl.el ends here