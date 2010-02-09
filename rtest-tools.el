;;; rtest-tools.el --- Miscellaneous code helpful with rtest

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

(if (boundp 'emacs-version)
  (eval-and-compile
    (require 'cl)))


;;;;;;;;;;;;;;;;;
;;;; List testers

(defun tehom-list-is-some-permutation-of (lis &rest items)
  ""

  (tehom-lists-are-equal-permuted 
    lis (apply 'list items) :test 'equal))


(defun tehom-lists-are-equal-permuted (lis0 lis1 &rest flags)
  ""
  
  (and
    (apply 'subsetp lis0 lis1 flags)
    (apply 'subsetp lis1 lis0 flags)))


;;;;;;;;;;;;;;;;;;;;
;;Predefined objects

(defstruct rtest-struct
  ""
  my-field
  my-second-field)

(defstruct (rtest-struct-list (:type list) :named)
  ""
  my-field
  my-second-field)

(defstruct (rtest-struct-vector (:type vector) :named)
  ""
  my-field
  my-second-field)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;let-defaliases


(defun set-function (func-sym new-func)
  ""

  (setf (symbol-function func-sym) 
    (cond
      ;;If it's a symbol, use its function, if it has one.
      ((symbolp new-func)
	(if (fboundp new-func)
	  (symbol-function new-func)
	  (error "Symbol has no function")))
      
      ;;If it's a function, use it directly.
      ((functionp new-func)
	new-func)

      ;;If it's a lambda list, eval it and use it.  This is needed for
      ;;Common Lisp.
      ((and
	 (consp new-func)
	 (eq (car new-func) 'lambda))
	(eval new-func))
      
      (t
	(error "Attempt to set a function to a non-function")))))

(eval-when-compile
  (setf
    (get 'set-function 'rtest-setup)
    '(progn
       (defun set-function^foo ()
	 ""
	 (error "The tests never call this function as itself"))

       (defconst set-function^foo-original-symbol-function 
	 (symbol-function 'set-function^foo) "" )
       (defun set-function^foo-unchanged-p ()
	 ""
	 (equal 
	   (symbol-function 'set-function^foo) 
	   set-function^foo-original-symbol-function))
  
       (defun set-function^reset ()
	 ""
	 (set-function 
	   'set-function^foo
	   set-function^foo-original-symbol-function))))

  (setf
    (get 'set-function 'rtest-suite)
    '("set-function"
       ( "Set a function to another function"
	 (progn
	   (set-function^reset)
	   (set-function 'set-function^foo 'car)
	   (set-function^foo '(1 2)))
	 
	 1)

       ( "Set a function to a lambda list"
	 (progn
	   (set-function^reset)
	   (set-function 'set-function^foo 
	     #'(lambda (x)
		 (+ x 3)))
	   (set-function^foo 44))
	 47)

       ( "NOT A REAL TEST.  Undo the changes we made to set-function^foo"
	 (set-function^reset)
	 :test t)
       
       )))



(defmacro let-defalias (func-sym new-func &rest body)
  "Alias FUNC-SYM's function to NEW-FUNC's for the duration of the call."
  
  `(let
     ((old-func (symbol-function ',func-sym)))
     (set-function ',func-sym ',new-func)

     (unwind-protect
       (progn ,@body )

       (setf (symbol-function ',func-sym) old-func))))

(eval-when-compile
  (setf
    (get 'let-defalias 'rtest-suite)
    '("let-defalias"
       (rtest rtest-borrow-setup 'set-function)

       ( "Temporarily set a function to a lambda list"
	 (progn
	   (set-function^reset)
	   (let-defalias
	     set-function^foo (lambda (&rest dummy) 432)
	     (set-function^foo)))

	 :test
	 (and
	   (equal RESULT 432)
	   (and (set-function^foo-unchanged-p))))

       )))



(defmacro let-defaliases (alias-list &rest body)
  "Alias the car of each element of ALIAS-LIST's function to its cdr's
for the duration of the call."
    

  `(let
     ((old-funcs 
	(mapcar
	  #'(lambda (x)
	      (symbol-function (first x)))
	  ',alias-list)))

     (loop
       for alias in ',alias-list
       do (set-function (first alias) (second alias)))
     

     (unwind-protect
       (progn ,@body )
       
       (loop
	 for alias in ',alias-list
	 for old in old-funcs
	 do (setf (symbol-function (first alias)) old)))))

(eval-when-compile
  (setf
    (get 'let-defaliases 'rtest-suite)
    '("let-defaliases"
       (rtest rtest-borrow-setup 'set-function)

       ( "Set zero functions, do nothing special"
	 (progn
	   (set-function^reset)
	   (let-defaliases
	     ()
	     (car '(54))))
	 54)


       ( "Set one function"
	 (progn
	   (set-function^reset)
	   (let-defaliases
	     ((set-function^foo (lambda (&rest dummy) 432)))
	     (set-function^foo)))
	 
	 :test
	 (and
	   (equal RESULT 432)
	   (and (set-function^foo-unchanged-p))))
       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Useful entry points to let-defaliases.

(defun build-supplying-alias (supplier)
  ""
  
  `(,(car supplier) 
     (lambda (&rest dummy)
       ,(second supplier))))

(eval-when-compile

  (setf
    (get 'build-supplying-alias 'rtest-suite)
    '("build-supplying-alias"
       ((build-supplying-alias '(foo "abc"))
	 '(foo (lambda (&rest dummy) "abc"))
         ))))


(defmacro let-defaliases-supply (suppliers &rest body)
  "Whitebox test supplying return values for calls in BODY.

Temporarily define function aliases that supply objects when called
inside BODY.  Their scope is dynamic, not lexical.

SUPPLIERS is a list of supplier forms.  They usually appear similar to
normal function calls.  The head is the function to be aliased, the
body is the value to be returned."

  (let
    ((the-aliases
       (mapcar
	 #'build-supplying-alias
	 suppliers)))
    
  `(let-defaliases ,the-aliases
     ,@body)))

(eval-when-compile
  (setf
    (get 'let-defaliases-supply 'rtest-suite)
    '("let-defaliases-supply"
       (rtest rtest-borrow-setup 'set-function)
       (
	 (let-defaliases-supply ((set-function^foo "abc"))
	   (set-function^foo 12 "fgh" 65))
	 "abc")
       )))



(defun build-receiving-alias
  (receiver syms)
  ""
  
  (let*
    ( 
      (parms (cdr receiver))
      (sym-assignments
	(mapcar*
	  #'(lambda (a b) `(setq ,a ,b))
	   syms parms)))

    `(,(car receiver)
       (lambda ,parms
	 (declare (special ,@syms))
	 ,@sym-assignments))))

(eval-when-compile
  (setf
    (get 'build-receiving-alias 'rtest-suite)
    '("build-receiving-alias"
       (
	 (build-receiving-alias '(foo a b) '(A B))
	 '(foo 
	    (lambda (a b) 
	      (declare (special A B)) 
	      (setq A a) 
	      (setq B b))))
       
       )))


(defmacro let-defaliases-receive (receivers &rest body)
  "Whitebox test to accumulate values from calls in BODY.

Temporarily define function aliases that accumulate their received
values into symbols accessible in BODY.  The functions aliased do not
do any work, they merely accept values.  The aliases are dynamically
scoped, not lexically scoped.

RECEIVERS is a list of receiver forms.  They usually appear similar to
normal function calls.  The head is the function to be aliased, the
body is a list of args.  The args' names aren't used.

Limitation: a function can only accumulate one value per parameter, no
matter how many times it's called.  If you need it to do more, use
let-defaliases directly."

  (let*
    (
      ;;Make a sym for each element of the cdr.  gensym doesn't do
      ;;much with the arg, but it's an easy way to get equally many
      ;;syms.
      (the-syms
	(mapcar
	  #'(lambda (receiver)
	      (mapcar #'gensym (cdr receiver)))
	  receivers))
      
      (the-syms-flat
	(apply 'append the-syms)) 
      
      (the-aliases
	(mapcar*
	  #'build-receiving-alias
	  receivers the-syms))

      (the-manager
	`(list ,@the-syms-flat)))
    

    `(let
       ,the-syms-flat
       (declare (special ,@the-syms-flat))
       (let-defaliases ,the-aliases
	 ,@body
	 ,the-manager))))


(eval-when-compile
  (setf
    (get 'let-defaliases-receive 'rtest-suite)
    '("let-defaliases-receive"

       ( "Objects passed to calls within BODY are returned by
let-defaliases-receive" 
	 (let-defaliases-receive
	   ((* arg0 arg1))
	   (* 2 3))

	 '(2 3))
       
       ( "Objects are returned in the order declared."
	 (let-defaliases-receive
	   ((* arg0 arg1) (/ arg2 arg3))
	   (progn
	     (/ 9 11)
	     (* 2 3)))
	 '(2 3 9 11))

       )))


 



;;;;;;;;;;;;;;;;;;;;;;;
;;;; Testing


(eval-when-compile
  (setf
    (get 'rtest-tools 'rtest-suite)
    '("rtest-tools"

       set-function
       let-defalias
       let-defaliases
       build-supplying-alias
       let-defaliases-supply
       build-receiving-alias
       let-defaliases-receive
       )))

(provide 'rtest-tools)

;;; rtest-tools.el ends here