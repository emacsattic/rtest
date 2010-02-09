;;; pattern-plus.el --- Entry points to pattern functionality.  

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

;; 

;;; Code:

(require 'pattern-destructure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Entry points:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;###autoload
(defmacro pattern-lambda (pattern &rest body)
  "Call BODY in a lambda expression with one arg bound to PATTERN."
  
  (pattern-build-lambda pattern body t))


;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defmacro pattern-setq (pattern object)
  "A pattern-using setq"

  (pattern-build-form 
    #'pattern-build-setq-call
    pattern object nil t))

(eval-when-compile
  (setf
    (get 'pattern-setq 'rtest-suite)
    '("pattern-setq"
       ( "It works"
	 (let* 
	   ((A 1))
	   (pattern-setq A 12 )
	   A)
	 12)
       
       ( "It works along with pattern"
	 (let* 
	   ((A 1))
	   (pattern-setq (list 5 A 3) '(5 12 3) )
	   A)
	 12)

       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defmacro pattern-bind (pattern object &rest body)
  "A souped-up destructuring-bind "

  (pattern-build-bind pattern object `(progn ,@body)))


(eval-when-compile
  (setf
    (get 'pattern-bind 'rtest-suite)
    '("pattern-bind"
       ( "Works on single symbols easily"
	 (pattern-bind A 1 A)
	 
	 1)
       
       ( "Works on single symbols easily"
	 (pattern-bind A 1 (+ 100 A))
	 101)

       (
	 (pattern-bind (list A B) '(1 2)
	   (+ A B 100))
	 
	 103)

       ( "Binds to nil without a problem"
	 (pattern-bind
	  (list A) '(nil) A)
	 nil)

       ( "Understands deval"
	 (pattern-bind
	   (deval (+ 2 3)) '5 A)
	 nil)
       
       
       )))




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A pattern-case analogous to case and typecase

(defun pattern-single-case
  (object catch-sym pattern guard &rest body)
  "Build a single case for pattern-case."

  (pattern-build-bind pattern object
   `(when
      ,guard
      (throw ,catch-sym
	(progn ,@body)))))



(eval-when-compile
  (setf
    (get 'pattern-single-case 'rtest-setup)
    '(progn
       (defmacro pattern-single-case^1 (object pat0 gua0 pat1 gua1)
	 "Help testing by constructing exactly 2 cases only varying in
object, patterns and guards"
    
	 `(let
	    ((obj ',object))
	    (catch 'pattern-case
	      (eval 
		(pattern-single-case 
		  obj ''pattern-case ',pat0 ',gua0 ''first))
	      (eval 
		(pattern-single-case 
		  obj ''pattern-case ',pat1 ',gua1 ''second)))))))
  

  (setf
    (get 'pattern-single-case 'rtest-suite)
    '("pattern-single-case"

       ( (pattern-single-case^1 1 
	   1 t 
	   2 t)
	 'first)


       ( (pattern-single-case^1 '(1 2) 
	   (list A B) t 
	   2 t)
	 'first)

       ( (pattern-single-case^1 '(1 2) 
	   (list A B) (= 1 B) 
	   (list A B) (= 2 B))
	 'second)

       
       )))

;;;###autoload
(defmacro pattern-case (object &rest cases)
  "Like `case' but with patterns.

Each case is of the form \(pattern guard . body\)

For each case in turn:

If pattern matches OBJECT, its free variables are bound to the
respective parts of object and guard is evalled.

If guard evals to non-nil, body is evalled and no more cases are
considered."

  (caselet-worker 
    #'pattern-single-case nil object cases))



(eval-when-compile
  (setf
    (get 'pattern-case 'rtest-suite)
    '("pattern-case"
       ((pattern-case 2
	  (1 t 'first )
	  (2 t 'second))
	 
	 'second)

       ("A pattern where nothing matches returns nil"
	 (pattern-case 3
	  (1 t 'first )
	  (2 t 'second))
	 
	 nil)

       ("Patterns work"
	 (pattern-case '(1 2)
	  ((list A B) t 'first )
	  (2          t 'second))
	 
	 'first)
       

       ( "Guards work"
	 (pattern-case '(1 2)
	   ((list A B) (= B 1) 'first )
	   ((list A B) (= B 2) 'second))
	 
	 'second)
         
       )))


;;;###autoload
(defmacro pattern-ecase (object &rest cases)
  "Like `ecase' but with patterns.  If nothing matches it's an error.

Each case is of the form \(pattern guard . body\)

For each case in turn:

If pattern matches OBJECT, its free variables are bound to the
respective parts of object and guard is evalled.

If guard evals to non-nil, body is evalled and no more cases are
considered."

  (caselet-worker 
    #'pattern-single-case
    `(error "No pattern was matched")
    object cases))


(eval-when-compile
  (setf
    (get 'pattern-ecase 'rtest-suite)
    '("pattern-ecase"
       ("A pattern where nothing matches throws an error"
	 (pattern-ecase 3
	   (1 t 'first )
	   (2 t 'second))
	 
	 :predicate rtest-error-p)
         
       )))


(eval-when-compile
  (setf
    (get 'pattern-plus 'rtest-suite)
    '("pattern-plus"
       pattern-setq
       pattern-ecase
       pattern-case
       pattern-single-case
       pattern-bind
       )))

(provide 'pattern-plus)

;;; pattern-plus.el ends here
