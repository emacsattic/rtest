;;; caselet.el --- cond/case statements with bindings before tests.

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

;; None.


;;; Code:

(provide 'caselet)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cond helper, case helper

;;This is a huge opportunity for optimization.  Shared beginnings
;;could be automatically shared, and so forth.  But we won't do that
;;here.  What mite make sense is a compiler-macro plus a property for
;;the case-function that described a function that computed common
;;beginnings between N different cases, with which we'd build a tree
;;of nested partial cases.  That optimization would work the same way
;;for condlet-worker

(defun caselet-worker (case-function default-clause object-sym cases)
  "Build general `case'-like statements."

  
  (let*
    (
      (catch-sym
	''condlet)
      (sym
	(gensym))
      (case-forms
	(mapcar
	  #'(lambda
	      (one-case)
	      (apply case-function sym catch-sym one-case))

	  cases)))
    `(catch ,catch-sym
       (let
	 ((,sym ,object-sym))
	 ,@case-forms 
	 ,default-clause))))


(defun condlet-worker (case-function default-clause cases)
  "Build general `cond'-like statements."

  (let*
    (
      (catch-sym
	''condlet)
      
      (case-forms
	(mapcar
	  #'(lambda
	      (one-case)
	      (apply case-function catch-sym one-case))
	  cases)))

    `(catch ,catch-sym
       ,@case-forms 
       ,default-clause)))

;;Tests have been covered by functions that use them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;condlet, condlet*

;;Similar to cond, except that the forms are bound around the case as
;;if by `let' or `let*', and only then is its guard tested.  Cases are
;;of the form:
;;
;;  ((LET-FORMS...) GUARD BODY...).  

(defun condlet*-case (catch-sym let-forms guard &rest body)
  ""

  `(let* ,let-forms
     (when
       ,guard
       (throw ,catch-sym
	 (progn ,@body)))))

;;As above, but using only `let'
(defun condlet-case (catch-sym let-forms guard &rest body)
  ""

  `(let ,let-forms
     (when
       ,guard
       (throw ,catch-sym
	 (progn ,@body)))))

(eval-when-compile
  (defmacro condlet*-case^1 (let-forms guard body)
    ""
    
    `(catch 'ca
       (eval
	 (condlet*-case ''ca ',let-forms ',guard ',body))))

  (setf
    (get 'condlet*-case 'rtest-suite)
    '("condlet*-case"
       ( "If the test succeeds, it throws the answer"
	 (condlet*-case^1 ((a 12)) t 4543)
	 
	 4543)
       
       ( "When the test fails, it doesn't throw anything"
	 (catch 'ca
	   (progn
	     (eval
	       (condlet*-case ''ca '((a 12)) nil '(4543)))
	     65))
	 65)
       
       ( "The let-forms are bound around the body"
	 (condlet*-case^1 ((a 112)) t a)
	 112)

       ( "The let-forms are bound around the test"
       (condlet*-case^1 ((a 112)) (numberp a) a)
	 112)
       
       )))

(defmacro condlet* (&rest cases)

  "Similar to cond, except that the forms are bound around the case as
if by `let*', and only then is its guard tested.  Cases are of the
form:

  ((LET-FORMS...) GUARD BODY...)"

  (condlet-worker #'condlet*-case nil cases))

(defmacro condlet (&rest cases)

  "Similar to cond, except that the forms are bound around the case as
if by `let', and only then is its guard tested.  Cases are of the
form:

  ((LET-FORMS...) GUARD BODY...)"

  (condlet-worker #'condlet-case nil cases))


(eval-when-compile
  (setf
    (get 'condlet-worker 'rtest-suite)
    '("condlet-worker"
       ( "Worker runs"
	 (condlet-worker
	   #'condlet*-case nil '((((a 12)) t a )))
 
	 :test (not (rtest-error-p RESULT))
	 :type cons)
       

       ( "An condlet* binds its body"
	 (condlet* 
	   (((a 12)) t a ))
	 12)
       
       ( "Bindings apply to their specific clause only"
	 (condlet* 
	   (((a 12)) nil a )
	   (((a 13)) t   a))
	 
	 13)

       )))

;;condlet, condlet*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Plans for and-let*, invented by another guy but still very nice.

;;Overall body plan would be:
;;  
;; `(catch 'failed
;;   (let*
;;     ,clauses
;;     ,@body))

;; ;;Each clause would be:
;; 
;; `(,name (or ,code (throw 'failed nil)))

;; ;;It would be invoked like:
;; 
;; (and-let* 
;;   ( (thing (get-thing)) 
;;     (head (when (consp thing) (car thing))) 
;;     (value (my-transform head)))
;;   (do-stuff value))


;;;;;;;;;;;;;;;;
;;Overall tests:

(eval-when-compile
  (setf
    (get 'caselet 'rtest-suite)
    '("caselet"
       condlet-worker 
       condlet*-case)))


;;; caselet.el ends here 
