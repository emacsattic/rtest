;;; rtest-field.el --- Structure field handling for rtest

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

;;;;;;;;;;;;;
;;;; Requirements

;;Portability
(when (boundp 'emacs-version)
  (eval-and-compile
    (require 'cl)
    (require 'tehom-cl)))

;; Other requirements
(require 'rtest-visible)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rtest-get-field-accessor
  (struct-sym field-sym)
  ""
  (let*
    ((struct-name
       (symbol-name struct-sym))
      (field-name
	(tehom-get-field-sym-name field-sym))
      (field-accessor-name
	(concat struct-name "-" field-name))
      (field-accessor-sym
	(intern field-accessor-name)))
    field-accessor-sym))


(defun rtest-access-struct-field (field-sym obj)
  ""
  
  (check-type field-sym symbol)

  
  (let* 
    ((struct-sym  
       (tehom-type-of obj)))
    
    (funcall
      (rtest-get-field-accessor struct-sym field-sym)

      obj)))


(eval-when-compile
  (setf
    (get 'rtest-access-struct-field 'rtest-suite)
    '("rtest-access-struct-field"

       ( "Access a structure"
	 (rtest-access-struct-field
	   :my-field
	   (make-rtest-struct :my-field '(A B C)))
	 '(A B C))
       
       ( "Access a list-like structure"
	 (rtest-access-struct-field
	   :my-field
	   (make-rtest-struct-list :my-field '(A B C)))
	 '(A B C))

       )))


(defun rtest-access-field
  (object field-key)
  "Access the part of OBJECT corresonding to FIELD-KEY.

If OBJECT is ... FIELD-KEY must be:

 * a structure.  A symbol naming a field, ordinarily beginning with a
   colon \(:\).

 * a list.  A non-negative integer.

 * an ordinary vector.  A non-negative integer."

  (condition-case err
    (typecase field-key

      ;;Implies object is a sequence.
      (integer 
	(elt object field-key))
      
      ;;Implies object is a structure.  
      (symbol
	(rtest-access-struct-field field-key object))

      (t
	(rtest-signal
	  (rtest-make-single-failure-by-list
	    "This field-key type isn't supported." 
	    (field-key)))))

    (error
      (rtest-signal
	(rtest-make-single-failure-by-list 
	  "Can't access that field"
	  (field-key err))))))

(eval-when-compile
  ;;This uses the predefined structure rtest-struct in rtest-tools

  (setf
    (get 'rtest-access-field 'rtest-suite)
    '("rtest-access-field"

       ( "A struct"
	 (rtest-access-field  
	   (make-rtest-struct :my-field '(A B C))
	   :my-field)
	 
	 '(A B C))

       ( "A vector"
	 (rtest-access-field [1 1 45] 2)
	 45)

       ( "A list"
	 (rtest-access-field '(1 1 45) 2)
	 45)

       ( "A field that isn't a real field"
	 (rtest-access-field (make-rtest-struct) :not-a-field)
	 :predicate rtest-error-p)

       )))




;;;;;;;;;;;;;;
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro tehom-with-struct-slots (slot-list obj type-sym &rest body)
    "Evaluate BODY with the symbols in SLOT-LIST bound to the respective
slots of OBJ.  OBJ's type-sym is assumed to be TYPE-SYM.

This is essentially with-slots for defstructs."
  
    (cond 
      ((eq type-sym 'values)
	`(multiple-value-bind ,slot-list ,obj ,@body)) 

      ((eq type-sym 'list)
	`(destructuring-bind ,slot-list ,obj ,@body))

      (t
	(let*
	  ( 
	    (object (gensym))
	    (bind-list 
	      (mapcar
		#'(lambda (sym)
		    (let
		      (
			(accessor 
			  (rtest-get-field-accessor type-sym sym)))
		      `(,sym (,accessor ,object))))
	 
		slot-list)))
    
	  `(let
	     ((,object ,obj))
	     (let
	       ,bind-list
	       ,@body
	       )))))))

(eval-when-compile
  (setf
    (get 'tehom-with-struct-slots 'rtest-suite)
    '("tehom-with-struct-slots"
       
       ( "It runs on normal structures"
	 (tehom-with-struct-slots
	   (my-field my-second-field)
	   (make-rtest-struct :my-field "a") rtest-struct
	   (list my-field my-second-field))
	 '("a" nil))

       ( "It runs on list structures"
	 (tehom-with-struct-slots
	   (my-field my-second-field)
	   (make-rtest-struct-list :my-field "a") rtest-struct-list
	   (list my-field my-second-field))
	 '("a" nil))

       ( "It runs on true lists"
	 (tehom-with-struct-slots
	   (my-field my-second-field)
	   (list "a" nil) list
	   (list my-field my-second-field))
	 '("a" nil))

       ( "It runs on values"
	 (tehom-with-struct-slots
	   (my-field my-second-field)
	   (values "a" nil) values
	   (list my-field my-second-field))
	 '("a" nil))

       ( "It evaluates the object only once"
	 (let
	   ((a 1))
	   (tehom-with-struct-slots
	     (my-field my-second-field)
	     (make-rtest-struct :my-field (incf a)) rtest-struct
	     (list my-field my-second-field))
	   a)
	 2)

         
       )))



(eval-when-compile
  (setf
    (get 'rtest-field 'rtest-suite)
    '("rtest-field"
       rtest-access-struct-field
       rtest-access-field
       tehom-with-struct-slots
       )))

(provide 'rtest-field)

;;; rtest-field.el ends here