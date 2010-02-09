;;; local-vars.el --- macro to allow just-in-time local variables.
;;
;; Copyright (C) 1998 Tom Breton
;;
;; Time-stamp: <2000-01-22 15:29:05 Tehom>
;; Author: Tom Breton <tob@world.std.com>
;; Maintainer: Nobody
;; Created: 20 Oct 1998
;; Version: 1.0
;; Keywords: 

;; Purpose of this package:
;; To allow just-in-time local variables.
;; 
;; Installation instructions:
;;
;; Put local-vars.el somewhere in your load-path.
;;
;; Usage instructions:
;; 
;; Put 
;;   ( autoload using-local-vars "local-vars" "" nil macro)
;; at the beginning of your code.  Call using-local-vars whenever you
;; like after that point.
;;

;;Example of how it's used:
;;(using-local-vars
;;	(do-this)	
;;	(local-var a b) 
;;	(do-that b)
;;	(local-var c d)
;;	(do-other-thing c)) 
;;
;; ==>
;;
;;(let
;;	(a c)  ;;Here they're bound, as nil.
;;	(do-this)	
;;	(setq a b) ;;Here a is assigned to
;;	(do-that b)
;;	(setq c d) ;;Here c is assigned to
;;	(do-other-thing c))

;; Known bugs:
;;
;; LCD Archive Entry:
;; 
;; local-vars.el||Tehom@localhost
;; |Allow just-in-time local variables.
;; Tue Oct 20 18:17:16 1998|0.0||
;;

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Future maintainers, please hit 'M-x time-stamp' when you're done.

;;; Change Log:

;; 1999-06-10 15:01:29 Tehom:
;; block-using-local-vars: New function

;; 1998-10-21 16:08:52 Tehom: 
;; Fixed a tiny potential bug: var-list and bodies hadn't been
;; actually bound in using-local-vars; bound them.

;;; Code:

(if (boundp 'emacs-version)
  (require 'cl))


(defsubst local-var-form-p (form)
  "Return whether a form is a local-var form."
  (and (consp form)
    (eq 'local-var (car form))))


(defmacro using-local-vars (&rest body)

  "Allow local variables to be declared just-in-time.

This statement is similar to a let statement, except that the local
variables are not written in a block at the beginning of the form,
they are scattered thruout the form.

Each local variable is introduced by a local-var statement, which only
has meaning inside a using-local-vars form.  For the time being, each
local-var statement can only bind one variable.

Usage: \( using-local-vars ... (local-var a value) ...  \)"

  ( let
    ( var-list bodies
      reversed-var-list reversed-bodies)

    ( dolist ( form body)

      (if 
	(local-var-form-p form)
      
	;;Process (local-var ...) forms specially.
	(progn

	  ;;Since we don't support multiple variables per local-var
	  ;;statement, we check the number of args.  If we looped,
	  ;;we'd merely check >= 3.
	  ( if
	    ( /= (length form ) 3)
	    (error "local-var must have exactly 2 arguments"))

	  ( let
	  ( (my-var   (cadr form) )
	    (my-value (caddr form)))

	  ;;Don't allow the same local variable to be created twice.
	  (if 
	    (memq my-var reversed-var-list)
	    (error "You mustn't create the same local variable twice" )

	    ;;Accumulate the local variables to both places, processed
	    ;;appropriately for each.
	    (setq 
	      reversed-var-list 
	      (cons my-var reversed-var-list)
	      reversed-bodies   
	      (cons (list 'setq my-var my-value) reversed-bodies)))))
	

	;;Accumulate normal body forms normally.
	(setq reversed-bodies   
	  (cons form reversed-bodies))))
  
    (setq var-list (reverse reversed-var-list))
    (setq bodies   (reverse reversed-bodies))

    ;;Make the list to be returned, spreading bodies.
    (apply 'list 'let var-list bodies)))

(defmacro block-using-local-vars (name &rest body)
  "Combine using-local-vars and block from the cl package."
  (if (boundp 'emacs-version)
    (require 'cl))
  `(block ,name (using-local-vars ,@body)))



;;Similar code that removes duplications and proceeds.  This works,
;;but it is not the behavior I want.
'(
   ;;Process (local-var ...) forms specially.
   (let
     ( (my-var   (cadr form) )
       (my-value (caddr form)))

     ;;Accumulate the local variables to both places, processed
     ;;appropriately for each.  add-to-list removes duplicates.
     (add-to-list 'reversed-var-list my-var)
     (setq reversed-bodies   
       (cons (list 'setq my-var my-value) reversed-bodies)))
      
   )

(provide 'local-vars )

;;; local-vars.el ends here
