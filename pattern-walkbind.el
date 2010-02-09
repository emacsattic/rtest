;;; pattern-walkbind.el --- Walking/binding functions for deval in rtest

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

;;; Code:

(when (boundp 'emacs-version)
  (eval-and-compile
    (require 'cl)
    (require 'tehom-cl)))

(require 'rtest-visible)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Binding functions.

;;Tests between equal symbols are not done in the walker itself, so
;;symbols get bound but not tested.

;;Thruout this file there is difference between a predicate-like
;;"eager failure" strategy and a report-like "diligent failure"
;;strategy.  Everything beginning with pat-r- uses report style,
;;everything beginning with pat-p- uses predicate-style.


(declaim
  (ftype
    (function (t &rest t) rtest-any-valid-return)
    pat-r-walkbind-bind
    pat-r-walkbind-match
    pat-r-walkbind-ignore
    pat-r-walkbind-satisfy
    pat-r-walkbind-by-accessor
    pat-r-walkbind-every
    pat-r-walkbind-to-nils
    pat-r-walkbind-whole-list
    pat-r-walkbind-shorter-list
    pat-r-walkbind-list
    pat-r-walkbind-optional
    pat-r-walkbind-vector
    )
  

  (ftype
    (function (t &rest t) bool)
    pat-p-walkbind-bind
    pat-p-walkbind-match
    pat-p-walkbind-ignore
    pat-p-walkbind-satisfy
    pat-p-walkbind-by-accessor
    ))



;;;;;;;;;;
;;Managing the different walkbind approaches

;;These are the *root names* for indirection, not the actual functions
;;that do the work.  Those names will be generated wrt the root names.
;;It requires some use of defalias.

;;It would be nice if each group of actual functions were in their own
;;package, 1 package per walkbind-approach, but we can't do that.

(defconst pattern-all-walkbinders 
  '(
     pattern-walkbind-bind         
     pattern-walkbind-match        
     pattern-walkbind-ignore       
     pattern-walkbind-satisfy      
     pattern-walkbind-by-accessor  
     pattern-walkbind-every        
     pattern-walkbind-to-nils      
     pattern-walkbind-whole-list
     pattern-walkbind-shorter-list
     pattern-walkbind-list
     pattern-walkbind-optional
     pattern-walkbind-vector
     )
  "" )


;;Declare the walker heads as special variables so that when they are
;;bound, that binding is seen by all the nested calls.  Common Lisp
;;needs that.
(mapcar
  #'(lambda (sym)
      (defvar sym))
  pattern-all-walkbinders)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro pattern-with-walkers (walk-symbol-mangler &rest body)
    "Execute BODY with walkers bound according to WALK-SYMBOL-MANGLER.

WALK-SYMBOL-MANGLER must be an unquoted function name or lambda form.
It must transform a symbol starting with `pattern-' to the symbol of
an appropriate walker function."

    (let*
      ( (mangler walk-symbol-mangler)
	(walker-bindings
	  (mapcar
	    #'(lambda (sym)
		(list 
		  sym
		  `#',(funcall mangler sym)))
	    pattern-all-walkbinders)))
      `(let
	 ,walker-bindings
	 ,@body))))

(eval-when-compile
  (setf
    (get 'pattern-with-walkers 'rtest-suite)
    '("pattern-with-walkers"
       (
	 (pattern-with-walkers pat-r-walk-symbol-mangler 
	   pattern-walkbind-bind)
	 'pat-r-walkbind-bind)
       )))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro pattern-declare-walkbinder-support (walk-symbol-mangler)
    ""
  
    `(progn
       (declaim
	 (ftype
	   (function (t &rest t) rtest-any-valid-return)
	   ,@(mapcar walk-symbol-mangler pattern-all-walkbinders)))
     
       (defun 
	 ,(funcall walk-symbol-mangler 'pattern-apply-walker) 
	 (obj data)

	 "Try to match a pattern to an object"
  
	 (pattern-with-walkers
	   ,walk-symbol-mangler
	   (pattern-apply-walker obj data))))))



;;;;;;;;;;;
;;For specific types of walker


(defun pat-r-walk-symbol-mangler (sym)
  ""
  (let
    ( (name (symbol-name sym))
      (pos
	(eval-when-compile
	  (length "pattern-"))))
    (assert 
      (string= 
	(downcase (substring name 0 pos)) 
	"pattern-"))
      
    (intern
      (concat "pat-r-" (substring name pos)))))

(eval-when-compile
  (setf
    (get 'pat-r-walk-symbol-mangler 'rtest-suite)
    '("pat-r-walk-symbol-mangler"
       (
	 (funcall #'pat-r-walk-symbol-mangler 'pattern-walkbind-bind)
	 'pat-r-walkbind-bind)
       
       )))



;;;;;;;;;;
;;Helpers


;;Top level for applying a walker.
;;Created by `pattern-declare-walkbinder-support' using
;;`pat-r-walk-symbol-mangler' 
(defun pat-r-apply-walker (obj data)
  ""
  
  (pattern-with-walkers
    pat-r-walk-symbol-mangler
    (pattern-apply-walker obj data)))

;;Second level for applying a walker.
(defun pattern-apply-walker
  (obj data)
  ""
  (apply
    (car data)
    obj
    (cdr data)))

(eval-when-compile
  (setf
    (get 'pattern-apply-walker 'rtest-suite)
    '("pattern-apply-walker"
       ( "This used to be exactly correct, but is questionable now"
	 (let
	   ((A 0))
	   (pattern-apply-walker 'A '(set 12))
	   (= A 12))
	 t)
       
       ( "pattern-apply-walker sees walker bindings"
	 (let
	   ((A 0))
	   (pattern-with-walkers pat-r-walk-symbol-mangler
	     (pattern-apply-walker 12 '(pat-r-walkbind-bind A)))
	   (= A 12))
	 t)

       ( "Same test, but more concise"
	 (let
	   ((A 0))
	   (pat-r-apply-walker 12 '(pat-r-walkbind-bind A))
	   (= A 12))
	 t)

       )))



;;;;;;;;;;;;;
;;Walker-like helpers

;;CLisp doesn't like lexical-let, so these 2 functions have been
;;disabled.

'
(defun pat-r-make-walkbind-on-object (object)
  ""
  
  (lexical-let
    ((object object))
    (lambda (walker)
      (pattern-apply-walker object walker))))
'
(defalias 'pat-p-make-walkbind-on-object
  'pat-r-make-walkbind-on-object)

;;Special-purpose walkers.
(declaim (inline pat-r-walkbind-map pat-r-walkbind-to-nils))
(defun pat-r-walkbind-map (objects walkers)
  ""
  (mapcar* #'pattern-apply-walker objects walkers))



(eval-when-compile

  (setf
    (get 'pat-r-walkbind-map 'rtest-suite)
    '("pat-r-walkbind-map"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       ( (pat-r-walkbind-map 
	   '(1 1)
	   (list pat-r-match-1 pat-r-match-1))
	 '(nil nil))
       
       ( (pat-r-walkbind-map 
	   '(1 2)
	   (list pat-r-match-1 pat-r-match-1))
	 :map
	 ((nil)
	   (:type rtest-some-bad-grade)))
       
       
       )))


(defun pat-p-walkbind-map (objects walkers)
  ""
  
  (every #'pattern-apply-walker objects walkers))


(defun pat-r-walkbind-to-nils (walkers)
  ""
  '
  (loop
    for walker in walkers
    collect (pattern-apply-walker nil walker))

  (mapcar
    #'(lambda (walker)
	(pattern-apply-walker nil walker))
     walkers))

(eval-when-compile
  (setf
    (get 'pat-r-walkbind-to-nils 'rtest-suite)
    '("pat-r-walkbind-to-nils"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       (
	 (pat-r-walkbind-to-nils
	   (list pat-r-match-nil pat-r-match-nil))
	 '(nil nil))
       
       ( "Fails gracefully when some walker won't take nil"
	 (pat-r-walkbind-to-nils
	   (list pat-r-match-nil pat-r-match-1))
	 :map
	 ((nil)
	   (:type rtest-some-bad-grade)))
       
       )))

(defun pat-p-walkbind-to-nils (walkers)
  ""
  (loop
    for walker in walkers
    always (pattern-apply-walker nil walker)))

;;;;;;;;;;;;;;;;;;;;;;
;;True walkers

(defun pat-r-walkbind-bind (obj sym)
  ""
  
  (set sym obj)
  nil)

(eval-when-compile
  (setf
    (get 'pat-r-walkbind-bind 'rtest-suite)
    '("pat-r-walkbind-bind"
       ((pat-r-walkbind-bind 1 'A)
	 nil)
       )))

(defun pat-p-walkbind-bind (obj sym)
  ""
  
  (set sym obj)
  t)

;;;;;;;;;;;;;;;;;;;
;;For comparisons that don't need compare-time evaluation.

(defun pat-r-walkbind-match (obj comparand)
  ""

  (rtest-maybe-make-single-failure
    (equal obj comparand)
    "Didn't match"
    (list obj comparand)))

(eval-when-compile
  (setf
    (get 'pat-r-walkbind-match 'rtest-setup)
    '(progn
       (defconst pat-r-match-1
	 (list #'pat-r-walkbind-match 1)
	 "")
       (defconst pat-r-match-2
	 (list #'pat-r-walkbind-match 2)
	 "")
       (defconst pat-r-match-3
	 (list #'pat-r-walkbind-match 3)
	 "")
       (defconst pat-r-match-nil
	 (list #'pat-r-walkbind-match nil)
	 "")))
  
  (setf
    (get 'pat-r-walkbind-match 'rtest-suite)
    '("pat-r-walkbind-match"
       (
	 (pat-r-walkbind-match 1 1)
	 nil)

       (
	 (pat-r-walkbind-match 1 2)
	 :type rtest-some-bad-grade)

       )))


(defalias 'pat-p-walkbind-match 'equal)

;;;;;;;;;;;;;;;

;;For objects that should be ignored, eg shared objects' extra
;;appearances.

(defalias 'pat-r-walkbind-ignore 'ignore)

(defun pat-p-walkbind-ignore (&rest dummy)
  ""
  t)

;;;;;;;;;;;;;;;

(defun pat-r-walkbind-satisfy (obj predicate)
  ""
  (rtest-grade-by-predicate
    obj predicate *bound-syms* *bound-vals*))

(defun pat-p-walkbind-satisfy (obj predicate)
  ""
  (funcall predicate obj))

;;;;;;;;;;;;;;;
;;Same for all styles, because it just forwards the call to another
;;function. 

(defun pat-r-walkbind-by-accessor (object field-access-func walker)
  "Walk the accessed field of object."

  (pattern-apply-walker (funcall field-access-func object) walker))

(defalias 'pat-p-walkbind-by-accessor 'pat-r-walkbind-by-accessor)

;;;;

(defun pat-r-walkbind-every (object walkers)
  "Call each of WALKERS, collect all their results.
This is used for tasks like checking several different fields of a
structure."

  (rtest-maybe-make-rtest-nested-test-failures
    (loop
      for walker in walkers
      collect (pattern-apply-walker object walker))))



(defun pat-p-walkbind-every (object walkers)
  ""
  
  (loop
    for walker in walkers
    always (pattern-apply-walker object walker)))



;;;;;;;;;;;;;;;;

;;Helper function
(defun pattern-compare-normal-length (list0 list1)
  "Count the minimum length of two possibly dotted lists.


Returns a list of
minimum-length,
remainder of first list, 
remainder of second list."
  
  (loop
    for el0 on list0
    for el1 on list1

    count t into min-length

    finally return
    (list min-length el0 el1)))


(eval-when-compile
  (setf
    (get 'pattern-compare-normal-length 'rtest-suite)
    '("pattern-compare-normal-length"
       ( "For ordinary lists, comparable is the minimum of the lengths"
	 (pattern-compare-normal-length
	  '(1 2) '(1 2 3 4)) 
	 :comparand
	 '(2 () (3 4)))

       ( "For ordinary lists, comparable is the minimum of the lengths"
	 (pattern-compare-normal-length
	  '(1 2 3 4) '(1 2 3)) 
	 :comparand
	 '(3 (4) ()))

       ( "Dotted-list objects don't consider their final element comparable"
	 (pattern-compare-normal-length
	   '(1 2 . 3) '(1 2 3 4))
	 :comparand
	 '(2 3 (3 4)))

       )))

;;;;;;;;;;;;;;;;;;
;;The following is all report-style.  Predicate-style wouldn't bother
;;collecting reports.

;;;;;;;;
;;;;These are similar for predicate style, but don't make any object,
;;;;and use `and' for explicit connections.

;;The same number of objects gives us nothing to assign to
;;rest-walker, unless there's a tail.  To deal with tail, we could
;;take a min-length, take a rest-objects... but would nil mean "walk
;;nil", or "there was no other object"?  Well, it's the same regardless.
(defun pat-r-walkbind-whole-list
  (objects rest-object walkers rest-walker)
  ""
  (rtest-maybe-make-rtest-nested-test-failures
    (pat-r-walkbind-map objects walkers)

    (cond
      ;;If there's a tail object and a walker for it, apply it.
      (rest-walker
	(list
	  (pattern-apply-walker rest-object rest-walker)))

      ;;If there's a tail object but no walker for it, fail.
      (rest-object
	(list
	  (rtest-make-single-failure "Arglist is too long."
	    rest-object)))

      ;;If there's neither, we're OK
      (t nil))))



(eval-when-compile
  (setf
    (get 'pat-r-walkbind-whole-list 'rtest-suite)
    '("pat-r-walkbind-whole-list"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       ( "OK if it all matches and rest-walker can accept nil"
	 (pat-r-walkbind-whole-list
	   '(1)
	   nil
	   (list pat-r-match-1)
	   pat-r-match-nil)
	 nil)
       
       ( "Fails if rest-walker can't accept nil"
	 (pat-r-walkbind-whole-list
	   '(1)
	   nil
	   (list pat-r-match-1)
	   pat-r-match-1)
	 :type rtest-some-bad-grade)

       ( "Fails if walkers don't match object-list"
	 (pat-r-walkbind-whole-list
	   '(2)
	   nil
	   (list pat-r-match-1)
	   pat-r-match-nil)
	 :type rtest-some-bad-grade)

       ( "Walks dotted lists OK"
	 (pat-r-walkbind-whole-list
	   '(4)
	   5
	   '((pat-r-walkbind-match 4))
	   '(pat-r-walkbind-match 5))
	 nil)
       

       )))



(defun pat-r-walkbind-shorter-list
  (objects walkers walkers-nthcdr rest-walker)
  ""
  
  ;;If objects is a dotted list, it shouldn't have been passed here.
  (assert (true-list-p objects))

  (rtest-maybe-make-rtest-nested-test-failures
    (pat-r-walkbind-map objects walkers)
    (pat-r-walkbind-to-nils walkers-nthcdr)
    (if rest-walker
      (pat-r-walkbind-to-nils
	(list rest-walker)))))


(eval-when-compile
  (setf
    (get 'pat-r-walkbind-shorter-list 'rtest-suite)
    '("pat-r-walkbind-shorter-list"

       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       ( "OK if it all matches up to min-length and the rest and
rest-walker can accept nil" 
	 (pat-r-walkbind-shorter-list
	   '(1)
	   (list pat-r-match-1 pat-r-match-nil)
	   nil
	   pat-r-match-nil)
	 nil)

       ( "Fails if the pattern doesn't match" 
	 (pat-r-walkbind-shorter-list
	   '(2)
	   (list pat-r-match-1 pat-r-match-nil)
	   nil
	   pat-r-match-nil)
	 :type rtest-some-bad-grade)

       ( "Fails if some part of the rest can't accept nil" 
	 (pat-r-walkbind-shorter-list
	   '(1)
	   (list pat-r-match-1)
	   (list pat-r-match-1)
	   pat-r-match-nil)
	 :type rtest-some-bad-grade)

       ( "Fails if rest-walker can't accept nil" 
	 (pat-r-walkbind-shorter-list
	   '(1)
	   (list pat-r-match-1 pat-r-match-nil)
	   nil
	   pat-r-match-1)
	 :type rtest-some-bad-grade)


       )))

(defun pat-r-walkbind-list (objects walkers rest-walker)
  "Walk WALKERS over OBJECTS, which must be a list.

REST-WALKER walks any extra objects as a whole."

  (destructuring-bind
    (min-length objects-nthcdr walkers-nthcdr)
    (pattern-compare-normal-length objects walkers)

    (if walkers-nthcdr 

      ;;Fewer objects than walkers is an error.
      (rtest-make-single-failure 
	"Arglist is too short" 
	objects)

      (pat-r-walkbind-whole-list 
	objects 
	objects-nthcdr
	walkers rest-walker))))


(eval-when-compile
  (setf
    (get 'pat-r-walkbind-list 'rtest-suite)
    '("pat-r-walkbind-list"
       (rtest
	 rtest-inherit
	 :inherit-from 'pat-r-walkbind-whole-list
	 :do-call      
	 #'(lambda (objects rest-object walkers rest-walker)
	     (pat-r-walkbind-list
	       (append objects rest-object) walkers rest-walker)))

       )))


(defun pat-r-walkbind-optional (objects walkers rest-walker)
  "Walk WALKERS over OBJECTS, which must be a list.
There may be fewer OBJECTS than WALKERS.  In that case, all remaining
WALKERS must be able to accept nil. 

REST-WALKER walks any extra objects as a whole."


  (destructuring-bind
    (min-length objects-nthcdr walkers-nthcdr)
    (pattern-compare-normal-length objects walkers)

    (if walkers-nthcdr 

      (if
	objects-nthcdr
	(rtest-make-single-failure "Arglist ends in a dotted pair
that doesn't match"
	  objects-nthcdr)
	(pat-r-walkbind-shorter-list 
	  objects walkers walkers-nthcdr rest-walker))

      (pat-r-walkbind-whole-list 
	objects 
	objects-nthcdr
	walkers rest-walker))

    ))

(eval-when-compile
  (setf
    (get 'pat-r-walkbind-optional 'rtest-suite)
    '("pat-r-walkbind-optional"
       (rtest
	 rtest-inherit
	 :inherit-from 'pat-r-walkbind-whole-list
	 :do-call      
	 #'(lambda (objects rest-object walkers rest-walker)
	     (pat-r-walkbind-optional
	       (append objects rest-object) walkers rest-walker)))

       (rtest
	 rtest-inherit
	 :inherit-from 'pat-r-walkbind-shorter-list
	 :do-call      
	 #'(lambda (objects walkers walkers-nthcdr rest-walker)
	     (pat-r-walkbind-optional
	       objects
	       (append walkers walkers-nthcdr) rest-walker)))

       )))

(defun pat-r-walkbind-splicing-list (objects walkers rest-walker)
  ""
  (declare (ignore objects walkers rest-walker))
  (error "Internal error: pat-r-walkbind-splicing-list is a placeholder
only.  It should never actually be called") )



(defun pat-r-walkbind-vector (objects walkers rest-walker)
  ""
  ;;Complain if it's not a vector.
  (if
    (not (vectorp objects))
    (rtest-make-single-failure 
      "Object is not a vector" 
      objects)

    ;;Convert it, then treat it like a list.
    (pat-r-walkbind-list
      (rtest-vec->list objects) walkers rest-walker)))

;;;;;;;;;;;;

(defun pat-p-walkbind-whole-list
  (objects rest-object walkers rest-walker)
  ""
  (and
    (pat-p-walkbind-map objects walkers)

    (cond
      ;;If there's a tail object and a walker for it, apply it.
      (rest-walker
	(pattern-apply-walker rest-object rest-walker))

      ;;If there's a tail object but no walker for it, fail.
      (rest-object nil)

      ;;If there's neither, we're OK
      (t t))))


(defun pat-p-walkbind-shorter-list
  (objects walkers walkers-nthcdr rest-walker)
  ""
  
  ;;If objects is a dotted list, it shouldn't have been passed here.
  (assert (true-list-p objects))

  (and
    (pat-p-walkbind-map objects walkers)
    (pat-p-walkbind-to-nils walkers-nthcdr)
    (if rest-walker
      (pat-p-walkbind-to-nils
	(list rest-walker)))))

(defun pat-p-walkbind-list (objects walkers rest-walker)
  "Walk WALKERS over OBJECTS, which must be a list.

REST-WALKER walks any extra objects as a whole."

  (destructuring-bind
    (min-length objects-nthcdr walkers-nthcdr)
    (pattern-compare-normal-length objects walkers)

    (if walkers-nthcdr 
      nil
      (pat-p-walkbind-whole-list 
	objects 
	objects-nthcdr
	walkers rest-walker))))

(defun pat-p-walkbind-optional (objects walkers rest-walker)
  "Walk WALKERS over OBJECTS, which must be a list.
There may be fewer OBJECTS than WALKERS.  In that case, all remaining
WALKERS must be able to accept nil. 

REST-WALKER walks any extra objects as a whole."


  (destructuring-bind
    (min-length objects-nthcdr walkers-nthcdr)
    (pattern-compare-normal-length objects walkers)

    (if walkers-nthcdr 

      (if
	objects-nthcdr
	nil
	(pat-p-walkbind-shorter-list 
	  objects walkers walkers-nthcdr rest-walker))

      (pat-p-walkbind-whole-list 
	objects 
	objects-nthcdr
	walkers rest-walker))))

(defun pat-p-walkbind-vector (objects walkers rest-walker)
  ""
  ;;Complain if it's not a vector.
  (if
    (not (vectorp objects))
    nil
    ;;Convert it, then treat it like a list.
    (pat-p-walkbind-list
      (rtest-vec->list objects) walkers rest-walker)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;These 2 functions exist so that deval can use `&optional'-like
;;functionality and still maintain reversibility.  They're not
;;attached to any particular approach.

(defun pattern-optional (&rest args)

  "Like list, except not appending trailing nil arguments.  More
useful for what it does devalled then when evalled."

  (let*
    ((rargs (reverse args)))
    
    (while
      (and
	rargs
	(not (car rargs)))
      (pop rargs))

    (apply #'list (reverse rargs))))


(defun pattern-optional* (&rest args)

  "Like list*, except not appending trailing nil arguments.  More
useful for what it does devalled then when evalled.

NB, it is the last passed arg, that can make a dotted list if it is
non-nil, not the last non-nil arg.  Ie, if the last arg is nil, it
acts like \(apply #'pattern-optional args\)"


  (if
    (car (last args))
    ;;If the last arg is non-nil, use everything.
    (apply #'list* args)
    ;;Otherwise it is the same as pattern-optional.
    (apply #'pat-r-walkbind-optional args)))

(eval-when-compile
  (setf
    (get 'pattern-optional 'rtest-suite)
    '("pattern-optional"
       ((pattern-optional 1 2 3 )
	 '(1 2 3))

       ((pattern-optional* 1 2 3 )
	 '(1 2 . 3))

       (
	 (pattern-optional 1 2 nil )
	 '(1 2))

       (
	 (pattern-optional 1 nil 3 )
	 '(1 nil 3))

       (
	 (pattern-optional* 1 nil 3 )
	 '(1 nil . 3))

       (
	 (pattern-optional 1 nil nil )
	 '(1))

       
       )))



(eval-when-compile
  (setf
    (get 'pat-r-walkbind 'rtest-suite)
    '("pat-r-walkbind"
       pattern-with-walkers
       pat-r-walk-symbol-mangler

       pattern-apply-walker
       pat-r-walkbind-bind
       pat-r-walkbind-match
       pat-r-walkbind-map
       pat-r-walkbind-to-nils
       pattern-compare-normal-length
       pat-r-walkbind-whole-list
       pat-r-walkbind-shorter-list
       pat-r-walkbind-list
       pat-r-walkbind-optional
       pattern-optional
       
       )))

(provide 'pattern-walkbind)

;;; pattern-walkbind.el ends here