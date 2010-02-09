;;; pattern-deval.el --- Deval functions for pattern in rtest

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

;;;;;;;;;;;;;;
;;Requirements

(when (boundp 'emacs-version)
  (eval-and-compile
    (require 'cl)
    (require 'tehom-cl)))

(require 'rtest-visible)
(require 'rtest-error)
(require 'pattern-walkbind)


;;;;;;;;;
;;Helpers



;; The values returned are:
;; * The walk-and-bind function object
;; * The list of symbols bound, as (lexical-symbol . generated-symbol)
(defstruct (pattern-deval-return (:type list))
  "What the devallers return, suitable for building something to walk
a objects of that structure"
  walker
  vars)
(defalias 'pattern-deval-return->list 'identity)

(defstruct (pattern-postbind (:type list) :named)
  ""
  inside
  outside)

(defstruct (pattern-prebind (:type list) :named)
  ""
  inside
  form)

;; pattern-somebind-inside retrieves the internal symbol for both
;; pattern-postbind and pattern-prebind.
(defalias 'pattern-somebind-inside 'second)

(defstruct (pattern-delist-data (:type list))
  ""
  name
  walkers 
  rest-walker)

(defun pattern-change-walker
  (list-walker walkbind-func)
  "Transform a walker by changing its head."
  (list* walkbind-func
    (cdr list-walker)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Destructuring functions



(declaim
  ;;Functions that arrange both walkers and vars.
  (ftype 
    (function (t) pattern-deval-return)
    pattern-deval
    pattern-devector
    pattern-eval
    pattern-dequote
    pattern-desymbol-value
    pattern-decons)

  ;;Functions that only arrange walkers, which are collected for them
  ;;externally. 
  (ftype 
    (function (t) cons)
    pattern-delist
    pattern-delist*
    pattern-deappend))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Individual functions

(defun pattern-deval (obj)
  ""
  
  (cond

    ((listp obj)
      (pattern-decall (car obj) (cdr obj)))
    
    ((vectorp obj)
      (error "We don't support vectors yet"))    

    ((symbolp obj)
      (pattern-desymbol-value obj))

    ;;Otherwise it's a constant
    (t
      (pattern-dequote obj))))


(eval-when-compile
  (setf
    (get 'pattern-deval 'rtest-setup)
    '(progn
      (defmacro pattern-deval^1 (pattern object)
	""
	`',(pattern-apply-walker
	     object
	     (nth-value 0 (pattern-deval pattern))))
  
      (defmacro pattern-deval^2 (walk-builder object)
	"Test-feeder for subsidiary functions that want to check that
they're applied correctly."
	`(pattern-apply-walker
	   ,object
	   (nth-value 0 ,walk-builder)))))

  (setf
    (get 'pattern-deval 'rtest-suite)
    '("pattern-deval"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       (
	 (pattern-deval
	   1)
	 :comparand
	 `(,pat-r-match-1 nil))
       
       (
	 (pattern-deval^1 '(2) (2))
	 nil)
       
       ( (pattern-deval^1 '(2) (3))
	 :type rtest-some-bad-grade)
       

       ( "Test that the returned object is walked correctly."
	 (pattern-deval^1 (list A) (1) )
	 nil)
       

       ((pattern-deval
	 '(cons 4 5))
	 :comparand
	 '((pat-r-walkbind-list 
	     ((pat-r-walkbind-match 4))
	     (pat-r-walkbind-match 5))
	    nil))
       
       ((pattern-deval
	 '(list* 4 5))
	 :comparand
	 '((pat-r-walkbind-list 
	     ((pat-r-walkbind-match 4))
	     (pat-r-walkbind-match 5))
	    nil))


       ( "Test that the returned object is walked correctly."
	 (pattern-deval^1 (cons 4 5) (4 . 5) )
	 nil)       

       (
	 (pattern-deval
	   '`(,A 3 (4 . 5) ,A))

	 :pattern
	 (t
	   `((pat-r-walkbind-list
	       ((pat-r-walkbind-bind ,G5973)
		 (pat-r-walkbind-match 3)
		 (pat-r-walkbind-list
		   ((pat-r-walkbind-match 4))
		   (pat-r-walkbind-match 5))
		 (pat-r-walkbind-bind ,G5974))
	       nil)
	      ((pattern-postbind ,G5973 A)
		(pattern-postbind ,G5974 A)))))


       ( "We don't handle vectors yet"
	 (pattern-deval [1])
	 :predicate rtest-error-p)

       ( "We splice backquotes correctly"
	 (pattern-deval^1
	   `( (2 ,@A))
	   ( (2 G98779)))

	 nil)

       ;;Test the other functions.
       pattern-map-deval
       pattern-find-unmaker-function
       pattern-find-uneval-function
       pattern-build-un-ctor-by-struct-type
       pattern-call-normal-destructurer
       pattern-dequote
       pattern-desymbol-value
       pattern-delist
       pattern-delist*
       pattern-devector
       pattern-debq-finish-list
       pattern-debq-do-cons
       pattern-set-devaller-none
       pattern-debq
       pattern-debq-vector
       pattern-debq-do-middle-cons
       pattern-debq-finish-list

       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(de)eval a list-form


(defun pattern-set-devaller-special (func defunc)
  "DEFUNC must be a function that takes a list of args and
returns a pattern-deval-return"
  
  (setf
    (get func 'pattern-destructurer)
    (list defunc)))

(defun pattern-set-devaller-normal (func defunc)
  "DEFUNC must be a function that takes 1 arg, a list of walkers, and
returns a walker."
  
  (setf
    (get func 'pattern-destructurer)
    (list #'pattern-call-function defunc)))

(defun pattern-set-devaller-none (func)
  "Explicitly disallow a function"
  
  (setf
    (get func 'pattern-destructurer)
    (list #'rtest-error "There is no devaller for "func)))

(eval-when-compile
  (setf
    (get 'pattern-set-devaller-none 'rtest-setup)
    '(progn
       (pattern-set-devaller-none 'setq)))

  (setf
    (get 'pattern-set-devaller-none 'rtest-suite)
    '("pattern-set-devaller-none"
       ("pattern-set-devaller-none properly stops pattern-decall"
	 (pattern-decall 'setq '())
	 :predicate rtest-error-p)
       
       )))



(defun pattern-find-explicit-uneval-function (head)
  "Find a suitable uneval function."

  (get head 'pattern-destructurer))


(defun pattern-find-unmaker-function
  (head)
  "Use symbol's name to figure out an unctor function."
  (let
    ((raw-name
       (symbol-name head)))

    (when
      ;;Common Lisp doesn't have string-match.
      (and
	(>= (length raw-name) 5)
	(string= 
	  (downcase (substring raw-name 0 5)) 
	  "make-"))
      (let*
	((start-of-type-name
	   (match-end 0))
	  (type-name
	    (substring raw-name start-of-type-name)))

	(list #'pattern-build-un-ctor-by-struct-type type-name)))))


(eval-when-compile
  (setf
    (get 'pattern-find-unmaker-function 'rtest-suite)
    '("pattern-find-unmaker-function"

       ( "Finds something for a known structure."
	 (pattern-find-unmaker-function #'make-rtest-struct)
	 :test RESULT)

       )))


(defun pattern-find-uneval-function (head)
  ""
  (check-type head symbol)
  
  (or
    (pattern-find-explicit-uneval-function head)
    (pattern-find-unmaker-function head)))


(eval-when-compile
  (setf
    (get 'pattern-find-uneval-function 'rtest-suite)
    '("pattern-find-uneval-function"
       ((pattern-find-uneval-function 'cons)
	 (list #'pattern-call-function #'pattern-delist*))

       ((pattern-find-uneval-function 'list)
	 (list #'pattern-call-function #'pattern-delist))
       
       ((pattern-find-uneval-function 'pattern-deval)
	 (list #'pattern-eval))

       (rtest
	 rtest-inherit
	 :inherit-from 'pattern-find-unmaker-function
	 :do-call      #'pattern-find-uneval-function)
       
       )))


(defun pattern-apply-decall (decall-data args)
  ""

  (let
    ( (destruct-function (car decall-data))
      (use-args (append (cdr decall-data) args)))
  
    (cond
      ((not (functionp destruct-function))
	(rtest-error destruct-function" is not a function"))
      
      (t
	(apply destruct-function use-args)))))

(defun pattern-decall (head args)
  ""

  (when
    (not (true-list-p args))
    (rtest-error
      "Can't \(de\)call with a dotted list.  Try \(cons ...\) or backquote
syntax."))

  (let* 
    ( 
      (decall-data 
	(pattern-find-uneval-function head)))
    
    (cond
      ((null decall-data)
	(rtest-error "Couldn't find an uneval function for "head))

      (t
	(pattern-apply-decall decall-data args)))))



(eval-when-compile
  (setf
    (get 'pattern-call-normal-destructurer 'rtest-suite)
    '("pattern-call-normal-destructurer"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       
       '( "Works, but can't test it without pattern"
	 (pattern-decall 'list '(A B))
	  :pattern
	 (t
	   `((pat-r-walkbind-list 
	       ( (pat-r-walkbind-bind ,G0) 
		 (pat-r-walkbind-bind ,G1))
	       nil) 
	      ( (pattern-postbind ,G0 A) 
		(pattern-postbind ,G1 B)))))
       

       ((pattern-decall
	  'cons '(1 2))
	 :comparand
	 `((pat-r-walkbind-list
	      (,pat-r-match-1)
	      ,pat-r-match-2)
	    nil))
       
       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Map the args in a list, useful for functions, not special forms.

(defun pattern-map-deval (args)
  ""
  
  (loop
    for arg in args
    for (walker vars) = 
    (pattern-deval-return->list
      (pattern-deval arg))

    collect walker into walker-list
    append  vars   into vars-list

    ;;This is *not* a pattern-deval-return, whose walker is singular.
    finally return (values walker-list vars-list)))

(eval-when-compile
  (setf
    (get 'pattern-map-deval 'rtest-suite)
    '("pattern-map-deval"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       ((pattern-map-deval '(1 2 3))
	 
	 `(( ,pat-r-match-1 
	     ,pat-r-match-2
	     ,pat-r-match-3) 
	    nil))
       

       )))


(defun pattern-call-function (func &rest args)
  "Handle calling for functions with no special requirements"
  (multiple-value-bind
    (walkers vars)
    (pattern-map-deval args)
    
    (make-pattern-deval-return 
       :walker (funcall func walkers)
       :vars   vars)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Support specific functions

;;;;;
;;ignore


(defun pattern-deignore (&rest args)
  "Walk to match nil"

  (make-pattern-deval-return 
     :walker (list 'pat-r-walkbind-match nil)))

(pattern-set-devaller-special 'ignore #'pattern-deignore)

;;;;;;;
;;quote

(defun pattern-dequote (obj)
  "Walk to match OBJ"

  (make-pattern-deval-return 
    :walker (list 'pat-r-walkbind-match obj)))


(pattern-set-devaller-special 'quote #'pattern-dequote)


(eval-when-compile
  (setf
    (get 'pattern-dequote 'rtest-suite)
    '("pattern-dequote"
       (
	 (let
	   ((A 12))
	   (pattern-dequote A))
	 
	 '((pat-r-walkbind-match 12) nil))

       ((pattern-dequote 12)
	 '((pat-r-walkbind-match 12) nil))

       )))


;;;;;;;;;;
;;function

(defun pattern-defunction (arg)
  ""
 
  ;;This satisfies the same tests, but is it proper?  ISTM it should
  ;;work in both Elisp and Common Lisp

  (pattern-dequote (eval (list 'function arg))))


(pattern-set-devaller-special 'function #'pattern-defunction)


;;Untested

;;;;;;;;;;;;;;
;;symbol-value

(defun pattern-desymbol-value (old-sym)
  ""
  (check-type old-sym symbol)
  (if
    ;;The special symbol `_' matches anything.
    (eq old-sym '_)
    (make-pattern-deval-return
      :walker (list 'pat-r-walkbind-ignore))
    
    (let
      ((local-sym (gensym)))

      (make-pattern-deval-return 
	:walker (list 'pat-r-walkbind-bind local-sym)
	:vars
	(list
	  (make-pattern-postbind
	    :inside  local-sym
	    :outside old-sym))))))

(pattern-set-devaller-special 'symbol-value #'pattern-desymbol-value)


(eval-when-compile
  (setf
    (get 'pattern-desymbol-value 'rtest-suite)
    '("pattern-desymbol-value"

       ((pattern-desymbol-value 'A)
	 :pattern
	  (t 
	    `((pat-r-walkbind-bind ,G6) ((pattern-postbind ,G6 A)))))
       
       ( "We treat `_' specially, letting it match anything and not
comparing between matches."
	 (pattern-desymbol-value '_)
	 '((pat-r-walkbind-ignore) nil))
       
       )))


;;;;;;;;;;;;;
;;pattern-deval

(defun pattern-eval (expression)
  "Match against the value of EXPRESSION, computed at runtime"

  (let
    ((sym (gensym)))
    
    (make-pattern-deval-return 
      :walker 
      (list 'pat-r-walkbind-match sym)
      :vars   
      (list
	(make-pattern-prebind :inside sym :form expression)))))


(pattern-set-devaller-special 'pattern-deval #'pattern-eval)
(pattern-set-devaller-special 'deval         #'pattern-eval) 
;;Tested in -destructure.


;;;;;;
;;list

(defun pattern-delist (walkers)
  ""

  (make-pattern-delist-data
    :name        'pat-r-walkbind-list
    :walkers     walkers))

(pattern-set-devaller-normal 'list #'pattern-delist)


(eval-when-compile
  (setf
    (get 'pattern-delist 'rtest-setup)
    '(progn
       (defun pattern-delist^1 (&rest args)
	 ""
	 (apply #'pattern-call-function #'pattern-delist args))))

  (setf
    (get 'pattern-delist 'rtest-suite)
    '("pattern-delist"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       (
	 (pattern-delist^1 1 2 3)
	 :comparand
	 `((pat-r-walkbind-list 
	     (,pat-r-match-1
	       ,pat-r-match-2
	       ,pat-r-match-3) 
	     nil) 
	    nil))

       ( (pattern-delist^1 2)
	 :comparand
	 `((pat-r-walkbind-list (,pat-r-match-2) nil) nil)
	 )

       ;;Needs pattern to test
       '( (pattern-delist^1 'A)
	 '((pat-r-walkbind-list 
	    ((pat-r-walkbind-bind G70454)) nil) 
	   ((A . G70454))))
       
       ( "It applies OK"
	 (pattern-deval^2
	   (pattern-delist^1 'A) '(1))
	 
	 nil)

       )))

;;;;;;;
;;list*, cons

(defun pattern-delist* (walkers)
  ""
  (let
    ((butlast-walker (butlast walkers))
      (last-walker (car (last walkers))))

    (if
      (not butlast-walker)
      ;;If there is only the last walker, use it directly, not as the
      ;;end of a list. 
      last-walker
      
      (progn

	;;If last-walker walks a list, include its walkers directly
	;;and try again with its rest.
	(while
	  (and
	    last-walker
	    (eq (car last-walker) 'pat-r-walkbind-list))
	  (callf append butlast-walker 
	    (pattern-delist-data-walkers last-walker))
	  (setq last-walker 
	    (pattern-delist-data-rest-walker last-walker)))
    
	(make-pattern-delist-data
	  :name        'pat-r-walkbind-list
	  :walkers     butlast-walker
	  :rest-walker last-walker)))))




(pattern-set-devaller-normal 'list* #'pattern-delist*)
(pattern-set-devaller-normal 'cons  #'pattern-delist*)

(eval-when-compile
  (setf
    (get 'pattern-delist* 'rtest-setup)
    '(progn
      (defun pattern-delist*^1 (&rest args)
	""
	(apply #'pattern-call-function #'pattern-delist* args))))
  (setf
    (get 'pattern-delist* 'rtest-suite)
    '("pattern-delist*"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       ((pattern-delist*^1 1 2 3)
	 :comparand
	 `((pat-r-walkbind-list 
	     (,pat-r-match-1
	       ,pat-r-match-2)
	     ,pat-r-match-3) 
	    nil))

       ((pattern-delist*^1 1)
	 :comparand
	 `(,pat-r-match-1 nil))

       ( "We merge list-tails into a single list-walker if possible"
	 (pattern-delist*
	   `( (pat-r-walkbind-match 0)
	      (pat-r-walkbind-list 
	       (,pat-r-match-1)
	       (pat-r-walkbind-list 
		 (,pat-r-match-2)
		 (pat-r-walkbind-list 
		   (,pat-r-match-3)
		   nil)))))
	
	 `(pat-r-walkbind-list 
	    ( (pat-r-walkbind-match 0)
	      ,pat-r-match-1
	      ,pat-r-match-2
	      ,pat-r-match-3) 
	    nil))
       
       
       )))


;;;;;;;;;
;;vector

(defun pattern-devector (vec)
  ""
  (destructuring-bind
    (walker vars)
    ;;Process it as a list.
    (apply #'pattern-call-function #'pattern-delist (rtest-vec->list vec))


    ;;Turn it back into a vector-walker
    (make-pattern-deval-return 
       :walker (pattern-change-walker walker 'pat-r-walkbind-vector)
       :vars   vars)))


(pattern-set-devaller-special 'vector #'pattern-devector)


(eval-when-compile
  (setf
    (get 'pattern-devector 'rtest-suite)
    '("pattern-devector"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       ( (pattern-devector [0 1])
	 :comparand
	 `(
	   (pat-r-walkbind-vector 
	     ((pat-r-walkbind-match 0)
	       ,pat-r-match-1) nil)
	   nil))

       ( "Test that it walks OK"
	 (pattern-deval^2 (pattern-devector [A 1]) '[2 1])
	 nil)
       
       )))

;;;;;;;;
;;make-STRUCTURE-NAME


(defun pattern-build-un-ctor-by-struct-type (struct-type &rest clauses)
  "Return a walker for the cted object.
This only works with default ctors."
  
  (loop
    for (key value) on clauses by #'cddr

    for (walker vars) = 
    (pattern-deval-return->list
      (pattern-deval value))

    collect
    (list
      'pat-r-walkbind-by-accessor
      (rtest-get-field-accessor struct-type key)
      walker)
    into walker-list
    
    append vars into vars-list
    finally return 
    (make-pattern-deval-return 
      :walker 
      (list
	'pat-r-walkbind-every
	;;This could prepend a type-check walker, but we don't have
	;;typecheck walkbinds yet.
	walker-list)
      :vars  vars-list )))



(eval-when-compile
  (setf
    (get 'pattern-build-un-ctor-by-struct-type 'rtest-suite)
    '("pattern-build-un-ctor-by-struct-type"
       ( "With no args, gives nil \(later could give just a type test\)"
	 (pattern-build-un-ctor-by-struct-type
	   'rtest-struct)
	 :comparand
	 '
	 ((pat-r-walkbind-every nil) nil))

       ( "With args, walks the various fields \(No check on doing 'em twice\)"
	 (pattern-build-un-ctor-by-struct-type
	   'rtest-struct :my-field 12)
	 :comparand
	 '
	 ((pat-r-walkbind-every 
	    ((pat-r-walkbind-by-accessor 
	       rtest-struct-my-field 
	       (pat-r-walkbind-match 12)))) 
	   nil))

       ( "Test that it walks OK"

	 (pattern-deval^2
	   (pattern-build-un-ctor-by-struct-type
	     'rtest-struct :my-field 12)
	   (make-rtest-struct :my-field 12))
	 nil)
       
       
       )))


;;;;;;;;;;;


(defconst pattern-sequence-alist
  '(
     ('pat-r-walkbind-list     t   'pat-r-walkbind-splicing-list)
     ('pat-r-walkbind-optional nil nil)
     ('pat-r-walkbind-vector   t   nil)
     )

  "Data describing the walker functions.

\(NAME appendable-p spliced-variant\)")

(defun pattern-walks-sequence-p (obj)
  ""

  (let*
    ((head (car obj))
      (data (assq head pattern-sequence-alist)))
    
    (and data (second data))))

;;;;;;;;
;;append


(defun pattern-deappend (walkers)
  "Deval `append'
WALKERS is a list of list-walkers, which must all terminate except
possibly the last one."

  (cond
    (
      (not
	(every #'pattern-walks-sequence-p walkers))
      (rtest-error "Append can only append sequences"))

    (
      (some 
	#'pattern-delist-data-rest-walker 
	(butlast walkers))
      (rtest-error "Append can't append unterminated lists"))

    (t
      (let
	( (butlast-walkers (butlast walkers))
	  (last-walker (car (last walkers))))
      (make-pattern-delist-data
	:name
	'pat-r-walkbind-list
	:walkers
	(append
	  (apply #'append
	    (mapcar #'pattern-delist-data-walkers butlast-walkers))
	  (pattern-delist-data-walkers last-walker))
	:rest-walker 
	(pattern-delist-data-rest-walker last-walker))))))

(pattern-set-devaller-normal 'append  #'pattern-deappend)

;;Untested.

;;;;;;;;;;;
;;backquote

;;Backquote needs to know that its symbol is eg, SYSTEM::BACKQUOTE,
;;which varies between systems, so we figure it out at compile-time.

(defconst pattern-backquote-sym (eval-when-compile (car '`()))       "" )
(defconst pattern-unquote-sym   (eval-when-compile (caaadr '`(,A)))  "" )
(defconst pattern-splice-sym    (eval-when-compile (caaadr '`(,@A))) "" )
(defconst pattern-nsplice-sym   (eval-when-compile (caaadr '`(,.A))) "" )

(eval-when-compile
  "Constants for testing list walking and list splicing"
  (setf
    (get 'pattern-debq 'rtest-setup)
    '(progn
       (rtest-borrow-setup 'pat-r-walkbind-match)
       (defconst pattern-debq-walk-list-1
	 (make-pattern-delist-data
	   :name        'pat-r-walkbind-list
	   :walkers     (list pat-r-match-1))
	 "" )

       (defconst pattern-debq-walk-list-1-2
	 (make-pattern-delist-data
	   :name        'pat-r-walkbind-list
	   :walkers     (list pat-r-match-1)
	   :rest-walker pat-r-match-2)
	 "" )

       (defconst pattern-debq-walk-splice-1
	 (make-pattern-delist-data
	   :name        'pat-r-walkbind-splicing-list
	   :walkers     (list pat-r-match-1)
	   :rest-walker nil)
	 "" )

       (defconst pattern-debq-walk-splice-1-2
	 (make-pattern-delist-data
	   :name        'pat-r-walkbind-splicing-list
	   :walkers     (list pat-r-match-1)
	   :rest-walker pat-r-match-2)
	 "" )

  
       (defconst pattern-debq-walk-splice-nil-2
	 (make-pattern-delist-data
	   :name        'pat-r-walkbind-splicing-list
	   :walkers     '()
	   :rest-walker pat-r-match-2)
	 "" ))))



(defun pattern-debq-finish-list (form)
  "Return unbackquote walker for a debackquoted object at the end of a
list.  This means that there is an implicit nil after it."

  (case
    (car form)

    ;;If it's to be spliced in, eg `(... ,@B), the walker has the same
    ;;data as before, just with a non-splicing head because we just
    ;;spliced it in.  This holds whether it has middle walkers or not.
    ('pat-r-walkbind-splicing-list
      (pattern-change-walker form 'pat-r-walkbind-list))
    
    ;;Otherwise it's the last middle walker of a list which has no
    ;;final walker.
    (t
      (make-pattern-delist-data
	:name	     'pat-r-walkbind-list
	:walkers     (list form)
	:rest-walker nil))))



(eval-when-compile
  (setf
    (get 'pattern-debq-finish-list 'rtest-suite)
    '("pattern-debq-finish-list"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       (rtest rtest-borrow-setup 'pattern-debq)

       ((pattern-debq-finish-list pat-r-match-1)
	 
	 pattern-debq-walk-list-1)

       ( "A splice at the end of a list becomes a normal list."
	 (pattern-debq-finish-list
	   pattern-debq-walk-splice-1-2)
	 pattern-debq-walk-list-1-2)

       )))




(defun pattern-debq-get-uplevel-walkers (walker)
  "Return a list of the walkers that would constitute this walker when
it's spliced into the level above it.  Error if the part can't be used
up 1 level, eg because it's unterminated."
  (if
    (eq 
      (pattern-delist-data-name walker)
      'pat-r-walkbind-splicing-list)
    (progn
      (if
	(pattern-delist-data-rest-walker walker)
	(rtest-error 
	  "Can't splice an indefinitely-long list except at the end"))
      (pattern-delist-data-walkers walker))
    (list walker)))


(defun pattern-debq-do-middle-cons 
  (car-walker cdr-walker)
  "Return a walker for a debackquoted cons cell in the middle of a list"

  ;;There should never be a splice after our dot.  It should have
  ;;already been transformed by pattern-debq-finish-list.
  (assert 
    (not 
      (eq 
	(pattern-delist-data-name cdr-walker)
	'pat-r-walkbind-splicing-list)))

  (pattern-delist*
    (append
      (pattern-debq-get-uplevel-walkers car-walker)
      (list cdr-walker))))


(eval-when-compile

  (setf
    (get 'pattern-debq-do-middle-cons 'rtest-suite)
    '("pattern-debq-do-middle-cons"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)
       (rtest rtest-borrow-setup 'pattern-debq)

       ( "We combine dotted cells correctly"
	 (pattern-debq-do-middle-cons
	   pat-r-match-1 pat-r-match-2)

	 (make-pattern-delist-data
	   :name        'pat-r-walkbind-list
	   :walkers     (list pat-r-match-1)
	   :rest-walker pat-r-match-2))


       ( "We don't allow an indefinite splice in the middle"
	 (pattern-debq-do-middle-cons
	   pattern-debq-walk-splice-1-2
	   pat-r-match-2)
	 
	 :predicate rtest-error-p)

       ( "We allow a splice in the middle"
	 (pattern-debq-do-middle-cons
	   pattern-debq-walk-splice-1
	   pat-r-match-2)
	 
	 pattern-debq-walk-list-1-2)

       )))



(defun pattern-debq-do-cons (car-arg cdr-arg)
  "Return pattern-deval-return for a debackquoted cons cell"

  (destructuring-bind
    (car-walker car-vars)
    (pattern-debq car-arg)

    (if
      (not cdr-arg)
      (make-pattern-deval-return 
	:walker (pattern-debq-finish-list car-walker)
	:vars   car-vars)

      (destructuring-bind
	(cdr-walker cdr-vars)
	(pattern-debq cdr-arg)

	(make-pattern-deval-return 
	   :walker (pattern-debq-do-middle-cons car-walker cdr-walker)
	   :vars   (append car-vars cdr-vars))
	))))


(eval-when-compile
  (setf
    (get 'pattern-debq-do-cons 'rtest-suite)
    '("pattern-debq-do-cons"
       (rtest rtest-borrow-setup 'pat-r-walkbind-match)

       ( "Handles normal lists"
	 (pattern-debq-do-cons 1 '(2))
	 :comparand
	 `((pat-r-walkbind-list 
	     (,pat-r-match-1 ,pat-r-match-2) nil) 
	    nil))

       ( "Handles dotted lists"
	 (pattern-debq-do-cons 1 '(2 . 3) )
	 :comparand
	 `((pat-r-walkbind-list 
	     (,pat-r-match-1 
	       ,pat-r-match-2)
	     ,pat-r-match-3) nil))

       ( "Merges tails into a single list"
	 (pattern-debq-do-cons 1 '(2 3) )

	 `((pat-r-walkbind-list 
	    (,pat-r-match-1 
	      ,pat-r-match-2
	      ,pat-r-match-3) nil) 
	   nil))

       ( "Refuses dotted splices, which are wrong."
	 (pattern-debq-do-cons 1 '(\,@ 2) )
	 :predicate rtest-error-p)


       
       )))


(defun pattern-make-splicing-form (form)
  ""
  (case (car form)
    
    ('pat-r-walkbind-splicing-list
      (rtest-error "The form "form" tried to splice twice at once"))

    ('pat-r-walkbind-list 
      (pattern-change-walker form 'pat-r-walkbind-splicing-list))
    
    (t
      (make-pattern-delist-data
	:name        'pat-r-walkbind-splicing-list
	:walkers     '()
	:rest-walker form))))


(eval-when-compile
  (setf
    (get 'pattern-make-splicing-form 'rtest-suite)
    '("pattern-make-splicing-form"
       (
	 (pattern-make-splicing-form pat-r-match-2)
	 pattern-debq-walk-splice-nil-2)

       )))


(defun pattern-debq-listish
  (object)
  "Return walkers a var-list for a backquoted list-like expression."
  (cond

    ;;Handle the unquoters.  There are no real function calls, just
    ;;head-driven unquoters.  Fortunately, backquote itself breaks its
    ;;expressions into head-driven lists.
    
    ((eq (car object) pattern-unquote-sym)
      (pattern-deval
	(second object)))

    ((or
       (eq (car object) pattern-splice-sym)
       (eq (car object) pattern-nsplice-sym))
      
      (destructuring-bind
	(walker vars)
	(pattern-deval (second object))
	
	(make-pattern-deval-return 
	   :walker (pattern-make-splicing-form walker)
	   :vars   vars)))

    (t
      (pattern-debq-do-cons (car object) (cdr object)))))

(eval-when-compile
  (setf
    (get 'pattern-debq-listish 'rtest-suite)
    '("pattern-debq-listish"
       ( (pattern-debq-listish '(\, A))
	 :pattern
	 (t
	   `((,'pat-r-walkbind-bind ,G7) ((A . ,G7)))))

       (
	 (pattern-debq-listish '(\,@ A))
	  
	 :pattern
	 (t
	   `(
	      (,'pat-r-walkbind-splicing-list 
		nil 
		(,'pat-r-walkbind-bind ,G7)) 
	      ((A . ,G7)))))

       )))

;;We handle vectors as lists, then transform them back.  
(defun pattern-debq-vector
  (object)
  ""

  (destructuring-bind
    (list-walker vars)
    (pattern-debq-listish
      (rtest-vec->list object))

    (if
      (not
	(eq
	  (pattern-delist-data-name list-walker)
	  'pat-r-walkbind-list))
      (rtest-error "Internal error: We didn't build a proper list.")
    
      (let* 
	((vec-walker
	   (pattern-change-walker list-walker 'pat-r-walkbind-vector)))
	
	(make-pattern-deval-return 
	  :walker vec-walker
	  :vars   vars)))))


;;Untested
(eval-when-compile
  (setf
    (get 'pattern-debq-vector 'rtest-suite)
    '("pattern-debq-vector"
       )))


(defun pattern-debq (object)
  "Return walker and vars for a backquoted object"
  
  (cond
    ((consp object)
      (pattern-debq-listish object))
    
    ((vectorp object)

      (pattern-debq-vector object))
    
    ;;Use it as a constant
    ((atom object)
      (pattern-dequote object))))

(eval-when-compile
  (setf
    (get 'pattern-debq 'rtest-suite)
    '("pattern-debq"

       ( "This handles splices"
	 (pattern-debq  '(\,@ 2) )
	 (list pattern-debq-walk-splice-nil-2
	   nil))

       ( "This handles splices of symbols"
	 (pattern-debq '(\,@ A))

	 :map
	 ( () (:predicate identity) ))

       ( "Splicing patterns run OK"
	 (pattern-deval^2
	   (pattern-debq (second '`( 2 ,@A)))
	   '( 2 G98779))

	 nil)

       (
	 (pattern-debq (second '`( ,@A)))

	 :pattern
	 (t
	   `((pat-r-walkbind-list nil
	       (pat-r-walkbind-bind ,G6025))
	      ((pattern-postbind ,G6025 A)))))
       
       

       ( "Can't splice an indefinitely-long list except at the end"
	 (pattern-debq (second '`( ,@A 2)))
	  
	 :predicate rtest-error-p)
       
       ( "An indefinitely-long list splices in at the end."
	 (pattern-deval '`( 2 ,@A))

	 :pattern
	 (t
	   `((pat-r-walkbind-list
	       ((pat-r-walkbind-match 2))
	       (pat-r-walkbind-bind ,G6030))
	      ((pattern-postbind ,G6030 A)))))
       

       
       )))

(defun pattern-debq-top (object)
  "Return pattern-deval-return for a backquoted object."

  (pattern-debq object))

(pattern-set-devaller-special pattern-backquote-sym #'pattern-debq-top)


;;End backquote functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Overall tests are on the symbol pattern-deval

(provide 'pattern-deval)

;;; pattern-deval.el ends here