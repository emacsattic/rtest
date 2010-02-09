;;; rtest-inherit.el --- Code that handles inheriting tests in rtest

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

(require 'rtest-suite)
(require 'rtest-probe)


;;;;;;;;;;;


(defun rtest-one-inherited-probe 
  (probe-report base-probe base-clauses probe-builder own-clauses)
  ""

  (let
    (
      (use-clauses
	(append
	  (rtest-get-only-tests-in-set rtest-grader-keys base-clauses)
	  own-clauses))
      (use-probe
	(funcall probe-builder base-probe)))
	
    (rtest-one-probe-2 probe-report use-probe use-clauses)))

(defun rtest-inherit-one-probe
  (probe-builder test-form own-clauses)
  ""

  (rtest-one-probe-4
    test-form
    #'rtest-one-inherited-probe
    probe-builder own-clauses))


(eval-when-compile

  (setf
    (get 'rtest-inherit-one-probe 'rtest-setup)
    '(progn
       (defun rtest-calc-all-bindings%1 (base-probe)
	 "Functions like this return the new probe or throw an error"
  
	 (if
	   (eq
	     (car base-probe)
	     'rtest-calc-bindings)

	   `(rtest-calc-all-bindings 
	      (list
		(make-rtest-clause :let ,(second base-probe))))

	   (throw 'rtest-cant-inherit nil)))
  
       (defconst rtest-inherit-one-probe^succeed
	 '("A test that succeeds"
	    (rtest-calc-bindings 'nil)
	    :comparand
	    '(nil nil))
	 "")

       (defconst rtest-inherit-one-probe^fail
	 '("A test that fails"
	    (rtest-calc-bindings 'nil)
	    :comparand
	    '(5 nil))
	 "")))

  (setf
    (get 'rtest-inherit-one-probe 'rtest-suite)
    '("rtest-inherit-one-probe"
       (rtest rtest-borrow-setup 'rtest-inherit-one-probe)

       ( "Successful inherited tests also return nil"
	 (rtest-inherit-one-probe
	   #'rtest-calc-all-bindings%1
	   rtest-inherit-one-probe^succeed
	   '())
	 nil)

       ( "Failing inherited tests also return a failure"
	 (rtest-inherit-one-probe
	   #'rtest-calc-all-bindings%1
	   rtest-inherit-one-probe^fail
	   '())
	 :type rtest-some-failure)
       

       )))


(defun rtest-inherit-one-test-object 
  (probe-builder test-object own-clauses)
  "Interpret a test object as an inherited test.

Inherited types are:
  * List, a test object which is converted by probe-builder.

Lists that begin with quotes and all other types are ignored
silently."

  (typecase test-object
    (cons
      (case (car test-object)
	;;Skip quoted objects
	(quote (rtest-make-skip-summary))

	;;Skip (rtest...) lists, because we're not ready for nested
	;;inheritance yet.
	(rtest (rtest-make-skip-summary))

	;;Do list objects as simple tests
	(t
	  (let
	    ((failure 
	       (rtest-inherit-one-probe 
		 probe-builder test-object own-clauses)))
	    (rtest-make-fail-summary failure)))))

    ;;Skip other objects
    (t
      (rtest-make-skip-summary))))



(eval-when-compile
  (setf
    (get 'rtest-inherit-one-test-object 'rtest-suite)
    '("rtest-inherit-one-test-object"
       (rtest
	 rtest-inherit-raw
	 :docstring "It normally just calls to rtest-inherit-one-probe"
	 :inherit-from  'rtest-inherit-one-probe
	 :probe-builder
	 #'(lambda (base-probe)
	     `(rtest-fail-summary-failure
		(rtest-inherit-one-test-object ,@(cdr base-probe)))))
       
       ( "Skip quoted objects"
	 (rtest-inherit-one-test-object nil ''() nil)
	 (rtest-make-skip-summary))
       
       ( "Skip rtest objects for now, which would be nested."
	 (rtest-inherit-one-test-object nil '(rtest error "abc") nil)
	 (rtest-make-skip-summary))

       )))



(defun rtest-make-inheriting-closure (probe-builder own-clauses)
  "Return a closure that calls rtest-inherit-one-test-object"

  (lexical-let
    ( 
      (probe-builder probe-builder)
      (own-clauses   own-clauses))
    
    #'(lambda (test-object)
	(rtest-inherit-one-test-object 
	  probe-builder test-object own-clauses))))


(eval-when-compile
  (setf
    (get 'rtest-make-inheriting-closure 'rtest-suite)
    '("rtest-make-inheriting-closure"

       (rtest rtest-borrow-setup 'rtest-inherit-one-probe)
       (
	 (funcall
	   (rtest-make-inheriting-closure 
	     #'rtest-calc-all-bindings%1
	     '())
	   rtest-inherit-one-probe^succeed)
	 (rtest-make-fail-summary nil))
       
       (
	 (funcall
	   (rtest-make-inheriting-closure 
	     #'rtest-calc-all-bindings%1
	     '())
	   rtest-inherit-one-probe^fail)

	 :map
	 (
	   (1)
	   (1)
	   (:type rtest-some-failure)))
       
       
       )))

(defun rtest-make-probe-builder 
  (expected-func find-probe do-call alter-output)
  ""

  ;;Save these objects in case they are changed later.  Note that in
  ;;the case of functions, the values contain the functions, so this
  ;;works. 
  (lexical-let
    ( (expected-func expected-func)
      (do-call       do-call)
      (find-probe    find-probe)
      (alter-output  alter-output))
    ;;Return a closure which takes the base probe as input
    #'(lambda (base-exp)
	(let
	  ;;Find the base call lexically.
	  ((base-call (funcall find-probe base-exp)))
	  (if (eq (car base-call) expected-func)
	    ;;Return a executable form
	    `(funcall
	       #',alter-output
	       (funcall 
		 #',do-call
		 ,@(cdr base-call)))
	    ;;Or throw an error if we can't build one
	    (throw 'rtest-cant-inherit nil))))))


;;;;;;;;;;;;;
;;Entry-point

(defun* rtest-inherit-raw
  (&key 
    probe-builder 
    inherit-from 
    (own-clauses '())
    (docstring "Unnamed inherited test"))
  "
INHERIT-FROM is the symbol whose suite should be inherited from.
PROBE-BUILDER can be any function that accepts the base probe
expression and returns an expression to be evaluated instead."
  (rtest-one-suite 
    (rtest-symbol-to-suite inherit-from)
    (rtest-make-inheriting-closure probe-builder own-clauses)))

(eval-when-compile
  (setf
    (get 'rtest-inherit-raw 'rtest-suite)
    '("rtest-inherit-raw"
       (rtest rtest-borrow-setup 'rtest-inherit-one-probe)
       (
	 (rtest-inherit-raw
	   :inherit-from  'rtest-calc-bindings
	   :probe-builder #'rtest-calc-all-bindings%1)
	 :map ((4) (0) ()))

       (rtest
	 rtest-inherit-raw
	 :docstring "It works within rtest"
	 :inherit-from  'rtest-calc-bindings
	 :probe-builder #'rtest-calc-all-bindings%1)
       

       )))


(defun* rtest-inherit
  (&key 
    inherit-from 
    do-call
    (find-probe   #'identity)
    (alter-output #'identity)
    (own-clauses  '())
    (docstring    "Unnamed inherited test"))
  "Inherit the test-suite INHERIT-FROM.

Terminology: The \"base function\" is the function that has the
original test suite, usually the same as INHERIT-FROM.

The \"derived function\" is the function inheriting the test suite, ie
the function that's *not* INHERIT-FROM.

DO-CALL, which must be supplied, should call the derived function.

FIND-PROBE is passed the entire probe expression and finds the part
that actually calls the base function.  It defaults to #'identity.

ALTER-OUTPUT is passed the probe's output and should return the output
appropriate to the base function.

OWN-CLAUSES are the clauses that should be used in the new tests.  All
original `:around' and `:let' forms are stripped."
  
  (let
    ((probe-builder 
       (rtest-make-probe-builder 
	 inherit-from find-probe do-call alter-output)))

    (rtest-one-suite 
      (rtest-symbol-to-suite inherit-from)
      (rtest-make-inheriting-closure probe-builder own-clauses))))

(eval-when-compile
  (setf
    (get 'rtest-inherit 'rtest-suite)
    '("rtest-inherit"
       rtest-inherit-one-probe
       rtest-make-inheriting-closure
       rtest-inherit-raw
       )))

(provide 'rtest-inherit)

;;; rtest-inherit.el ends here