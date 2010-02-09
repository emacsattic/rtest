;;; rtest-parse.el --- Parse test clauses for rtest

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

;;Portability
(when (boundp 'emacs-version)
  (eval-and-compile
    (require 'cl)
    (require 'tehom-cl)))

;; Other requirements
(require 'rtest-visible)


;;Fake a (defstruct (rtest-clause (type: cons)) key value)
(defalias 'rtest-clause-key   'car)
(defalias 'rtest-clause-value 'cdr)
(defun make-rtest-clause (key value)
  ""
  
  (cons key value))

(defun rtest-parse-some-tests-in-order (tests test-set)
  ""
  
  (let* 
    ((value (car tests)))

    ;;The first element is a comparand unless it's a test keyword.
    (if
      (memq value test-set)
      (list 
	'()
	tests)
      (list 
	(list (make-rtest-clause :comparand value)) 
	(cdr tests)))))

(eval-when-compile
  (setf
    (get 'rtest-parse-some-tests-in-order 'rtest-suite)
    '("rtest-parse-some-tests-in-order"

       ( "The first element normally becomes a comparand"
	 (rtest-parse-some-tests-in-order '(l) rtest-grader-keys)
	 :test
	 (equal RESULT '(((:comparand . l)) nil)))

       ( "If the first element is a recognized key, it's not used."
	 (rtest-parse-some-tests-in-order '(:test l) rtest-grader-keys)
	 :test
	 (equal RESULT '(nil (:test l))))

       ( "Any further elements aren't used."
	 (rtest-parse-some-tests-in-order '(l :comparand m)
	   rtest-grader-keys) 
	 :test
	 (equal RESULT '(((:comparand . l)) (:comparand m))))

       )))



(defun rtest-parse-some-tests-by-key (clauses test-set)
  ""

  (assert-in-rtest (evenp (length clauses))
    "After there are keys, list should be even in length" clauses)

  ;;Loop thru by 2's.
  (loop
    for (key value) on clauses by #'cddr
    do
    (assert-in-rtest (memq key test-set)
      "Key should be one of the known keys" key test-set)

    collect (make-rtest-clause key value)))

(eval-when-compile
  (setf
    (get 'rtest-parse-some-tests-by-key 'rtest-suite)
    '("rtest-parse-some-tests-by-key"
       
       ( "Uneven lists are rejected"
	 (rtest-parse-some-tests-by-key '(l) rtest-grader-keys)
	 :predicate rtest-error-p)

       ( "Non-keywords in keyword position are rejected."
	 (rtest-parse-some-tests-by-key '(:not-a-key l)
	   rtest-grader-keys) 
	 :predicate rtest-error-p)

       ( 
	 (rtest-parse-some-tests-by-key '(:comparand l)
	   rtest-grader-keys)
	 '((:comparand . l)))

       ( "Multiple pairs become the respective tests"
	 (rtest-parse-some-tests-by-key '(:comparand l :test m)
	   rtest-grader-keys)
	 '((:comparand . l) (:test . m)))

       )))


(defun* rtest-parse-clauses (clauses test-set)
  ""

  (assert-in-rtest (listp clauses)
    "Expression should be a list" clauses)

  ;;if there are zero elements, don't do anything complicated, return
  ;;no clauses.
  (when
    (= (length clauses) 0)
    (return-from rtest-parse-clauses '()))

  
  (let
    (canon-clauses)
    
    ;;Get some canon-clauses in order, using some of clauses
    (multiple-value-setq
      (canon-clauses clauses)
      (values-list
	(rtest-parse-some-tests-in-order clauses test-set)))

    ;;Add more canon-clauses by key, using the rest of clauses
    (callf append canon-clauses 
      (rtest-parse-some-tests-by-key clauses test-set))

    canon-clauses))



(defun rtest-parse-tests (tests)
  ""

  (rtest-parse-clauses tests rtest-grader-keys))

;;In effect this checks out rtest-parse-clauses as well, but more
;;concisely.
(eval-when-compile
  (setf
    (get 'rtest-parse-tests 'rtest-suite)
    '("rtest-parse-tests"
       
       ( "Malformed, non-list tests throw error"
	 (rtest-parse-tests 1)
	 :predicate rtest-error-p)

       ( "No tests gives no canonized tests"
	 (rtest-parse-tests '())
	 '())

       ( "Malformed expressions throw error"
	 (rtest-parse-tests '(1 :comparand))
	 :predicate rtest-error-p)

       ( "The first element is normally treated as a comparand."
	 (rtest-parse-tests '(l))
	 '((:comparand . l)))

       ( "If the first element is a recognized keyword, it's used as
half of a pair."
	 (rtest-parse-tests '(:comparand l))
	 '((:comparand . l)))

       ( "Pairs are associated into test objects"
	 (rtest-parse-tests '(:predicate numberp)) 
	 '((:predicate . numberp)))

       ( "Pairs are associated into test objects"
	 (rtest-parse-tests '(:test (numberp RESULT))) 
	 '((:test . (numberp RESULT))))

       ( "Pairs are associated into test objects"
	 (rtest-parse-tests '(:comparand l :predicate numberp)) 
	 '((:comparand . l) (:predicate . numberp)))

       )))




;;;;;;;;;;;;;;;;;;;;;;

(defun rtest-parse-form (clause-form)
  ""
  
  (let*
    (
      (clause-list
	(rtest-parse-clauses clause-form rtest-all-keys)))

    (apply 'append
      (mapcar 
	#'(lambda (clause)
	    "Handle `:include's."
	    (if
	      (eq (rtest-clause-key clause) rtest-include-key)
	      ;;Recurse
	      (rtest-parse-form (eval (rtest-clause-value clause)))
	      (list clause))) 
	clause-list))))


(eval-when-compile
  (setf
    (get 'rtest-parse-form 'rtest-setup)
    '(progn
       (defconst rtest-add-includes-obj-0 
	 '(:around (let((a 12))))
	 "An includable object for testing." )))

  (setf
    (get 'rtest-parse-form 'rtest-suite)
    '("rtest-parse-form"

       ( "An ordinary clause gives a list of just itself."
	 (rtest-parse-form '(:test t))
	 '((:test . t)))

       ;;Some tests to make sure our test-object makes sense
       ( "rtest-add-includes-obj-0 evaluates OK"
	 (car
	   (rtest-parse-clauses 
	     '(:include rtest-add-includes-obj-0)
	     rtest-all-keys))
	 '(:include . rtest-add-includes-obj-0))
       
       ( "The value of the above evaluates to a valid form."
	 (eval (rtest-clause-value '(:include . rtest-add-includes-obj-0)))
	 '(:around (let ((a 12)))))

       ("The above parses to a canon-test"
	 (rtest-parse-clauses '(:around (let ((a 12)))) rtest-all-keys)
	 '((:around let ((a 12))))
	 :test (= (length RESULT) 1))
       
       ( "An include clause gives a list of its direct contents."
	 (rtest-parse-form
	   '(:include rtest-add-includes-obj-0))
	 '((:around let ((a 12)))))

       ("Works in rtest"
	 12 a
	 :include rtest-add-includes-obj-0)

       )))

(eval-when-compile
  (setf
    (get 'rtest-parse 'rtest-suite)
    '("rtest-parse"
       rtest-parse-some-tests-in-order
       rtest-parse-some-tests-by-key
       rtest-parse-tests
       rtest-parse-form
       )))

(provide 'rtest-parse)

;;; rtest-parse.el ends here

