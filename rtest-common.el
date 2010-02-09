;;; rtest-common.el --- top of code common to both Elisp and CL rtest

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

;; See the rtest docs

;;; Code:

;;;;;;;;;;;;;;;


;;Include the whole system.
(require 'rtest-suite)
(require 'rtest-htmlify )
(require 'rtest-plus   )
(require 'rtest-tools)
(require 'rtest-inherit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Testing

(eval-when-compile
  (setf 
    (get 'rtest-rtest 'rtest-suite)
    '("rtest-rtest"

       ;;Test other modules of rtest
       rtest-grade
       rtest-probe
       rtest-suite
       rtest-htmlify
       rtest-plus
       rtest-tools
       rtest-inherit
       rtest-system-specific

       ;;A few tests harking back to days of yore
       demo1
       demo4))


  ;;Demonstrations
  (setf 
    (get 'demo1 'rtest-suite) 
    '("three ways of writing the same test"
       ("Implicit test"
	 (/ 30 2)
	 15
	 )

       (;; the description string is optional
	 (/ 30 2)
	 15
	 )

       ("Implicit test with a calculated comparand"
	 (/ 30 2)
	 (* 3 5)
	 )

       ("Explicit test"
	 (/ 30 2)
	 :test
	 (eq RESULT 15)
	 )

       ))

  
  (setf 
    (get 'demo4 'rtest-suite) 
    '("Demonstrations of error recognition"

       ( "Expect an error of some kind, using rtest-error-p"
	 (+ "Not a number" "Not one either")
	 :test
	 (rtest-error-p RESULT))

       ))


  ;;DELIBERATE FAILURES to exercise the reporter code and the
  ;;connection to it.  Failures should be clearly reported and should
  ;;not propagate errors outside of rtest.  Clarity of reporting has
  ;;to be tested by inspection, no way to automate that.

  (setf
    (get 'rtest-common-delibfail 'rtest-suite)
    '("rtest-common-deliberate:  Deliberate failures to show
how the failures look."

     ("Deliberate error to demonstrate report format"
       (/ 30 2)
       17)

       
       ("An error thrown by the grader expression itself will not
masquerade as a successful test"
	 t
	 :test
	 (progn 
	   (error "Not a real error, but an incredible simulation")
	   t))

       ("A probe without tests makes a failure."
	 345)

       ("An unparseable test makes a failure"
	 1 1 1 1)


       ( "A map with multiple misses still makes a clear report."
	 (list 1 1 '((A B C)))
	 
	 :map
	 ((0) (0) ((A B C))))


       ;;:every (:type rtest-single-failure) would be correct
       ( "Problems in syntax after :every are reported clearly."
	 '((rtest-single-failure "a" "b"))

	 :every rtest-single-failure)
       
       ( "Keys that aren't keys are reported"
	 1 :comparand 1 :not-a-key 1)

       ( "Uneven-length clauses are reported"
	 1 :comparand)

       ("Failures in arounds are reported clearly"
	 1 :around 17)

       ("When there aren't enuff forms to satisfy map, report it clearly"
	 '(1)
	 :map ((1) (2)))

       ( rtest
	 progn
	 "User-supplied tests can fail appropriately"
	 (rtest-make-fail-summary 
	   (rtest-make-single-failure
	     "Deliberate failure" 
	     nil)))

       ( rtest
	 error
	 "User-supplied tests that cause errors don't crash us")

       ( rtest
	 progn
	 "User-supplied tests that return badly-typed results don't
crash us.")

       )))


(provide 'rtest-common)

;;; rtest-common.el ends here