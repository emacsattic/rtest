;;; rtest-compat.el --- rtest code incompatible with Common Lisp

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

(eval-and-compile
  (require 'cl)
  (require 'tehom-cl))

;;;;;;;;;;;;;;;;
;; Configuration

(defconst rtest-permanent-init-forms 
  '(:around (save-excursion))
  "" )

;;;;;;;;;;;;;;
;;Direct setup

;;We need a bit more space for some of our own tests.
(setq max-lisp-eval-depth 350)

;;;;;;;;;;;;;;;;;
;;rtest functions

(defalias 'rtest-err->typeable 'cdr)

(defun rtest-signal (data)
  (signal 'rtest-general-error data))

(defalias 'rtest-print-to-string 'pp-to-string)

(defun rtest-convert-to-string (piece)
  "Convert PIECE to a string, regardless what it originally was."
  (prin1-to-string piece t))

(defun rtest-result-to-list (result cheat-multiples)
  ""
  
  (if
    cheat-multiples
    result
    (list result)))

(defun rtest-get-testable-functions ()
  "Return a list of names of all test suite symbols."

  (loop
    for x being the symbols of obarray
    if (get x 'rtest-suite)
    collect (symbol-name x)))

;;;;;;;;;;;;;;;
;;;; User tools

(defun rtest-error-p (obj)
  "t if OBJ is any sort of error, otherwise nil."

  (and
    (consp obj)
    (symbolp (car obj))
    (get (car obj) 'error-conditions)))

;;;;
;;Tests incompatible with Common Lisp

(eval-when-compile
  (setf
    (get 'rtest-system-specific 'rtest-suite)
    '( rtest-demo-buffer-support
       demo3))

  (setf 
    (get 'rtest-demo-buffer-support 'rtest-suite) 
    '("How rtest supports buffers.  This is only meaningful in Elisp"
       
       ( "In Elisp, each test is run inside of a save-excursion, so
set-buffer is allowed."
	 (rtest-one-probe-0
	   ;;Dummy test that just sets buffer
	   '((set-buffer (get-buffer-create "*scratch*"))
	      :test t))
	  
	 :test (eq buf (current-buffer))
	 :let ((buf (current-buffer))))

       ( "switch-to-buffer can be made safe by using save-window-excursion"
	 (rtest-one-probe-0
	   ;;Dummy test that just sets buffer and window
	   '((switch-to-buffer (get-buffer-create "*scratch*"))
	      :test t
	      :around (save-window-excursion)))
	  
	 :test (equal win (current-window-configuration))
	 :let ((win (current-window-configuration))))
       

       ( "with-temp-buffer is safe in an :around form"
	 (insert "abc")
	
	 ;; If our string appears right before point, then the insert succeeded
	 ;; Note that the result of the probe is not important; we're looking
	 ;; at what the probe did to the current buffer
	 :test
	 (eq (point)
	   (save-excursion
	     (and (re-search-backward "abc" nil t)
	       (match-end 0))))
	 :around (with-temp-buffer))

       ))

  (setf 
    (get 'demo3 'rtest-suite) 
    '("tests of setenv/getenv"
       (
	 (setenv "rtest" "foo")
	 :test
	 (equal "foo" (getenv "rtest")))
       
       (
	 (setenv "rtest" nil)
	 :test
	 (null (getenv "rtest")))
       
       (
	 (getenv "USER")
	 :test
	 (equal RESULT user-login-name))
       
       )))


(provide 'rtest-compat)

;;; rtest-compat.el ends here