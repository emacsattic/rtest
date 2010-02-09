;;; rtest-knownbuf.el --- Elisp-only, rtest in buffers with known contents.  

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

;; 

;;; Code:

(provide 'rtest-knownbuf)

;;Requirements
(require 'cl)
(require 'rtest-visible)
(require 'rtest-field)

;;Internal variables
(defvar rtest-scratch-buffer nil "" )

;;;;;;;
;;Types

(defstruct rtest-known-buffer
  ""
  buffer
  contents
  filename
  ;;setup-form is a list-form that does any further setup.
  setup-form
  force-setup)


(defun rtest-knownbuf-acquire-buffer (obj)
  "Assure that OBJ contains a buffer.
OBJ must be a rtest-known-buffer."

  (unless (rtest-known-buffer-buffer obj)
    (setf (rtest-known-buffer-buffer obj)
      (generate-new-buffer "*Rtest Known Buffer*"))

    ;;Remember that it's not set up yet.
    (setf (rtest-known-buffer-force-setup obj)
      t)))

(eval-when-compile
  (setf
    (get 'rtest-knownbuf-acquire-buffer 'rtest-suite)
    '("rtest-knownbuf-acquire-buffer"
       (
	 (rtest-knownbuf-acquire-buffer obj)
	 :test (rtest-known-buffer-buffer obj)
	 :let ((obj (make-rtest-known-buffer))))

       )))


(defun rtest-knownbuf-acquire-string (obj)
   "Assure that OBJ contains a string.
OBJ must be a rtest-known-buffer and must have either filename or contents."

  (tehom-with-struct-slots
    (contents filename)
    obj rtest-known-buffer

    (when (not contents)
      (unless filename 
	(error "Neither contents nor filename was given"))
      (let*
	( 
	  (file-in-buf 
	    (find-buffer-visiting filename))
	  (buf 
	    (or 
	      file-in-buf
	      (find-file-noselect filename))))
   
	;;Acquire file contents.
	(with-current-buffer buf
	  (setf (rtest-known-buffer-contents obj) (buffer-string)))

	;;Done using the file
	(unless file-in-buf
	  (kill-buffer buf))))))


(defun rtest-knownbuf-set-contents (obj)
  "Refresh the test buffer contents if needed."

  (tehom-with-struct-slots
    (buffer force-setup contents setup-form)
    obj rtest-known-buffer

    (with-current-buffer buffer
      (when (or buffer-undo-list force-setup)
	(erase-buffer)
	(insert contents)
	(when setup-form 
	  (eval setup-form))
	(setq buffer-undo-list nil)))))
  

(defun rtest-get-known-buffer-by-data (obj)
  "Get a buffer whose contents are according to OBJ.
OBJ must be a rtest-known-buffer."

  (rtest-knownbuf-acquire-string obj)
  (rtest-knownbuf-acquire-buffer obj)
  (rtest-knownbuf-set-contents   obj)

  (rtest-known-buffer-buffer obj))

  


(defun rtest-get-known-buffer-by-string (str)
  "Get a buffer whose contents are STR.
STR must be a string.

This uses a permanent buffer, overwriting each time.  If for some
incredible reason you need parallel test buffers, use
rtest-get-known-buffer-by-data instead."

  (setq rtest-scratch-buffer
    (or rtest-scratch-buffer
      (generate-new-buffer "*Rtest Known Buffer*")))
  
  (with-current-buffer rtest-scratch-buffer
    (erase-buffer)
    (insert str))
  rtest-scratch-buffer)

(defun rtest-knownbuf-get-filled-buffer (obj)
  "Get a buffer with known contents.
OBJ must be a string or a rtest-known-buffer."

  (typecase obj
    (string             
      (rtest-get-known-buffer-by-string obj))
    (rtest-known-buffer 
      (rtest-get-known-buffer-by-data   obj))))


(defun rtest-knownbuf-go-to-position (regexp match-num end-of-match)
  "Go to the position given by REGEXP.

REGEXP is a regular expression or nil
MATCH-NUM is nil or the number of the group inside REGEXP that the
point should be on.

This must be called inside the buffer in question."

  (cond
    (regexp
      (setq match-num (or match-num 0))
      (goto-char (point-min))
      (let
	((success
	   (re-search-forward regexp nil t)))
	(if success
	  (goto-char 
	    (if end-of-match
	      (match-end       match-num)
	      (match-beginning match-num)))
	  (goto-char (point-min)))))
    
    (t
      (goto-char (point-min)))))

(eval-when-compile
  (setf
    (get 'rtest-knownbuf-go-to-position 'rtest-suite)
    '("rtest-knownbuf-go-to-position"
       
       ( "We normally leave the point at the start of the whole pattern"
	 ( with-buffer-containing 
	   (list "abcdef" "cd")
	   (insert "x")
	   (buffer-string))
	 "abxcdef")
       
       ( "We can leave the point at a sub-pattern"
	 ( with-buffer-containing 
	   (list "abcdef" "c\\(d\\)e" 1)
	   (insert "x")
	   (buffer-string))
	 "abcxdef")
       
       ( "We can leave the point at the end of a pattern or sub-pattern"
	 ( with-buffer-containing 
	   (list "abcdef" "cd" 0 t)
	   (insert "x")
	   (buffer-string))
	 "abcdxef")

       ( "A regexp that doesn't match leaves the point at the beginning"
	 ( with-buffer-containing 
	   (list "abcdef" "zzz")
	   (insert "x")
	   (buffer-string))
	 "xabcdef")

       ( "A match-num whose group doesn't exist is an error"
	 ( with-buffer-containing 
	   (list "abcdef" "cde" 1))

	 :predicate rtest-error-p)

       )))




(defun rtest-knownbuf-get-known-buffer-aux 
  (obj &optional regexp match-num end-of-match)
  "Get a fully correct buffer.  Don't call this directly."
  
  (let
    ((buf (rtest-knownbuf-get-filled-buffer obj)))
    (with-current-buffer buf
      (rtest-knownbuf-go-to-position regexp match-num end-of-match))
    
    buf))

(defun rtest-knownbuf-get-known-buffer (contents)
  "Get a fully correct buffer"

  (cond
    ((atom contents)
      (rtest-knownbuf-get-known-buffer-aux contents))
    
    (t
      (apply #'rtest-knownbuf-get-known-buffer-aux contents))))


;;;;;;;;;;;;;
;;Entry point

(defmacro with-buffer-containing (contents &rest body)
  "Execute BODY inside a buffer with known contents.
This restores the original buffer even in case of error.

In rtest, this is useful with `:around'. If the contents should be
empty, consider simply using `with-temp-buffer'."

  `
  (with-current-buffer 
    (rtest-knownbuf-get-known-buffer ,contents)
    ,@body))


(eval-when-compile
  (setf
    (get 'with-buffer-containing 'rtest-suite)
    '("with-buffer-containing"
       
       ( "Restores the original buffer even in case of error."
	 (rtest-one-probe-0
	   ;;Dummy test
	   '(
	      (with-buffer-containing "abc"
		(error "An error"))
	      :test t))

	 :test (equal win (current-window-configuration))
	 :let ((win (current-window-configuration))))

       )))


(eval-when-compile
  (setf
    (get 'rtest-knownbuf 'rtest-suite)
    '("rtest-knownbuf"
       rtest-knownbuf-acquire-buffer
       with-buffer-containing)))


;;; rtest-knownbuf.el ends here
