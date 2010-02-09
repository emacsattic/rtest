;;; arrange.el --- Rearrange and filter lists

;; Copyright (C) 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: tools, extensions, lisp

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

;; This is for manually editing lists.  NB, not for editing individual
;; elements, for manually rearranging and removing elements from a
;; list as a whole.  

;; kill-line (^K/k) and yank (^Y/y) work as you expect, but never
;; discard elements from the kill-list.  You can always yank them
;; back, no matter how far down they are.

;; flip (f) is a very useful command, not to be overlooked.  It swaps
;; active entries for killed entries.  Say you have a list you want to
;; mostly discard.  You can kill the few elements you do want and flip
;; it, and there's your short list.

;; arrange-done (x) exits and saves your changes.

;; arrange-quit (q) exits and throws an error, which presumably will
;; stop calling applications from thinking the user meant to continue.

;; 

;;; Non-features

;; arrange-done could make parameterized finished-ness tests, eg it
;; could prompt with (y-or-n-p "Are you all done?")

;; A numeric arg could make arrange-yank-line yank from elsewhere
;; in the list, with 1 being only the default.

;; ...or in a more visual mode, a command to move an element to the
;; top, a combination of kill, beginning-of-buffer, and yank.  Thus
;; one would flip to the kill-list, move the intended element(s) to
;; the top, flip back, and yank.

;; It could save the position of point between flips, so there'd be
;; two positions, one for each flip.

;; The entry points should be more parameterizable.  We could take
;; {nil, `other-window', `other-frame'} keys.  We could take
;; pre-/post-process functions, the way arrange-syms* uses
;; `symbol-name' and `intern-soft'.

;; Other entry points could pre-fill the kill list as well as the
;; buffer, so that some entries would default to active, others to
;; killed.

;; The `undo' key M-_ could be bound to undo both the buffer and the
;; kill-list.  That is far too much work for what can always be done
;; just by yanking in the same place.

;;; Code:

(require 'cl) 
(require 'electric) 


;;;;;;;
;;Configuration

(defvar arrange-expert nil 
  "" )

;;;;;;;;;;;
;;Constants
(defconst arrange-buffer-name "*Arrange*" 
  "Name of the buffer arrange uses to interact with the user." )

(defconst arrange-regexp "^.+$" 
  "Regular expression to match a line.  No reason to change this." )

;;

(defvar arrange-kill-list '() 
  "Pseudo kill-ring for lines.  Unlike a ring, it never discards old
elements." ) 

(defvar arrange-mode nil "non-nil if the buffer is in arrange mode" )

(defvar arrange-mode-map 
  
  (let
    ((keymap (make-sparse-keymap)))
    
    (define-key keymap "\C-k" #'arrange-kill-line)
    (define-key keymap "k"    #'arrange-kill-line)
    (define-key keymap "\C-y" #'arrange-yank-line)
    (define-key keymap "y"    #'arrange-yank-line)
    (define-key keymap "\C-m" #'arrange-done)
    (define-key keymap "x"    #'arrange-done)
    (define-key keymap "q"    #'arrange-quit)
    (define-key keymap "f"    #'arrange-flip)

    keymap)
  
   "Keymap for Arrange mode" )

;;;; Utility macros.

;; Note: These work nicely with electric.el. You're encouraged to
;; borrow them.


(defmacro with-working-buffer (buf &rest body)
  "Do work involving user interaction in BUF.

Restore original buffer when done."
  
  `(let
     ((original-buffer (current-buffer))
       (the-buf ,buf))
    
     (unwind-protect
       (progn
	 (switch-to-buffer the-buf)

	 ,@body)
      
       (switch-to-buffer original-buffer))))

(defmacro with-working-buffer-other-window (buf &rest body)
  "Do work involving user interaction in BUF in other window.

Restore original buffer when done."
  
  `(let
     ( (original-buffer (current-buffer))
       (the-buf ,buf))
    
     (unwind-protect
       (progn
	 (switch-to-buffer-other-window the-buf)

	 ,@body)

       (delete-windows-on the-buf)
       (switch-to-buffer original-buffer))))

(defmacro with-working-buffer-other-frame (buf &rest body)
  "Do work involving user interaction in BUF in other frame.

Restore original buffer when done."  

  `(let
     ((original-buffer (current-buffer))
       (the-buf ,buf))
    
     (unwind-protect
       (progn
	 (switch-to-buffer-other-frame the-buf)

	 ,@body)

       ;;This is a bit guessy, and if the user has selected the buffer
       ;;in a different frame it could delete the wrong frame.
       (delete-frame (window-frame (get-buffer-window the-buf)))
       (switch-to-buffer original-buffer))))


;;;;;;;;

(defun arrange-mode ()
  "Arrange mode.

This is for manually editing lists.  NB, not for editing individual
elements, for manually rearranging and removing elements from a list
as a whole.
\\<arrange-mode-map>
\\[arrange-kill-line] and \\[arrange-yank-line] work like kill-line and
yank, but never discard elements from the kill-list.  You can always
yank them back, no matter how far down they are.

\\[arrange-flip] is a very useful command, not to be overlooked.  It
swaps active entries for killed entries.  Say you have a list you want
to mostly discard.  You can kill the few elements you do want and flip
it, and there's your short list.

\\[arrange-done] exits and saves your changes.

\\[arrange-quit] exits and throws an error, which presumably will
stop calling applications from thinking the user meant to continue."
  
  (interactive)

  (when (not arrange-mode)
    (kill-all-local-variables)
    (make-variable-buffer-local 'arrange-mode)
    (make-variable-buffer-local 'arrange-kill-list)

    (setq arrange-mode t)
    (setq major-mode 'arrange-mode)
    (setq mode-name "Arrange")
    (put 'arrange-mode 'mode-class 'special)

    (setq buffer-read-only t)
    (buffer-disable-undo)

    ;;Inherit from whatever keymap is in use.
    (let
      ((current-map (current-local-map)))
      (when
	(not (eq arrange-mode-map current-map))
	(set-keymap-parent arrange-mode-map current-map)))

    (use-local-map arrange-mode-map)))



(defun arrange-collect-contents () 
  "Collect the contents of the buffer as a list of strings."
  (progn
    (goto-char 1)
    (loop while
      (search-forward-regexp arrange-regexp nil t)
      collect
      (match-string 0))))

(defun arrange-set-contents
  (args)
  "Set the contents of the buffer.
ARGS must be a list of strings"

  (let
    ((inhibit-read-only t))
    (erase-buffer)
    (goto-char 1)
    ;;Write the symbol-names, one to a line.
    (dolist
      (str args)
      (insert str "\n"))))


(defun arrange-build-prompt (prompt)
  ""
  (concat
    (or prompt "Arrange the list.  ")
    (unless arrange-expert
      "RET when done, `q' to quit.")))


(defun arrange-do-work
  (args &optional prompt)
  "Let the user rearrange a list of strings."
  (progn
    (arrange-mode)
    (arrange-set-contents args)
    (setq arrange-kill-list 'nil)
    (let
      ;; All electric's code throws nil whereas arrange-done throws t,
      ;; so we know whether the edit was aborted.
      ( 
	(success
	 (catch 'arrange-tag
	   (Electric-command-loop 
	     'arrange-tag 
	     (arrange-build-prompt prompt)
	     t))))
      (if
	(not success)
	(error "Aborted")))
    (arrange-collect-contents)))



;;;;;;;;;;
;;Commands

;; This is safe against killing beyond the last line.
(defun arrange-kill-line (&optional arg)
  "Kill the current line.

If ARG is a number, kill that many lines.
Stops on the first bad entry."
  
  (interactive "p")
  (let
    ((inhibit-read-only t))
    
    (beginning-of-line)
    (dotimes (i (or arg 1))
      (if 
	(looking-at arrange-regexp)
	(let
	  ((str (match-string 0)))
      
	  (delete-region (match-beginning 0) (1+ (match-end 0)))
	  (push str arrange-kill-list))
	(return)))))


;; This is safe against yanking beyond the end of the kill-list.
(defun arrange-yank-line (&optional arg)
  "Yank the top entry from the kill ring

ARG is unused."
  
  (interactive "p")
  (let
    ((inhibit-read-only t))
    
    (let
      ((str (pop arrange-kill-list)))
      (when str
	(beginning-of-line)
	(insert str "\n")))))


(defun arrange-flip ()
  "Swap active entries for killed entries"
  
  (interactive)
  (let
    ((inhibit-read-only t)
      (new-kill-list
	(arrange-collect-contents)))

    (arrange-set-contents arrange-kill-list)
    (setq arrange-kill-list new-kill-list)))

(defun arrange-done ()
  "Exit Arrange successfully"
  
  (interactive)
  (throw 'arrange-tag t))

(defun arrange-quit ()
  "Quit arrange, throwing an error"
  
  (interactive)
  (throw 'arrange-tag nil))


;;;;;;;;;;;;;
;;Entry points
;;

(defun arrange-strings (args &optional prompt)
  "Arrange a list of strings"

  (with-working-buffer
    (get-buffer-create arrange-buffer-name)
  
    (arrange-do-work args prompt)))

(defun arrange-strings-other-window (args &optional prompt)
  "Arrange a list of strings in the other window"

  (with-working-buffer-other-window
    (get-buffer-create arrange-buffer-name)
  
    (arrange-do-work args prompt)))

(defun arrange-strings-other-frame (args &optional prompt)
  "Arrange a list of strings in the other frame"

  (with-working-buffer-other-frame
    (get-buffer-create arrange-buffer-name)
  
    (arrange-do-work args prompt)))

;; Alternate entry points dealing with symbols.

(defun arrange-syms (syms &optional prompt)
  "Arrange a list of symbols"

  (mapcar
    #'intern-soft
    (arrange-strings 
      (mapcar 
	#'symbol-name
	syms))))

(defun arrange-syms-other-window (syms &optional prompt)
  "Arrange a list of symbols in the other window."

  (mapcar
    #'intern-soft
    (arrange-strings-other-window
      (mapcar 
	#'symbol-name
	syms))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests


(eval-when-compile

  ;; We test by defaliasing Electric-command-loop to simply issue a body
  ;; of commands, leaving manual interaction out of it.
  (defmacro test-arrange-do-work
    (&rest body)
    ""
    `(with-temp-buffer
       (let-defalias Electric-command-loop
	 (lambda
	   (&rest dummy)
	   ,@body)
	 (arrange-do-work
	   '("alpha" "beta" "gamma" "delta")))))

  ;; This code will not appear in the compiled (.elc) file
  (setf
    (get 'arrange 'rtest-suite)
    '("arrange-do-work"

       ;;arrange-do-work
       ( "With no user commands issued, the list is unchanged."
	 (test-arrange-do-work t)
	 '("alpha" "beta" "gamma" "delta"))

       ;arrange-done
       ( "arrange-done gives us the list as it stands."
	 (test-arrange-do-work 
	   (arrange-done))
	 '("alpha" "beta" "gamma" "delta"))

       ;;arrange-quit
       ( "arrange-quit throws an error beyond arrange-do-work."
	 (test-arrange-do-work 
	   (arrange-quit))
	 :predicate rtest-error-p)

       ;;arrange-kill-line
       ( "arrange-kill-line kills a line"

	 (test-arrange-do-work
	   (beginning-of-buffer)
	   (arrange-kill-line)
	   (arrange-done))
	 
	 '("beta" "gamma" "delta"))
       
       ( "arrange-kill-line kills the line it's on"

	 (test-arrange-do-work
	   (beginning-of-buffer)
	   (next-line 2)
	   (arrange-kill-line)
	   (arrange-done))
	 '("alpha" "beta" "delta"))

       ( "With an arg, arrange-kill-line kills that many lines"

	 (test-arrange-do-work
	   (beginning-of-buffer)
	   (arrange-kill-line 2)
	   (arrange-done))
	 
	 '("gamma" "delta"))

       ;;Doesn't try to kill beyond the end of the buffer.  How to
       ;;test that?

       ;;arrange-yank-line
       ( "arrange-yank-line yank a line from the kill-list"

	 (test-arrange-do-work
	   (beginning-of-buffer)
	   (setq arrange-kill-list '("epsilon"))
	   (arrange-yank-line)
	   (arrange-done))
	 '("epsilon" "alpha" "beta" "gamma" "delta"))

       ( "arrange-yank-line does nothing if the kill-list is empty"

	 (test-arrange-do-work
	   (beginning-of-buffer)
	   (setq arrange-kill-list '())
	   (arrange-yank-line)
	   (arrange-done))
	 '("alpha" "beta" "gamma" "delta"))

       ;;arrange-flip
       ( "arrange-flip swaps kill-list for visible entries"

	 (test-arrange-do-work
	   (beginning-of-buffer)
	   (setq arrange-kill-list '("epsilon"))
	   (arrange-flip)
	   (arrange-done))
	 '("epsilon"))

       ( "Two applications of arrange-flip leave the buffer as it was."

	 (test-arrange-do-work
	   (beginning-of-buffer)
	   (arrange-flip)
	   (arrange-flip)
	   (arrange-done))
	 '("alpha" "beta" "gamma" "delta"))
       

       )))


;;Manual tests.  Testing this complicated interaction automatically is
;;difficult to do completely, but C-u C-x C-e underneath each for will
;;demonstarte that it works.

'
(arrange-strings '("alpha" "beta" "gamma" "delta"))

'
(arrange-strings-other-window '("alpha" "beta" "gamma" "delta"))

'
(arrange-strings-other-frame '("alpha" "beta" "gamma" "delta"))


'
(arrange-syms-other-window '(alpha beta gamma delta))


;;;;;;;;

(provide 'arrange)

;;; arrange.el ends here