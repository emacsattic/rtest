;;; rtest-report.el --- Reporting for rtest

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

(require 'cl)
(require 'rtest-visible)
(require 'rtest-format)


;;;;;;;;;;;
;;Configuration.  


(defcustom rtest-report-viewer 
  #'rtest-view-report-by-psgml 
  "How to view the rtest report" 
  :group 'rtest
  :type
  '(choice
    (function-item 
      rtest-view-report-by-psgml 
      :tag "Via psgml")
    (function-item 
      browse-url-of-buffer 
      :tag "Via the default browser")
    (function-item 
      pop-to-buffer 
      :tag "Via just looking at it")
    (restricted-sexp :match-alternatives (functionp)
      :tag "Via some other method, a function taking 1 arg, BUFFER")))


(defvar rtest-error-buffer "*Test Error*")


;;;;;;;;;;;;;;;;;;
;;Present a report

(defun rtest-write-report (html-string-tree &optional buf-name)
  "Write a report into an appropriate buffer."

  (setq buf-name (or buf-name rtest-error-buffer))
  (let
    ((buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (erase-buffer)
      (apply #'insert (flatten html-string-tree))
      (goto-char (point-min)))

    buf))

(defun rtest-view-report (html-string-tree &optional buf-name)
  ""
  
  (let
    ((buf
       (rtest-write-report html-string-tree buf-name)))
    
    (funcall rtest-report-viewer buf)))


(defun rtest-report-html 
  (test-count fail-count result-string-tree)
  "Report the results of running suites."

  (if 
    (zerop fail-count)
    (message 
      (cond 
	((= 1 test-count)
	  "The single test ran successfully")
	((= 2 test-count)
	  "Both tests ran successfully")
	(t "All %d tests ran successfully"))
      test-count)
    (progn
      (message "%d failure%s detected"
	fail-count
	(if (= 1 fail-count) "" "s"))

      
      (rtest-view-report result-string-tree))))


;;;;;;;;;;;;;
;;Via psgml

;;;;;;;;;;;;;;;
;;Configuration
(defcustom rtest-psgml-view-hooks 
  (list 
    #'sgml-hide-tags
    #'tehom-psgml-indent-all
    #'(lambda ()
	(goto-char (point-min))))
  "Hooks to run after writing/before viewing a report via psgml"
  :type  'hook
  :group 'rtest)


;;;;;;;;;;;;;;;;;;;;
;;Internal Variables

(defvar rtest-html-mode nil "" )


(defun tehom-psgml-indent-all ()
  ""
  
  (interactive)
  (goto-char 1)
  (while (not (eobp))
    (sgml-indent-line)
    (forward-line)))


(defun rtest-view-report-by-psgml (buf)
  "View a report via psgml."

  (pop-to-buffer buf)

  (make-variable-buffer-local 'rtest-html-mode)
  (unless rtest-html-mode
    (html-mode)
    (setq rtest-html-mode t))

  (run-hooks 'rtest-psgml-view-hooks))







(provide 'rtest-report)

;;; rtest-report.el ends here