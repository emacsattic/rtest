;;; rtest-format.el --- A simple format system to bridge Elisp and CL

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

;;Requirements
(require 'rtest-compat)

(defun rtest-format-list (&rest pieces)
  "Concat a list of objects or strings into a single string."
  (apply #'concat
    (mapcar
      #'rtest-convert-to-string
      pieces)))

(eval-when-compile
  (setf
    (get 'rtest-format-list 'rtest-suite)
    '("rtest-format-list"
       ((rtest-format-list 
	  "Values: "'rtest-newline" == "rtest-newline)
	 "Values: rtest-newline == <BR>\n")

       ((rtest-format-list "Values: num == "145)
	 "Values: num == 145")
       
       )))


(defun flatten (tree)
  "Flattens TREE, i.e., returns a single list containing the
   same atoms as TREE but with any internal lists 'dissolved'. For example,
   (flatten '(a (b c) d))  ==>  (a b c d)

This does not handle dotted lists, see flatten-tree*"

  (typecase tree 
    (null '())
    (cons
      ;;In a true tree, it's never a dotted list.  mapcan is safe to
      ;;use because we only act on what we ourselves return, which is
      ;;never the input structure's own cons cell.
      (mapcan #'flatten tree))
    ;;Listify atoms.  mapcan will nconc them later, so the conses are not
    ;;wasted. 
    (t (list tree))))


(eval-when-compile
  (setf
    (get 'flatten 'rtest-suite)
    '("flatten"
       ((flatten '("abc"))
	 '("abc"))

       ((flatten '("abc" nil))
	 '("abc"))

       ((flatten '(nil))
	 '())
       
       ("Returns a list even for a bare atom"
	 (flatten "abc")
	 '("abc"))

       ( "Doesn't change the input structure"
	 (let* 
	   ((my-tree '((("abc") "d") "e")))
	   (flatten my-tree)
	   my-tree)
	 
	 '((("abc") "d") "e"))
       
       )))

;;A version that handles dotted lists, adapted from Mark Kantrowitz's
;;extensions.lisp, originally from Brad Miller's CL-LIB.  Thanks to
;;both.  I made it flatten nil leaves as well, and made it use
;;typecase for clarity.  It still has a bug in that a bare atom does
;;not become a list but remains bare.  It could just copy the other
;;and use mapcan-dotted-list
'  
(defun flatten-tree* (tree)
  "Flattens TREE, i.e., returns a single list containing the
   same atoms as TREE but with any internal lists 'dissolved'. For example,
   (flatten '(a (b c) d))  ==>  (a b c d)

This works on dotted trees as well.
Bug:  a bare atom does not become a list but remains bare."

  (typecase tree 
    (null '())
    (cons
      (let
	((flat-cdr (flatten-tree* (cdr tree)))
	  (the-car (car tree)))
	(typecase the-car
	  (null flat-cdr)
	  (cons (append (flatten-tree* the-car) flat-cdr))
	  ;;Don't use append here or we'll snag vectors too.
	  (t    (cons the-car flat-cdr)))))
    (t tree)))


(defun rtest-stringtree-p (tree)
  ""
  
  (cond
    ((atom tree) (stringp tree))
    (t (every #'rtest-stringtree-p tree))))

(eval-when-compile
  (setf
    (get 'rtest-stringtree-p 'rtest-suite)
    '("rtest-stringtree-p"
       ((rtest-stringtree-p 1)
	 nil)

       (
	 (rtest-stringtree-p "Ab")
	 t)

       (
	 (rtest-stringtree-p '("a" ("b" "c")))
	 t)

       )))


(eval-when-compile
  (setf
    (get 'rtest-format 'rtest-suite)
    '("rtest-format"
       rtest-format-list
       flatten
       rtest-stringtree-p
       )))

(provide 'rtest-format)

;;; rtest-format.el ends here

