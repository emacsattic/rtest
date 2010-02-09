;;
;; subdirs.el
;; Generated on Tue 25 Apr, 2000  4:25 PM by Tom Breton
;; By write-subdirs-el, written by Tehom (Tom Breton)
;;

(normal-top-level-add-to-load-path
  '("ancillary"))

;;;### (autoloads (pattern-ecase pattern-case pattern-bind pattern-setq
;;;;;;  pattern-lambda) "pattern-plus" "backup/pattern-plus.el" (14542
;;;;;;  37819))
;;; Generated autoloads from backup/pattern-plus.el

(autoload (quote pattern-lambda) "pattern-plus" "\
Call BODY in a lambda expression with a single arg bound to PATTERN" nil (quote macro))

(autoload (quote pattern-setq) "pattern-plus" "\
" nil (quote macro))

(autoload (quote pattern-bind) "pattern-plus" "\
" nil (quote macro))

(autoload (quote pattern-case) "pattern-plus" "\
Like `case' but with patterns.

Each case is of the form (pattern guard . body)

For each case in turn:

If pattern matches OBJECT, its free variables are bound to the
respective parts of object and guard is evalled.

If guard evals to non-nil, body is evalled and no more cases are
considered." nil (quote macro))

(autoload (quote pattern-ecase) "pattern-plus" "\
Like `ecase' but with patterns.  If nothing matches it's an error.

Each case is of the form (pattern guard . body)

For each case in turn:

If pattern matches OBJECT, its free variables are bound to the
respective parts of object and guard is evalled.

If guard evals to non-nil, body is evalled and no more cases are
considered." nil (quote macro))

;;;***

;;;### (autoloads (rtest-assert rtest-case) "rtest-plus" "backup/rtest-plus.el"
;;;;;;  (14542 37815))
;;; Generated autoloads from backup/rtest-plus.el

(autoload (quote rtest-case) "rtest-plus" "\
Like `case', but uses rtest grader clauses." nil (quote macro))

(autoload (quote rtest-assert) "rtest-plus" "\
Like assert, but FORM must be an rtest.
This is never continuable and ignores further arguments." nil (quote macro))

;;;***

;;;### (autoloads (comint-redirect-results-list-from-process comint-redirect-results-list
;;;;;;  comint-redirect-send-command-to-process comint-redirect-send-command
;;;;;;  comint-redirect-send-input) "comint-redirect" "ancillary/comint-redirect.el"
;;;;;;  (14474 5135))
;;; Generated autoloads from ancillary/comint-redirect.el

(autoload (quote comint-redirect-send-input) "comint-redirect" "\
Send current input to process in current buffer, with output to OUTPUT-BUFFER.
With prefix arg, echo output in process buffer." t nil)

(autoload (quote comint-redirect-send-command) "comint-redirect" "\
Send COMMAND to process in current buffer, with output to OUTPUT-BUFFER.
With prefix arg, echo output in process buffer.

If NO-DISPLAY is non-nil, do not show the output buffer." t nil)

(autoload (quote comint-redirect-send-command-to-process) "comint-redirect" "\
Send COMMAND to PROCESS, with output to OUTPUT-BUFFER.
With prefix arg, echo output in process buffer.

If NO-DISPLAY is non-nil, do not show the output buffer." t nil)

(autoload (quote comint-redirect-results-list) "comint-redirect" "\
Send COMMAND to current process. 
Return a list of expressions in the output which match REGEXP.
REGEXP-GROUP is the regular expression group in REGEXP to use." t nil)

(autoload (quote comint-redirect-results-list-from-process) "comint-redirect" "\
Send COMMAND to PROCESS. 
Return a list of expressions in the output which match REGEXP.
REGEXP-GROUP is the regular expression group in REGEXP to use." t nil)

;;;***

;;;### (autoloads (safe-equal) "safe-equal" "ancillary/safe-equal.el"
;;;;;;  (14536 30910))
;;; Generated autoloads from ancillary/safe-equal.el

(autoload (quote safe-equal) "safe-equal" "\
" nil nil)

;;;***

;;;### (autoloads (tehom-inflisp-eval-multiple-value-list tehom-inflisp-eval
;;;;;;  tehom-inflisp-eval-void) "tehom-inflisp" "ancillary/tehom-inflisp.el"
;;;;;;  (14539 7413))
;;; Generated autoloads from ancillary/tehom-inflisp.el

(autoload (quote tehom-inflisp-eval-void) "tehom-inflisp" "\
Evaluate `FORM' in the inferior lisp, always returning nil." nil nil)

(autoload (quote tehom-inflisp-eval) "tehom-inflisp" "\
Evaluate `FORM' in the inferior lisp, returning a single value." nil nil)

(autoload (quote tehom-inflisp-eval-multiple-value-list) "tehom-inflisp" "\
Evaluate `FORM' in the inferior lisp, returning multiple values.

Conceptually, this is like:
  (multiple-value-list
    (tehom-inflisp-eval FORM... ))
which wouldn't actually work." nil nil)

;;;***

;;;### (autoloads (pattern-ecase pattern-case pattern-bind pattern-setq
;;;;;;  pattern-lambda) "pattern-plus" "pattern-plus.el" (14596 37550))
;;; Generated autoloads from pattern-plus.el

(autoload (quote pattern-lambda) "pattern-plus" "\
Call BODY in a lambda expression with one arg bound to PATTERN." nil (quote macro))

(autoload (quote pattern-setq) "pattern-plus" "\
A pattern-using setq" nil (quote macro))

(autoload (quote pattern-bind) "pattern-plus" "\
A souped-up destructuring-bind " nil (quote macro))

(autoload (quote pattern-case) "pattern-plus" "\
Like `case' but with patterns.

Each case is of the form (pattern guard . body)

For each case in turn:

If pattern matches OBJECT, its free variables are bound to the
respective parts of object and guard is evalled.

If guard evals to non-nil, body is evalled and no more cases are
considered." nil (quote macro))

(autoload (quote pattern-ecase) "pattern-plus" "\
Like `ecase' but with patterns.  If nothing matches it's an error.

Each case is of the form (pattern guard . body)

For each case in turn:

If pattern matches OBJECT, its free variables are bound to the
respective parts of object and guard is evalled.

If guard evals to non-nil, body is evalled and no more cases are
considered." nil (quote macro))

;;;***

;;;### (autoloads (rtest-inferior-arrange-suites rtest-inferior-add-suites
;;;;;;  rtest-inferior-run-all rtest-inferior-run-suites rtest-inferior-run-raw-suites
;;;;;;  rtest-inferior-next-sexp) "rtest-cl-entry" "rtest-cl-entry.el"
;;;;;;  (14597 56590))
;;; Generated autoloads from rtest-cl-entry.el

(autoload (quote rtest-inferior-next-sexp) "rtest-cl-entry" "\
" t nil)

(autoload (quote rtest-inferior-run-raw-suites) "rtest-cl-entry" "\
Run one or more test suites, from all possible suites." t nil)

(autoload (quote rtest-inferior-run-suites) "rtest-cl-entry" "\
Run some of the active suites.
If there are none, make some suites active and run them." t nil)

(autoload (quote rtest-inferior-run-all) "rtest-cl-entry" "\
Run all the active suites" t nil)

(autoload (quote rtest-inferior-add-suites) "rtest-cl-entry" "\
Add suites to the list of active suites" t nil)

(autoload (quote rtest-inferior-arrange-suites) "rtest-cl-entry" "\
Arrange the list of active suites" t nil)

;;;***

;;;### (autoloads (rtest-load-only-suites rtest-hide-tests rtest-defun
;;;;;;  rtest-insert-suite) "rtest-edit" "rtest-edit.el" (14597 64201))
;;; Generated autoloads from rtest-edit.el

(autoload (quote rtest-insert-suite) "rtest-edit" "\
Insert a template for a new test suite.
Prompts for the NAME of the variable to use, and for a short DOCSTRING
describing the purpose of the test suite.
  If rtest-expert (which see) is non-nil, some helpful comments are also
inserted." t nil)

(autoload (quote rtest-defun) "rtest-edit" "\
Run tests on the function or suite under point.

If prefix ARG is non-nil, eval it first.

Does nothing if the buffer is not in a known lisp mode." t nil)

(autoload (quote rtest-hide-tests) "rtest-edit" "\
Hide all the (eval-when-compile ... ) forms.

hide-show mode must be active for this to work, see `hs-minor-mode'." t nil)

(autoload (quote rtest-load-only-suites) "rtest-edit" "\
Eval the blocks protected by eval-when-compile." t nil)

;;;***

;;;### (autoloads (rtest-arrange-suites rtest-add-suites rtest-run-all
;;;;;;  rtest-run-suites rtest-run-raw-suites rtest-next-sexp) "rtest-entry"
;;;;;;  "rtest-entry.el" (14596 44194))
;;; Generated autoloads from rtest-entry.el

(autoload (quote rtest-next-sexp) "rtest-entry" "\
" t nil)

(autoload (quote rtest-run-raw-suites) "rtest-entry" "\
Run one or more test suites, from all possible suites." t nil)

(autoload (quote rtest-run-suites) "rtest-entry" "\
Run some of the active suites.
If there are none, make some suites active and run them." t nil)

(autoload (quote rtest-run-all) "rtest-entry" "\
Run all the active suites" t nil)

(autoload (quote rtest-add-suites) "rtest-entry" "\
Add suites to the list of active suites" t nil)

(autoload (quote rtest-arrange-suites) "rtest-entry" "\
Arrange the list of active suites" t nil)

;;;***

;;;### (autoloads (rtest-assert rtest-case) "rtest-plus" "rtest-plus.el"
;;;;;;  (14596 41423))
;;; Generated autoloads from rtest-plus.el

(autoload (quote rtest-case) "rtest-plus" "\
Like `case', but uses rtest grader clauses." nil (quote macro))

(autoload (quote rtest-assert) "rtest-plus" "\
Like assert, but FORM must be an rtest.
This is never continuable and ignores further arguments." nil (quote macro))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
