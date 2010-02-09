;;; comint-redirect.el --- Redirect comint output to multiple buffers

;; Author:  Peter Breton
;; Created: Sun Sep 19 1999
;; Version: $Id: comint-redirect.el,v 1.12 1999/12/17 09:35:35 pbreton Exp $
;; Keywords: 
;; Time-stamp: <1999-12-17 08:33:31 pbreton>

;;; Commentary:

;;; Change log:
;; $Log: comint-redirect.el,v $
;; Revision 1.12  1999/12/17 09:35:35  pbreton
;; Added comint-redirect-send-input function
;;
;; Revision 1.11  1999/11/16 20:09:43  pbreton
;; More docstring cleanups
;; Always use process filter instead of over-riding comint-preoutput-filter-functions
;; In comint-redirect-results-list, skip past command if we see it in the output.
;;
;; Revision 1.10  1999/11/15 00:16:10  pbreton
;; comint-redirect-verbose is now a defcustom
;; More docstring cleanup
;; Added comint-redirect-insert-matching-regexp variable.
;; Added comint-redirect-perform-sanity-check variable.
;; Added comint-redirect-use-process-filter variable.
;; Added comint-redirect-original-filter-function variable.
;; Added code to set process filter instead of over-riding
;; `comint-preoutput-filter-functions'
;; Renamed comint-redirect-filter to comint-redirect-preoutput-filter
;; Added new comint-redirect-filter function
;; Added code to clobber comint-redirect-finished-regexp in output
;; In comint-redirect-send-command, added sanity check to make sure prompt is
;; set correctly
;; Added autoloads for comint-redirect-results functions
;;
;; Revision 1.9  1999/11/11 14:28:57  pbreton
;; Sets modeline when redirection is in effect
;; Added comint-redirect-results-list and comint-redirect-results-list-from-process functions
;;
;; Revision 1.8  1999/11/10 14:01:08  pbreton
;; Removed `comint-redirect-current-comint-buffer'; unnecessary when comint.el patch is applied
;; Docstring fixes and cleanups
;;
;; Revision 1.7  1999/11/09 21:15:45  pbreton
;; Added comint-redirect-completed variable, which is set when redirection has completed.
;; comint-redirect-{setup,complete} now use this variable
;; `comint-redirect-cleanup' always deletes `comint-redirect-filter' from comint-preoutput-filter-functions
;; `comint-redirect-filter' only prints message if comint-redirect-verbose is set
;; `comint-redirect-send-command' and `comint-redirect-send-command-to-process' have a no-display
;; flag which inhibits display of the output buffer
;;
;; Revision 1.6  1999/11/08 19:43:51  pbreton
;; In comint-redirect-send-command-to-process, make sure process argument
;; is really a process before calling process buffer on it.
;;
;; Revision 1.5  1999/11/08 12:56:28  pbreton
;; Docstring cleanups
;;
;; Revision 1.4  1999/11/06 23:20:05  pbreton
;; Added comint-redirect-remove-redirection function, in case cleanup fails
;;
;; Revision 1.3  1999/11/06 22:49:22  pbreton
;; Incorporates docstring cleanups per RMS suggestions
;; Changed comint-redirect-{register,unregister} function names to comint-redirect-{setup/cleanup}
;; Use comint-redirect-filter-functions
;;
;; Revision 1.2  1999/09/26 01:48:07  pbreton
;; Uses buffer-local variables instead of comint-redirect-alist.
;; Uses comint-redirect-current-comint-buffer to store the comint buffer.
;;
;; Revision 1.1  1999/09/20 15:55:36  pbreton
;; First version
;;
;;

;; This little add-on for comint is intended to make it easy to get
;; output from currently active comint buffers into another buffer,
;; or buffers, and then go back to using the comint shell.
;; 
;; My particular use is SQL interpreters; I want to be able to execute a
;; query using the process associated with a comint-buffer, and save that
;; somewhere else. Because the process might have state (for example, it
;; could be in an uncommitted transaction), just running starting a new
;; process and having it execute the query and then finish, would not
;; work. I'm sure there are other uses as well, although in many cases
;; starting a new process is the simpler, and thus preferable, approach.
;; 
;; The basic implementation is as follows: comint-redirect changes the
;; preoutput filter functions (comint-preoutput-filter-functions) to use
;; its own filter. The filter puts the output into the designated buffer,
;; or buffers, until it sees a regexp that tells it to stop (by default,
;; this is the prompt for the interpreter, comint-prompt-regexp). When it
;; sees the stop regexp, it restores the old filter functions, and runs
;; comint-redirect-hook.
;;
;; Each comint buffer may only use one redirection at a time, but any number
;; of different comint buffers may be simultaneously redirected.
;;
;; NOTE: It is EXTREMELY important that `comint-prompt-regexp' be set to the
;; correct prompt for your interpreter, or that you supply a regexp that says
;; when the redirection is finished. Otherwise, redirection will continue
;; indefinitely. The code now does a sanity check to ensure that it can find
;; a prompt in the comint buffer; however, it is still important to ensure that
;; this prompt is set correctly.
;; 
;;; Code:

(require 'comint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom comint-redirect-verbose nil
  "*If non-nil, print messages each time the redirection filter is invoked.
Also print a message when redirection is completed."
  :group 'comint
  :type 'boolean
)

;; Directly analagous to comint-preoutput-filter-functions
(defvar comint-redirect-filter-functions nil
  "List of functions to call before inserting redirected process output.
Each function gets one argument, a string containing the text received
from the subprocess.  It should return the string to insert, perhaps
the same string that was received, or perhaps a modified or transformed
string.

The functions on the list are called sequentially, and each one is given
the string returned by the previous one.  The string returned by the
last function is the text that is actually inserted in the redirection buffer.")

(make-variable-buffer-local 'comint-redirect-filter-functions)

;; Internal variables

(defvar comint-redirect-output-buffer nil
  "The buffer or list of buffers to put output into.")

(defvar comint-redirect-finished-regexp nil
  "Regular expression that determines when to stop redirection in Comint.
When the redirection filter function is given output that matches this regexp, 
the output is inserted as usual, and redirection is completed.")

(defvar comint-redirect-insert-matching-regexp nil
  "If non-nil, the text that ends a redirection is included in it.
More precisely, the text that matches `comint-redirect-finished-regexp'
and therefore terminates an output redirection is inserted in the
redirection target buffer, along with the preceding output.")

(defvar comint-redirect-echo-input nil
  "Non-nil means echo input in the process buffer even during redirection.")

(defvar comint-redirect-completed nil
  "Non-nil if redirection has completed in the current buffer.")

(defvar comint-redirect-original-mode-line-process nil
  "Original mode line for redirected process.")

(defvar comint-redirect-perform-sanity-check t
  "If non-nil, check that redirection is likely to complete successfully.
More precisely, before starting a redirection, verify that the
regular expression `comint-redirect-finished-regexp' that controls
when to terminate it actually matches some text already in the process
buffer.  The idea is that this regular expression should match a prompt
string, and that there ought to be at least one copy of your prompt string
in the process buffer already.")

(defvar comint-redirect-original-filter-function nil
  "The process filter that was in place when redirection is started.
When redirection is completed, the process filter is restored to
this value.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comint-redirect-setup (output-buffer 
			      comint-buffer
			      finished-regexp
			      &optional echo-input)
  "Set up for output redirection.
This function sets local variables that are used by `comint-redirect-filter'
to perform redirection.

Output from COMINT-BUFFER is redirected to OUTPUT-BUFFER, until something
in the output matches FINISHED-REGEXP. 

If optional argument ECHO-INPUT is non-nil, output is echoed to the
original comint buffer.

This function is called by `comint-redirect-send-command-to-process',
and does not normally need to be invoked by the end user or programmer.
"
  (with-current-buffer comint-buffer

    (make-local-variable 'comint-redirect-original-mode-line-process)
    (setq comint-redirect-original-mode-line-process mode-line-process)

    (make-local-variable 'comint-redirect-output-buffer)
    (setq comint-redirect-output-buffer output-buffer)

    (make-local-variable 'comint-redirect-finished-regexp)
    (setq comint-redirect-finished-regexp finished-regexp)

    (make-local-variable 'comint-redirect-echo-input)
    (setq comint-redirect-echo-input echo-input)

    (make-local-variable 'comint-redirect-completed)
    (setq comint-redirect-completed nil)

    (setq mode-line-process 
	  (if mode-line-process 
	      (list (concat (elt mode-line-process 0) " Redirection"))
	    (list ":%s Redirection")))
    ))

(defun comint-redirect-cleanup ()
  "End a Comint redirection.  See `comint-redirect-send-command'."
  (interactive)
  ;; Restore the process filter
  (set-process-filter (get-buffer-process (current-buffer))
		      comint-redirect-original-filter-function)
  ;; Restore the mode line
  (setq mode-line-process comint-redirect-original-mode-line-process)
  ;; Set the completed flag
  (setq comint-redirect-completed t))

;; Because the cleanup happens as a callback, it's not easy to guarantee
;; that it really occurs.
(defalias 'comint-redirect-remove-redirection 'comint-redirect-cleanup)

(defun comint-redirect-filter (process input-string)
  "Filter function which redirects output from PROCESS to a buffer or buffers.
The variable `comint-redirect-output-buffer' says which buffer(s) to
place output in.

INPUT-STRING is the input from the comint process.

This function runs as a process filter, and does not need to be invoked by the 
end user."
  (and process
       (with-current-buffer (process-buffer process)
	 (comint-redirect-preoutput-filter input-string)
	 ;; If we have to echo output, give it to the original filter function
	 (and comint-redirect-echo-input
	      comint-redirect-original-filter-function
	      (funcall comint-redirect-original-filter-function
		       process input-string)))))


(defun comint-redirect-preoutput-filter (input-string)
  "Comint filter function which redirects comint output to a buffer or buffers.
The variable `comint-redirect-output-buffer' says which buffer(s) to
place output in.

INPUT-STRING is the input from the comint process.

This function does not need to be invoked by the end user."
  (let ((output-buffer-list
	 (if (listp comint-redirect-output-buffer)
	    comint-redirect-output-buffer
	   (list comint-redirect-output-buffer)))
	(filtered-input-string input-string)
	)

    ;; If there are any filter functions, give them a chance to modify the string
    (let ((functions comint-redirect-filter-functions))
      (while (and functions filtered-input-string)
	(setq filtered-input-string 
	      (funcall (car functions) filtered-input-string))
	(setq functions (cdr functions))))


    ;; Clobber `comint-redirect-finished-regexp'
    (or comint-redirect-insert-matching-regexp
	(and (string-match comint-redirect-finished-regexp filtered-input-string)
	     (setq filtered-input-string
		   (replace-match "" nil nil filtered-input-string))))
	
    ;; Send output to all registered buffers
    (save-excursion
      (mapcar
       (function
	(lambda(buf)
	  ;; Set this buffer to the output buffer
	  (set-buffer (get-buffer-create buf))
	  ;; Go to the end of the buffer
	  (goto-char (point-max))
	  ;; Insert the output
	  (insert filtered-input-string)
	  ))
       output-buffer-list))

    ;; Message
    (and comint-redirect-verbose
	 (message "Redirected output to buffer(s) %s" 
		  (mapconcat 'identity output-buffer-list " ")))

    ;; If we see the prompt, tidy up
    ;; We'll look for the prompt in the original string, so nobody can
    ;; clobber it
    (and (string-match comint-redirect-finished-regexp input-string)
	 (progn
	   (and comint-redirect-verbose
		(message "Redirection completed"))
	   (comint-redirect-cleanup)
	   (run-hooks 'comint-redirect-hook)))
    ;; Echo input?
    (if comint-redirect-echo-input
	filtered-input-string
      "")
    ))

;;;###autoload
(defun comint-redirect-send-input (output-buffer echo)
  "Send current input to process in current buffer, with output to OUTPUT-BUFFER.
With prefix arg, echo output in process buffer."
  (interactive "BOutput Buffer: \nP")
  (let ((process (get-buffer-process (current-buffer))))
    (if (not process)
	(error "No process for current buffer")
      ;; Cribbed from comint-send-input
      (let* ((pmark (process-mark process))
	     (intxt (if (>= (point) (marker-position pmark))
			(progn (if comint-eol-on-send (end-of-line))
			       (buffer-substring pmark (point)))
		      (let ((copy (funcall comint-get-old-input)))
			(goto-char pmark)
			(insert copy)
			copy)))
	     (input (if (not (eq comint-input-autoexpand 'input))
			;; Just whatever's already there
			intxt
		      ;; Expand and leave it visible in buffer
		      (comint-replace-by-expanded-history t pmark)
		      (buffer-substring pmark (point)))))
	     (comint-redirect-send-command
	      input output-buffer echo)
	     ;; FIXME: Should indicate that the command is finished
	     )
	)))

;;;###autoload
(defun comint-redirect-send-command (command output-buffer echo &optional no-display)
  "Send COMMAND to process in current buffer, with output to OUTPUT-BUFFER.
With prefix arg, echo output in process buffer.

If NO-DISPLAY is non-nil, do not show the output buffer."
  (interactive "sCommand: \nBOutput Buffer: \nP")
  (let ((process (get-buffer-process (current-buffer))))
    (if process
	(comint-redirect-send-command-to-process
	 command output-buffer (current-buffer) echo no-display)
      (error "No process for current buffer"))
    ))

;;;###autoload
(defun comint-redirect-send-command-to-process 
  (command output-buffer process echo &optional no-display)
  "Send COMMAND to PROCESS, with output to OUTPUT-BUFFER.
With prefix arg, echo output in process buffer.

If NO-DISPLAY is non-nil, do not show the output buffer."
  (interactive "sCommand: \nBOutput Buffer: \nbProcess Buffer: \nP")
  (let* (
	 ;; The process buffer
	 (process-buffer
	  (if (processp process)
	      (process-buffer process)
	    process))
	 (proc (get-buffer-process process-buffer))
	 )
    ;; Change to the process buffer
    (set-buffer process-buffer)

    ;; Make sure there's a prompt in the current process buffer
    (and comint-redirect-perform-sanity-check
	 (save-excursion
	   (goto-char (point-max))
	   (or (re-search-backward 
		comint-prompt-regexp
		nil
		t)
	       (error "No prompt found or `comint-prompt-regexp' not set properly"))))

    ;;;;;;;;;;;;;;;;;;;;;
    ;; Set up for redirection
    ;;;;;;;;;;;;;;;;;;;;;
    (comint-redirect-setup 
     ;; Output Buffer
     output-buffer 
     ;; Comint Buffer
     (current-buffer) 
     ;; Finished Regexp
     comint-prompt-regexp
     ;; Echo input
     echo
     )

    ;;;;;;;;;;;;;;;;;;;;;
    ;; Set the filter
    ;;;;;;;;;;;;;;;;;;;;;
    ;; Save the old filter
    (setq comint-redirect-original-filter-function
	  (process-filter proc))
    (set-process-filter proc 'comint-redirect-filter)

    
    ;;;;;;;;;;;;;;;;;;;;;
    ;; Send the command
    ;;;;;;;;;;;;;;;;;;;;;
    (process-send-string 
     (current-buffer)
     (concat command "\n"))

    ;;;;;;;;;;;;;;;;;;;;;
    ;; Show the output
    ;;;;;;;;;;;;;;;;;;;;;
    (or no-display
	 (display-buffer 
	  (get-buffer-create
	   (if (listp output-buffer)
	       (car output-buffer)
	     output-buffer))))
    ))

;;;###autoload
(defun comint-redirect-results-list (command regexp regexp-group)
  "Send COMMAND to current process. 
Return a list of expressions in the output which match REGEXP.
REGEXP-GROUP is the regular expression group in REGEXP to use."
  (interactive)
  (comint-redirect-results-list-from-process 
   (get-buffer-process (current-buffer))
   command
   regexp
   regexp-group)
  )

;;;###autoload
(defun comint-redirect-results-list-from-process (process command regexp regexp-group)
  "Send COMMAND to PROCESS. 
Return a list of expressions in the output which match REGEXP.
REGEXP-GROUP is the regular expression group in REGEXP to use."
  (interactive)
  (let ((output-buffer " *Comint Redirect Work Buffer*")
	results)
    (save-excursion
      (set-buffer (get-buffer-create output-buffer))
      (erase-buffer)
      (comint-redirect-send-command-to-process
       command
       output-buffer
       process
       nil
       t
       )
      ;; Wait for the process to complete
      (set-buffer (process-buffer process))
      (while (null comint-redirect-completed)
	(accept-process-output nil 1))
      ;; Collect the output
      (set-buffer output-buffer)
      (goto-char (point-min))
      ;; Skip past the command, if it was echoed
      (and (looking-at command)
	   (forward-line))
      (while (re-search-forward regexp nil t) 
	(setq results
	      (cons (buffer-substring-no-properties
		     (match-beginning regexp-group)
		     (match-end regexp-group))
		    results)))
      results
      )))

(provide 'comint-redirect)

;;; comint-redirect.el ends here

;; Local Variables:
;; autocompile: t
;; End:
