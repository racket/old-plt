; Shriram Krishnamurthi (shriram@cs.rice.edu)
; Fri Jan 10 22:01:02 CST 1997

; Generously plagiarized from sml-proc.el and its support files, for
; SML, written by Lars Bo Nielsen and Matthew J. Morley.

; The domicile of this file will probably be PLTHOME/mzrice/mzrice.el.

; These are routines that provide a rudimentary Emacs interface to
; MzRice.  Note that they don't work in the REP, due to the way MzRice
; assigns source locations therein (each expression is assumed to
; start at <1,1,0>, so how to tell which expression the error came
; from? -- comint is also downright stupid at finding prompts).
; 
; Similarly, the `send-' procedures of CMU Scheme mode and of comint
; will not be of much use.  The best way of using this code is to
; `load' the file(s) in question into the REP, then use
; `mzrice-next-error' to be taken to the error location.

; I don't expect there are many bugs, but shortcomings are manifold:
; 
; - Has only rudimentary ability to tell if the buffer has been
;   modified.  (Modifications render the source locations invalid.)
;
; - Doesn't provide a simple `load' interface.  (Could enhance this
;   further with temporary files -- platform dependence issues?)
;
; - Could provide menus and other niceties.  Not even a key-binding.
;
; Comments welcome.

(defvar mzrice-stdin-filename "stdin")

(defvar mzrice-error-regexp
  "^\\(.*\\):\\([0-9]+\\)\.\\([0-9]+\\)-\\([0-9]+\\)\.\\([0-9]+\\) ")

(defun mzrice-error-parser (pt)
  (save-excursion
    (goto-char pt)
    (re-search-forward mzrice-error-regexp)
    (list (let ((filename (buffer-substring
			   (match-beginning 1)
			   (match-end 1))))
	    (if (string-equal filename
			      mzrice-stdin-filename)
		nil
	      filename))		; filename
	  (string-to-int
	   (buffer-substring
	    (match-beginning 2)
	    (match-end 2)))		; start line
	  (1-
	   (string-to-int
	    (buffer-substring
	     (match-beginning 3)
	     (match-end 3))))		; start column
	  (string-to-int
	   (buffer-substring
	    (match-beginning 4)
	    (match-end 4)))		; finish line
	  (1-
	   (string-to-int
	    (buffer-substring
	     (match-beginning 5)
	     (match-end 5)))))))	; finish column

(defvar mzrice-error-cursor 0)

(defun mzrice-next-error ()
  (interactive)
  (let ((case-fold-search nil))
    (if (not scheme-buffer)
	(error "No current process."))
    (pop-to-buffer scheme-buffer)
    (let ((saved-location (point)))
      (if (> mzrice-error-cursor (point-max)) ; The buffer has been edited;
	  (setq mzrice-error-cursor 0))	      ; assume it's all gone.
      (goto-char mzrice-error-cursor)
      (if (re-search-forward mzrice-error-regexp (point-max) t)
	  (let* ((parse (mzrice-error-parser (match-beginning 0)))
		 (file (nth 0 parse))
		 (start-line (nth 1 parse))
		 (start-column (nth 2 parse))
		 (finish-line (nth 3 parse))
		 (finish-column (nth 4 parse)))
	    (setq mzrice-error-cursor (point))

	    (set-window-start (get-buffer-window scheme-buffer)
			      (save-excursion (beginning-of-line) (point)))
	    (cond
	     ((not file)
	      (error "Can't handle errors in REP."))
	     ((file-readable-p file)
	      (switch-to-buffer-other-window (find-file-noselect file))
	      (goto-line finish-line)
	      (forward-char finish-column)
	      (push-mark)
	      (goto-line start-line)
	      (forward-char start-column))
	     (t
	      (error (concat "Can't read file `" file "'.")))))
	(message "No error message(s) found.")
	(goto-char saved-location)))))
