; Shriram Krishnamurthi (shriram@cs.rice.edu)
; Fri Jan 10 22:01:02 CST 1997
; Sun Jan 12 11:58:59 CST 1997

; Generously plagiarized from sml-proc.el and its support files, for
; SML, written by Lars Bo Nielsen and Matthew J. Morley.

; The domicile of this file will probably be
;    plt/collects/drscheme-jr/drscheme-jr.el
; To load it, use this in your .emacs:
; (load (concat (or (getenv "PLTHOME") "/usr/local/lib/plt")
; 	        "/"
; 	        "plt/collects/drscheme-jr/drscheme-jr"))

; These are routines that provide a rudimentary Emacs interface to
; DrScheme Jr.  
; 
; Note that the `send-' procedures of CMU Scheme mode and of comint
; will not be of much use.  The best way of using this code is to
; `load' the file(s) in question into the REP, then use
; `drs-next-error' to be taken to the error location.

; I don't expect there are many bugs, but shortcomings are manifold:
; 
; - Has only rudimentary ability to tell if the REP buffer has been
;   modified.
;
; - Doesn't provide a simple `load' interface.  (Could enhance this
;   further with temporary files -- platform dependence issues?)
;
; - Could provide menus and other niceties.  Not even a key-binding
;   for now.
; 
; Comments welcome.

(defvar drs-stdin-filename "stdin")

(defvar drs-error-regexp
  "^\\(.+\\)\\[\\([0-9]+\\)-\\([0-9]+\\)\\]:[0-9]+\.[0-9]+-[0-9]+\.[0-9]+$")

(defun drs-error-parser (pt)
  (save-excursion
    (goto-char pt)
    (re-search-forward drs-error-regexp)
    (list (let ((filename (buffer-substring
			   (match-beginning 1)
			   (match-end 1))))
	    (if (string-equal filename
			      drs-stdin-filename)
		nil
	      filename))		; filename
	  (1+
	   (string-to-int
	    (buffer-substring
	     (match-beginning 2)
	     (match-end 2))))		; start offset (Emacs indexes from 1)
	  (1+
	   (string-to-int
	    (buffer-substring
	     (match-beginning 3)
	     (match-end 3)))))))	; finish offset (Emacs indexes from 1)

(defvar drs-error-cursor 0)

(defvar drs-previous-buffer 'dummy)

(defun drs-next-error ()
  (interactive)
  (let ((case-fold-search nil))
    (if (not scheme-buffer)
	(error "No current process."))
    (pop-to-buffer scheme-buffer)
    (let ((saved-location (point)))
      (if (not (eq (current-buffer) drs-previous-buffer))
	  (progn
	    (setq drs-error-cursor 0)
	    (setq drs-previous-buffer (current-buffer))))
      (if (> drs-error-cursor (point-max)) ; The buffer has been edited;
	  (setq drs-error-cursor 0))	      ; assume it's all gone.
      (goto-char drs-error-cursor)
      (if (re-search-forward drs-error-regexp (point-max) t)
	  (let* ((parse (drs-error-parser (match-beginning 0)))
		 (file (nth 0 parse))
		 (start-offset (nth 1 parse))
		 (finish-offset (nth 2 parse)))
	    (setq drs-error-cursor (point))

	    (set-window-start (get-buffer-window scheme-buffer)
			      (save-excursion (beginning-of-line) (point)))
	    (cond
	     ((not file)
	      (switch-to-buffer-other-window scheme-buffer))
	     ((file-readable-p file)
	      (switch-to-buffer-other-window (find-file-noselect file)))
	     (t
	      (error (concat "Can't read file `" file "'."))))
	    (goto-char finish-offset)
	    (push-mark)
	    (goto-char start-offset))
	(message "No error message(s) found.")
	(goto-char saved-location)))))
