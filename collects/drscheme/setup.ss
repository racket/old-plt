; If drscheme:use-setup? is #t, the following five variables must also be set
; to valid values (the first 4 to directories and the 5th to a file name):
; 
; drscheme:setup-dir
; drscheme:lab-dir
; drscheme:hw-dir
; drscheme:user-dir
; drscheme:setup-file


(define drscheme:setup-base-dir
  (lambda ()
    (or (directory-exists? drscheme:user-dir)
	(let loop ([whole drscheme:user-dir])
	  (let ([one (file-name-from-path whole)]
		[dir (path-only whole)])
	    (if (string=? one "")
		(loop dir)
		(if (or (null? one)
			(null? dir))
		    #f
		    (if (or (directory-exists? dir)
			    (loop dir))
			(and (mred:get-choice
			      (format "Should I create the directory ~s in ~s?"
				      one
				      dir)
			      "Create" "Cancel")
			     (if (make-directory whole)
				 #t
				 (begin
				   (wx:message-box
				    (format "I could not create ~s in ~s."
					    one
					    dir))
				   #f)))
			#f))))))))

(define drscheme:do-setup
  (lambda (type-str)
    (when (and drscheme:use-setup? (drscheme:setup-base-dir))
      (let ([get-file-list directory-list]
	    [num-str (wx:get-text-from-user 
		      (string-append "Enter " type-str " number:")
		      "Setup")])
	(if 
	 (string? num-str)
	 (let*
	     ([name (string-append type-str num-str)]
	      [source-dir(string-append 
			  (cond [(string=? type-str "lab") drscheme:lab-dir]
				[(string=? type-str "hw") drscheme:hw-dir]
				[else drscheme:setup-dir])
			  name)]
	      [dest-dir (string-append drscheme:user-dir name)]
	      [source-file (string-append source-dir drscheme:setup-file)])
	   (if (directory-exists? source-dir)
	       (if (or (directory-exists? dest-dir)
		       (make-directory dest-dir))
		   (begin
		     (current-directory dest-dir)
		     (let copy-loop ([files-left 
				      (get-file-list source-dir)])
		       (unless (null? files-left)
			 (let ([file (car files-left)])
			   (if (not (file-exists? 
				     (file-name-from-path file)))
			       (wx:copy-file 
				file (file-name-from-path file))))
			 (copy-loop (cdr files-left))))
		     (wx:message-box 
		      (string-append "Done setting up " name))
		     (if (file-exists? source-file)
			 (load source-file)))
		   (wx:message-box
		    (string-append "Could not create directory: " dest-dir "."
				   newline-string
				   "Perhaps the permissions are set incorrectly"
				   newline-string
				   "or perhaps there a file with that name.")))
	       (wx:message-box
		(string-append "No setup material for " name)))))))))
