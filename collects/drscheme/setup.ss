(define drscheme:parameters@
  (unit/sig plt:parameters^
    (import [mred : mred^])

    (mred:set-preference-default 'drscheme:scheme-level 'core)
    (define pref (mred:get-preference 'drscheme:scheme-level))

    (define case-sensitive? #t)
    (define allow-set!-on-undefined? (case pref
				       [(advanced) #t]
				       [else #f]))
    (define unmatched-cond/case-is-error? #t)
    (define allow-improper-lists? (case pref
				       [(advanced) #t]
				       [else #f]))
    (define check-syntax-level pref)))

(define drscheme:setup@
  (unit/sig drscheme:setup^
    (import [mred : mred^]
	    [mzlib : mzlib:core^])
    
    (mred:debug:printf 'invoke "drscheme:setup@")

    (mred:set-preference-default 'drscheme:wrap-program? #f)
    (mred:set-preference-default 'drscheme:config #f)
    (mred:set-preference-default 'drscheme:use-setup? (eq? (mred:get-preference 'drscheme:config) 'mzschemeowl))
    (mred:set-preference-default 'drscheme:setup-dir "/home/comp210/")
    (mred:set-preference-default 'drscheme:lab-dir "/home/comp210/Labs/")
    (mred:set-preference-default 'drscheme:hw-dir "/home/comp210/Hw/")
    (mred:set-preference-default 'drscheme:user-dir (expand-path "~/comp210/"))
    (mred:set-preference-default 'drscheme:setup-file "setup.ss")

    (mred:set-preference-default 'drscheme:use-setup? #f)

    ; Startup files:
    (mred:set-preference-default 'drscheme:startup-files '())

    (mred:add-preference-panel
     "DrScheme"
     (lambda (parent)
       (let* ([main (make-object mred:vertical-panel% parent)]
	      [choice-callback
	       (let ([state #t])
		 (lambda (_ evt)
		   (mred:set-preference 'drscheme:scheme-level
					(case (send evt get-command-int)
					  [(0) 'core]
					  [(1) 'structured]
					  [(2) 'side-effects]
					  [(3) 'advanced]))
		   (when state
		     (set! state #f)
		     (unless (mred:get-choice "Any changes to this setting will not take effect until DrScheme is restarted"
					      "Continue Working"
					      "Exit")
		       (mred:exit)))))]
	     [choice (make-object mred:choice% main choice-callback
				  "Language"
				  -1 -1 -1 -1
				  (list "Functional Scheme"
					"Functional Scheme Plus Structures"
					"R4RS Scheme Plus Structures"))])
	 (send choice set-selection 
	       (case (mred:get-preference 'drscheme:scheme-level)
		 [(core) 0]
		 [(structured) 1]
		 [(side-effects) 2]
		 [(advanced) 3]))
	 main)))

    (define setup-base-dir
      (lambda ()
	(or (directory-exists? (mred:get-preference 'drscheme:user-dir))
	    (let loop ([whole (mred:get-preference 'drscheme:user-dir)])
	      (let ([one (mzlib:file@:file-name-from-path whole)]
		    [dir (mzlib:file@:path-only whole)])
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
    
    (define do-setup
      (lambda (type-str)
	(when (and (mred:get-preference 'drscheme:use-setup?)
		   ((mred:get-preference 'drscheme:setup-base-dir)))
	  (let ([get-file-list directory-list]
		[num-str (wx:get-text-from-user 
			  (string-append "Enter " type-str " number:")
			  "Setup")])
	    (if 
	     (string? num-str)
	     (let*
		 ([name (string-append type-str num-str)]
		  [source-dir(string-append 
			      (cond [(string=? type-str "lab")
				     (mred:get-preference 'drscheme:lab-dir)]
				    [(string=? type-str "hw") 
				     (mred:get-preference 'drscheme:hw-dir)]
				    [else 
				     (mred:get-preference 'drscheme:setup-dir)])
			      name)]
		  [dest-dir (string-append (mred:get-preference 'drscheme:user-dir) name)]
		  [source-file (string-append source-dir (mred:get-preference 'drscheme:user-dir))])
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
					 (mzlib:file@:file-name-from-path file)))
				   (wx:copy-file 
				    file (mzlib:file@:file-name-from-path file))))
			     (copy-loop (cdr files-left))))
			 (wx:message-box 
			  (string-append "Done setting up " name))
			 (if (file-exists? source-file)
			     (load source-file)))
		       (wx:message-box
			(string-append "Could not create directory: " dest-dir "."
				       (string #\newline)
				       "Perhaps the permissions are set incorrectly"
				       (string #\newline)
				       "or perhaps there a file with that name.")))
		   (wx:message-box
		    (string-append "No setup material for " name)))))))))))
