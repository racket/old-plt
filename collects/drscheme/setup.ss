(define drscheme:setup@
  (unit/sig drscheme:setup^
    (import [mred : mred^]
	    [mzlib : mzlib:core^])
    
    (mred:debug:printf 'invoke "drscheme:setup@")

    (mred:set-preference-default 'drscheme:tools 
				 (list (cons "Toy" (build-path "drscheme" "toy.ss"))
				       (cons "Donkey" (build-path "donkey" "donkey.ss"))
				       (cons "Spidey" (build-path "mrspidey" "mrspidey.ss"))
				       (cons "The Debugger" (build-path "drscheme" "debugger" "debugger.ss"))))

    (mred:set-preference-default 'drscheme:config #f)
    (mred:set-preference-default 'drscheme:use-setup? (eq? (mred:get-preference 'drscheme:config) 'mzschemeowl))
    (mred:set-preference-default 'drscheme:setup-dir "/home/comp210/")
    (mred:set-preference-default 'drscheme:lab-dir "/home/comp210/Labs/")
    (mred:set-preference-default 'drscheme:hw-dir "/home/comp210/Hw/")
    (mred:set-preference-default 'drscheme:user-dir (expand-path "~/comp210/"))
    (mred:set-preference-default 'drscheme:setup-file "setup.ss")

    (mred:set-preference-default 'drscheme:use-setup? #f)

    ; User-settable
    (mred:set-preference-default 'drscheme:verify-quit? #t)
    (mred:set-preference-default 'drscheme:advanced? #f)
    (mred:set-preference-default 'drscheme:wrap-program? #t)
    (mred:set-preference-default 'drscheme:always-check? #t)
    (mred:set-preference-default 'drscheme:print-as-input? #t)
    (mred:set-preference-default 'drscheme:switch-to-in-main? #f)
    (mred:set-preference-default 'drscheme:allow-x-selection? #t)
    (mred:set-preference-default 'drscheme:allow-analysis? #t)
    (mred:set-preference-default 'drscheme:allow-stepper? #t)
    (mred:set-preference-default 'drscheme:mzscheme-type 'mred)

    ; Mode flags:
    (mred:set-preference-default 'drscheme:allow-execute? #t)
    (mred:set-preference-default 'drscheme:start-show-mode 'both)

    ; Startup files:
    (mred:set-preference-default 'drscheme:startup-files '())

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
