
(module pre-installer mzscheme
  (require (lib "setup-extension.ss" "make")
	   (lib "compile.ss" "dynext")
	   (lib "link.ss" "dynext")
	   (lib "launcher.ss" "launcher"))

  (define (do-pre-installer plthome)
    (define mach-id (string->symbol (path->string (system-library-subpath #f))))

    (with-handlers ([(lambda (x)
		       (and (exn:fail? x)
			    (memq (system-type) '(windows macos))))
		     (lambda (x)
		       (fprintf (current-error-port)
				"readline: build failed as expected for this platform: ~a~n"
				(if (exn? x)
				    (exn-message x)
				    "???")))])
      (pre-install plthome
		   (collection-path "readline")
		   "mzrl.c"
		   (build-path (collection-path "readline")
			       "readline")
		   ;; header subdirs
		   (list "readline")
		   ;; unix libs
		   (list "readline")
		   ;; windows libs
		   (list "readline")
		   ;; unix extra libs (assume always there)
		   (append
		    (if (and (file-exists? "/usr/lib/libncurses.so")
			     (not (file-exists? "/usr/lib/libcurses.so")))
			(list "ncurses")
			(list "curses"))
		    (case mach-id
		      [(sparc-solaris i386-solaris) (list "termcap")]
		      [else null]))
		   ;; Windows extra libs (assume always there)
		   null
		   ;; Extra depends:
		   (list "mzrl.ss")
		   ;; Last-chance k:
		   (lambda (k)
		     ;; More platform-specific compiler flags.
		     (with-new-flags
		      current-extension-compiler-flags
		      (case mach-id
			[(rs6k-aix)
			 (list "-DNEEDS_SELECT_H")]
			[else null])
		      (k))))))

  (define (pre-installer home)
    (do-pre-installer home)
    (when (memq '3m (available-mzscheme-variants))
      (parameterize ([link-variant '3m]
		     [compile-variant '3m])
	(do-pre-installer home))))

  (provide pre-installer))
