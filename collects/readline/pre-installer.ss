
(module pre-installer mzscheme
  (require (lib "setup-extension.ss" "make")
	   (lib "compile.ss" "dynext"))

  (define (pre-installer plthome)
    (define mach-id (string->symbol (system-library-subpath)))

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
		    [(sparc-solaris i386-solaris) (list "-ltermcap")]
		    [else null]))
		 ;; Windows extra libs (assume always there)
		 null
		 ;; Last-chance k:
		 (lambda (k)
		   ;; More platform-specific compiler flags.
		   (with-new-flags
		    current-extension-compiler-flags
		    (case mach-id
		      [(rs6k-aix)
		       (list "-DNEEDS_SELECT_H")]
		      [else null])
		    (k)))))

  (provide pre-installer))
