
(module pre-installer mzscheme
  (require (lib "setup-extension.ss" "make")
	   (lib "compile.ss" "dynext")
	   (lib "link.ss" "dynext")
	   (lib "restart.ss")
	   (lib "launcher.ss" "launcher")
	   (lib "file.ss"))

  (define (pre-installer plthome)
    (define (go file)
      (pre-install plthome
		   (collection-path "openssl")
		   file
		   (build-path (collection-path "openssl") 
			       "openssl")
		   ;; header subdirs
		   (list "openssl")
		   ;; unix libs
		   (list "ssl" "crypto")
		   ;; windows libs
		   (list "libeay32" "ssleay32")
		   ;; unix extra libs (assume always there)
		   null
		   ;; Windows extra libs (assume always there)
		   (list "wsock32.lib")
		   ;; Extra depends:
		   (list "mzssl.ss")
		   ;; Last-chance k:
		   (lambda (k) (k))))
    (go "mzssl.c")

    ;; Build for 3m when it looks like we can/should.
    ;; This is a hack --- hopefully temporary!
    (let ([3m-dir (build-path "compiled" "native" (system-library-subpath) "3m")])
      (parameterize ([current-directory (collection-path "openssl")])
	(when (and (memq (system-type) '(unix macosx))
		   (memq '3m (available-mzscheme-variants))
		   (directory-exists? (build-path 'up 'up "src" "mzscheme" "gc2"))
		   (or (not (file-exists? (build-path 3m-dir "mzssl.so")))
		       ((file-or-directory-modify-seconds (build-path 3m-dir 'up "mzssl.so"))
			. > .
			(file-or-directory-modify-seconds (build-path 3m-dir "mzssl.so")))))
	  (make-directory* 3m-dir)
	  (restart-mzscheme #() 
			    (lambda (x) x)
			    (list->vector 
			     (list
			      "-qr"
			      (build-path 'up 'up "src" "mzscheme" "gc2" "xform.ss")
			      (build-path 'up 'up "src" "mzscheme" "gc2" "ctok.ss")
			      (format "gcc -E -DOS_X -I~s -I~s" 
				      (build-path 'up 'up "include")
				      (build-path 'up 'up "src" "mzscheme" "gc2"))
			      "mzssl.c"
			      (build-path 3m-dir "mzssl.c")))
			    void)
	  (parameterize ([link-variant '3m])
	    (use-standard-linker 'cc) ; reset library flags
	    (go (build-path 3m-dir "mzssl.c")))))))

  (provide pre-installer))
