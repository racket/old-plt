
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
		   (let ([libs 
			  (append
			   (let ([v (getenv "PLT_EXTENSION_LIB_PATHS")])
			     (if v 
				 (path-list-string->path-list v)
				 null))
			   (list (build-path (collection-path "openssl") 
					     "openssl"
					     "lib")))])
		     (if (ormap (lambda (lib)
				  (and (file-exists? (build-path lib "libeay32xxxxxxx.lib"))
				       (file-exists? (build-path lib "ssleay32xxxxxxx.lib"))))
				libs)
			 ;; Use mangleable names:
			 (list "libeay32xxxxxxx" "ssleay32xxxxxxx")
			 ;; Use simple names:
			 (list "libeay32" "ssleay32")))
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
    (let ([3m-dir (build-path "compiled" "native" (system-library-subpath #f) "3m")]
	  [mzssl.so (if (eq? 'windows (system-type)) "mzssl.dll" "mzssl.so")])
      (parameterize ([current-directory (collection-path "openssl")])
	(when (and (memq (system-type) '(unix macosx windows))
		   (memq '3m (available-mzscheme-variants))
		   (directory-exists? (build-path 'up 'up "src" "mzscheme" "gc2")))
	  (when (or (not (file-exists? (build-path 3m-dir mzssl.so)))
		    ((file-or-directory-modify-seconds (build-path 3m-dir 'up mzssl.so))
		     . > .
		     (file-or-directory-modify-seconds (build-path 3m-dir mzssl.so))))
	    (make-directory* 3m-dir)
	    (restart-mzscheme #() 
			      (lambda (x) x)
			      (list->vector 
			       (list
				"-qr"
				(build-path 'up 'up "src" "mzscheme" "gc2" "xform.ss")
				(let ([inc (build-path 'up 'up "include")]
				      [extras (cond ((getenv "PLT_EXTENSION_LIB_PATHS") =>
						     (lambda (ext)
						       (apply string-append
							      (map (lambda (p)
								     (format 
								      " ~a~s"
								      (if (eq? 'windows (system-type))
									  " /I"
									  " -I")
								      (build-path p "include")))
								   (path-list-string->path-list ext '())))))
						    (else ""))])
				  (if (eq? 'windows (system-type))
				      (format "cl.exe /MT /E /I~s /I~s~a" 
					      inc
					      (build-path (collection-path "openssl") "openssl" "include")
					      extras)
				      (format "gcc -E -DOS_X -I~s~a" inc extras)))
				"mzssl.c"
				(build-path 3m-dir "mzssl.c")))
			      void))
	  (parameterize ([link-variant '3m])
	    (with-new-flags current-extension-compiler-flags
			    (if (eq? 'windows (system-type))
				'("/Zi")
				null)
	      (with-new-flags current-extension-linker-flags
			      (if (eq? 'windows (system-type))
				  '("/Zi")
				  null)
	        (go (build-path 3m-dir "mzssl.c"))))))))

    ;; Under windows, put "{lib,sll}eay32" into the system folder when
    ;; they're in a "precompiled" dir.
    (when (eq? 'windows (system-type))
      (let ([dir (build-path (collection-path "openssl")
			     "precompiled"
			     "native"
			     (system-library-subpath #f))])
	(when (directory-exists? dir)
	  (let ([l (directory-list dir)])
	    (let ([libeay (ormap (lambda (f)
				   (regexp-match #rx"^libeay32.*[.]dll$" f))
				 l)]
		  [ssleay (ormap (lambda (f)
				   (regexp-match #rx"^ssleay32.*[.]dll$" f))
				 l)])
	      (when (and libeay ssleay)
		(let ([sys-dir (find-system-path 'sys-dir)])
		  (let ([move-over
			 (lambda (f)
			   (unless (file-exists? (build-path sys-dir f))
			     (printf "  Installing ~a into system directory~n" f)
			     (copy-file (build-path dir f)
					(build-path sys-dir f))))])
		    (move-over (car libeay))
		    (move-over (car ssleay)))))))))))

  (provide pre-installer))
