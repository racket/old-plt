(module make-archive mzscheme
  (require (lib "pack.ss" "setup")
	   (lib "file.ss"))

  (define tmp-dir (find-system-path 'temp-dir))
  (define work-dir (build-path tmp-dir "mk-openssl-plt"))

  (when (directory-exists? work-dir)
    (error 'make-archive "please delete leftover work directory: ~a"
	   work-dir))

  (make-directory work-dir)

  (define ssl-target-dir (build-path work-dir "collects" "openssl"))

  (make-directory* ssl-target-dir)

  (define (copy-files from to re)
    (for-each (lambda (f)
		(when (and (file-exists? (build-path from f))
			   (regexp-match re f)
			   (not (regexp-match #rx"~$" f))
			   (not (regexp-match #rx"[.]plt$" f))
			   (not (regexp-match #rx"^#.*#$" f))
			   (not (regexp-match #rx"^[.]#" f)))
		  (copy-file (build-path from f) (build-path to f))))
	      (directory-list from)))

  (copy-files (collection-path "openssl") ssl-target-dir #rx".")

  (unless (eq? (system-type) 'unix)
    (let ()
      (define pre-dir (build-path ssl-target-dir "precompiled" "native" (system-library-subpath)))
      
      (make-directory* pre-dir)
      
      (copy-files (build-path (collection-path "openssl")
			      "compiled"
			      "native"
			      (system-library-subpath))
		  pre-dir
		  (if (eq? (system-type) 'windows)
		      #rx"[.]dll$"
		      #rx"[.]so$"))
      
      (when (eq? (system-type) 'windows)
	(error 'make-archive
	       "windows still needs ssl dlls..."))
      
      'done))

  (parameterize ([current-directory work-dir])
    (pack "openssl.plt"
	  "OpenSSL for PLT"
	  (list (build-path "collects" "openssl"))
	  '(("openssl"))
	  (lambda (x) #t) ; filter nothing
	  #t
	  'file
	  #f
	  #t
	  null ;; FIXME - we need better version tracking!
	  '(("openssl"))))

  (define dest  (format "openssl.~a.plt" (case (system-type)
					   [(windows) "i386-win32"]
					   [(macosx) "ppc-macosx"]
					   [(else) "src"])))
  
  (when (file-exists? dest)
    (delete-file dest))
  (copy-file (build-path work-dir "openssl.plt") dest)

  (delete-directory/files work-dir)

  (printf "Output to ~a~n" dest))
