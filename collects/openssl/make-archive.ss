
;; This module creates openssl.<platform>.plt.
;;
;; For Windows and Mac OS X, it creates an archive
;; with a "precompiled" subdirectory containing
;; the compiled extension.
;; 
;; For Windows, it also arranges for
;;  {lib,sll}eay32<version>.dll
;; to be in the archive, and it mangles the extension
;; to replace references to 
;;  {lib,sll}eay32xxxxxxx.dll
;; to the versioned names. The xxxxxxx DLLs must
;; be in the PLT directory (two levels up from here)
;; to be compied and version-mangled.

(module make-archive mzscheme
  (require (lib "pack.ss" "setup")
	   (lib "file.ss"))

  (define tmp-dir (find-system-path 'temp-dir))
  (define work-dir (build-path tmp-dir "mk-openssl-plt"))

  (when (directory-exists? work-dir)
    (error 'make-archive "please delete leftover work directory: ~a"
	   work-dir))

  (make-directory work-dir)

  (printf "Working in ~a~n" work-dir)

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
	;; Assume that the xxxx-ized versions of the DLLs are in
	;; the plt directory:
	(let* ([new-version (substring
			     (regexp-replace*
			      "alpha"
			      (format "~a_000000000" (version))
			      "a")
			     0
			     7)]
	       [target-name
		(lambda (x)
		  (format "~aeay32~a.dll" x new-version))]
	       [move-dll
		(lambda (x)
		  (copy-file (build-path (collection-path "openssl")
					 'up 'up
					 (format "~aeay32xxxxxxx.dll" x))
			     (build-path pre-dir 
					 (target-name x))))])
	  (move-dll "lib")
	  (move-dll "ssl")

	  ;; Mangle xxxxxxxx to a version:
	  (let ([fixup
		 (lambda (f)
		   (let ([p (build-path pre-dir f)])
		     (let ([i (open-input-file p)]
			   [o (open-output-file p 'append)])
		       (let loop ()
			 (file-position i 0)
			 ;; This is a poor technique for updating the files, but
			 ;;  it works well enough.
			 (let ([m (regexp-match-positions #rx"eay32xxxxxxx" i)])
			   (when m
				 (file-position o (caar m))
				 (display "eay32" o)
				 (display new-version o)
				 (loop))))
		       (close-input-port i)
		       (close-output-port o))))])
	    (fixup "mzssl.dll")
	    (fixup (target-name "lib"))
	    (fixup (target-name "ssl")))))

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
