
;; This module creates sgl-<version>.<platform>.plt.
;;  It was shamefully copied from the openssl
;;  collection, then adapted.
;;  (Adaptations: no special SSL libs for Windows,
;;   but need to handle "gl-vectors" and "examples" sub-dirs for
;;   all platforms.)
;;
;; For Windows and Mac OS X, it creates an archive
;; with a "precompiled" subdirectory containing
;; the compiled extension.

(module make-archive mzscheme
  (require (lib "pack.ss" "setup")
	   (lib "file.ss")
	   (lib "cmdline.ss"))

  (define target-sys-type (system-type))

  (command-line
   "make-archive"
   (current-command-line-arguments)
   (once-each
    [("-s" "--src") "Make source bundle"
     (set! target-sys-type 'unix)]))

  (define tmp-dir (find-system-path 'temp-dir))
  (define work-dir (build-path tmp-dir "mk-sgl-plt"))

  (when (directory-exists? work-dir)
    (error 'make-archive "please delete leftover work directory: ~a"
	   work-dir))

  (make-directory work-dir)

  (printf "Working in ~a~n" work-dir)

  (define ssl-target-dir (build-path work-dir "collects" "sgl"))

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

  (define (copy-libs from to)
    (copy-files from to
		(if (eq? target-sys-type 'windows)
		    #rx"[.]dll$"
		    #rx"[.]so$")))

  (make-directory* (build-path ssl-target-dir "gl-vectors"))
  (make-directory* (build-path ssl-target-dir "examples"))

  (copy-files (collection-path "sgl") ssl-target-dir #rx".")
  (copy-files (collection-path "sgl" "gl-vectors") 
	      (build-path ssl-target-dir "gl-vectors")
	      #rx".")
  (copy-files (collection-path "sgl" "examples") 
	      (build-path ssl-target-dir "examples")
	      #rx".")

  (unless (eq? target-sys-type 'unix)
    (let ()
      (define pre-dir (build-path ssl-target-dir "precompiled" "native" (system-library-subpath)))
      (define vec-pre-dir (build-path ssl-target-dir "gl-vectors" "precompiled" "native" (system-library-subpath)))

      (make-directory* pre-dir)
      (make-directory* vec-pre-dir)
      
      (copy-libs (build-path (collection-path "sgl")
			     "compiled"
			     "native"
			     (system-library-subpath))
		 pre-dir)

      (copy-libs (build-path (collection-path "sgl")
			     "gl-vectors"
			     "compiled"
			     "native"
			     (system-library-subpath))
		 vec-pre-dir)

      'done))
	
  (parameterize ([current-directory work-dir])
    (pack "sgl.plt"
	  "sgl"
	  (list (build-path "collects" "sgl"))
	  '(("sgl"))
	  (lambda (x) #t) ; filter nothing
	  #t
	  'file
	  #f
	  #t ;; plt-relative
	  null ;; FIXME - we need better version tracking!
	  '(("sgl"))
	  #t)) ;; rel to PLTHOME

  (define dest  (format "sgl-~a.~a.plt" 
			(version)
			(case target-sys-type
			  [(windows) "i386-win32"]
			  [(macosx) "ppc-macosx"]
			  [else "src"])))
  
  (when (file-exists? dest)
    (delete-file dest))
  (copy-file (build-path work-dir "sgl.plt") dest)

  (delete-directory/files work-dir)

  (printf "Output to ~a~n" dest))
