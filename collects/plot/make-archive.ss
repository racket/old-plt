
;; This module creates plot-<version>.<platform>.plt.
;;
;; For Windows, Sparc and Mac OS X, it creates an archive
;; with a "precompiled" subdirectory containing
;; the compiled extension.
;; 

(module make-archive mzscheme
  (require 
   (lib "pack.ss" "setup")
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
  (define work-dir (build-path tmp-dir "mk-plot-plt"))

  (when (directory-exists? work-dir)
    (error 'make-archive "please delete leftover work directory: ~a"
	   work-dir))

  (make-directory work-dir)

  (printf "Working in ~a~n" work-dir)

  (define (copy-files from to re)
    (for-each 
     (lambda (f)
       (when
         (and 
           (file-exists? (build-path from f))
           (regexp-match re f)
           (not (regexp-match #rx"[.]o$" f))
	   (not (regexp-match #rx"[.]a$" f))
           (not (regexp-match #rx"~$" f))
           (not (regexp-match #rx"[.]plt$" f))
           (not (regexp-match #rx"^#.*#$" f))
           (not (regexp-match #rx"^[.]#" f)))
          (copy-file (build-path from f) (build-path to f))))
     (directory-list from)))
 
  (define inplace-structure
    '(()
      ("tests")
      ("src")
      ("src" "plplot")
      ("src" "fit")
      ("src" "gd")))
  
  (define copied-structure
    '(("libpng")
      ("zlib")))
  
  (define plot-target-dir
    (build-path work-dir "collects" "plot"))
  
  (define (copy-all directories source target)  
    (for-each
     (lambda (dir)
       (let ((dest-dir (apply build-path target dir)))
         (make-directory* dest-dir)
         (copy-files 
          (apply build-path source dir) 
          (apply build-path target dir)
          #rx"[.]")))
     directories))
  
  (copy-all inplace-structure (collection-path "plot") plot-target-dir )
  (copy-all copied-structure 
            (simplify-path 
             (build-path 
              (collection-path "mzlib" )
              'up
              'up
              "src"
              "wxcommon"))
            (build-path plot-target-dir "src"))
 
  (unless (eq? target-sys-type 'unix)
    (let ()
      (define pre-dir (build-path plot-target-dir "precompiled" "native" (system-library-subpath)))

      (make-directory* pre-dir)
      
      (copy-files (build-path (collection-path "plot")
			      "compiled"
			      "native"
			      (system-library-subpath))
		  pre-dir
		  (if (eq? target-sys-type 'windows)
		      #rx"[.]dll$"
		      #rx"[.]so$"))      
      'done))

  (parameterize ([current-directory work-dir])
    (pack "plot.plt"
	  "PLOT for PLT"
	  (list (build-path "collects" "plot"))
	  '(("plot"))
	  (lambda (x) #t) ; filter nothing
	  #t
	  'file
	  #f
	  #t ;; plt-relative
	  null ;; FIXME - we need better version tracking!
	  '(("plot"))
	  #t)) ;; rel to PLTHOME

  (define dest  (format "plot-~a.~a.plt" 
			(version)
			(case target-sys-type
			  [(windows) "i386-win32"]
			  [(macosx) "ppc-macosx"]
			  [else "src"])))
  
  (when (file-exists? dest)
    (delete-file dest))
  (copy-file (build-path work-dir "plot.plt") dest)

  (delete-directory/files work-dir)

  (printf "Output to ~a~n" dest))
