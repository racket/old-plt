#!/bin/sh
string=? ; exec ${PLTHOME}/bin/mzscheme -mgrq $0 "$@"

;; This script creates plot-<version>.<platform>.plt.
;;
;; will create either a binary distribution, containing
;; all binary files in compiled/native or a "src" distro
;; if --src is specified

(require 
 (lib "pack.ss	" "setup")
 (lib "file.ss")
 (lib "cmdline.ss"))

(define target-sys-type (system-type))

(define plot-src-collection-name "plot")

(command-line
 "make-archive"
 (current-command-line-arguments)
 (once-each
  [("-s" "--src") "Make source bundle"
   (set! target-sys-type 'src)]))

(define tmp-dir (find-system-path 'temp-dir))
(define work-dir (build-path tmp-dir "mk-plot-plt"))

(when (directory-exists? work-dir)
      (error 'make-archive "please delete leftover work directory: ~a"
	     work-dir))

(display target-sys-type)	

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

(define base-structure
  '(()
    ("tests")))

(define src-structure
  '(("src")				
    ("src" "fit")
    ("src" "tmp")
    ("src" "docs")))

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
	#rx"[.]|hdindex")))
   directories))

; copy over all the docs

(copy-all base-structure (collection-path plot-src-collection-name) plot-target-dir)

(define copied-dirs (list (build-path "collects" "plot")))

(if (eq? target-sys-type 'src)  ; this is for a src distro    
    (copy-all src-structure (collection-path plot-src-collection-name) plot-target-dir )
    ;; for binary distro    
    (let 
        ((pre-dir (build-path plot-target-dir "precompiled" ))
	 (plot-doc-dir
	  (build-path work-dir "collects" "doc" "plot")))
      (make-directory* plot-doc-dir)


      ;; copy plot docs into collects/doc
      (for-each
       (lambda (file)
	 (copy-file file (build-path plot-doc-dir (file-name-from-path file))))
       (find-files
	(lambda (file)
	  (or
	   (regexp-match
	    #rx"hdindex$"
	    file)
	   (and
	    (not
	     (regexp-match
	      #rx".tex" file))
	    (regexp-match
	     #rx"plot-docs"
	     file))))
	(build-path (collection-path plot-src-collection-name) "src" "docs")))

      (set! copied-dirs (cons (build-path "collects" "doc")))

      (make-directory* pre-dir)              
      (copy-directory/files
       (build-path (collection-path plot-src-collection-name)
		   "compiled"
		   "native")
       (build-path pre-dir "native"))))       

(parameterize ([current-directory work-dir])
	      (pack "plot.plt"
		    "PLOT for PLT"
		    copied-dirs
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
			[(src) "src"]		
			[else "bin"])))

(when (file-exists? dest)
      (delete-file dest))
(copy-file (build-path work-dir "plot.plt") dest)

(delete-directory/files work-dir)

(printf "Output to ~a~n" dest)

