(require-library "pretty.ss")
(require-library "file.ss")

(invoke-unit/sig
 (unit/sig ()
   (import mzlib:pretty-print^
	   mzlib:file^)

   (define (build-defn var file)
     `(define ,var
	',(build-rhs (load file))))

   (define (build-rhs cats)
     (apply append (map (lambda (cat) (map car (cadr cat))) cats)))

   (define iplt-dir
     (build-path (collection-path "mzlib")
		 'up
		 'up
		 'up
		 "iplt"))

   (unless (directory-exists? iplt-dir)
     (error 'build-prims.ss
	    "cannot find iplt tree, expected it in: ~s"
	    iplt-dir))

   (define lang-level-dir (normalize-path (build-path iplt-dir
						      "doc"
						      "language-levels")))

   (define prims-filename (build-path (collection-path "userspce") "prims.ss"))

   (call-with-output-file prims-filename
     (lambda (port)
       (pretty-print
	`(unit/sig plt:prims^
	   (import)
	   ,(build-defn 'beginning (build-path lang-level-dir "beginner-funcs.ss"))
	   ,(build-defn 'intermediate (build-path lang-level-dir "intermediate-funcs.ss"))
	   ,(build-defn 'advanced (build-path lang-level-dir "advanced-funcs.ss")))
	port))
     'text 'truncate)

   (printf "wrote ~a~n" prims-filename))
 mzlib:pretty-print^
 mzlib:file^)
