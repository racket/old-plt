(require-library "pretty.ss")
(require-library "file.ss")

(invoke-unit/sig
 (unit/sig ()
   (import mzlib:pretty-print^
	   mzlib:file^)

   (define (build-defn var cats)
     `(define ,var
	',(build-rhs cats)))

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

   (define beginning-cats
     (load (build-path lang-level-dir "beginner-funcs.ss")))
   (define intermediate-cats
     (load (build-path lang-level-dir "intermediate-funcs.ss")))
   (define advanced-cats
     (load (build-path lang-level-dir "advanced-funcs.ss")))

   (define prims-filename (build-path (collection-path "userspce") "prims.ss"))

   (call-with-output-file prims-filename
     (lambda (port)
       (pretty-print
	`(unit/sig plt:prims^
	   (import)
	   ,(build-defn 'beginning beginning-cats)
	   ,(build-defn 'intermediate intermediate-cats)
	   ,(build-defn 'advanced advanced-cats))
	port))
     'text 'truncate)
   (printf "wrote ~a~n" prims-filename)
   
   (define prims-docs-filename (build-path (collection-path "drscheme" "tools" "syncheck")
                                           "prims.ss"))

   (define (make-spaces str)
     (apply string (map (lambda (x) #\space) (string->list (symbol->string str)))))

   (define (build-sets ht-sym cats)
     (apply append
            (map (lambda (cat) 
                   (map (lambda (prim)
                          `(hash-table-put! ,ht-sym ',(car prim)
                                           '(,(format "~a: ~a" (car prim) (cadr prim))
                                             ,@(map (lambda (x)
						      (format "~a  ~a" (make-spaces (car prim)) x))
						    (cddr prim)))))
                        (cadr cat)))
                 cats)))

   (call-with-output-file prims-docs-filename
     (lambda (port)
       (pretty-print
	`(unit/sig drscheme:syncheck:prims^
	   (import)
           (define beginning (make-hash-table))
           (define intermediate (make-hash-table))
           (define advanced (make-hash-table))
           (define (initialize-tables)
             (initialize-tables-helper))
           (define (initialize-tables-helper)
             (set! initialize-tables-helper void)
             ,@(append (build-sets 'beginning beginning-cats)
                       (build-sets 'intermediate intermediate-cats)
                       (build-sets 'advanced advanced-cats))))
	port))
     'text 'truncate)
   (printf "wrote ~a~n" prims-docs-filename))
 mzlib:pretty-print^
 mzlib:file^)
