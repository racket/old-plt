(current-directory "/home/robby/plt/misc")
(require-library "match.ss")
;(require-library "file.ss")

(load "hierarchy.ss")

(define name-only
  (lambda (x) 
    (let*-values ([(pa a _2) (split-path x)]
		  [(pb b _2) (split-path pa)]
		  [(pc c _2) (split-path pb)])
      (build-path b a))))

(define space "")

(define add-relationT 
  (lambda (x y)
    (printf "~aadd-relation: ~s ~s~n" space x y)
    (add-relation x y)))

(define drscheme:tool-directories
   (quicksort (directory-list (build-path plt:home-directory
                                          "drscheme"
                                          "tools"))
              string-ci<=?))

(define find-references
  (let ([badones (map normalize-path 
		      (list "/home/robby/plt/drscheme/basis.ss"
			    "/home/robby/plt/mzscheme/collects/standard/compiler.ss"
			    "/home/robby/plt/mzscheme/collects/standard/referr.ss"))])
    (lambda (t-filename sexp)
      (unless (member t-filename badones)
	(match sexp
	  [('begin-elaboration-time . x)
	   (when (and (list? x)
		      (not (null? x)))
	     (find-references t-filename (eval `(begin ,@x))))]
	  [('reference-unit/sig filename)
	   (let ([path (normalize-path (eval filename))])
	     (add-relationT (string->symbol (name-only path))
			    (string->symbol (name-only t-filename)))
	     (traverse-file path))]
	  [('reference-library-unit/sig filename location)
	   (let ([path (normalize-path (find-library (eval filename) location))])
	     (add-relationT (string->symbol (name-only path))
			    (string->symbol (name-only t-filename)))
	     (traverse-file path))]
	  [('reference-library-unit/sig filename)
	   (let* ([location "standard"]
		  [path (normalize-path (find-library (eval filename) location))])
	     (add-relationT (string->symbol (name-only path))
			    (string->symbol (name-only t-filename)))
	     (traverse-file path))]
	  [(x . y)
	   (find-references t-filename x)
	   (find-references t-filename y)
	   '(for-each (lambda (x) (find-references t-filename x)) l)]
	  [else (void)])))))

(define traverse-file
  (lambda (filename)
    (set! space (string-append "  " space))
    (printf "~atraversing file: ~a~n" space filename)
    (let ([old-dir (current-directory)])
      (current-directory (path-only filename))
      (find-references filename (call-with-input-file filename
				  (lambda (p)
				    (let loop ()
				      (let ([x (read p)])
					(if (eof-object? x)
					    null
					    (cons x (loop))))))))
      (current-directory old-dir))
    (set! space (substring space 2 (string-length space)))
    (printf "~afinished file: ~a~n" space filename)))

(define mred:plt-home-directory (normalize-path "/home/robby/plt"))
(define plt:home-directory mred:plt-home-directory)

;(define frame (build-canvas-frame))

(define drs-roots
  (map normalize-path
       (list "/home/robby/plt/main.ss"
	     ;"/home/robby/plt/mred/collects/userspce/gusrspcu.ss"
	     )))
(for-each (lambda (root)
	    (add-relationT (string->symbol (name-only root)) #f)
	    (traverse-file root))
	  drs-roots)

(output-postscript "drs-hierarchy.ps" 1/2)
