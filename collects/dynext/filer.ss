
(unit/sig
 dynext:file^
 (import)

 (define (append-zo-suffix s)
   (string-append s ".zo"))

 (define (append-c-suffix s)
   (string-append s ".c"))

 (define (append-constant-pool-suffix s)
   (string-append s ".kp"))

 (define (append-object-suffix s)
   (string-append
    s
    (case (system-type)
      [(unix beos macos) ".o"]
      [(windows) ".obj"])))

 (define (append-extension-suffix s)
   (string-append
    s
    (case (system-type)
      [(unix beos macos) ".so"]
      [(windows) ".dll"])))

 (define-values (extract-base-filename/ss
		 extract-base-filename/c
		 extract-base-filename/kp
		 extract-base-filename/o
		 extract-base-filename/ext)
   (let ([mk
	  (lambda (pat kind simple)
	    (letrec ([extract-base-filename
		      (case-lambda
		       [(s p)
			(let ([m (regexp-match (format "^(.*)\\.(~a)$" pat) s)])
			  (cond
			   [m (cadr m)]
			   [p (error p "not a ~a filename (doesn't end with ~a): ~a" kind simple s)]
			   [else #f]))]
		       [(s) (extract-base-filename s #f)])])
	      extract-base-filename))])
     (values
      (mk "[sS][sS]|[sS][cC][mM]" "Scheme" ".ss or .scm")
      (mk "[cC]|[cC][cC]|[cC][xX][xX]|[cC][+][+]" "C" ".c, .cc, .cxx, or .c++")
      (mk "[kK][pP]" "constant pool" ".kp")
      (mk (case (system-type)
	    [(unix beos macos) "[oO]"]
	    [(windows) "[oO][bB][jJ]"])
	  "compiled object"
	  (append-object-suffix ""))
      (mk (case (system-type)
	    [(unix beos macos) "[sS][oO]"]
	    [(windows) "[dD][lL][lL]"])
	  "MzScheme extension"
	  (append-extension-suffix ""))))))

