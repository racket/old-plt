
(module file-unit mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "include.ss"))

  (require "file-sig.ss")

  (provide dynext:file@)

  (define dynext:file@
    (unit/sig dynext:file^
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
	   [(unix beos macos macosx) ".o"]
	   [(windows) ".obj"])))

      (define (append-extension-suffix s)
	(string-append
	 s
	 (case (system-type)
	   [(unix beos macos macosx) ".so"]
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
	   (mk "[cC]|[cC][cC]|[cC][xX][xX]|[cC][pP][pP]|[cC][+][+]" "C" ".c, .cc, .cxx, .cpp, or .c++")
	   (mk "[kK][pP]" "constant pool" ".kp")
	   (mk (case (system-type)
		 [(unix beos macos macosx) "[oO]"]
		 [(windows) "[oO][bB][jJ]"])
	       "compiled object"
	       (append-object-suffix ""))
	   (mk (case (system-type)
		 [(unix beos macos macosx) "[sS][oO]"]
		 [(windows) "[dD][lL][lL]"])
	       "MzScheme extension"
	       (append-extension-suffix ""))))))))

