
(module sig mzscheme
  (require (lib "unitsig.ss"))
  (require-for-syntax (lib "list.ss"))
  (provide plt:basis^
	   define-struct/parse)
  
  (define-syntax (define-struct/parse stx)
    (syntax-case stx ()
      [(_ str (fields ...))
       (unless (symbol? (syntax-object->datum (syntax str)))
         (error 'define-struct/parse "no super structs allowed"))
       (let* ([defn (syntax->list (local-expand (syntax (define-struct str (fields ...))) 'define-values))]
              [_ (unless (and (pair? defn)
                              (eq? (syntax-e (first defn)) 'define-values))
                   (error 'define-struct/parse
                          "expand didn't return expected value: ~s~n" (syntax-object->datum defn)))]
              
              [make/parse (string->symbol
                           (string-append
                            "make-"
                            (symbol->string (syntax-e (syntax str)))
                            "/parse"))]
	      [unparse (string->symbol 
			(string-append
			 (symbol->string (syntax-e (syntax str)))
			 "/unparse"))]
	      [evens
	       (lambda (l)
		 (let loop ([l l])
		   (cond
		    [(null? l) null]
		    [(null? (cdr l)) (error 'evens)]
		    [else (cons (car l)
				(loop (cddr l)))])))])
         (with-syntax ([bindings (second defn)]
		       [all-bindings (list* make/parse unparse (second defn))]
		       [(original-names ...) (second defn)]
		       [(selectors ...) (evens (cdddr (syntax->list (second defn))))]
		       [exp (third defn)]
                       [unparser unparse]
		       [make/parser make/parse]
                       [predicate? (third (syntax->list (second defn)))]
                       [maker-name (second (syntax->list (second defn)))])
           (syntax
            (define-values all-bindings
	      (let-values ([bindings exp])
		(let ([make/parser
		       (lambda (inits)
			 (let ([select-field
				(lambda (field)
				  (let ([m (assq field inits)])
				    (unless m
				      (error 'make/parser "no binding for: ~a" field))
				    (unless (= (length m) 2)
				      (error 'make/parser "malformed binding: ~a" m))
				    (cadr m)))])
			   (maker-name (select-field 'fields) ...)))]
		      [unparser
		       (lambda (struct)
			 (unless (predicate? struct)
			   (error 'unparser "expected an instance of <struct:~a>, got ~e"
				  'str struct))
			 (list (list 'fields (selectors struct)) ...))])
		  (values make/parser unparser original-names ...)))))))]))
  
  (define-signature plt:basis^
    (initial-line
     initial-offset
     initial-column
     
     initialize-parameters
     settings
     get-default-setting
     get-default-setting-name
     
     drscheme-load-handler
     
     raw-reader
     
     beginner-language?
     intermediate-language?
     advanced-language?
     full-language?
     
     error-display/debug-handler
     current-setting
     bottom-escape-handler
     
     drscheme-print
     
     format-source-loc
     
     primitive-eval
     primitive-load
     syntax-checking-primitive-eval
     
     process
     process-file
     process-sexp
     
     (struct process-finish (error?))
     
     setting-name->number
     number->setting
     setting/unparse
     (struct setting (key
		      name
		      language-defining-module

		      read-decimal-as-exact?
		      case-sensitive?
		      allow-reader-quasiquote?
		      disallow-untagged-inexact-numbers

		      whole/fractional-exact-numbers

		      printing
		      use-pretty-printer?
		      sharing-printing?
		      abbreviate-cons-as-list?
		      print-tagged-inexact-numbers
		      print-booleans-as-true/false
		      print-exact-as-decimal?
		      print-.-symbols-without-bars
		      
		      define-argv?))
     make-setting/parse
     
     teaching-level?
     
     find-setting-named
     add-setting
     copy-setting
     
     r4rs-style-printing?)))
