
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
       (let ([defn (local-expand (syntax (define-struct str (fields ...)))
				 'internal-define
				 (list (quote-syntax define-values)))]
	     [evens
	      (lambda (l)
		(let loop ([l l])
		  (cond
		   [(null? l) null]
		   [(null? (cdr l))
		    (error 'define-struct/parse "internal error.1")]
		   [else (cons (car l)
			       (loop (cddr l)))])))])
         (with-syntax ([(_ (struct: make pred? selectors/mutators ...) exp) defn]
                       [unparse (datum->syntax-object
				 stx
				 (string->symbol 
				  (string-append
				   (symbol->string (syntax-e (syntax str)))
				   "/unparse")))]
		       [make/parse (datum->syntax-object
				    stx
				    (string->symbol
				     (string-append
				      "make-"
				      (symbol->string (syntax-e (syntax str)))
				      "/parse")))])
	   (with-syntax ([(selectors ...)
			  (evens (syntax->list (syntax (selectors/mutators ...))))])
	     (syntax
	      (define-values (make/parse unparse struct: make pred? selectors/mutators ...)
		(let-values ([(struct: make pred? selectors/mutators ...) exp])
		  (let ([make/parse
			 (lambda (inits)
			   (let ([select-field
				  (lambda (field)
				    (let ([m (assq field inits)])
				      (unless m
					(error 'make/parse "no binding for: ~a" field))
				      (unless (= (length m) 2)
					(error 'make/parse "malformed binding: ~a" m))
				      (cadr m)))])
			     (make (select-field 'fields) ...)))]
			[unparse
			 (lambda (struct)
			   (unless (pred? struct)
			     (error 'unparse "expected an instance of <struct:~a>, got ~e"
				    'str struct))
			   (list (list 'fields (selectors struct)) ...))])
		    (values make/parse unparse struct: make pred? selectors/mutators ...))))))))]))
    
  (define-signature plt:basis^
    (initial-line
     initial-offset
     initial-column
     
     initialize-parameters
     get-settings
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
