(module macro mzscheme
  (require-for-syntax (lib "list.ss"))
  (provide define-struct/parse)
  
  (define-syntax (define-struct/parse stx)
    (syntax-case stx ()
      [(_ str (fields ...))
       (syntax (define-struct/parse str (fields ...) (current-inspector)))]
      [(_ str (fields ...) inspector-exp)
       (unless (symbol? (syntax-object->datum (syntax str)))
         (error 'define-struct/parse "no super structs allowed"))
       (let ([defn (local-expand
		    (syntax (define-struct str (fields ...) inspector-exp))
		    (syntax-local-context)
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
                       [->vector
                        (datum->syntax-object
			 (syntax str)
                         (string->symbol 
                          (string-append
                           (symbol->string (syntax-e (syntax str)))
                           "->vector")))]
                       [unparse (datum->syntax-object
				 (syntax str)
				 (string->symbol 
				  (string-append
				   (symbol->string (syntax-e (syntax str)))
				   "/unparse")))]
		       [make/parse (datum->syntax-object
				    (syntax str)
				    (string->symbol
				     (string-append
				      "make-"
				      (symbol->string (syntax-e (syntax str)))
				      "/parse")))]
                       [struct:-name
                        (datum->syntax-object
                         stx
                         (string->symbol
                          (string-append
                           "struct:"
                           (symbol->string (syntax-e (syntax str))))))])
                      (with-syntax ([(selectors ...)
                                     (evens (syntax->list (syntax (selectors/mutators ...))))])
                                   (syntax
                                    (define-values (make/parse unparse ->vector struct: make pred? selectors/mutators ...)
                                      (let-values ([(struct: make pred? selectors/mutators ...) exp])
                                        (let ([make/parse
                                               (lambda (inits)
                                                 (let ([select-field
                                                        (lambda (fld)
                                                          (let ([m (assq fld inits)])
                                                            (unless m
                                                              (error 'make/parse "no binding for: ~a" fld))
                                                            (unless (= (length m) 2)
                                                              (error 'make/parse "malformed binding: ~a" m))
                                                            (cadr m)))])
                                                   (make (select-field 'fields) ...)))]
                                              [unparse
                                               (lambda (struct)
                                                 (unless (pred? struct)
                                                   (error 'unparse "expected an instance of <struct:~a>, got ~e"
                                                          'str struct))
                                                 (list (list 'fields (selectors struct)) ...))]
                                              [->vector
                                               (lambda (struct)
                                                 (unless (pred? struct)
                                                   (error '->vector "expected an instance of <struct:~a>, got ~e"
                                                          'str struct))
                                                 (vector 'struct:-name (selectors struct) ...))])
                                          (values make/parse unparse ->vector struct: make pred? selectors/mutators ...))))))))])))
