(module macro mzscheme
  (require (lib "class.ss"))
  (require-for-syntax (lib "class.ss")
		      (lib "stx.ss" "syntax")
                      (lib "struct.ss" "syntax")
                      (lib "list.ss"))
  
  (provide mixin make-->vector)
  
  (define-syntax (make-->vector stx)
    (syntax-case stx ()
      [(_ name) ; a struct type name
       (identifier? (syntax name))
       (let ([info (syntax-local-value (syntax name))])
         (if (struct-declaration-info? info)
             (with-syntax ([(accessor ...)
                            (reverse
                             (filter identifier? (list-ref info 3)))])
               (syntax
                (lambda (s)
                  (vector (accessor s) ...))))
             (raise-syntax-error
              #f
              "not a declared structure type name"
              stx
              (syntax name))))]))
  
  (define-syntax mixin
    (lambda (stx)
      (syntax-case stx ()
	[(_ (from ...) (to ...) clauses ...)
         (let ([extract-renamed-names
                (lambda (x)
                  (map (lambda (x) (syntax-case x ()
                                     [(internal-name external-name) (syntax external-name)]
                                     [else x]))
                       (syntax->list x)))])
           (with-syntax ([(from-ids ...) (generate-temporaries (syntax (from ...)))]
                         [(to-ids ...) (generate-temporaries (syntax (to ...)))]
                         [(super-vars ...)
                          (apply
                           append
                           (map (lambda (stx)
                                  (syntax-case stx (inherit rename override override-final)
                                    [(inherit names ...) (extract-renamed-names (syntax (names ...)))]
                                    [(rename [x names] ...) (syntax->list (syntax (names ...)))]
                                    [(override names ...) (extract-renamed-names (syntax (names ...)))]
                                    [(override-final names ...) (extract-renamed-names (syntax (names ...)))]
                                    [else null]))
                                (syntax->list (syntax (clauses ...)))))]

		         ;; syntax system stuff for super-instantiate, super-make-object, and this
                         [this (datum->syntax-object (stx-car stx) 'this stx)]
                         [super-instantiate (datum->syntax-object (stx-car stx) 'super-instantiate stx)]
                         [super-make-object (datum->syntax-object (stx-car stx) 'super-make-object stx)]
			 [super-new (datum->syntax-object (stx-car stx) 'super-new stx)]
                         [mixin-name (or (with-syntax ([tmp (syntax-local-name)])
                                           (syntax (quote tmp)))
                                         (syntax (quote mixin)))])
	     
	     ;; Build the class expression first, to give it a good src location:
	     (with-syntax ([class-expr
			    (syntax/loc stx
                               (class*/names
				 (this super-instantiate super-make-object super-new) super% (to-ids ...)
				 clauses ...))])

	       ;; Now build mixin proc, again to give it a good src location:
	       (with-syntax ([mixin-expr
			      (syntax/loc stx
			        (lambda (super%)
				  (unless (class? super%)
				    (error mixin-name "argument ~a not a class" super%))
				  (unless (implementation? super% from-ids)
				    (error mixin-name "argument ~s does not implement ~s" super% from-ids))
				  ...
				  class-expr))])

		 ;; Finally, build the complete mixin expression:
		 (syntax/loc stx
		   (let ([from-ids from] ...)
		     (let ([to-ids to] ...)
		       
		       (let ([all-from (list from-ids ...)])
			 (void)
			 (unless (interface? from-ids)
			   (error 'mixin
				  "expected interfaces for from, got: ~e, others ~e"
				  from-ids
				  all-from)) ...)
		       
		       (let ([all-to (list to-ids ...)])
			 (void)
			 (unless (interface? to-ids)
			   (error 'mixin
				  "expected interfaces for to, got: ~e, others ~e"
				  to-ids
				  all-to)) ...)
		       
		       (let ([ensure-interface-has?
			      (lambda (x)
				(unless (or (method-in-interface? x from-ids) ...)
				  (error 'mixin
					 "method `~a' not in any of ~a, but was referenced in definition"
					 x (list from-ids ...))))])
			 (void)
			 (ensure-interface-has? (quote super-vars)) ...)

		       mixin-expr)))))))]))))
