#cs(module typecheck mzscheme
	   (require "prims.ss"
		    (prefix ast: "ast.ss")
		    (lib "match.ss")
		    (lib "pretty.ss")
		    )
	   (provide typecheck-all)

	   (define (typecheck-all stmt)
	     (typecheck-ml stmt (empty-context)))

	   (define (typecheck-ml stmt context)
	     (match stmt
		    [($ ast:expression desc src)
		     (typecheck-expr desc context)]
		    [else
		     (pretty-print (list "Cannot typecheck: " stmt))]))

	   (define (typecheck-expr desc context)
	     (match desc
		    [($ ast:pexp_constant const)
		     (constant-check (eval const))]

		    [($ ast:pexp_tuple xlist)
		     (make-tuple (map typecheck-ml xlist (repeat context (length xlist))))]

		    [($ ast:pexp_ifthenelse test ifexp elseexp)
		     (let ([testt (typecheck-ml test context)]
			   [ifexpt (typecheck-ml ifexp context)]
			   [elseexpt (typecheck-ml elseexp context)])
		       (if (not (types-equal? testt "bool"))
			   (pretty-print (list "Expected bool but got: " testt))
			   (if (not (types-equal? ifexpt elseexpt))
			       (pretty-print (list "Expected " ifexpt " but got " elseexpt))
			       ifexpt)))]

		    [($ ast:pexp_apply proc lelist)
		     (let ([funt (typecheck-ml proc context)]
			   [argst (typecheck-lelist lelist context)])
		       (typecheck-application funt argst))]

		    [($ ast:pexp_let rec bindings expr)
		     (let ([all-bindings (typecheck-bindings rec bindings context)])
		       (typecheck-ml expr (append all-bindings context)))]
		    [else
		     (pretty-print (list "Cannot typecheck expression: " desc))]))

	   (define (typecheck-lelist lelist context)
	     (if (null? lelist)
		 null
		 (cons (typecheck-ml (cdar lelist) context) (typecheck-lelist (cdr lelist) context))))

	   (define (typecheck-application funt argst)
	     (if (not (arrow? funt))
		 (pretty-print (list "Expected function but got: " funt))
		 (let ([arglist (arrow-arglist funt)])
		   (if (= (length arglist) (length argst))
		       (if (compare-each arglist argst)
			   (arrow-result funt))
		       (pretty-print (list "Expected " (length arglist) " arguments but found " (length argst)))))))

	   (define (typecheck-bindings rec bindings context)
	     (if (null? bindings)
		 null
		 (let* ([cur-bind (car bindings)]
			[constraint (get-constraint (car cur-bind))])
		   (if rec
		       (error "Cannot do rec yet!")
		       (let ([rtype (typecheck-ml (cdr cur-bind) context)])
			 (if (tvar? constraint)
			     (cons (cons (car cur-bind) rtype) (typecheck-bindings rec (cdr bindings) context))
			     (if (types-equal? constraint rtype)
				 (cons (cons (car cur-bind) rtype) (typecheck-bindings rec (cdr bindings) context))
				 (error (format "This expression has type ~a but is here used with type ~a" rtype constraint)))))))))

	   (define (get-constraint pattern)
	     (match (ast:pattern-ppat_desc pattern)
		    [($ ast:ppat_var variable)
		     (make-tvar 'a)]
		    [($ ast:ppat_tuple tlist)
		     (make-tuple (map get-constraint tlist))]
		    [($ ast:ppat_constraint pat ct)
		     null]
		    [($ ast:ppat_any dummy)
		     (make-tvar 'a)]
		    [else
		      (let [(src-loc (ast:pattern-ppat_src pattern))]
			(raise-syntax-error #f "Not a variable" pattern))]))
		    
		   
	   (define (compare-each firstlist secondlist)
	     (if (null? firstlist)
		 #t
		 (if (not (equal? (car firstlist) (car secondlist)))
		     (pretty-print (list "Expected " (car firstlist) "but found " (car secondlist)))
		     (compare-each (cdr firstlist) (cdr secondlist)))))
	     
	   (define (types-equal? t1 t2)
	     (cond
	      [(and (tvar? t1) (not (tvar? t2)))
	       #t]
	      [(and (not (tvar? t1)) (tvar? t2))
	       #t]
	      [(and (tvar? t1) (tvar? t2))
	       (equal? (tvar-name t1) (tvar-name t2))]
	      [(and (arrow? t1) (arrow? t2))
	       (and (= (length (arrow-arglist t1) (arrow-arglist t2))) (eval `(and ,@(map types-equal? (arrow-arglist t1) (arrow-arglist t2)))) (types-equal? (arrow-result t1) (arrow-result t2)))]
	      [(and (tuple? t1) (tuple? t2))
	       (and (= (length (tuple-list t1) (tuple-list t2))) (eval `(and ,@(map types-equal? (tuple-list t1) (tuple-list t2)))))]
	      [(and (string? t1) (string? t2))
	       (equal? t1 t2)]
	      [else
	       #f]))

	   (define (constant-check const)
	     (cond
	      [(integer? const) "int"]
	      [(float? const) "float"]
	      [(char? const) "char"]
	      [(boolean? const) "bool"]
	      [(string? const) "string"]
	      [else "unit"]))
	   
	   (define (repeat x n)
	     (if (<= n 0)
		 null
		 (cons x (repeat x (- n 1)))))

	   (define-struct arrow (arglist result) (make-inspector))
	   (define-struct tvar (name) (make-inspector))

	   (define (empty-context) (lambda (lvar) (error "variable not bound"))) 
)