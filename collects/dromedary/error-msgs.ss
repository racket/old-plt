#cs(module error-msgs mzscheme
	   (require
	    (prefix ast: "ast.ss")
	    "prims.ss"
	    (lib "pretty.ss")
	    (lib "match.ss")
	    )
	   (provide raise-error longident->src print-type)

	   ;;raise-error syntax-name * string * source-object * (U src syntax) -> void
	   (define (raise-error kind name message-string so src)
	     (raise-syntax-error kind message-string (make-error-so name so src)))

	   ;;make-error-so (syntax name parameter) * symbol * src -> syntax-object
	   (define (make-error-so name id src)
	     (datum->syntax-object #f id (build-src-list src name)))

	   ;; build-src-list src * (syntax name parameter) -> (U bool (list loc int int int int))
	   (define (build-src-list src name)
	     (cond
	      [(syntax? src) ;(pretty-print "syntax in build-src-list")
	       (list (syntax-source src)
		     (syntax-line src)
		     (syntax-column src)
		     (syntax-position src)
		     (syntax-span src))]
	      [(ast:src? src) (if (and (= (ast:src-line src) 0)
				       (= (ast:src-col src) 0)
				       (= (ast:src-pos src) 0)
				       (= (ast:src-span src) 0))
				  #f
				  (list name 
					(ast:src-line src)
					(ast:src-col src)
					(ast:src-pos src)
					(ast:src-span src)))]
	      [else #f]))

	   ;;longident->src longident -> src
	   (define (longident->src longident)
	     (match longident
		    [($ ast:lident name) (ast:make-src
					  (syntax-line name)
					  (syntax-column name)
					  (syntax-position name)
					  (syntax-span name))]
		    [($ ast:ldot first name) (let ([first-src (longident->src first)])
					       (ast:make-src
						(ast:src-line first-src)
						(ast:src-col first-src)
						(ast:src-pos first-src)
						(+ 1 (ast:src-span first-src) (syntax-span name))))]
		    [($ ast:lapply rand rator) (let ([rand-src (longident->src rand)]
						     [rator-src (longident->src rator)])
						 (ast:make-src
						  (ast:src-line rand-src)
						  (ast:src-col rand-src)
						  (ast:src-pos rand-src)
						  (+ 1 (ast:src-span rand-src) (ast:src-span rand-src))))]))
	     
	     


	   (define (print-type type)
	     (cond
;	      [(value-set? type) (let ([nmap (unconvert-tvars (value-set-type type) mappings)])
;				   (cons (make-value-set (value-set-name type) (car nmap)) (cdr nmap)))]
	      [(tvariant? type) (tvariant-name type)]
	      [(usertype? type) (format "~a~a" 
					(cond
					 [(= 0 (length (usertype-params type)))
					  ""]
					 [(= 1 (length (usertype-params type)))
					  (string-append (print-type (car (usertype-params type))) " ")]
					 [else
					  (letrec
					      ([writelist (lambda (alist)
							    (if (null? (cdr alist))
								(print-type (car alist))
								(format "~a~a" (format "~a, " (print-type (car alist)))
									       (writelist (cdr alist)))))])
					    (format "(~a) " (writelist (usertype-params type))))])
					 (usertype-name type))]
	      [(list? type) (map print-type type)]
	      [(string? type) type]
	      [(<tuple>? type) (format "~a" (letrec ([writetuple (lambda (tlist)
								   (if (null? (cdr tlist))
								       (print-type (car tlist))
								       (string-append (format "~a * " (print-type (car tlist)))
										      (writetuple (cdr tlist)))))])
					      (writetuple (<tuple>-list type))))]
	      [(arrow? type) (format "~a -> ~a" (print-type (car (arrow-arglist type))) (print-type (arrow-result type)))]
	      [(tlist? type) (format "~a list" (print-type (tlist-type type)))]
	      [(tarray? type) (format "~a array" (print-type (tarray-type type)))]
	      [(option? type) (format "~a option" (print-type (option-type type)))]
	      [(ref? type) (format "~a ref" (print-type (ref-type type)))]
;	      [(mlexn? type) (let ([newtypes (if (null? (tconstructor-argtype (mlexn-types type)))
;						(cons null mappings)
;						(unconvert-tvars (tconstructor-argtype (mlexn-types type)) mappings))])
;			       (cons (make-mlexn (mlexn-name type) (make-tconstructor (car newtypes) (tconstructor-result (mlexn-types type)))) (cdr newtypes)))]
	      [(tvar? type) 
;	       (format "tvar of ~a" (let ([dbox (tvar-tbox type)])
;				      (cond
;				       [(string? dbox) dbox]
;				       [(null? (unbox dbox)) "()"]
;				       [else
;					(print-type (unbox dbox))])))]
	       (let ([dbox (tvar-tbox type)])
		 (cond
		  [(string? dbox) dbox]
		  [else
		   (print-type (unbox dbox))]))]
	      [else ;(cons type mappings)]))
	       (format "~a" type)]))

)