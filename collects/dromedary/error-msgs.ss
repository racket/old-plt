#cs(module error-msgs mzscheme
	   (require
	    (prefix ast: "ast.ss")
	    (lib "pretty.ss")
	    (lib "match.ss")
	    )
	   (provide raise-error longident->src)

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
	     
	     
	     

			

)