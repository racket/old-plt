(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
	   (lib "pretty.ss")
	   "compile.ss"
	   "typecheck.ss"
	   "parse.ss"
	   "prims.ss")

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) 
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) 
		       lang%))))
      
      (define current-type '("unit"))

      (define (format-typevalue value)
	(let ([firsttype (car current-type)])
	  (begin
	    (set! current-type (cdr current-type))
	    (if (void? value)
		(format "~a" (ml-tstyle firsttype))
		(format "~a = ~a" (ml-tstyle firsttype) (ml-style value))))))


      (define (ml-tstyle type)
	(cond
	 [(tvariant? type)
	  (format "type ~a = ~a" (tvariant-name type)
		  (letrec ([vtypes (lambda (varlist)
				     (if (null? varlist)
					 ""
					 (if (null? cdr varlist)
					     (ml-tstyle (car varlist))
					     (format "~a * ~a" (ml-tstyle (car varlist)) (vtypes (cdr varlist))))))]
			   [vars (lambda (names variants)
				   (if (null? names)
				       ""
				       (if (null? (cdr names))
					   (format "~a of ~a" (car names) (vtypes (tuple-list (tconstructor-argtype (car variants)))))
					   (format "~a of ~a | ~a" (car names) (vtypes (tuple-list (tconstructor-argtype (car variants)))) (vars (cdr names) (cdr variants))))))])
		    (vars (tvariant-varnames type) (tvariant-variantlist type))))]
	 [(tlist? type)
	  (format "~a list" (ml-tstyle (tlist-type type)))]
	 [(arrow? type)
	  (if (> (length (arrow-arglist type)) 1)
	      "Bad function!"
	      (if (arrow? (car (arrow-arglist type)))
		  (format "(~a) -> ~a" (car (arrow-arglist type)) (arrow-result type))
		  (format "~a -> ~a" (car (arrow-arglist type)) (arrow-result type))))]
	 [(tuple? type)
	  (letrec ([tupleformat (lambda (tlist)
				  (let ([fstring (if (tuple? (car tlist))
						     (format "(~a)" (ml-tstyle (car tlist)))
						     (format "~a" (ml-tstyle (car tlist))))])
				    (if (null? (cdr tlist))
					fstring
					(format "~a, ~a" fstring (tupleformat (cdr tlist))))))])
	    (tupleformat (tuple-list type)))]
	 [(tvar? type)
	  (tvar-tbox type)]
	 [else type]))

      (define (ml-style value)
	(cond
	 [(list? value)
	  (letrec ([listformat (lambda (clist)
				 (if (null? clist)
				     "]"
				     (if (null? (cdr clist))
					 (format "~a~a" (ml-style (car clist)) (listformat (cdr clist)))
					 (format "~a; ~a" (ml-style (car clist)) (listformat (cdr clist))))))])
	    (string-append "[" (listformat value)))]
	 [(procedure? value) "<fun>"]
	 [(tuple? value)
	  (letrec ([tupleformat (lambda (tlist)
				  (if (null? (cdr tlist))
				      (if (tuple? (car tlist))
					  (format "(~a)" (ml-style (car tlist)))
					  (format "~a" (ml-style (car tlist))))
				      (if (tuple? (car tlist))
					  (format "(~a), ~a" (ml-style (car tlist)) (tupleformat (cdr tlist)))
					  (format "~a, ~a" (ml-style (car tlist)) (tupleformat (cdr tlist))))))])
	    (tupleformat (tuple-list value)))]
	 [(boolean? value)
	  (if value
	      "true"
	      "false")]
	 [(string? value)
	  (format "\"~a\"" value)]
	 [else value]))

      (define lang%
        (class* object% (drscheme:language:language<%>)
          (define/public (config-panel parent)
            (case-lambda
	     [() null]
	     [(x) (void)]))
          (define/public (default-settings) null)
          (define/public (default-settings? x) #t)
          (define/public (front-end input settings)
            (let-values ([(port name current-parse)
                          (if (string? input)
                              (values (open-input-file input) #f null);(path->complete-path input))
                              (let ([text (drscheme:language:text/pos-text input)])
                                (values
                                 (open-input-string
                                  (send text
                                        get-text
                                        (drscheme:language:text/pos-start input)
                                        (drscheme:language:text/pos-end input)))
                                 text null)))])

	(lambda ()
		(if name
		    (if (null? current-parse)
			(if (eof-object? (peek-char port))
			    eof
			    (letrec ([flatten
				      (lambda (ccomp)
					(if (null? ccomp)
					    null
					    (if (list? (car ccomp))
						(append (flatten (car ccomp)) (flatten (cdr ccomp)))
						(cons (car ccomp) (flatten (cdr ccomp))))))]
				     [cur-parse (parse-ml-port port name)])
			     
			    (begin
			      (set! current-type (flatten (typecheck-all cur-parse name)))
			      (set! current-parse  (flatten (compile-all cur-parse name)))
				    
			      (let ([firstexp (car current-parse)])
				(begin
				  (set! current-parse (cdr current-parse))
				  firstexp)))))
			(let ([firstexp (car current-parse)])
			  (begin
			    (set! current-parse (cdr current-parse))
			    firstexp)))
		    (begin
		      (display "doing this")
		    (read-syntax input port))
		    ))))
	  (define/public (get-style-delta) #f)
          (define/public (get-language-position) (list "Dromedary"))
          (define/public (get-language-name) "Dromedary")
          (define/public (get-language-numbers) (list 10))
          (define/public (get-teachpack-names) null)
          (define/public (marshall-settings x) x)
          (define/public (on-execute settings run-in-user-thread)
            (dynamic-require '(lib "prims.ss" "dromedary") #f)
            (let ([path ((current-module-name-resolver) '(lib "prims.ss" "dromedary") #f #f)]
                  [n (current-namespace)])
              (run-in-user-thread
               (lambda ()
		 (error-display-handler 
		  (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
		 (current-eval 
		  (drscheme:debug:make-debug-eval-handler (current-eval)))
                 (with-handlers ([void (lambda (x)
                                         (printf "~a~n"
                                                 (exn-message x)))])
                   (namespace-attach-module n path)
		   (namespace-require 'mzscheme)
                   (namespace-require path))))))
          (define/public (render-value value settings port port-write) (begin
									 (pretty-print (format "~a ~a ~a ~a" value settings port port-write))
									 (display (format-typevalue (eval value)) port)));(format-typevalue value) port))
          (define/public (render-value/format value settings port port-write width) (display (format-typevalue value) port))
          (define/public (unmarshall-settings x) x)
	  (define/public (create-executable settings parent src-file dest-file)
	    '(let ([code (compile-simplified (simplify (parse-a60-file src-file)
						      base-importing-stx)
					    base-importing-stx)])
	      (make-embedding-executable dest-file
					 #f #f
					 '((#f (lib "base.ss" "algol60")))
					 null
					 (compile
					  `(module m (lib "base.ss" "algol60")
					     ,code))
					 (list "-mvqe" "(require m)"))))
	  (define/public (get-one-line-summary) "A Caml-like dialect of ML")
          
          (super-instantiate ()))))))
