(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
	   (lib "pretty.ss")
	   "compile.ss"
	   "typecheck.ss"
	   "parse.ss"
	   "prims.ss"
           (lib "string-constant.ss" "string-constants"))

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

      ;; format-typevalue: scheme-value -> string
      (define (format-typevalue value)
	(let ([firsttype (car current-type)])
	  (begin
	    (set! current-type (cdr current-type))
	    (if (<voidstruct>? value)
		(if (arrow? firsttype)
		    (format "~a = <fun>" (ml-tstyle firsttype))
		    (format "~a~n" (ml-tstyle firsttype)))
		(format "~a = ~a~n" (ml-tstyle firsttype) (ml-style value))))))

      
      ;; ml-tstyle: ml-type -> string
      (define (ml-tstyle type)
	(cond
	 [(mlexn? type)
	  (format "exception ~a" (mlexn-name type))]
	 [(value-set? type)
	  (format "val ~a : ~a" (value-set-name type) (ml-tstyle (value-set-type type)))]
	 [(option? type)
	  (format "~a option" (ml-tstyle (option-type type)))]
	 [(ref? type)
	  (format "~a ref" (ml-tstyle (ref-type type)))]
	 [(tvariant? type)
	  (format "type ~a~a = ~a" (tvariant-name type)
		  (if (null? (tvariant-params type))
		      ""
		      (let ([nparams (map ml-tstyle (tvariant-params type))])
			(printf "~a" (tvariant-params type))
			(printf "~a" nparams)
		      (format " ~a" nparams)))
		  (letrec ([vtypes (lambda (varlist)
				     (if (null? varlist)
					 ""
					 (if (null? (cdr varlist))
					     (format (if (<tuple>? (car varlist))
							 "(~a)"
							 "~a")
					     (ml-tstyle (car varlist)))
					     (format (if (<tuple>? (car varlist))
							 "(~a) * ~a"
							 "~a * ~a")
							 (ml-tstyle (car varlist)) (vtypes (cdr varlist))))))]
			   [vars (lambda (names variants)
				   (if (null? names)
				       ""
				       (if (null? (cdr names))
					   (format "~a~a" 
						   (car names) 
						   (if (or (string? (car variants)) (usertype? (car variants)))
						       ""
						       (format " of ~a" (if (<tuple>? (tconstructor-argtype (car variants)))
									    (vtypes (<tuple>-list (tconstructor-argtype (car variants))))
									    (ml-tstyle (tconstructor-argtype (car variants))))
						       
)))
					   (format "~a~a | ~a" (car names) (if (or (string? (car variants)) (usertype? (car variants)))
									       ""
									       (format " of ~a" (if (<tuple>? (tconstructor-argtype (car variants)))
												    (vtypes (<tuple>-list (tconstructor-argtype (car variants))))
												    (ml-tstyle (tconstructor-argtype (car variants)))))) 
						   (vars (cdr names) (cdr variants))))))])
		    (vars (tvariant-varnames type) (tvariant-variantlist type))))]
	 [(tlist? type)
	  (format "~a list" (let ([ns (ml-tstyle (tlist-type type))])
			      (if (<tuple>? (tlist-type type))
				  (format "(~a)" ns)
				  ns)))]
	 [(arrow? type)
	  (if (> (length (arrow-arglist type)) 1)
	      "Bad function!"
	      (if (arrow? (car (arrow-arglist type)))
		  (format "(~a) -> ~a" (ml-tstyle (car (arrow-arglist type))) (ml-tstyle (arrow-result type)))
		  (format "~a -> ~a" (ml-tstyle (car (arrow-arglist type))) (ml-tstyle (arrow-result type)))))]
	 [(<tuple>? type)
	  (letrec ([<tuple>format (lambda (tlist)
				  (let ([fstring (if (<tuple>? (car tlist))
						     (format "(~a)" (ml-tstyle (car tlist)))
						     (format "~a" (ml-tstyle (car tlist))))])
				    (if (null? (cdr tlist))
					fstring
					(format "~a * ~a" fstring (<tuple>format (cdr tlist))))))])
	    (<tuple>format (<tuple>-list type)))]
	 [(tvar? type)
	  (tvar-tbox type)]
	 [(usertype? type)
	  (let ([n (usertype-name type)])
	    (cond
	     [(null? (usertype-params type)) n]
	     [(= 1 (length (usertype-params type))) (format "~a ~a" (car (usertype-params type)) n)]
	     [else (format "(~a) ~a" 
			   (letrec ([writelist (lambda (alist)
						 (if (null? (cdr alist))
						     (format "~a" (ml-style (car alist)))
						     (string-append (format "~a, " (ml-tstyle (car alist)))
								    (writelist (cdr alist)))))])
			     (writelist (usertype-params type)))
			   n)]))]
	 [(syntax? type)
	  "Why the hell is a type a sytnax object?"]
	 [else type]))

      ;; ml-style: scheme-value -> string
      (define (ml-style value)
	(cond
	 [(<user-type>? value)
	  (let*-values ([(sinfo skipped) (struct-info value)]
			[(name-sym init-fnum auto-fnum acc-proc mut-proc immut-list sst skipped) (struct-type-info sinfo)]
			[(ftype) (acc-proc value 0)])
		       (string-append (format "~a" name-sym)
				      (cond
;				       [(<tuple>? ftype) (format " (~a)" (ml-style ftype))]
				       [(not ftype) ""]
				       [else (format " (~a)" (ml-style ftype))])))]
			
	 [(|Some|? value) (format "Some ~a" (let [(ns (ml-style (|Some|-tlist value)))]
					      (if (<tuple>? (|Some|-tlist value))
						  (format "(~a)" ns)
						  ns)))]
	 [(|None|? value) "None"]
	 [(box? value) (format "{contents = ~a}" (ml-style (unbox value)))]
	 [(<unit>? value) "()"]
	 [(list? value)
	  (letrec ([listformat (lambda (clist)
				 (if (null? clist)
				     "]"
				     (let* ([ns (ml-style (car clist))]
					    [tns (if (<tuple>? (car clist))
						     (format "(~a)" ns)
						     ns)])
				       (if (null? (cdr clist))
					   (format "~a~a" tns (listformat (cdr clist)))
					   (format "~a; ~a" tns (listformat (cdr clist)))))))])
	    (string-append "[" (listformat value)))]
	 [(procedure? value) ;(begin (pretty-print (format "procedure ~a" value))
				    "<fun>"
				;    )
				    ]
	 [(<tuple>? value)
	  (letrec ([<tuple>format (lambda (tlist)
				  (if (null? (cdr tlist))
				      (if (<tuple>? (car tlist))
					  (format "(~a)" (ml-style (car tlist)))
					  (format "~a" (ml-style (car tlist))))
				      (if (<tuple>? (car tlist))
					  (format "(~a), ~a" (ml-style (car tlist)) (<tuple>format (cdr tlist)))
					  (format "~a, ~a" (ml-style (car tlist)) (<tuple>format (cdr tlist))))))])
	    (<tuple>format (<tuple>-list value)))]
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
	  (define/public (front-end/complete-program input settings) (front-end input settings 0))
          (define/public (front-end/interaction input settings) (front-end input settings (drscheme:language:text/pos-start input)))
          (define/public (front-end input settings offset)
            (let-values ([(port name current-parse)
                          (if (string? input)
                              (values (open-input-file input) #f null);(path->complete-path input))
                              (let ([text (drscheme:language:text/pos-text input)])
;				(parse-error-port (lambda ()
;						    (open-input-string
;						     (send text get-text
;							   (drscheme:language:text/pos-start input)
;							   (drscheme:language:text/pos-end input)))))
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
				     [cur-parse (parse-ml-port port name offset)]
				     [syntaxify (lambda (stmt)
						  (datum->syntax-object #f
									stmt
									#f))])
			     
			    (begin
			      (set! current-type (flatten (typecheck-all cur-parse name)))
			      (set! current-parse (map syntaxify (flatten (compile-all cur-parse name))))
			      ;(pretty-print (format "current-type: ~e" current-type))
			      ;(pretty-print (format "current-parse: ~e" (map syntax-object->datum current-parse)))
				    
			      (let ([firstexp (car current-parse)])
				(begin
				  (set! current-parse (cdr current-parse))
				  firstexp)))))
			(let ([firstexp (car current-parse)])
			  (begin
			    (set! current-parse (cdr current-parse))
			    firstexp)))
		    (read-syntax input port))
		    )))
          
	  (define/public (get-style-delta) #f)
          (define/public (get-language-position) (list (string-constant experimental-languages) "Dromedary"))
          (define/public (get-language-name) "Dromedary")
          (define/public (get-language-url) #f)
          (define/public (get-language-numbers) (list 1000 20))
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
									 (display (ml-style value) port)));(format-typevalue value) port))
          (define/public (render-value/format value settings port port-write width)
		   (display (format-typevalue value) port))
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
