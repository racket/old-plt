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
	    (if (<voidstruct>? value)
		(format "~e~n" (ml-tstyle firsttype))
		(format "~e = ~e~n" (ml-tstyle firsttype) (ml-style value))))))


      (define (ml-tstyle type)
	(cond
	 [(value-set? type)
	  (format "val ~e : ~e" (value-set-name type) (ml-tstyle (value-set-type type)))]
	 [(option? type)
	  (format "~e option" (ml-tstyle (option-type type)))]
	 [(ref? type)
	  (format "~e ref" (ml-tstyle (ref-type type)))]
	 [(tvariant? type)
	  (format "type ~e = ~e" (tvariant-name type)
		  (letrec ([vtypes (lambda (varlist)
				     (if (null? varlist)
					 ""
					 (if (null? (cdr varlist))
					     (ml-tstyle (car varlist))
					     (format "~e * ~e" (ml-tstyle (car varlist)) (vtypes (cdr varlist))))))]
			   [vars (lambda (names variants)
				   (if (null? names)
				       ""
				       (if (null? (cdr names))
					   (format "~e~e" (car names) (if (string? (car variants))
										   ""
										   (format " of ~e" (if (<tuple>? (tconstructor-argtype (car variants)))
													(vtypes (<tuple>-list (tconstructor-argtype (car variants))))
													(ml-tstyle (tconstructor-argtype (car variants)))))))
					   (format "~e~e | ~e" (car names) (if (string? (car variants))
									       ""
									       (format " of ~e" (if (<tuple>? (tconstructor-argtype (car variants)))
												    (vtypes (<tuple>-list (tconstructor-argtype (car variants))))
												    (ml-tstyle (tconstructor-argtype (car variants)))))) 
						   (vars (cdr names) (cdr variants))))))])
		    (vars (tvariant-varnames type) (tvariant-variantlist type))))]
	 [(tlist? type)
	  (format "~e list" (ml-tstyle (tlist-type type)))]
	 [(arrow? type)
	  (if (> (length (arrow-arglist type)) 1)
	      "Bad function!"
	      (if (arrow? (car (arrow-arglist type)))
		  (format "(~e) -> ~e" (car (arrow-arglist type)) (arrow-result type))
		  (format "~e -> ~e" (car (arrow-arglist type)) (arrow-result type))))]
	 [(<tuple>? type)
	  (letrec ([<tuple>format (lambda (tlist)
				  (let ([fstring (if (<tuple>? (car tlist))
						     (format "(~e)" (ml-tstyle (car tlist)))
						     (format "~e" (ml-tstyle (car tlist))))])
				    (if (null? (cdr tlist))
					fstring
					(format "~e, ~e" fstring (<tuple>format (cdr tlist))))))])
	    (<tuple>format (<tuple>-list type)))]
	 [(tvar? type)
	  (tvar-tbox type)]
	 [(syntax? type)
	  "Why the hell is a type a sytnax object?"]
	 [else type]))

      (define (ml-style value)
	(cond
	 [(option? value)
	  (if (<voidstruct>? (option-type value))
	      "None"
	      (format "Some ~e" (ml-style (option-type value))))]
	 [(box? value) (format "{contents = ~e}" (ml-style (unbox value)))]
	 [(<unit>? value) "()"]
	 [(list? value)
	  (letrec ([listformat (lambda (clist)
				 (if (null? clist)
				     "]"
				     (if (null? (cdr clist))
					 (format "~e~e" (ml-style (car clist)) (listformat (cdr clist)))
					 (format "~e; ~e" (ml-style (car clist)) (listformat (cdr clist))))))])
	    (string-append "[" (listformat value)))]
	 [(procedure? value) "<fun>"]
	 [(<tuple>? value)
	  (letrec ([<tuple>format (lambda (tlist)
				  (if (null? (cdr tlist))
				      (if (<tuple>? (car tlist))
					  (format "(~e)" (ml-style (car tlist)))
					  (format "~e" (ml-style (car tlist))))
				      (if (<tuple>? (car tlist))
					  (format "(~e), ~e" (ml-style (car tlist)) (<tuple>format (cdr tlist)))
					  (format "~e, ~e" (ml-style (car tlist)) (<tuple>format (cdr tlist))))))])
	    (<tuple>format (<tuple>-list value)))]
	 [(boolean? value)
	  (if value
	      "true"
	      "false")]
	 [(string? value)
	  value]
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
				     [cur-parse (parse-ml-port port name)]
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
          (define/public (get-language-position) (list "Dromedary"))
          (define/public (get-language-name) "Dromedary")
          (define/public (get-language-url) #f)
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
