; beginner-checker.ss

(unit/sig stepper:beginner-checker^
  (import [z : zodiac:system^]
          mzlib:function^
          [e : stepper:error]
          stepper:shared^)
  
    
  (define (check-expression-list exp-list struct-creator-records)
    (cond
      
      ; the variable forms 
      
      [(z:bound-varref? expr) 'ok]
      
      [(z:top-level-varref? expr) 'ok]

      [(z:app? expr)
       (let* ([fun (z:app-fun expr)]
              [num-args (length (z:app-args expr))])
         (if (z:top-level-varref? fun)
             (let ([fun-name (z:varref-var fun)])
               (if (memq fun-name (s:get-global-defined-vars))
                   (let ([fun-val (parameterize ([current-namespace (get-namespace)])
                                    (global-defined-value fun-name))])
                     (if (and (structure-constructor-procedure? fun-val)
                              (not (procedure-arity-includes? num-args)))
                         (e:static-error "wrong number of arguments (~s) passed to value constructor ~s."
                                         num-args
                                         fun-name)
                         'ok))
                   (let ([creator-record (assq fun-name struct-creator-records)])
                     (if (and creator-record
                              (not (= num-args (cadr creator-record))))
                         (e:static-error "wrong number of arguments (~s) passed to value constructor ~s."
                                         num-args
                                         fun-name)
                         'ok))))
             'ok))]

      [(z:struct-form? expr)
		(let ([super-expr (z:struct-form-super expr)]
		      [raw-type (read->raw (z:struct-form-type expr))]
		      [raw-fields (map read->raw (z:struct-form-fields expr))])
		  (if super-expr
		      (let+ ([val (values annotated-super-expr free-vars-super-expr) 
				  (non-tail-recur super-expr)]
			     [val annotated
				  `(#%struct 
				    ,(list raw-type annotated-super-expr)
				    ,raw-fields)]
                             [val debug-info (make-debug-info-normal free-vars-super-expr)])
			    (values (wcm-wrap debug-info annotated) free-vars-super-expr))
		      (values (wcm-wrap (make-debug-info-normal null)
                                        `(#%struct ,raw-type ,raw-fields)) 
                              null)))]

	       [(z:if-form? expr) 
		(let+
		 ([val (values annotated-test free-vars-test) 
		       (non-tail-recur (z:if-form-test expr))]
		  [val (values annotated-then free-vars-then) 
		       (tail-recur (z:if-form-then expr))]
		  [val (values annotated-else free-vars-else) 
		       (tail-recur (z:if-form-else expr))]
		  [val annotated `(#%begin
                                   (#%set! ,if-temp ,annotated-test)
                                   ,(break-wrap
                                     `(#%if (#%boolean? ,if-temp)
                                       (#%if ,if-temp
                                        ,annotated-then
                                        ,annotated-else)
                                       (#%raise (,make-not-boolean
                                                 (#%format ,not-boolean-error-format
                                                  ,if-temp)
                                                 (#%current-continuation-marks)
                                                 ,if-temp)))))]
                  [val if-temp-varref-list (list (create-bogus-bound-varref if-temp))]
		  [val free-vars (var-set-union if-temp-varref-list
                                                free-vars-test 
                                                free-vars-then 
                                                free-vars-else)]
		  [val debug-info (make-debug-info-app (var-set-union tail-bound if-temp-varref-list)
                                                       free-vars
                                                       'none)]
                  [val wcm-wrapped (wcm-wrap debug-info annotated)]
                  [val outer-annotated `(#%let ((,if-temp (#%quote ,*unevaluated*))) ,wcm-wrapped)])
		 (values outer-annotated free-vars))]
	       
	       [(z:quote-form? expr)
                (values `(#%quote ,(read->raw (z:quote-form-expr expr))) null)]
	       
	       ; there is no begin, begin0, or let in beginner. but can they be generated? 
	       ; for instance by macros? Maybe.
	       
	       [(z:define-values-form? expr)
		(let+ ([val vars (z:define-values-form-vars expr)]
		       [val _ (map check-for-keyword vars)]
		       [val var-names (map z:varref-var vars)]
                       
                       ; NB: this next recurrence is NOT really tail, but we cannot
                       ; mark define-values itself, so we mark the sub-expr as
                       ; if it was in tail posn (i.e., we must hold on to 
                       ; bindings).
		       
                       [val val (z:define-values-form-val expr)]
                       [val (values annotated-val free-vars-val)
			    (tail-recur val)]
		       [val free-vars (varref-remove* vars free-vars-val)])
                      (cond [(z:case-lambda-form? val)
                             (values `(#%define-values ,var-names
                                       (#%let ((,closure-temp ,annotated-val))
                                        (,update-closure-record-name ,closure-temp (#%quote ,(car var-names)))
                                        ,closure-temp))
                                     free-vars)]
                            [(z:struct-form? val)
                             (values `(#%define-values ,var-names
                                       ,(wrap-struct-form var-names annotated-val)) 
                                     free-vars)]
                            [else
                             (values `(#%define-values ,var-names
                                       ,annotated-val) 
                                     free-vars)]))]
	       
	       ; there is no set! in beginner level
	       
	       [(z:case-lambda-form? expr)
		(let* ([annotate-case
			(lambda (arglist body)
			  (let ([var-list (map create-bogus-bound-varref 
                                               (map z:binding-var
                                                    (z:arglist-vars arglist)))])
			    (let-values ([(annotated free-vars)
					  (lambda-body-recur body)])
			      (let* ([new-free-vars (varref-remove* var-list free-vars)]
                                     [args (arglist->ilist arglist)]
                                     [new-annotated (list (improper-map z:binding-var args) annotated)])
                                (improper-foreach check-for-keyword args)
                                (improper-foreach mark-never-undefined args)
				(list new-annotated new-free-vars)))))]
		       [pile-of-results (map annotate-case 
					     (z:case-lambda-form-args expr)
					     (z:case-lambda-form-bodies expr))]
		       [annotated-bodies (map car pile-of-results)]
		       [annotated-case-lambda (cons '#%case-lambda annotated-bodies)] 
		       [new-free-vars (apply var-set-union (map cadr pile-of-results))]
		       [closure-info (make-debug-info-app 'all new-free-vars 'none)]
		       [hash-wrapped `(#%let ([,closure-temp ,annotated-case-lambda])
				       (,closure-table-put! (,closure-key-maker ,closure-temp) 
                                        (,make-closure-record 
                                         #f
                                         ,closure-info 
                                         #f))
				       ,closure-temp)])
		  (values hash-wrapped
			  new-free-vars))]
	       
	       ; there's no with-continuation-mark in beginner level.
	       
	       ; there are _definitely_ no units or classes
	       
	       [else
		(print-struct #t)
		(e:internal-error
		 expr
		 (format "stepper:annotate/inner: unknown object to annotate, ~a~n" expr))])