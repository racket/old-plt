(unit/sig stepper:annotate^
  (import [z : zodiac:system^]
	  mzlib:function^
	  [e : stepper:error^]
          [utils : stepper:cogen-utils^]
          [s : stepper:model^]
	  stepper:shared^
          stepper:client-procs^)
  
  ; ANNOTATE SOURCE CODE
  
  ; gensyms for annotation:
  
  ; the mutator-gensym is used in building the mutators that go into certain marks.
  ; (define mutator-gensym (gensym "mutator-"))
  
  ; the `closure-temp' symbol is used for the let which wraps created closures, so
  ; that we can stuff them into the hash table.
  
  ; closure-temp: uninterned-symbol
  
  (define closure-temp (gensym "closure-temp-"))
  
  ; list-partition takes a list and a number, and returns two lists; the first one contains the
  ; first n elements of the list, and the second contains the remainder.  If n is greater than
  ; the length of the list, the exn:application:mismatch exception is raised.
  
  (define (list-partition lst n)
    (if (= n 0)
        (values null lst)
        (if (null? lst)
            (list-ref lst 0) ; cheap way to generate exception
            (let-values ([(first rest) (list-partition (cdr lst) (- n 1))])
              (values (cons (car lst) first) rest)))))
  
  ; dual-map : (('a -> (values 'b 'c)) ('a list)) -> (values ('b list) ('c list))
  
  (define (dual-map f lst) 
    (if (null? lst)
        (values null null)
        (let-values ([(a b) (f (car lst))]
                     [(a-rest b-rest) (dual-map f (cdr lst))])
          (values (cons a a-rest) (cons b b-rest)))))
    
  ; var-set-union takes some lists of varrefs where no element appears twice in one list, and 
  ; forms a new list which is the union of the sets.
  
  ; varref-remove* removes the varrefs in a-set from the varrefs in b-set
  
  (define (varref-remove* a-set b-set)
    (remove* a-set 
             b-set 
             (lambda (a-var b-var) 
               (eq? (z:varref-var a-var)
                    (z:varref-var b-var)))))
    
  (define (varref-set-pair-union a-set b-set)
    (cond [(or (eq? a-set 'all) (eq? b-set 'all)) 'all]
          [else (append a-set (varref-remove* a-set b-set))]))
  
  (define var-set-union
    (lambda args
      (foldl varref-set-pair-union
	     null
	     args)))
  
  (define (var-set-intersect a-set b-set)
    (varref-remove* (varref-remove* a-set b-set) b-set))
      
  (define never-undefined? never-undefined-getter)
  (define (mark-never-undefined parsed) (never-undefined-setter parsed #t))
   
  (define (interlace a b)
    (foldr (lambda (a b built)
             (cons a (cons b built)))
           null
           a
           b))
        
  (define (closure-key-maker closure)
    closure)
  
  ; debug-key: this key will be used as a key for the continuation marks.
  
  (define debug-key (gensym "debug-key-"))

  ; translate-varref : returns the name the varref will get in the final output
  
  (define (translate-varref expr)
    (if (or (z:top-level-varref? expr) (not (z:parsed-back expr))) ; top level or extra-bogus varrefs
        (z:varref-var expr)
        (utils:get-binding-name (z:bound-varref-binding expr))))
  
  ; bindings->varrefs : turn a list of bindings into a list of bogus varrefs
  
  (define (bindings->varrefs bindings)
    (map create-bogus-bound-varref
         (map z:binding-var bindings)
         bindings))
  
  ; make-debug-info builds the thunk which will be the mark at runtime.  It contains 
  ; a source expression (in the parsed zodiac format) and a set of z:varref/value pairs.
  ;((z:parsed (union (list-of z:varref) 'all) (list-of z:varref) (list-of z:varref) symbol) ->
  ; debug-info)
  
  (define (make-debug-info source tail-bound free-vars label)
    (let* ([kept-vars (if (eq? tail-bound 'all)
                          free-vars
                          (var-set-intersect tail-bound    ; the order of these arguments is important if
                                                           ; the tail-bound varrefs don't have bindings
                                             free-vars))]
            [var-clauses (map (lambda (x) 
                               (let ([var (translate-varref x)])
                                 `(cons (#%lambda () ,var)
                                        (cons ,x
                                              null))))
                             kept-vars)])
      `(#%lambda () (list ,source (#%quote ,label) ,@var-clauses))))
  
  
  ; wrap-struct-form 
  
  (define (wrap-struct-form names annotated)
    (let* ([arg-temps (build-list (length names) get-arg-varref)]
           [arg-temp-syms (map z:varref-var arg-temps)]
           [struct-proc-names (cdr names)]
           [closure-records (map (lambda (proc-name) `(,make-closure-record
                                                       (#%quote ,proc-name) 
                                                       (#%lambda () #f)
                                                       ,(eq? proc-name (car struct-proc-names))))
                                 struct-proc-names)]
           [proc-arg-temp-syms (cdr arg-temp-syms)]
           [setters (map (lambda (arg-temp-sym closure-record)
                           `(,closure-table-put! ,arg-temp-sym ,closure-record))
                         proc-arg-temp-syms
                         closure-records)]
           [full-body (append setters (list `(values ,@arg-temp-syms)))])
      `(#%let-values ((,arg-temp-syms ,annotated)) ,@full-body)))

  ; update-closure-record-name : adds a name to an existing closure table record,
  ; if there is one for that value.
  
  (define (update-closure-record-name value name)
    (let* ([closure-record (closure-table-lookup value)]
           [old-name (closure-record-name closure-record)])
      (if old-name
          (e:internal-error "closure-record already has a name: ~a" old-name)
          (set-closure-record-name! closure-record name))))
  
  
  (define initial-env-package null)
  
  ; annotate takes a list of zodiac:read expressions, a list of zodiac:parsed expressions,
  ; a list of previously-defined variables, and a break routine to be called at breakpoints.
  ; actually, I'm not sure that annotate works for more than one expression, even though
  ; it's supposed to take a whole list.  I wouldn't count on it. Also, both the red-exprs
  ; and break arguments may be #f, the first during a zodiac:elaboration-evaluator call,
  ; the second during any non-stepper use.
  
  (define (annotate red-exprs parsed-exprs input-struct-proc-names break)
    (local
	(  
         (define (make-break kind)
           `(#%lambda returned-value-list
             (,break (continuation-mark-set->list
                      (current-continuation-marks) 
                      (#%quote ,debug-key))
                     (#%quote ,kind)
                     returned-value-list)))
  
         ; wrap creates the w-c-m expression.
         
         (define (simple-wcm-wrap debug-info expr)
           `(#%with-continuation-mark (#%quote ,debug-key) ,debug-info ,expr))
         
         (define (wcm-pre-break-wrap debug-info expr)
           (if break
               (simple-wcm-wrap debug-info `(#%begin (,(make-break 'result-break)) ,expr))
               (simple-wcm-wrap debug-info expr)))
         
         (define (break-wrap expr)
           (if break
               `(#%begin (,(make-break 'normal)) ,expr)
               expr))
         
         (define (simple-wcm-break-wrap debug-info expr)
           (simple-wcm-wrap debug-info (break-wrap expr)))
         
         (define (return-value-wrap expr)
           (if break
               `(#%let* ([result ,expr])
                 (,(make-break 'result-break) result)
                 result)
               expr))

;  For Multiple Values:         
;           `(#%call-with-values
;             (#%lambda ()
;              expr)
;             (#%lambda result-values
;              (,(make-break 'result-break) result-values)
;              (#%apply #%values result-values))))

         (define (find-read-expr expr)
           (let ([offset (z:location-offset (z:zodiac-start expr))])
             (let search-exprs ([exprs red-exprs])
               (let* ([later-exprs (filter 
                                    (lambda (expr) 
                                      (<= offset (z:location-offset (z:zodiac-finish expr))))
                                    exprs)]
                      [expr 
                       (car later-exprs)])
                 (if (= offset (z:location-offset (z:zodiac-start expr)))
                     expr
                     (cond
                       ((z:scalar? expr) (e:static-error "starting offset inside scalar:" offset))
                       ((z:sequence? expr) 
                        (let ([object (z:read-object expr)])
                            (cond
                            ((z:list? expr) (search-exprs object))
                            ((z:vector? expr) 
                             (search-exprs (vector->list object))) ; can source exprs be here?
                            ((z:improper-list? expr)
                             (search-exprs (search-exprs object))) ; can source exprs be here?
                            (else (e:static-error "unknown expression type in sequence" expr)))))
                       (else (e:static-error "unknown read type" expr))))))))
  
         (define (struct-procs-defined expr)
           (if (and (z:define-values-form? expr)
                    (z:struct-form? (z:define-values-form-val expr)))
               (map z:varref-var (z:define-values-form-vars expr))
               null))
         
         (define struct-proc-names (apply append input-struct-proc-names
                                          (map struct-procs-defined parsed-exprs)))
         
         (define (non-annotated-proc? varref)
           (let ([name (z:varref-var varref)])
             (or (s:check-pre-defined-var name)
                 (memq name struct-proc-names))))
         
         ; annotate/inner takes 
         ; a) a zodiac expression to annotate
         ; b) a list of all varrefs s.t. this expression is tail w.r.t. their bindings
         ;    or 'all to indicate that this expression is tail w.r.t. _all_ bindings.
         ; c) a list of bound-varrefs of 'floating' variables; i.e. lexical bindings  NO: TAKEN OUT
         ;    whose value must be captured in order to reconstruct outer expressions. 
         ;    Necessitated by 'unit', useful for 'letrec*-values'.
         ; d) a boolean indicating whether this expression will be the r.h.s. of a reduction
         ;    (and therefore should be broken before)
         ; e) a boolean indicating whether this expression is top-level (and therefore should
         ;    not be wrapped, if a begin).
         ;
         ; it returns
         ; a) an annotated s-expression
         ; b) a list of varrefs for the variables which occur free in the expression
         ;
	 ;(z:parsed (union (list-of z:varref) 'all) (list-of z:varref) bool bool -> 
         ;          sexp (list-of z:varref))
	 
	 (define (annotate/inner expr tail-bound pre-break? top-level?)
	   
	   (let* ([tail-recur (lambda (expr) (annotate/inner expr tail-bound #t #f))]
                  [define-values-recur (lambda (expr) (annotate/inner expr tail-bound #f #f))]
                  [non-tail-recur (lambda (expr) (annotate/inner expr null #f #f))]
                  [lambda-body-recur (lambda (expr) (annotate/inner expr 'all #t #f))]
                  [let-body-recur (lambda (expr vars) (annotate/inner expr (var-set-union tail-bound vars) #t #f))]
                  [make-debug-info-normal (lambda (free-vars)
                                            (make-debug-info expr tail-bound free-vars 'none))]
                  [make-debug-info-app (lambda (tail-bound free-vars label)
                                         (make-debug-info expr tail-bound free-vars label))]
                  [wcm-wrap (if pre-break?
                                wcm-pre-break-wrap
                                simple-wcm-wrap)]
                  [wcm-break-wrap (lambda (debug-info expr)
                                    (wcm-wrap debug-info (break-wrap expr)))])
	     
             ; find the source expression and associate it with the parsed expression
             
             (when red-exprs
               (set-expr-read! expr (find-read-expr expr)))
	     
	     (cond
	       
	       ; the variable forms 
	       
               [(z:varref? expr)
                (let* ([v (translate-varref expr)]
                       [real-v (if (z:top-level-varref? expr)
                                   v
                                   (z:binding-orig-name
                                    (z:bound-varref-binding expr)))]
                       [maybe-undef? (or (and (z:bound-varref? expr) 
                                              (not (never-undefined? (z:bound-varref-binding expr))))
                                         (utils:is-unit-bound? expr))]
                       [truly-top-level? (and (z:top-level-varref? expr) (not (utils:is-unit-bound? expr)))]
                       [_ (when truly-top-level?
                            (utils:check-for-syntax-or-macro-keyword expr))]
                       [free-vars (list expr)]
                       [debug-info (make-debug-info-normal free-vars)]
                       [annotated (if (and maybe-undef? (utils:signal-undefined))
                                      `(#%if (#%eq? ,v ,utils:the-undefined-value)
                                        (#%raise (,utils:make-undefined
                                                  ,(format utils:undefined-error-format real-v)
                                                  (#%current-continuation-marks)
                                                  (#%quote ,v)))
                                        ,v)
                                      v)])
                  (values (wcm-break-wrap debug-info (return-value-wrap annotated)) free-vars))]

               [(z:app? expr)
		(let+
		 ([val sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
		  [val arg-temps (build-list (length sub-exprs) get-arg-varref)]
                  [val arg-temp-syms (map z:varref-var arg-temps)] 
		  [val let-clauses (map (lambda (sym) `(,sym (#%quote ,*unevaluated*))) arg-temp-syms)]
		  [val pile-of-values
		       (map (lambda (expr) 
			      (let-values ([(annotated free) (non-tail-recur expr)])
				(list annotated free)))
			    sub-exprs)]
		  [val annotated-sub-exprs (map car pile-of-values)]
		  [val free-vars (apply var-set-union (map cadr pile-of-values))]
		  [val set!-list (map (lambda (arg-symbol annotated-sub-expr)
					`(#%set! ,arg-symbol ,annotated-sub-expr))
				      arg-temp-syms annotated-sub-exprs)]
                  [val new-tail-bound (var-set-union tail-bound arg-temps)]
		  [val app-debug-info (make-debug-info-app new-tail-bound arg-temps 'called)]
                  [val annotate-app? (let ([fun-exp (z:app-fun expr)])
                                        (and (z:top-level-varref? fun-exp)
                                             (non-annotated-proc? fun-exp)))]
		  [val final-app (break-wrap (simple-wcm-wrap app-debug-info 
                                                              (if annotate-app?
                                                                  (return-value-wrap arg-temp-syms)
                                                                  arg-temp-syms)))]
		  [val debug-info (make-debug-info-app new-tail-bound
                                                       (var-set-union free-vars arg-temps)
                                                       'not-yet-called)]
		  [val let-body (wcm-wrap debug-info `(#%begin ,@set!-list ,final-app))]
                  [val let-exp `(#%let ,let-clauses ,let-body)])
		 (values let-exp free-vars))]
	       
	       [(z:struct-form? expr)
		(let ([super-expr (z:struct-form-super expr)]
		      [raw-type (utils:read->raw (z:struct-form-type expr))]
		      [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
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
                  [val inner-annotated `(#%if ,if-temp
                                         ,annotated-then
                                         ,annotated-else)]
		  [val annotated `(#%begin
                                   (#%set! ,if-temp ,annotated-test)
                                   ,(break-wrap
                                     (if (utils:signal-non-boolean)
                                         `(#%if (#%boolean? ,if-temp)
                                           ,inner-annotated
                                           (#%raise (,utils:make-not-boolean
                                                     (#%format ,utils:not-boolean-error-format
                                                      ,if-temp)
                                                     (#%current-continuation-marks)
                                                     ,if-temp)))
                                         inner-annotated)))]
                  [val if-temp-varref-list (list (create-bogus-bound-varref if-temp #f))]
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
                (values (wcm-wrap (make-debug-info-normal null)
                                  `(#%quote ,(utils:read->raw (z:quote-form-expr expr)))) 
                        null)]
               
               [(z:begin-form? expr)
                (if top-level? 
                    (let+ ([val bodies (z:begin-form-bodies expr)]
                           [val (values annotated-bodies free-vars)
                                (dual-map (lambda (expr)
                                            (annotate/inner expr 'all #f #t))
                                          bodies)])
                       (values `(#%begin ,@annotated-bodies)
                               (apply var-set-union free-vars)))
                    (let+ ([val bodies (z:begin-form-bodies expr)]
                           [val (values all-but-last-body last-body-list) 
                                (list-partition bodies (- (length bodies) 1))]
                           [val last-body (car last-body-list)]
                           [val (values annotated-a free-vars-a)
                                (dual-map non-tail-recur all-but-last-body)]
                           [val (values annotated-final free-vars-final)
                                (tail-recur last-body)]
                           [val free-vars (apply var-set-union free-vars-final free-vars-a)]
                           [val debug-info (make-debug-info-normal free-vars)])
                       (values (wcm-wrap debug-info 
                                         `(#%begin ,@(append annotated-a (list annotated-final))))
                               free-vars)))]

               [(z:begin0-form? expr)
                (let+ ([val bodies (z:begin0-form-bodies expr)]
                       [val (values annotated-bodies free-vars-lists)
                            (dual-map non-tail-recur bodies)]
                       [val free-vars (apply var-set-union free-vars-lists)]
                       [val debug-info (make-debug-info-normal free-vars)])
                   (values (wcm-wrap debug-info
                                     `(#%begin0 ,@annotated-bodies))
                           free-vars))]
               
               ; gott in himmel! this transformation is complicated.  Just for the record,
               ; here's a sample transformation:
               ;(let-values ([(a b c) e1] [(d e) e2]) e3)
               ;
               ;turns into
               ;
               ;(let-values ([(dummy1 dummy2 dummy3 dummy4 dummy5)
               ;              (values *undefined* *undefined* *undefined* *undefined* *undefined*)])
               ;  (with-continuation-mark 
               ;   key huge-value
               ;   (begin
               ;     (set!-values (dummy1 dummy2 dummy3) e1)
               ;     (set!-values (dummy4 dummy5) e2)
               ;     (let-values ([(a b c d e) (values dummy1 dummy2 dummy3 dummy4 dummy5)])
               ;       e3))))
               ;
               ; let me know if you can do it in less.
                      
               [(z:let-values-form? expr)
                (let+ ([val var-sets (z:let-values-form-vars expr)]
                       [val var-set-list (apply append var-sets)]
                       [val vals (z:let-values-form-vals expr)]
                       [_ (for-each utils:check-for-keyword var-set-list)]
                       [_ (for-each mark-never-undefined var-set-list)]
                       [val dummy-var-sets
                            (let ([counter 0])
                              (map (lambda (var-set)
                                     (map (lambda (var) 
                                            (begin0 
                                              (get-arg-varref counter)
                                              (set! counter (+ counter 1))))
                                          var-set))
                                   var-sets))]
                       [val dummy-var-list (apply append dummy-var-sets)]
                       [val outer-dummy-initialization
                            `([,(map z:varref-var dummy-var-list)
                               (#%values ,@(build-list (length dummy-var-list) 
                                                       (lambda (_) '(#%quote *undefined*))))])]
                       [val (values annotated-vals free-vars-vals)
                            (dual-map non-tail-recur vals)]
                       [val set!-clauses
                            (map (lambda (dummy-var-set val)
                                   `(#%set!-values ,(map z:varref-var dummy-var-set) ,val))
                                 dummy-var-sets
                                 annotated-vals)]
                       [val inner-transference
                            `([,(map utils:get-binding-name var-set-list) 
                               (values ,@(map z:varref-var dummy-var-list))])]
                       [val (values annotated-body free-vars-body)
                            (let-body-recur (z:let-values-form-body expr) 
                                            (bindings->varrefs var-set-list))]
                       ; time to work from the inside out again
                       [val inner-let-values
                            `(#%let-values ,inner-transference ,annotated-body)]
                       [val middle-begin
                            `(#%begin ,@set!-clauses ,inner-let-values)]
                       [val free-vars (apply var-set-union (varref-remove* (bindings->varrefs var-set-list) free-vars-body)
                                             free-vars-vals)]
                       [val wrapped-begin
                            (wcm-wrap (make-debug-info-app (var-set-union tail-bound dummy-var-list)
                                                           (var-set-union free-vars dummy-var-list)
                                                           'none)
                                      middle-begin)]
                       [val whole-thing
                            `(#%let-values ,outer-dummy-initialization ,wrapped-begin)])
                   (values whole-thing free-vars))]
               
               [(z:letrec-values-form? expr)
                ; Are all RHSes values? ...
                (when (andmap z:case-lambda-form? (z:letrec-values-form-vals expr))
                  ; ...yes , mark vars as never undefined.
                  ; (We do this before annotating any RHS!)
                  (for-each (lambda (vars)
                              (for-each mark-never-undefined vars))
                            (z:letrec-values-form-vars expr)))
                (let+ ([val var-sets (z:letrec-values-form-vars expr)]
                       [val var-set-list (apply append var-sets)]
                       [val vals (z:letrec-values-form-vals expr)]
                       [_ (when (andmap z:case-lambda-form? vals)
                            (for-each mark-never-undefined var-set-list))]
                       [_ (for-each utils:check-for-keyword var-set-list)]
                       [val outer-initialization
                            `((,var-set-list (values ,@var-set-list)))]
                       [val (values annotated-bodies free-vars-vals)
                            (dual-map non-tail-recur vals)]
                       [val set!-clauses
                            (map (lambda (var-set val)
                                   `(#%set!-values ,var-set ,val))
                                 var-sets
                                 annotated-bodies)]
                       [val (values annotated-body free-vars-body)
                            (let-body-recur (z:letrec-values-form-body expr) 
                                            (bindings->varrefs var-set-list))]
                       [val middle-begin
                            `(#%begin ,@set!-clauses ,annotated-body)]
                       [val free-vars (apply var-set-union free-vars-body free-vars-vals)]
                       [val wrapped-begin
                            (wcm-wrap (make-debug-info-app (var-set-union tail-bound var-set-list)
                                                           (var-set-union free-vars var-set-list))
                                      middle-begin)]
                       [val whole-thing
                            `(#%let-values ,outer-initialization ,wrapped-begin)])
                   (values whole-thing (varref-remove* var-set-list free-vars)))]
               
	       [(z:define-values-form? expr)
		(let+ ([val vars (z:define-values-form-vars expr)]
		       [val _ (map utils:check-for-keyword vars)]
		       [val var-names (map z:varref-var vars)]
                       
                       ; NB: this next recurrence is NOT really tail, but we cannot
                       ; mark define-values itself, so we mark the sub-expr as
                       ; if it was in tail posn (i.e., we must hold on to 
                       ; bindings).
		       
                       [val val (z:define-values-form-val expr)]
                       [val (values annotated-val free-vars-val)
			    (define-values-recur val)]
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
	       
	       [(z:set!-form? expr)
                (utils:check-for-keyword (z:set!-form-var expr))
                (let+ ([val v (translate-varref (z:set!-form-var expr))]
                       [val (values annotated rhs-free-vars)
                            (non-tail-recur (z:set!-form-val expr))]
                       [val free-vars (var-set-union (list (z:set!-form-var expr)) rhs-free-vars)]
                       [val debug-info (make-debug-info-normal free-vars)])
                   (values `(#%set! ,v ,annotated) free-vars))]
                
	       [(z:case-lambda-form? expr)
		(let* ([annotate-case
			(lambda (arglist body)
			  (let ([var-list (bindings->varrefs (z:arglist-vars arglist))]
                                [args (utils:arglist->ilist arglist)])
                            (utils:improper-foreach utils:check-for-keyword args)
                            (utils:improper-foreach mark-never-undefined args)
			    (let-values ([(annotated free-vars)
					  (lambda-body-recur body)])
			      (let* ([new-free-vars (varref-remove* var-list free-vars)]
                                     [new-annotated (list (utils:improper-map utils:get-binding-name args) 
                                                          annotated)]) 
				(list new-annotated new-free-vars)))))]
		       [pile-of-results (map annotate-case 
					     (z:case-lambda-form-args expr)
					     (z:case-lambda-form-bodies expr))]
		       [annotated-bodies (map car pile-of-results)]
		       [annotated-case-lambda (cons '#%case-lambda annotated-bodies)] 
		       [new-free-vars (apply var-set-union (map cadr pile-of-results))]
		       [closure-info (make-debug-info-app 'all new-free-vars 'none)]
                       [wrapped-annotated (wcm-wrap (make-debug-info-normal null)
                                                    annotated-case-lambda)]
		       [hash-wrapped `(#%let ([,closure-temp ,wrapped-annotated])
				       (,closure-table-put! (,closure-key-maker ,closure-temp) 
                                        (,make-closure-record 
                                         #f
                                         ,closure-info 
                                         #f))
				       ,closure-temp)])
		  (values hash-wrapped
			  new-free-vars))]
	       
               
               [(z:with-continuation-mark-form? expr)
                (let+ ([val (values annotated-key free-vars-key)
                            (non-tail-recur (z:with-continuation-mark-form-key expr))]
                       [val (values annotated-val free-vars-val)
                            (non-tail-recur (z:with-continuation-mark-form-val expr))]
                       [val (values annotated-body free-vars-body)
                            (non-tail-recur (z:with-continuation-mark-form-body expr))]
                       [val free-vars (var-set-union free-vars-key free-vars-val free-vars-body)]
                       [val debug-info (make-debug-info-normal free-vars)])
                   (values (wcm-wrap debug-info
                                     `(#%with-continuation-mark
                                       annotated-key
                                       annotated-val
                                       annotated-body))
                           free-vars))]
	       
	       ; there are _definitely_ no units or classes
	       
	       [else
		(print-struct #t)
		(e:internal-error
		 expr
		 (format "stepper:annotate/inner: unknown object to annotate, ~a~n" expr))])))
         
         (define (annotate/top-level expr)
           (let-values ([(annotated dont-care)
                         (annotate/inner expr 'all #f #t)])
             annotated)))
         
         ; body of local
         
      (let* ([annotated-exprs (map (lambda (expr)
                                     (annotate/top-level expr))
                                   parsed-exprs)])
        
        (values annotated-exprs
                struct-proc-names)))))
	 
