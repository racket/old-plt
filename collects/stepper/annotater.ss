(unit/sig stepper:annotate^
  (import [z : zodiac:system^]
	  mzlib:function^
	  [e : stepper:error^]
          [s : stepper:settings^]
	  stepper:shared^)
  
    ; ANNOTATE SOURCE CODE
  
  ; gensyms for annotation:
  
  ; the mutator-gensym is used in building the mutators that go into certain marks.
  ; (define mutator-gensym (gensym "mutator-"))
  
  ; the `closure-temp' symbol is used for the let which wraps created closures, so
  ; that we can stuff them into the hash table.
  
  ; closure-temp: uninterned-symbol
  
  (define closure-temp (gensym "closure-temp-"))
  
  ; var-set-union takes some lists of varrefs where no element appears twice in one list, and 
  ; forms a new list which is the union of the sets.  when a top-level and a non-top-level
  ; varref have the same name, we must keep the non-top-level one.
  
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
    (varref-remove* (varref-remove* a-set b-set) a-set))
      
  #| .
     somehow, we need to translate zodiac structures back into scheme structures so that 
     we can hand them off to mzscheme.  By rights, that's an aries-like job.  So, there
     are two ways we could do this.
     
     First, we could subsume the needed forms into zodiac, so that this
     module could simply deliver the annotated code to aries, BUT we really don't want aries
     to further annotate it with additional source-position information. At least, that 
     seems a bit silly to me.
     
     The other alternative, which I'm tentatively pursuing, is to do an aries-like translation
     right here, but it means I have to COPY CODE from aries.  In particular, I need this
     arglist->ilist function. Ick.
     |#
    
  ; check-for-keyword/both : (bool -> (z:varref -> void))
  
  (define check-for-keyword/both
    (lambda (disallow-procedures?)
      (lambda (id)
	(let ([real-id
	       (cond
		 [(z:binding? id) (z:binding-orig-name id)]
		 [(z:top-level-varref? id) (z:varref-var id)]
		 [(z:bound-varref? id)
		  (z:binding-orig-name (z:bound-varref-binding id))]
		 [(z:symbol? id)
		  (z:read-object id)]
		 [else
		  (e:internal-error id "Given in check-for-keyword")])])
	  (when (and (keyword-name? real-id)
		     (or disallow-procedures?
			 (let ([gdv (global-defined-value real-id)])
			   (or (syntax? gdv)
			       (macro? gdv)))))
	    (e:static-error id "Invalid use of keyword ~s"
			    real-id))))))
  
  (define check-for-keyword (check-for-keyword/both #t))
  (define check-for-keyword/proc (check-for-keyword/both #f))

  ; Here's more code copied directly from aries.  This is getting worse and worse.
  
  (define the-undefined-value (letrec ((x x)) x))
  
  (define-struct (undefined struct:exn) (id))
  (define signal-undefined (make-parameter #t))
  (define undefined-error-format
    "Variable ~s referenced before definition or initialization")
  
  (define-struct (not-boolean struct:exn) (val))
  (define signal-not-boolean (make-parameter #f))
  (define not-boolean-error-format "Condition value is neither #t nor #f: ~e")

  ; and yet more copied code.  Ugh.
  
  (define (is-unit-bound? varref)
    (and (z:top-level-varref/bind/unit? varref)
	 (z:top-level-varref/bind/unit-unit? varref)))
  
  
  (define-values (never-undefined? mark-never-undefined)
    (let-values ([(getter setter) (z:register-client 'stepper:never-undefined (lambda () #f))])
      (values
       (lambda (parsed) (getter (z:parsed-back parsed)))
       (lambda (parsed) (setter (z:parsed-back parsed) #t)))))
   
  (define (interlace a b)
    (foldr (lambda (a b built)
             (cons a (cons b built)))
           null
           a
           b))
      
  (define all-defs-list-sym (gensym "all-defs-list-"))
  (define current-def-sym (gensym "current-def-"))

  (define (current-def-setter val)
    `(#%set! ,current-def-sym ,val))
  
  (define (closure-key-maker closure)
    closure)
  
  ; debug-key: this key will be used as a key for the continuation marks.
  
  (define debug-key (gensym "debug-key-"))

  ; make-debug-info builds the thunk which will be the mark at runtime.  It contains 
  ; a source expression (in the parsed zodiac format) and a set of z:varref/value pairs.
  ;((z:parsed (union (list-of z:varref) 'all) (list-of z:varref) (list-of z:varref) symbol) ->
  ; debug-info)
  
  (define (make-debug-info source tail-bound top-env free-vars label)
    (let* ([top-level-varrefs (filter z:top-level-varref? free-vars)]
           [bound-varrefs (filter z:bound-varref? free-vars)]
           [top-level-kept (if (eq? tail-bound 'all)
                               (var-set-union top-level-varrefs top-env)
                               null)]
           [lexical-kept (if (eq? tail-bound 'all)
                             bound-varrefs
                             (var-set-intersect bound-varrefs 
                                                tail-bound))]
           [kept-vars (append top-level-kept lexical-kept)]
           ; the reason I don't need var-set-union here is that these sets are guaranteed
           ; not to overlap.
            [var-clauses (map (lambda (x) 
                               (let ([var (z:varref-var x)])
                                 `(cons (#%lambda () ,var)
                                        (cons ,x
                                              null))))
                             kept-vars)])
      `(#%lambda () (list ,source (#%quote ,label) ,@var-clauses))))
  
  
  ; wrap-struct-form 
  
  (define (wrap-struct-form names annotated)
    (let* ([arg-temps (build-list (length names) get-arg-symbol)]
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
  

  ; annotate takes an expression to annotate and a `break' function which will be inserted in
  ; the code.  It returns an annotated expression, ready for evaluation.
  
  ; the zodiac-error-handler is an ugly hack and will hopefully disappear soon-ish.
  
  (define (annotate red-exprs parsed-exprs break)
    (local
	(  

         
         (define (make-break kind)
           `(#%lambda ()
             (,break (continuation-mark-set->list
                      (current-continuation-marks) 
                      (#%quote ,debug-key))
                     ,all-defs-list-sym
                     ,current-def-sym
                     (#%quote ,kind))))
         
         
  
         ; wrap creates the w-c-m expression.
         
         (define (simple-wcm-wrap debug-info expr)
           `(#%with-continuation-mark (#%quote ,debug-key) ,debug-info ,expr))
         
         (define (wcm-pre-break-wrap debug-info expr)
           (simple-wcm-wrap debug-info `(#%begin (,(make-break 'pre-break)) ,expr)))
         
         (define (break-wrap expr)
           `(#%begin (,(make-break 'normal)) ,expr))
         
         (define (simple-wcm-break-wrap debug-info expr)
           (simple-wcm-wrap debug-info (break-wrap expr)))
         
        
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
  
         ; find-defined-vars extracts a list of what variables an expression
         ; defines.  In the case of a top-level expression which does not
         ; introduce bindings, it lists the temporary variable which will
         ; be introduced to hold that binding.
         
         (define (find-defined-vars expr)
           (cond [(z:define-values-form? expr)
                  (map z:varref-var (z:define-values-form-vars expr))]
                 [else 
                  (list (top-level-exp-gensym-source expr))]))
         
         (define (top-defs exprs)
           (map find-defined-vars exprs))

         ; annotate/inner takes 
         ; a) a zodiac expression to annotate
         ; b) a list of all varrefs s.t. this expression is tail w.r.t. their bindings
         ;    or 'all to indicate that this expression is tail w.r.t. _all_ bindings.
         ; c) a list of all top-level variables which occur in the program
         ; d) a boolean indicating whether this expression will be the r.h.s. of a reduction
         ;    (and therefore should be broken before)
         ;
         ; it returns
         ; a) an annotated s-expression
         ; b) a list of varrefs for the variables which occur free in the expression
         ;
	 ;(z:parsed (union (list-of z:varref) 'all) (list-of z:varref) bool -> 
         ;          sexp (list-of z:varref))
	 
	 (define (annotate/inner expr tail-bound top-env pre-break?)
	   
           ; translate-varref: (bool bool -> sexp (listof varref))
	   
	   (let* ([tail-recur (lambda (expr) (annotate/inner expr tail-bound top-env #t))]
                  [define-values-recur (lambda (expr) (annotate/inner expr tail-bound top-env #f))]
                  [non-tail-recur (lambda (expr) (annotate/inner expr null top-env #f))]
                  [lambda-body-recur (lambda (expr) (annotate/inner expr 'all top-env #t))]
                  [make-debug-info-normal (lambda (free-vars)
                                            (make-debug-info expr tail-bound top-env free-vars 'none))]
                  [make-debug-info-app (lambda (tail-bound free-vars label)
                                         (make-debug-info expr tail-bound top-env free-vars label))]
                  [wcm-wrap (if pre-break?
                                wcm-pre-break-wrap
                                simple-wcm-wrap)]
                  [wcm-break-wrap (lambda (debug-info expr)
                                    (wcm-wrap debug-info (break-wrap expr)))]
                  
                  [translate-varref
                   (lambda (maybe-undef? top-level?)
                     (let* ([v (z:varref-var expr)]
                            [real-v (if (z:top-level-varref? expr)
                                        v
                                        (z:binding-orig-name
                                         (z:bound-varref-binding expr)))]
                            [free-vars (list expr)]
                            [debug-info (make-debug-info-normal free-vars)]
                            [annotated (if (and maybe-undef? (signal-undefined))
                                           `(#%if (#%eq? ,v ,the-undefined-value)
                                             (#%raise (,make-undefined
                                                       ,(format undefined-error-format real-v)
                                                       (#%current-continuation-marks)
                                                       (#%quote ,v)))
                                             ,v)
                                           v)])
                       (values (wcm-break-wrap debug-info annotated) free-vars)))])
	     
             ; find the source expression and associate it with the parsed expression
             
             (set-expr-read! expr (find-read-expr expr))
	     
	     (cond
	       
	       ; the variable forms 
	       
	       [(z:bound-varref? expr)
		(translate-varref 
		 (not (never-undefined? (z:bound-varref-binding expr)))
		 #f)]
	       
               [(z:top-level-varref? expr)
		(if (is-unit-bound? expr)
		    (translate-varref #t #f)
		    (begin
		      (check-for-keyword/proc expr)
		      (translate-varref #f #t)))]
	       
	       [(z:app? expr)
		(let+
		 ([val sub-exprs (cons (z:app-fun expr) (z:app-args expr))]
		  [val arg-temps (build-list (length sub-exprs) get-arg-symbol)]
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
		  [val final-app (break-wrap (simple-wcm-wrap app-debug-info arg-temp-syms))]
		  [val debug-info (make-debug-info-app new-tail-bound
                                                       (var-set-union free-vars arg-temps)
                                                       'not-yet-called)]
		  [val let-body (wcm-wrap debug-info `(#%begin ,@set!-list ,final-app))])
		 (values `(#%let ,let-clauses ,let-body) free-vars))]
	       
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
                (values (wcm-wrap (make-debug-info-normal null)
                                  `(#%quote ,(read->raw (z:quote-form-expr expr)))) 
                        null)]
	       
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
	       
	       ; there's no with-continuation-mark in beginner level.
	       
	       ; there are _definitely_ no units or classes
	       
	       [else
		(print-struct #t)
		(e:internal-error
		 expr
		 (format "stepper:annotate/inner: unknown object to annotate, ~a~n" expr))])))
         
         (define (annotate/top-level expr top-env)
           (let-values ([(annotated dont-care)
                         (annotate/inner expr 'all top-env #f)])
             (cond [(z:define-values-form? expr)
                    annotated]
                   [else
                    `(#%define-values ,(list (top-level-exp-gensym-source expr)) ,annotated)]))))
         
         ; body of local
         
      (let* ([defined-top-vars (top-defs parsed-exprs)]
             [top-env-vars (apply append defined-top-vars)]
             [top-env-varrefs (map create-bogus-top-level-varref top-env-vars)]
             [annotated-exprs (map (lambda (expr)
                                     (annotate/top-level expr top-env-varrefs))
                                   parsed-exprs)]
             [top-vars-annotation `((#%define-values (,all-defs-list-sym ,current-def-sym)
                                     (values (#%quote ,defined-top-vars) #f)))]
             [current-def-setters (build-list (length parsed-exprs) current-def-setter)]
             [top-annotated-exprs (interlace current-def-setters annotated-exprs)]
             [final-current-def-setter (current-def-setter (length parsed-exprs))]
             [final-break-exp (simple-wcm-break-wrap (make-debug-info '(#%quote no-source-expression)
                                                               'all 
                                                               (map create-bogus-top-level-varref
                                                                    (apply append defined-top-vars))
                                                               ()
                                                               'final) 
                                              'dont-evaluate-this-symbol)])
           
           (values (append top-vars-annotation top-annotated-exprs (list final-current-def-setter
                                                                         final-break-exp))
                   parsed-exprs))))
	 
  )
    
  
    
