(module annotate mzscheme
  (require (prefix utils: "utils.ss")
           "marks.ss"
           (prefix s: "model.ss")
           "shared.ss"
	   (lib "list.ss")
           "2vals.ss")

  (provide
   initial-env-package
   annotate)

;  (import [z : zodiac^]
;          [utils : stepper:cogen-utils^]
;          stepper:marks^
;          [s : stepper:model^]
;	  stepper:shared^
;          stepper:client-procs^)

  ; this procedure returns (map fn list), with all #f's stripped out.
  (define (mod-filter fn lst)
    (if (null? lst)
        null
        (let ([result (fn (car lst))]
              [rest (mod-filter fn (cdr lst))])
          (if result
              (cons result rest)
              rest))))
  
  ;; this looks wrong...
  (define (internal-error . x)
    (error 'annotater-internal-error "~s" x))

  ;; d->so just replicates the exp argument in a call to datum->syntax-object
  (define (d->so exp datum)
    (datum->syntax-object exp datum exp))
  
  ; ANNOTATE SOURCE CODE
  
  ; gensyms for annotation:
  
  ; the mutator-gensym is used in building the mutators that go into certain marks.
  ; (define mutator-gensym (gensym "mutator-"))
  
  ; the `closure-temp' symbol is used for the let which wraps created closures, so
  ; that we can stuff them into the hash table.
  
  ; closure-temp: uninterned-symbol
  
  (define closure-temp (gensym "closure-temp-"))
  
  ; 2vals-map : (('a -> (2vals 'b 'c)) ('a list)) -> (2vals ('b list) ('c list))
  ;  dual-map is like map, only for a procedure that returns (values a b), and its
  ;  result is (values a-list b-list)... the contract specifies this more clearly.
  
  (define (2vals-map f . lsts)
    (if (null? (car lsts))
        (2vals null null)
        (let*-2vals ([(a b) (apply f (map car lsts))]
                     [(a-rest b-rest) (apply dual-map f (map cdr lsts))])
          (2vals (cons a a-rest) (cons b b-rest)))))
  
  ; triple-map (('a -> (values 'b 'c 'd)) ('a list)) -> (values ('b list) ('c list) ('d list))
  
  (define (triple-map f . lsts)
    (letrec ([inr (lambda lsts
                    (if (null? (car lsts))
                        (values null null null)
                        (let*-values
                            ([(a b c) (apply f (map car lsts))]
                             [(a-rest b-rest c-rest) (apply inr (map cdr lsts))])
                          (values (cons a a-rest) (cons b b-rest) (cons c c-rest)))))])
      (apply inr lsts)))

  ; a BINDING is a syntax-object
  
  ; a BINDING-SET is (union 'all (listof BINDING))
  
  ; binding-set-union takes some lists of bindings where no element appears twice in one list, and 
  ; forms a new list which is the union of the sets.
  ; binding-set-union : (listof BINDING-SET) *-> BINDING-SET
  
  (define binding-set-union
    (lambda args
      (foldl binding-set-pair-union
	     null
	     args)))
  
  ; binding-set-pair-union: BINDING-SET BINDING-SET -> BINDING-SET
  
  (define (binding-set-pair-union a-set b-set)
    (cond [(or (eq? a-set 'all) (eq? b-set 'all)) 'all]
          [else (append a-set (remq* a-set b-set))]))
  

  ; binding-set-intersect : BINDING-SET BINDING-SET -> BINDING-SET
  
  (define (binding-set-intersect a-set b-set)
    (cond [(eq? a-set 'all) b-set]
          [(eq? b-set 'all) a-set]
          [else (remq* (remq* a-set b-set) b-set)]))
  
  ; binding-set-remove : BINDING-SET BINDING-SET -> BINDING-SET
  
  (define (binding-set-remove a-set b-set expr) ; removes a from b
    (cond [(eq? a-set 'all) null]
          [(eq? b-set 'all)
	   (internal-error expr "tried to remove finite set of bindings from 'all")]
          [else (remq* a-set b-set)]))
      
  ; WARNING: because of how syntax-property works, these properties will have a default value of #f.
  ; that's what we want, in this case.
  
  (define (never-undefined? stx)
    (syntax-property stx 'never-undefined))
  (define (mark-never-undefined parsed) 
    (syntax-property stx 'never-undefined #t))

  (define (lambda-bound-var? stx)
    (syntax-property stx 'lambda-bound-var))
  (define (mark-lambda-bound-var stx)
    (syntax-property stx 'lambda-bound-var #t))
  
  (define (interlace a b)
    (foldr (lambda (a b built)
             (cons a (cons b built)))
           null
           a
           b))
        
  (define (closure-key-maker closure)
    closure)

  ; make-debug-info builds the thunk which will be the mark at runtime.  It contains 
  ; a source expression and a set of binding/value pairs.
  ;(syntax-object BINDING-SET BINDING-SET BINDING-SET symbol boolean) -> debug-info)
     
  (define (make-debug-info source tail-bound free-bindings advance-warning label lifting?)
    (let* ([kept-bindings (if (eq? tail-bound 'all)
                              free-bindings
                              (binding-set-intersect tail-bound
                                                     free-bindings))]
           [var-clauses (map (lambda (x) 
                               (list x (d->so #f `(quote-syntax ,x))))
                             kept-bindings)]
           [let-bindings (filter (lambda (x) (not (lambda-bound-var? x))) 
                                 (append advance-warning kept-bindings))]
           [lifter-syms (map get-lifted-sym let-bindings)]
           [quoted-lifter-syms (map (lambda (b) 
                                      (d->so #f `(syntax-quote ,b))) lifter-syms)]
           [let-clauses (map list lifter-syms quoted-lifter-syms)])
      (make-full-mark source label (append var-clauses (if lifting? let-clauses null)))))
  
  ; cheap-wrap for non-debugging annotation
  
;  (define cheap-wrap
;    (lambda (zodiac body)
;      (when (not (z:zodiac? zodiac))
;        (error 'cheap-wrap "argument to cheap-wrap is not a zodiac expr: ~a" zodiac))
;      (let ([start (z:zodiac-start zodiac)]
;	    [finish (z:zodiac-finish zodiac)])
;	`(#%with-continuation-mark ,debug-key
;	  ,(make-cheap-mark (z:make-zodiac #f start finish))
;	  ,body))))
  
  ; wrap-struct-form 
  
  (define (wrap-struct-form names annotated)
    (let* ([arg-temps (build-list (length names) get-arg-binding)]
           [arg-temp-syms (map z:binding-var arg-temps)]
           [struct-proc-names (cdr names)]
           [closure-records (map (lambda (proc-name) (make-closure-record
                                                      proc-name 
                                                      (lambda () #f)
                                                      (eq? proc-name (car struct-proc-names))
                                                      #f))
                                 struct-proc-names)]
           [proc-arg-temp-syms (cdr arg-temp-syms)]
           [setters (map (lambda (arg-temp-sym closure-record)
                           `(,closure-table-put! ,arg-temp-sym ,closure-record))
                         proc-arg-temp-syms
                         closure-records)]
           [full-body (append setters (list `(values ,@arg-temp-syms)))])
      `(#%let-values ((,arg-temp-syms ,annotated)) ,@full-body)))
  
  (define initial-env-package null)
  
;  (define (flatten-unit-clauses clauses)
;    (if (null? clauses)
;        null
;        (if  (z:begin-form? (car clauses))
;             (append (flatten-unit-clauses (z:begin-form-bodies (car clauses)))
;                     (flatten-unit-clauses (cdr clauses)))
;             (cons (car clauses) (flatten-unit-clauses (cdr clauses))))))

  (define (extract-top-level-vars exprs)
    (apply append
           (map (lambda (expr)
                  (cond ([z:define-values-form? expr]
                         (z:define-values-form-vars expr))
                        (else null)))
                exprs)))
  
  ; annotate takes 
  ; a) a list of zodiac:read expressions,
  ; b) a list of zodiac:parsed expressions,
  ; c) a list of previously-defined variables, 
  ; d) a break routine to be called at breakpoints, and
  ; e) a symbol which indicates how to annotate the source.  Currently, there are three
  ;    styles: 'cheap-wrap, 'ankle-wrap, and 'foot-wrap.
  ; f) optionally, a list of symbols which modifies the annotation.  Currently, valid 
  ;    choices include: 'no-closure-capturing, which eliminates closure shadowing, and
  ;    'no-temps-for-varrefs, which prevents rewriting (to capture intermediate values)
  ;    of applications and ifs which consist
  ;    only of varrefs.  This one might go away later, or be toggled into a "no-opt" flag.
  ;
  ; actually, I'm not sure that annotate works for more than one expression, even though
  ; it's supposed to take a whole list.  I wouldn't count on it. Also, both the red-exprs
  ; and break arguments may be #f, the first during a zodiac:elaboration-evaluator call,
  ; the second during any non-stepper use.
  
  (define (annotate read-expr parsed-expr input-struct-proc-names break wrap-style . wrap-opts-list)
    (local
	((define cheap-wrap? (eq? wrap-style 'cheap-wrap))
         (define ankle-wrap? (eq? wrap-style 'ankle-wrap))
         (define foot-wrap? (eq? wrap-style 'foot-wrap))
         (define wrap-opts (cond [(null? wrap-opts-list) null]
                                 [(not (= (length wrap-opts-list) 1))
                                  (error 'annotate "wrong number of arguments.")]
                                 [(not (and (list? (car wrap-opts-list))
                                            (andmap symbol? (car wrap-opts-list))))
                                  (error 'annotate "wrap-opts-list argument must be a list of symbols. Given: ~a~n"
                                         (car wrap-opts-list))]
                                 [else (car wrap-opts-list)]))

         
         (define (make-break kind)
           (lambda returned-value-list
             (break (current-continuation-marks) debug-key kind returned-value-list)))

         ; wrap creates the w-c-m expression.
         
         (define (simple-wcm-wrap debug-info expr)
           (d->so `(with-continuation-mark ,debug-key ,debug-info ,expr)))
         
         (define (wcm-pre-break-wrap debug-info expr)
           (if break
               (simple-wcm-wrap debug-info (d->so expr `(begin (,(make-break 'result-break)) ,expr)))
               (simple-wcm-wrap debug-info expr)))
         
         (define (break-wrap expr)
           (if break
               (d->so expr `(begin (,(make-break 'normal-break)) ,expr))
               expr))
         
         (define (double-break-wrap expr)
           (if break
               (d->so expr `(begin (,(make-break 'double-break)) ,expr))
               expr))
         
         (define (simple-wcm-break-wrap debug-info expr)
           (simple-wcm-wrap debug-info (break-wrap expr)))
         
         (define (late-let-break-wrap var-names lifted-gensyms expr)
           (if break
               (let* ([interlaced (apply append (map list var-names lifted-gensyms))])
                 (d->so expr `(begin (,(make-break 'late-let-break) ,@interlaced) ,expr)))
               expr))
         
         (define (return-value-wrap expr)
           (if break
               (d->so expr
                      `(let* ([result ,expr])
                         (,(make-break 'result-break) result)
                         result))
               expr))

;  For Multiple Values:         
;           `(#%call-with-values
;             (#%lambda ()
;              expr)
;             (#%lambda result-values
;              (,(make-break 'result-break) result-values)
;              (#%apply #%values result-values))))

         
;         (define (find-read-expr expr)
;           (when (not (z:zodiac? expr))
;             (error 'find-read-expr "argument to find-read-expr is not a zodiac expr: ~a" expr))
;           (let ([offset (z:location-offset (z:zodiac-start expr))])
;             (let search-exprs ([exprs red-exprs])
;               (let* ([later-exprs (filter 
;                                    (lambda (expr) 
;                                      (<= offset (z:location-offset (z:zodiac-finish expr))))
;                                    exprs)]
;                      [expr 
;                       (car later-exprs)])
;                 (if (= offset (z:location-offset (z:zodiac-start expr)))
;                     expr
;                     (cond
;                       ((z:scalar? expr) (internal-error expr "starting offset inside scalar:" offset))
;                       ((z:sequence? expr) 
;                        (let ([object (z:read-object expr)])
;                            (cond
;                            ((z:list? expr) (search-exprs object))
;                            ((z:vector? expr) 
;                             (search-exprs (vector->list object))) ; can source exprs be here?
;                            ((z:improper-list? expr)
;                             (search-exprs (search-exprs object))) ; can source exprs be here? (is this a bug?)
;                            (else (internal-error expr "unknown expression type in sequence")))))
;                       (else (internal-error expr "unknown read type"))))))))
;  
;         (define (struct-procs-defined expr)
;           (if (and (z:define-values-form? expr)
;                    (z:struct-form? (z:define-values-form-val expr)))
;               (map z:varref-var (z:define-values-form-vars expr))
;               null))
;         
;         (define struct-proc-names (apply append input-struct-proc-names
;                                          (map struct-procs-defined parsed-exprs)))
         
;         (define (non-annotated-proc? varref)
;           (let ([name (z:varref-var varref)])
;             (or (and (s:check-pre-defined-var name)
;                      (not (eq? name 'apply)))
;                 (memq name struct-proc-names))))
         
         (define (top-level-annotate/inner expr)
           (annotate/inner expr 'all #f #t #f))
         
         ; annotate/inner takes 
         ; a) an expression to annotate
         ; b) a list of all bindings which this expression is tail w.r.t. 
         ;    or 'all to indicate that this expression is tail w.r.t. _all_ bindings.
         ; d) a boolean indicating whether this expression will be the r.h.s. of a reduction
         ;    (and therefore should be broken before)
         ; e) a boolean indicating whether this expression is top-level (and therefore should
         ;    not be wrapped, if a begin).
         ; g) information about the binding name of the given expression.  This is used 
         ;    to associate a name with a closure mark (though this may now be redundant)
         ;    and to set up a (let ([x y]) x) so that mzscheme gets the right inferred-name
         ;    for closures

         ; it returns (as a 2vals)
         ; a) an annotated s-expression
         ; b) a list of varrefs for the variables which occur free in the expression
         ;
	 ;(syntax-object BINDING-SET bool bool (union #f symbol (list binding symbol)) -> 
         ;          sexp (list-of z:varref))
         
	 (define (annotate/inner expr tail-bound pre-break? top-level? procedure-name-info)
	   
	   (let* ([tail-recur (lambda (expr) (annotate/inner expr tail-bound #t #f procedure-name-info))]
                  [define-values-recur (lambda (expr name) 
                                         (annotate/inner expr tail-bound #f #f name))]
                  [non-tail-recur (lambda (expr) (annotate/inner expr null #f #f #f))]
                  [result-recur (lambda (expr) (annotate/inner expr null #f #f procedure-name-info))]
                  [set!-rhs-recur (lambda (expr name) (annotate/inner expr null #f #f name))]
                  [let-rhs-recur (lambda (tail-bound)
                                   (lambda (expr bindings dyn-index-syms)
                                     (let* ([proc-name-info 
                                             (if (not (null? bindings))
                                                 (list (car bindings) (car dyn-index-syms))
                                                 #f)])
                                       (annotate/inner expr tail-bound #f #f proc-name-info))))]
                  [lambda-body-recur (lambda (expr) (annotate/inner expr 'all #t #f #f))]
                  ; note: no pre-break for the body of a let; it's handled by the break for the
                  ; let itself.
                  [let-body-recur (lambda (expr bindings) 
                                    (annotate/inner expr (binding-set-union tail-bound bindings) #f #f procedure-name-info))]
                  [cheap-wrap-recur (lambda (expr) (let-values ([(ann _) (tail-recur expr)]) ann))]
                  [no-enclosing-recur (lambda (expr) (annotate/inner expr 'all #f #f #f))]
                  [class-rhs-recur (lambda (expr read-name) (annotate/inner expr 'all #f #f (utils:read->raw read-name)))]
                  [make-debug-info-normal (lambda (free-bindings)
                                            (make-debug-info expr tail-bound free-bindings null 'none foot-wrap?))]
                  [make-debug-info-app (lambda (tail-bound free-bindings label)
                                         (make-debug-info expr tail-bound free-bindings null label foot-wrap?))]
                  [wcm-wrap (if pre-break?
                                wcm-pre-break-wrap
                                simple-wcm-wrap)]
                  [wcm-break-wrap (lambda (debug-info expr)
                                    (wcm-wrap debug-info (break-wrap expr)))]
                  [expr-cheap-wrap (lambda (annotated) (cheap-wrap expr annotated))]
                  [ankle-wcm-wrap (lambda (expr free-bindings)
                                    (simple-wcm-wrap (make-debug-info-normal free-bindings) expr))]
                  [appropriate-wrap (lambda (annotated free-bindings)
                                      (cond [cheap-wrap? (cheap-wrap expr annotated)]
                                            [ankle-wrap? (ankle-wcm-wrap annotated free-bindings)]
                                            [else (error 'appropriate-wrap "wrap is neither cheap nor ankle")]))]
                  [inferred-name-patch (lambda (annotated)
                                         (if (and procedure-name-info (not (memq 'no-closure-capturing wrap-opts)))
                                             (let ([name (ccond [(symbol? procedure-name-info) procedure-name-info]
                                                                [(and (list? procedure-name-info)
                                                                      (= (length procedure-name-info) 2))
                                                                 (car procedure-name-info)])])
                                               (d->so #f `(let* ([,name ,annotated]) ,name)))
                                             annotated))]
                  
                  ; The let transformation is complicated.
                  ; here's a sample transformation (not including 'break's):
                  ;(let-values ([(a b c) e1] [(d e) e2]) e3)
                  ;
                  ;turns into
                  ;
                  ;(let-values ([(a b c d e)
                  ;              (values *unevaluated* *unevaluated* *unevaluated* *unevaluated* *unevaluated*)])
                  ;  (with-continuation-mark 
                  ;   key huge-value
                  ;   (begin
                  ;     (set!-values (a b c) e1)
                  ;     (set!-values (d e) e2)
                  ;     e3)))
                  ;
                  ; note that this elaboration looks exactly like the one for letrec, and that's
                  ; okay, becuase expand guarantees that reordering them will not cause capture.
                  ; this is because a bound variable answers is considered bound by a binding only when
                  ; the pair answers true to bound-identifier=?, which is determined during (the first)
                  ; expand.
                  
                  ; another irritating point: the mark and the break that must go immediately 
                  ; around the body.  Irritating because they will be instantly replaced by
                  ; the mark and the break produced by the annotated body itself. However, 
                  ; they're necessary, because the body may not contain free references to 
                  ; all of the variables defined in the let, and thus their values are not 
                  ; known otherwise.  
                  ; whoops! hold the phone.  I think I can get away with a break before, and
                  ; a mark after, so only one of each.  groovy, eh?
                  
                  [let-abstraction
                   (lambda (vars-fn vals-fn body-fn output-identifier check-fn make-init-list)
                     (let*-values
                         ([(binding-sets) (vars-fn expr)]
                          [(binding-list) (apply append binding-sets)]
                          [(vals) (vals-fn expr)]
                          [(_1) (check-fn vals binding-list)]
                          [(lifted-gensym-sets) (map (lambda (x) (map get-lifted-sym x)) binding-sets)]
                          [(lifted-gensyms) (apply append lifted-gensym-sets)]
                          [(annotated-vals free-bindings-vals)
                           (dual-map (let-rhs-recur null) vals binding-sets lifted-gensym-sets)]
                          [(annotated-body free-bindings-body)
                           (let-body-recur (body-fn expr) binding-list)]
                          [(free-bindings) (remq* binding-list 
                                                  (apply binding-set-union free-bindings-body free-bindings-vals))])
                       (ccond [cheap-wrap?
                               (let* ([bindings
                                       (map (lambda (bindings val)
                                              `(,(map get-binding-name bindings) ,val))
                                            binding-sets
                                            annotated-vals)]
                                      [annotated
                                       `(,output-identifier ,bindings ,annotated-body)])
                                 (values (appropriate-wrap annotated free-bindings) free-bindings))]
                              [(or ankle-wrap? foot-wrap?)
                               (let* ([create-index-finder (lambda (binding)
                                                             `(,binding-indexer))]
                                      [unevaluated-list (make-init-list binding-names)]
                                      [outer-initialization
                                       (if ankle-wrap?
                                           `((,binding-names (#%values ,@unevaluated-list)))
                                           `([,(append lifted-gensyms binding-names)
                                              (#%values ,@(append (map create-index-finder binding-list)
                                                                  unevaluated-list))]))]
                                      [set!-clauses
                                       (map (lambda (binding-set val)
                                              `(#%set!-values ,(map get-binding-name binding-set) ,val))
                                            binding-sets
                                            annotated-vals)]
                                      ; time to work from the inside out again
                                      [middle-begin
                                       (double-break-wrap `(#%begin ,@set!-clauses ,(late-let-break-wrap binding-list
                                                                                                         lifted-gensyms
                                                                                                         annotated-body)))]
                                      [wrapped-begin
                                       (wcm-wrap (make-debug-info expr 
                                                                  (binding-set-union tail-bound binding-list)
                                                                  (binding-set-union free-bindings binding-list)
                                                                  null ; advance warning
                                                                  'let-body
                                                                  foot-wrap?)
                                                 middle-begin)]
                                      [whole-thing
                                       `(,output-identifier ,outer-initialization ,wrapped-begin)])
                                 (values whole-thing free-bindings))])))])
	     
             ; find the source expression and associate it with the parsed expression
             
             (when (and red-exprs foot-wrap?)
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
                       [free-bindings (cond [(z:bound-varref? expr)
                                             (list (z:bound-varref-binding expr))]
                                            [(utils:is-unit-bound? expr)
                                             (list (z:top-level-varref/bind-slot expr))]
                                            [else
                                             null])]
                       [annotated (if (and maybe-undef? (utils:signal-undefined))
                                      `(#%if (#%eq? ,v ,utils:the-undefined-value)
                                        (#%raise (,utils:make-undefined
                                                  ,(format utils:undefined-error-format real-v)
                                                  (#%current-continuation-marks)
                                                  (#%quote ,v)))
                                        ,v)
                                      v)]
                       [might-raise-exn? (or (and maybe-undef? (utils:signal-undefined)) truly-top-level?)])
                  (values (ccond [(or cheap-wrap? ankle-wrap?)
                                  (if might-raise-exn?
                                      (appropriate-wrap annotated free-bindings)
                                      annotated)]
                                 [foot-wrap?
                                  (wcm-break-wrap (make-debug-info-normal free-bindings) (return-value-wrap annotated))]) 
                          free-bindings))]
               
               ; the app form's elaboration looks like this, where M0 etc. stand for expressions, and t0 etc
               ; are temp identifiers that do not occur in the program:
               ; (M0 ...)
               ;
               ; goes to
               ;
               ;(let ([t0 *unevaluated*]
               ;      ...)
               ;  (with-continuation-mark
               ;   debug-key
               ;   huge-value
               ;   (set! t0 M0)
               ;   ...
               ;   (with-continuation-mark
               ;    debug-key
               ;    much-smaller-value
               ;    (t0 ...))))
               ; 
               ; 'break's are not illustrated.  An optimization is possible when all expressions M0 ... are
               ; varrefs.  In particular (where v0 ... are varrefs):
               ; (v0 ...)
               ;
               ; goes to
               ; 
               ; (with-continuation-mark
               ;  debug-key
               ;  debug-value
               ;  (v0 ...))
               ;
               ; in other words, no real elaboration occurs. Note that this doesn't work as-is for the
               ; stepper, because there's nowhere to hang the breakpoint; you want to see the break
               ; occur after all vars have been evaluated.  I suppose you could do (wcm ... (begin v0 ... (v0 ...)))
               ; where the second set are not annotated ... but stepper runtime is not at a premium.
               
               [(z:app? expr)
		(let*-values
                    ([(sub-exprs) (cons (z:app-fun expr) (z:app-args expr))]
                     [(annotated-sub-exprs free-bindings-sub-exprs)
                      (dual-map non-tail-recur sub-exprs)]
                     [(free-bindings) (apply binding-set-union free-bindings-sub-exprs)])
                  (values 
                   (ccond [cheap-wrap? (appropriate-wrap annotated-sub-exprs free-bindings)]
                          [(or ankle-wrap? foot-wrap?)
                           (if (and ankle-wrap?
                                    (andmap (lambda (expr) 
                                              (or (z:varref? expr)
                                                  (z:quote-form? expr)))
                                            sub-exprs)
                                    (memq 'no-temps-for-varrefs wrap-opts))
                               
                               ; this is the no-temps optimization:
                               ; (won't work for stepper unless no reductions happen on the vars in the app
                               
                               (let ([debug-info (make-debug-info-app tail-bound free-bindings 'called)])
                                 (wcm-break-wrap debug-info annotated-sub-exprs))
                               
                               (let* ([arg-temps (build-list (length sub-exprs) get-arg-binding)]
                                      [arg-temp-syms (map z:binding-var arg-temps)] 
                                      [let-clauses `((,arg-temp-syms 
                                                      (#%values ,@(map (lambda (x) *unevaluated*) arg-temps))))]
                                      [set!-list (map (lambda (arg-symbol annotated-sub-expr)
                                                        `(#%set! ,arg-symbol ,annotated-sub-expr))
                                                      arg-temp-syms annotated-sub-exprs)]
                                      [new-tail-bound (binding-set-union tail-bound arg-temps)]
                                      [app-debug-info (make-debug-info-app new-tail-bound arg-temps 'called)]
                                      [final-app (break-wrap (simple-wcm-wrap app-debug-info 
                                                                              (if (let ([fun-exp (z:app-fun expr)])
                                                                                    (and foot-wrap?
                                                                                         (z:top-level-varref? fun-exp)
                                                                                         (non-annotated-proc? fun-exp)))
                                                                                  (return-value-wrap arg-temp-syms)
                                                                                  arg-temp-syms)))]
                                      [debug-info (make-debug-info-app new-tail-bound
                                                                       (binding-set-union free-bindings arg-temps)
                                                                       'not-yet-called)]
                                      [let-body (wcm-wrap debug-info `(#%begin ,@set!-list ,final-app))])
                                 `(#%let-values ,let-clauses ,let-body)))])
                   free-bindings))]
	       
	       [(z:struct-form? expr)
		(let ([super-expr (z:struct-form-super expr)]
		      [raw-type (utils:read->raw (z:struct-form-type expr))]
		      [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
		  (if super-expr
		      (let*-values 
                          ([(annotated-super-expr free-bindings-super-expr) 
                            (non-tail-recur super-expr)]
                           [(annotated)
                            `(#%struct 
                              ,(list raw-type annotated-super-expr)
                              ,raw-fields)]
                           [(debug-info) (make-debug-info-normal free-bindings-super-expr)])
                        (values (ccond [(or cheap-wrap? ankle-wrap?) (appropriate-wrap annotated free-bindings-super-expr)]
                                       [foot-wrap? (wcm-wrap debug-info annotated)]) 
                                free-bindings-super-expr))
                      (let ([annotated `(#%struct ,raw-type ,raw-fields)])
                        (values (ccond [(or cheap-wrap? ankle-wrap?)
                                        (appropriate-wrap annotated null)]
                                       [foot-wrap?
                                        (wcm-wrap (make-debug-info-normal null) annotated)]) 
                                null))))]

	       [(z:if-form? expr) 
		(let*-values
                    ([(annotated-test free-bindings-test) 
                      (non-tail-recur (z:if-form-test expr))]
                     [(annotated-then free-bindings-then) 
                      (tail-recur (z:if-form-then expr))]
                     [(annotated-else free-bindings-else) 
                      (tail-recur (z:if-form-else expr))]
                     [(free-bindings) (binding-set-union free-bindings-test 
                                                         free-bindings-then 
                                                         free-bindings-else)]
                     [(if-temp-sym) (z:binding-var if-temp)]
                     [(inner-annotated) `(#%if ,if-temp-sym
                                          ,annotated-then
                                          ,annotated-else)]
                     [(annotated-2) (if (utils:signal-not-boolean)
                                        `(#%if (#%boolean? ,if-temp-sym)
                                          ,inner-annotated
                                          (#%raise (,utils:make-not-boolean
                                                    (#%format ,utils:not-boolean-error-format
                                                     ,if-temp-sym)
                                                    (#%current-continuation-marks)
                                                    ,if-temp-sym)))
                                        inner-annotated)]
                     [(test-is-varref?) (z:varref? (z:if-form-test expr))])
                  (if (or cheap-wrap? (and ankle-wrap?
                                           test-is-varref?
                                           (memq 'no-temps-for-varrefs wrap-opts)))
                      (let ([annotated (if (utils:signal-not-boolean)
                                           `(#%let ((,if-temp-sym ,annotated-test)) ,annotated-2)
                                           `(#%if ,annotated-test ,annotated-then ,annotated-else))])
                        (values (appropriate-wrap annotated free-bindings) free-bindings))
                      (let* ([annotated `(#%begin
                                          (#%set! ,if-temp-sym ,annotated-test)
                                          ,(break-wrap annotated-2))]
                             [debug-info (make-debug-info-app (binding-set-union tail-bound (list if-temp))
                                                              (binding-set-union free-bindings (list if-temp))
                                                              'none)]
                             [wcm-wrapped (wcm-wrap debug-info annotated)]
                             [outer-annotated `(#%let ((,if-temp-sym ,*unevaluated*)) ,wcm-wrapped)])
                        (values outer-annotated free-bindings))))]
	       
	       [(z:quote-form? expr)
                (let* ([raw (utils:read->raw (z:quote-form-expr expr))]
                       [annotated (if (or (number? raw) (procedure? raw))
                                      raw
                                      `(#%quote ,raw))])
                  (values (if (or cheap-wrap? ankle-wrap?)
                              annotated
                              (wcm-wrap (make-debug-info-normal null) annotated))
                          null))]
               
               [(z:begin-form? expr)
                (if top-level? 
                    (let*-values
                     ([(bodies) (z:begin-form-bodies expr)]
                      [(annotated-bodies free-bindings)
                       (dual-map (lambda (expr)
                                   (top-level-annotate/inner expr)) 
                                 bodies)])
                       (values `(#%begin ,@annotated-bodies)
                               (apply binding-set-union free-bindings)))
                    (let*-values 
                        ([(bodies) (z:begin-form-bodies expr)]
                         [(all-but-last-body last-body-list) 
                          (list-partition bodies (- (length bodies) 1))]
                         [(last-body) (car last-body-list)]
                         [(annotated-a free-bindings-a)
                          (dual-map non-tail-recur all-but-last-body)]
                         [(annotated-final free-bindings-final)
                          (tail-recur last-body)]
                         [(free-bindings) (apply binding-set-union free-bindings-final free-bindings-a)]
                         [(debug-info) (make-debug-info-normal free-bindings)]
                         [(annotated) `(#%begin ,@(append annotated-a (list annotated-final)))])
                       (values (ccond [(or cheap-wrap? ankle-wrap?) (appropriate-wrap annotated free-bindings)]
                                      [foot-wrap? (wcm-wrap debug-info annotated)])
                               free-bindings)))]

               [(z:begin0-form? expr)
                (let*-values
                    ([(bodies) (z:begin0-form-bodies expr)]
                     [(annotated-first free-bindings-first)
                      (result-recur (car bodies))]
                     [(annotated-bodies free-bindings-lists)
                      (dual-map non-tail-recur (cdr bodies))]
                     [(free-bindings) (apply binding-set-union free-bindings-first free-bindings-lists)]
                     [(debug-info) (make-debug-info-normal free-bindings)]
                     [(annotated) `(#%begin0 ,annotated-first ,@annotated-bodies)])
                  (values (ccond [(or cheap-wrap? ankle-wrap?) (appropriate-wrap annotated free-bindings)]
                                 [foot-wrap?
                                  (wcm-wrap debug-info annotated)])
                          free-bindings))]
               
                      
               [(z:let-values-form? expr)
                (let-abstraction z:let-values-form-vars
                                 z:let-values-form-vals
                                 z:let-values-form-body
                                 '#%let*-values
                                 (lambda (vals binding-list)
                                   (for-each utils:check-for-keyword binding-list)
                                   (for-each mark-never-undefined binding-list))
                                 (lambda (bindings)
                                   (build-list (length bindings) 
                                               (lambda (_) *unevaluated*))))]
               
               [(z:letrec-values-form? expr)
                (let-abstraction z:letrec-values-form-vars
                                 z:letrec-values-form-vals
                                 z:letrec-values-form-body
                                 '#%letrec-values
                                 (lambda (vals binding-list)
                                   (when (andmap z:case-lambda-form? vals)
                                     (for-each mark-never-undefined binding-list))
                                   (for-each utils:check-for-keyword binding-list))
                                 (lambda (bindings)
                                   bindings))]
               
	       [(z:define-values-form? expr)  
		(let*-values
                    ([(vars) (z:define-values-form-vars expr)]
                     [(_1) (map utils:check-for-keyword vars)]
                     [(binding-names) (map z:varref-var vars)]
                     [(val) (z:define-values-form-val expr)]
                     [(annotated-val free-bindings-val)
                      (define-values-recur val (if (not (null? binding-names))
                                                   (car binding-names)
                                                   #f))])
                  (values
                   (if  (and foot-wrap? (z:struct-form? val))
                        `(#%define-values ,binding-names
                          ,(wrap-struct-form binding-names annotated-val)) 
                         `(#%define-values ,binding-names ,annotated-val))
                   free-bindings-val))]
	       
	       [(z:set!-form? expr)
                (utils:check-for-keyword (z:set!-form-var expr)) 
                (let*-values 
                    ([(var) (z:set!-form-var expr)]
                     [(v) (translate-varref var)]
                     [(annotated-body rhs-free-bindings)
                      (set!-rhs-recur (z:set!-form-val expr) (if (z:top-level-varref? var)
                                                                 (z:varref-var var)
                                                                 (z:binding-orig-name (z:bound-varref-binding var))))]
                     [(free-bindings) (binding-set-union (if (z:top-level-varref? var)
                                                             null
                                                             (list (z:bound-varref-binding var)))
                                                         rhs-free-bindings)]
                     [(debug-info) (make-debug-info-normal free-bindings)]
                     [(annotated) `(#%set! ,v ,annotated-body)])
                   (values (ccond [(or cheap-wrap? ankle-wrap?) (appropriate-wrap annotated free-bindings)]
                                  [foot-wrap?
                                   (wcm-wrap (make-debug-info-normal free-bindings) annotated)])
                           free-bindings))]
                
               [(z:case-lambda-form? expr)
		(let*-values 
                    ([(annotated-cases free-bindings-cases)
                      (dual-map
                       (lambda (arglist body)
                         (let ([binding-list (z:arglist-vars arglist)]
                               [args (utils:arglist->ilist arglist)])
                           (utils:improper-foreach utils:check-for-keyword args) 
                           (utils:improper-foreach mark-never-undefined args)
                           (let*-values
                               ([(annotated free-bindings)
                                 (lambda-body-recur body)]
                                [(new-free-bindings) (binding-set-remove binding-list free-bindings expr)]
                                [(new-annotated) (list (utils:improper-map get-binding-name args) annotated)]) 
                             (values new-annotated new-free-bindings))))
                       (z:case-lambda-form-args expr)
                       (z:case-lambda-form-bodies expr))]
                     [(annotated-case-lambda) (cons '#%case-lambda annotated-cases)] 
                     [(new-free-bindings) (apply binding-set-union free-bindings-cases)]
                     [(closure-info) (make-debug-info-app 'all new-free-bindings 'none)]
                     [(procedure-name) (ccond [(symbol? procedure-name-info)
                                               procedure-name-info]
                                              [(pair? procedure-name-info)
                                               (car procedure-name-info)]
                                              [(eq? procedure-name-info #f)
                                               #f])]
                     [(closure-storing-proc)
                       (lambda (closure debug-info . extra)
                         (closure-table-put! closure (make-closure-record 
                                                      procedure-name
                                                      debug-info
                                                      #f
                                                      (if (not (null? extra))
                                                          (car extra)
                                                          #f)))
                         closure)])
                  (if cheap-wrap?
                      (values annotated-case-lambda new-free-bindings)
                      (let* ([patched (inferred-name-patch annotated-case-lambda)]
                             [captured
                              (if (memq 'no-closure-capturing wrap-opts)
                                  annotated-case-lambda
                                  (cond [(symbol? procedure-name-info)
                                         `(,closure-storing-proc ,patched ,closure-info)]
                                        [(pair? procedure-name-info)
                                         (if foot-wrap?
                                             `(,closure-storing-proc ,patched ,closure-info ,(cadr procedure-name-info))
                                             `(,closure-storing-proc ,patched ,closure-info #f))]
                                        [else
                                         `(,closure-storing-proc ,patched ,closure-info)]))])
                        (values 
                         (ccond [foot-wrap? 
                                 (wcm-wrap (make-debug-info-normal new-free-bindings)
                                           captured)]
                                [ankle-wrap? 
                                 captured]) ; no wcm is needed because evaluation of closures cannot cause exceptions.
                         new-free-bindings))))]
                                
               ; the annotation for w-c-m is insufficient for
               ; stepping: there must be an intermediate let & set!s to
               ; allow the user to see the computed values for the key and the
               ; value.
               
               [(z:with-continuation-mark-form? expr)
                (let*-values
                    ([(annotated-key free-bindings-key)
                      (non-tail-recur (z:with-continuation-mark-form-key expr))]
                     [(annotated-val free-bindings-val)
                      (non-tail-recur (z:with-continuation-mark-form-val expr))]
                     [(annotated-body free-bindings-body)
                      (result-recur (z:with-continuation-mark-form-body expr))]
                     [(free-bindings) (binding-set-union free-bindings-key free-bindings-val free-bindings-body)]
                     [(debug-info) (make-debug-info-normal free-bindings)]
                     [(annotated) `(#%with-continuation-mark
                                    ,annotated-key
                                    ,annotated-val
                                    ,annotated-body)])
                   (values (ccond [(or cheap-wrap? ankle-wrap?) (appropriate-wrap annotated free-bindings)]
                                  [foot-wrap? 
                                   (wcm-wrap debug-info annotated)])
                           free-bindings))]
               
               [foot-wrap?
                (internal-error expr "cannot annotate units or classes in foot-wrap mode")]
               
               [(z:unit-form? expr)
                (let* ([imports (z:unit-form-imports expr)]
                       [exports (map (lambda (export)
                                       (list (translate-varref (car export))
                                             (z:read-object (cdr export))))
                                     (z:unit-form-exports expr))]
                       [_ (for-each utils:check-for-keyword imports)]
                       [clauses (flatten-unit-clauses (z:unit-form-clauses expr))]
                       [unit-vars (extract-top-level-vars clauses)])
                  (cond 
                   [cheap-wrap?
                    (let*-values
                        ([(annotated-clauses free-vars-lists)
                          (dual-map (lambda (expr)
                                      (top-level-annotate/inner expr))
                                    clauses)]
                         [(annotated) `(#%unit
                                        (import ,@(map get-binding-name imports))
                                        (export ,@exports)
                                        ,@annotated-clauses)]
                         [(free-bindings) 
                          (binding-set-remove
                           (binding-set-union imports
                                              (map z:top-level-varref/bind-slot 
                                                   (filter z:top-level-varref/bind?
                                                           unit-vars)))
                           (apply binding-set-union free-vars-lists)
                           expr)])
                      (values (appropriate-wrap annotated free-bindings) free-bindings))]
                   [ankle-wrap?
                    (let*-values
                        ([(process-clause)
                          (lambda (clause)
                            (if (z:define-values-form? clause)
                                (let*-values
                                    ([(binding-names) (map z:varref-var (z:define-values-form-vars clause))]
                                     [(annotated-rhs free-vars)
                                      (set!-rhs-recur (z:define-values-form-val clause) 
                                                      (if (null? binding-names)
                                                          #f
                                                          (car binding-names)))]
                                     [(set!-form) `(#%set!-values ,binding-names ,annotated-rhs)])
                                  (values (appropriate-wrap set!-form null) free-vars))
                                (let*-values
                                    ([(annotated free-vars) (non-tail-recur clause)])
                                  (values annotated free-vars))))]
                         [(annotated-clauses free-vars-clauses)
                          (dual-map process-clause clauses)]
                         [(unit-var-names) (map z:varref-var unit-vars)]
                         [(unit-var-slots) (map z:top-level-varref/bind-slot unit-vars)]
                         [(initial-define-list)
                          (if (null? unit-var-names)
                              null
                              (list `(#%define-values ,unit-var-names (#%values ,@unit-var-names))))]
                         [(free-vars)
                          (apply binding-set-union unit-var-slots free-vars-clauses)]
                         [(annotated-innards)
                          (if (null? annotated-clauses)
                              null
                              (append initial-define-list
				      (list (appropriate-wrap `(#%begin ,@annotated-clauses) free-vars))))]
                         [(annotated-unit)
                          `(#%unit
                            (import ,@(map get-binding-name imports))
                            (export ,@exports)
                            ,@annotated-innards)]
                         [(free-vars-outside)
                          (binding-set-remove
                           (binding-set-union imports
                                              unit-var-slots)
                           free-vars
                           expr)])
                      (values annotated-unit free-vars-outside))]
                   [else (error expr 
                                "annotation style ~a for unit is neither 'ankle-wrap or 'cheap-wrap"
                                wrap-style)]))]
                         
               [(z:compound-unit-form? expr)
                (let ((imports (map get-binding-name
                                    (z:compound-unit-form-imports expr)))
                      (links (z:compound-unit-form-links expr))
                      (exports (z:compound-unit-form-exports expr)))
                  (let*-values
                      ([(new-links links-free-vars-sets)
                        (dual-map
                         (lambda (link-clause)
                           (let*-values 
                               ([(tag) (utils:read->raw (car link-clause))]
                                [(sub-unit sub-unit-free-vars) 
                                 (no-enclosing-recur (cadr link-clause))]
                                [(imports)
                                 (map (lambda (import)
                                        (if (z:lexical-varref? import)
                                            (translate-varref import)
                                            `(,(utils:read->raw (car import))
                                              ,(utils:read->raw (cdr import)))))
                                      (cddr link-clause))])
                             (values `(,tag (,sub-unit ,@imports)) sub-unit-free-vars)))
                           links)]
                       [(new-exports)
                        (map
                         (lambda (export-clause)
                           `(,(utils:read->raw (car export-clause))
                             (,(utils:read->raw (cadr export-clause))
                              ,(utils:read->raw (cddr export-clause)))))
                         exports)]
                       [(e) `(#%compound-unit
                              (import ,@imports)
                              (link ,@new-links)
                              (export ,@new-exports))]
                       [(free-bindings) (apply binding-set-union links-free-vars-sets)])
                    (values (appropriate-wrap e free-bindings) free-bindings)))]
               
               [(z:invoke-unit-form? expr)
                (let*-values ([(unit-exp free-bindings-unit) (non-tail-recur (z:invoke-unit-form-unit expr))]
                              [(free-bindings-other) (mod-filter (lambda (v) (cond [(z:bound-varref? v) (z:bound-varref-binding v)]
                                                                                   [(utils:is-unit-bound? v) 
                                                                                    (z:top-level-varref/bind-slot v)]
                                                                                   [else #f]))
                                                                 (z:invoke-unit-form-variables expr))]
                              [(annotated) `(#%invoke-unit ,unit-exp
                                             ,@(map translate-varref
                                                    (z:invoke-unit-form-variables expr)))]
                              [(free-bindings) (binding-set-union free-bindings-other free-bindings-unit)])
                  (values (appropriate-wrap annotated free-bindings) free-bindings))]
               
               [(z:interface-form? expr)
                (let*-values ([(vars) (z:interface-form-variables expr)]
                              [(super-exprs super-expr-free-sets)
                               (dual-map non-tail-recur (z:interface-form-super-exprs expr))]
                              [(_) (for-each utils:check-for-keyword vars)]
                              [(annotated) `(#%interface ,super-exprs
                                             ,@(map utils:read->raw vars))]
                              [(free-bindings) (apply binding-set-union super-expr-free-sets)])
                  (values (appropriate-wrap annotated free-bindings) free-bindings))]
               
               [(z:class*/names-form? expr)
                (let* ([process-arg
                        (lambda (element)
                          (if (pair? element)
                              (let-values ([(annotated free-vars) 
                                            (no-enclosing-recur (cdr element))])
                                (values
                                 (and (utils:check-for-keyword (car element))
                                      (list (get-binding-name (car element))
                                            annotated))
                                 free-vars))
                              (values
                               (and (utils:check-for-keyword element)
                                    (get-binding-name element))
                               null)))]
                       [paroptarglist->ilist
                        (lambda (paroptarglist)
                          (cond
                            ((z:sym-paroptarglist? paroptarglist)
                             (process-arg (car (z:paroptarglist-vars paroptarglist))))
                            ((z:list-paroptarglist? paroptarglist)
                             (dual-map process-arg (z:paroptarglist-vars paroptarglist)))
                            ((z:ilist-paroptarglist? paroptarglist)
                             (let-values ([(vars free-var-sets)
                                           (dual-map process-arg (z:paroptarglist-vars paroptarglist))])
                               (values
                                (let loop ((vars vars))
                                  (if (null? (cddr vars))
                                      (cons (car vars) (cadr vars))
                                      (cons (car vars) (loop (cdr vars)))))
                                free-var-sets)))
                            (else
                             (internal-error paroptarglist
					     "Given to paroptarglist->ilist"))))]
                       [process-clause
                        (lambda (clause)
                          (cond
                            [(z:public-clause? clause)
                             (let*-values ([(ann-exprs free-var-sets) 
                                            (dual-map class-rhs-recur 
                                                      (z:public-clause-exprs clause)
                                                      (z:public-clause-exports clause))])
                               (values 
                                `(public
                                   ,@(map (lambda (internal export expr)
                                            `((,(get-binding-name internal)
                                               ,(utils:read->raw export))
                                              ,expr))
                                          (z:public-clause-internals clause)
                                          (z:public-clause-exports clause)
                                          ann-exprs))
                                (z:public-clause-internals clause)
                                (apply binding-set-union free-var-sets)))]
                            [(z:override-clause? clause)
                             (let*-values ([(ann-exprs free-var-sets) 
                                            (dual-map class-rhs-recur 
                                                      (z:override-clause-exprs clause)
                                                      (z:override-clause-exports clause))])
                               (values
                                `(override
                                   ,@(map (lambda (internal export expr)
                                            `((,(get-binding-name internal)
                                               ,(utils:read->raw export))
                                              ,expr))
                                          (z:override-clause-internals clause)
                                          (z:override-clause-exports clause)
                                          ann-exprs))
                                (z:override-clause-internals clause)
                                (apply binding-set-union free-var-sets)))]
                            [(z:private-clause? clause)
                             (let*-values ([(ann-exprs free-var-sets) 
                                            (dual-map no-enclosing-recur (z:private-clause-exprs clause))])
                               (values
                                `(private
                                   ,@(map (lambda (internal expr)
                                            `(,(get-binding-name internal)
                                              ,expr))
                                          (z:private-clause-internals clause)
                                          ann-exprs))
                                (z:private-clause-internals clause)
                                (apply binding-set-union free-var-sets)))]
                            [(z:inherit-clause? clause)
                             (values
                              `(inherit
                                 ,@(map (lambda (internal inherited)
                                          `(,(get-binding-name internal)
                                            ,(utils:read->raw inherited)))
                                        (z:inherit-clause-internals clause)
                                        (z:inherit-clause-imports clause)))
                              (z:inherit-clause-internals clause)
                              null)]
                            [(z:rename-clause? clause)
                             (values
                              `(rename
                                ,@(map (lambda (internal import)
                                         `(,(get-binding-name internal)
                                           ,(utils:read->raw import)))
                                       (z:rename-clause-internals clause)
                                       (z:rename-clause-imports clause)))
                              (z:rename-clause-internals clause)
                              null)]
                            [(z:sequence-clause? clause)
                             (let*-values ([(ann-exprs free-var-sets) 
                                            (dual-map no-enclosing-recur (z:sequence-clause-exprs clause))])
                               (values
                                `(sequence
                                   ,@ann-exprs)
                                null
                                (apply binding-set-union free-var-sets)))]))]
                       [b-s-remove 
                        (lambda (a b) (binding-set-remove a b expr))])
                   (let*-values
                       ([(ann-super-expr free-bindings-super-expr)
                         (non-tail-recur (z:class*/names-form-super-expr expr))]
                        [(ann-interfaces free-binding-sets-interfaces)
                         (dual-map non-tail-recur (z:class*/names-form-interfaces expr))]
                        [(ann-clauses class-binding-sets free-binding-sets-clauses)
                         (triple-map process-clause (z:class*/names-form-inst-clauses expr))]
                        [(ann-args free-binding-sets-init-vars)
                         (paroptarglist->ilist (z:class*/names-form-init-vars expr))]
                        [(init-vars-bindings)
                         (map (lambda (b)
                                (if (pair? b) (car b) b))
                              (z:paroptarglist-vars (z:class*/names-form-init-vars expr)))]
                        [(free-bindings)
                         (apply binding-set-union
                                (b-s-remove (apply binding-set-union init-vars-bindings
                                                   (list (z:class*/names-form-this expr)
                                                         (z:class*/names-form-super-init expr))       
                                                   class-binding-sets)
                                            (apply binding-set-union 
                                                   (append free-binding-sets-init-vars
                                                           free-binding-sets-clauses)))
                                free-bindings-super-expr free-binding-sets-interfaces)]
                        [(annotated)
                         `(#%class*/names
                           (,(get-binding-name (z:class*/names-form-this expr))
                            ,(get-binding-name (z:class*/names-form-super-init expr)))
                           ,ann-super-expr
                           ,ann-interfaces
                           ,ann-args
                           ,@ann-clauses)])
                     (values (appropriate-wrap annotated free-bindings) free-bindings)))]          
	       
	       [else
		(internal-error
		 expr
                 "stepper:annotate/inner: unknown object to annotate, ~a~n"
                 expr)])))
         
         (define (annotate/top-level expr)
           (let-values ([(annotated dont-care)
                         (top-level-annotate/inner expr)])
             annotated)))
         
         ; body of local
(let* ([annotated-exprs (map (lambda (expr)
                                     (annotate/top-level expr))
                                   parsed-exprs)])
  ;(printf "annotated: ~n~a~n" (car annotated-exprs))
  (values annotated-exprs struct-proc-names))))
  
)))
