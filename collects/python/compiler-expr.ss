(module compiler-expr mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss") ;build-list
	   "compiler.ss"
	   "compiler-target.ss"
         ;  "empty-context.ss")
          ; "primitives.ss"
	   "runtime-support.ss"
           "runtime-context.ss"
           )
  
  (provide (all-defined))
  
  (define (py-so x) ;(datum->syntax-object #'here x)
                    ;(lambda (x) x)
                    ;x
;                    (datum->syntax-object #f x)
                    (datum->syntax-object (current-runtime-support-context) x)
                    )
  ; program identifiers should search the top level
;  (define program-context ;empty-context)
;                          #f)
  
  ;; unpack: (or symbol (listof symbol)) syntax-object -> sexp
  (define (unpack tuple var)
    (cond
      [(list? tuple) (map (lambda (i)
                            (unpack (list-ref tuple i)
                                    `(,(py-so 'python-index) ,var ,i)))
                          (build-list (length tuple) identity))]
      [else `[,(send tuple to-scheme) ,var]]))
  
  (define (target->parm-tup target)
    (cond
      [(is-a? target tidentifier%) (make-object identifier%
                                     (symbol->string (send target get-symbol))
                                     (send target get-start-pos)
                                     (send target get-end-pos))]
      [(or (is-a? target ttuple%)
           (is-a? target tlist-display%)) (map target->parm-tup
                                               (send target get-sub-targets))]
      [else (error "target->parm-tup invalid target")]))
  
  (define (target->parameters target)
    (make-object parameters%
      (list (list 'pos (target->parm-tup target)))
      (send target get-start-pos)
      (send target get-end-pos)))

  ;; generate-function-bindings: parameters% (listof syntax-object) (or false bindings-mixin%) -> (listof sexp)
  ;; generate the enclosing LET bindings for a function
  ;; if scope is not #f, its bindings are defined here (as void)
  (define (generate-function-bindings parms body-so scope)
    (let ([bindings
           (append (normalize-assoc-list
                    (flatten1
                     (map (lambda (tuple)
                            (unpack tuple
                                    (send (first-atom tuple) to-scheme)))
                          (filter list? (send parms get-pos)))))
                   (let ([seq (send parms get-seq)])
                     (if seq
                         (let ([seq (send seq to-scheme)])
                           `([,seq (list->py-tuple% ,seq)]))
                         empty))
                   ;(let ([dict (send parms get-dict)])
                   ;  (if dict
                   ;      (let ([dict (send dict to-scheme)])
                   ;        `([,dict (begin (printf "at first, dict is ~a~n" dict)
                   ;                        (assoc-list->py-dict% ,dict))]))
                   ;      empty))
                   (if scope
                       (map (lambda (b)
                              `[,(send b to-scheme) (void)])
                            (send scope get-bindings))
                       empty))]
          ;[body-src (syntax-e body-so)]
          )
      (if (empty? bindings)
          body-so
          `((let ,bindings ,@body-so)))))
  
  ;; generate-lambda: parameters% syntax-object -> sexp
  ;; generate a lambda.
  (define (generate-lambda parms body-so)
    #|(let* ([parms-so (send parms to-scheme)]
           [parms-src (syntax-e parms-so)])
      (if (or (null? parms-src)
              (null? (last-pair parms-src)))
          `(lambda ,parms-so ,body-so)
          `(opt-lambda ,parms-so ,body-so))))|#
    `(lambda ,(send parms to-scheme)
         ,body-so))

  ;; generate-py-lambda: symbol parameters% syntax-object -> sexp
  ;; generate a scheme-lambda->python-lambda wrapper (and the lambda)
  (define (generate-py-lambda name parms body-so)
    (let ([seq (send parms get-seq)]
          [dict (send parms get-dict)]
          [key (send parms get-key)])
      (let ([code-sxp `(make-py-code ',name
                                     ,(generate-lambda parms body-so)
                                     ,(length (send parms get-pos))
                                     ,(cond
                                        [(and seq dict) '(var-args kw-args)]
                                        [seq '(var-args)]
                                        [dict '(kw-args)]
                                        [else null]))])
        (if (and key (not (empty? key)))
            `(make-py-function ,code-sxp
                               ,(map (lambda (k)
                                       (send (cdr k) to-scheme))
                                     key))
            `(make-py-function ,code-sxp)))))
#|                                       
      `(procedure->py-function% ,(generate-lambda parms body-so)
                                ',name
                                ,(generate-list (map (lambda (p)
                                                       `',(send (first-atom p) to-scheme))
                                                     (send parms get-pos)))
                                ,(generate-list (map (lambda (k)
                                                       `(cons ',(send (car k) to-scheme)
                                                              ,(send (cdr k) to-scheme)))
                                                     (send parms get-key)))
                                ,(and seq (car `(',(send seq to-scheme))))
                                ,(and dict (car `(',(send dict to-scheme)))))))
|#

  (define (generate-list exprs)
    (if (empty? exprs)
        'null
        `(list ,@exprs)))

  (define parameters%
    (class ast-node%
      
      ;; parm-list: (listof (or/f (list/p (symbols 'dict) (is-a?/c identifier%))
      ;;                          (list/p (symbols 'seq) (is-a?/c identifier%))
      ;;                          (list/p (symbols 'key) parm-tup? (is-a?/c expression%))
      ;;                          (list/p (symbols 'pos) parm-tup?)))
      ;; (define (parm-tup? x)
      ;;   (or
      ;;    (is-a? x identifier%)
      ;;    (and (list? x) (andmap (lambda (x) (parm-tup? x)) x))))
      (init parm-list)
      
   ;   (printf "def parm-list: ~a~n" parm-list)
      
      ;; pos: (listof parm-tup?)
      (define pos null)
      ;; key: (cons/p parm-tup? (is-a?/c expression%))
      (define key null)
      ;; seq: (or/f false? (is-a?/c identifier%))
      (define seq #f)
      ;; dict: (or/f false? (is-a?/c identifier%))
      (define dict #f)
      
      (for-each
       (lambda (param)
         (cond
           ((eq? 'pos (car param))
            (cond
              ((null? key) (set! pos (cons (cadr param) pos)))
              (else
               (send (cadr param)
                     stx-err
                     "All parameters following a parameter with a default value must have a default value themselves"))))
           ((eq? 'key (car param))
            (set! key (cons (cons (cadr param) (caddr param)) key)))
           ((eq? 'seq (car param))
            (set! seq (cadr param)))
           (else
            (set! dict (cadr param)))))
       parm-list)
      
      (define (parm-tup-bindings x)
        (cond
          ((is-a? x identifier%) (list x))
          (else (apply append (map parm-tup-bindings x)))))
      
      (define parm-bindings
        (append
         (apply append (map parm-tup-bindings pos))
         (apply append (map (lambda (x) (parm-tup-bindings (car x)))
                            key))
         (if seq (list seq) null)
         (if dict (list dict) null)))
      (let ((ht (make-hash-table)))
        (for-each (lambda (id)
                    (cond
                      ((hash-table-get ht (send id get-symbol) (lambda () #f))
                       (send id stx-err "Duplicate identifier in function parameter list."))
                      (else
                       (hash-table-put! ht (send id get-symbol) (lambda () #f)))))
                  parm-bindings))
      
      (define (get-parm-list) parm-bindings)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (e) (send (cdr e) set-bindings! enclosing-scope)) key))
      
      ;(printf "parm-bindings: ~a~n" parm-bindings)
      ;(printf "def  pos: ~a~nkey: ~a~nseq: ~a~ndict: ~a~n" pos key seq dict)
      
      (define/public (get-pos) (reverse pos))
      (define/public (get-key) (reverse key))
      (define/public (get-seq) seq)
      (define/public (get-dict) dict)
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (let ([Ps (map (lambda (p)  ;; for tuple arguments, just use the id of the first item
                           (send (first-atom p) to-scheme))
                       (reverse pos))]
              [Ks (map (lambda (k)
                         ;;`[,(send (car k) to-scheme) ,(send (cdr k) to-scheme)])
                         ;; no longer using opt-lambda
                         (send (first-atom k) to-scheme))
                       (reverse key))])
        (->orig-so (cond
                     [(and seq dict)
                      (begin ;(printf "identifier binding for seq: ~a~n" (identifier-binding (send seq to-scheme)))
                             ;(printf "identifier binding for runtime things: ~a~n" (identifier-binding (->orig-so 'test)))
                       ;;`(,(send dict to-scheme) ,@Ps ,@Ks . ,(send seq to-scheme)))]
                       ;; using the same format as CPython now
                        `(,@Ps ,@Ks ,(send seq to-scheme) ,(send dict to-scheme)))]
                     [seq ;;`(,@Ps ,@Ks . ,(send seq to-scheme))]
                          `(,@Ps ,@Ks ,(send seq to-scheme))]
                     [dict ;;`(,(send dict to-scheme) ,@Ps ,@Ks)]
                           `(,@Ps ,@Ks ,(send dict to-scheme))]
                     [else `(,@Ps ,@Ks)]))))
                       
                           
                           
      
      (super-instantiate ())))
  
  (define expression%
    (class ast-node%
      (inherit stx-err)
      
      ;; to-target: -> (is-a?/c target%)
      ;; Raises an exception if the expression is not a valid
      ;; assignment target, otherwise returns a target%
      (define/public (to-target)
        (stx-err "Invalid target"))
      
      ;;daniel
      (define/override (to-scheme)
        (stx-err "Invalid usage of to-scheme on an expression% (I'm purely virtual)"))
      
      (super-instantiate ())))
    
  ;; 5.2.1
  (define identifier%
    (class expression%
      ;; name-string: string?  [or a symbol, sometimes, created by aug-assign%]
      (init name-string)
      ;; name: symbol?
      (define name (if (symbol? name-string)
                       name-string
                       (string->symbol name-string)))
      
      (define/public (get-symbol) name)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tidentifier% name start-pos end-pos))
      
      ;;daniel
      (inherit ->orig-so ->lex-so)
      (define/override (to-scheme)
            (->lex-so (get-symbol) (current-toplevel-context)))
      
      (super-instantiate ())))
  
  ;; 5.2.2
  (define literal%
    (class expression%
      
      ;; value: (or/f string? number?)
      (init-field value)
      
      (define/public (get-value) value)
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so #|(list (if (string? value)
                             (py-so 'string->py-string%)
                             (py-so 'number->py-number%))
                         value)))|#
                   (if (string? value)
                       `(make-py-string ,value)
                       `(make-py-number ,value))))
      
      (super-instantiate ())))
  
  ;; 5.2.3
  (define tuple%
    (class expression%
      
      ;; expressions: (listof (is-a?/c expression%))
      (init-field expressions)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object ttuple%
          (map (lambda (e) (send e to-target)) expressions) start-pos end-pos))
      
      (define (set-bindings! enclosing-scope)
        (for-each (lambda (e) (send e set-bindings! enclosing-scope)) expressions))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(make-py-tuple ;;,(py-so 'list->py-tuple%)
                      ,(generate-list (map (lambda (e)
                                             (send e to-scheme))
                                           expressions)))))
      
      (super-instantiate ())))
  
  ;; 5.2.4
  (define list-display%
    (class expression%
      ;; expressions: (listof (is-a?/c expression%))
      (init-field expressions)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tlist-display%
          (map (lambda (e) (send e to-target)) expressions) start-pos end-pos))
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (e) (send e set-bindings! enclosing-scope)) expressions))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(make-py-list ;;,(py-so 'list->py-list%)
                     ,(generate-list (map (lambda (e) (send e to-scheme))
                                          expressions)))))
      
      (super-instantiate ())))
  
  ;;daniel
  ;;;;;; [2*s for s in a]  .... expr = 2*s, for = for s in a .... for.targ = s, for.vals = a, for.iter = #f
  ;;;;;; [x+y for x,y in a] .... expr = x+y, for = for x,y in a .... for.targ = (x,y), for.vals = a, for.iter = #f
  
  (define list-comprehension%
    (class expression%
      ;; expr: (is-a?/c expression%)
      ;; for: (is-a?/c list-for%)
      (init-field expr for)
      
      (define/override (set-bindings! enclosing-scope)
        (send expr set-bindings! enclosing-scope)
        (send for set-bindings! enclosing-scope))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so (let ([result (gensym 'result)])
                     `(let ([,result null])
                        ,(send for to-scheme (send expr to-scheme) result)
                        (make-py-list ;;,(py-so 'list->py-list%)
                          ,result)))))
      
      (super-instantiate ())))
  
  (define list-for%
    (class ast-node%
      ;; targ-exp: (is-a?/c expression%)
      (init targ-exp)
      ;; vals: (is-a?/c expression%)
      ;; iter: (or/f false? (is-a?/c list-for%) (is-a?/c list-if%))
      (init-field vals iter)
      
      ;; targ: (is-a?/c target%)
      (define targ (send targ-exp to-target))
      
      (define/override (set-bindings! enclosing-scope)
        (send vals set-bindings! enclosing-scope)
        (when iter (send iter set-bindings! enclosing-scope))
        (send targ set-bindings! enclosing-scope))
      
      ;;daniel
      ;; -> target%
      (define/public (get-targ) targ)
      
      ;;daniel
      ;; -> expression%
      (define/public (get-vals) vals)
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme expr-so result)
        (let ([body (if iter
                        (send iter to-scheme expr-so result)
                        `(set! ,result (append ,result (cons ,expr-so null))))])
        (->orig-so `(for-each ,(generate-lambda (target->parameters targ)
                                                body
                                                #f)
;                              ,(cond
;                                [(is-a? targ tidentifier%)
;                                 `(lambda (,(send targ to-scheme))
;                                    ,body)]
;                                [(or (is-a? targ ttuple%)
;                                     (is-a? targ tlist-display%))
;                                 (let ([item (gensym 'item)])
;                                   `(lambda (,item)
;                                      (apply 
;                                       (lambda (,@(map (lambda (t) (send t to-scheme))
;                                                       (send targ get-sub-targets)))
;                                         ,body)
;                                       (,(py-so 'py-sequence%->list) ,item))))]
;                                [else (error "bad target for a list comprehension")])
                              (get-py-list ;;,(py-so 'py-sequence%->list)
                                ,(send vals to-scheme))))))
      
      
      (super-instantiate ())))
  
  (define list-if%
    (class ast-node%
      ;; test: (is-a?/c expression%)
      ;; iter: (or/f (is-a?/c list-for%) (is-a?/c list-if%))
      (init-field test iter)
      
      (define/override (set-bindings! enclosing-scope)
        (send test set-bindings! enclosing-scope)
        (when iter (send iter set-bindings! enclosing-scope)))

      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme expr-so result)
        (->orig-so `(,(py-so 'py-if) ,(send test to-scheme)
                                   (set! ,result (append ,result
                                                         (cons ,expr-so null))))))
      
      (super-instantiate ())))
  
  ;; 5.2.5
  (define dictionary-display%
    (class expression%
      ;; key-values: (listof (list/p (is-a?/c expression%) (is-a?/c expression%)))
      (init-field key-values)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x)
                    (send (car x) set-bindings! enclosing-scope)
                    (send (cadr x) set-bindings! enclosing-scope))
                  key-values))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(,(py-so 'assoc-list->py-dict%)
                     ,(generate-list (let ([key-id (gensym 'key)])
                                       (map (lambda (key-value-pair)
                                              (apply (lambda (key value)
                                                       `(list (let ([,key-id (->scheme ,(send key to-scheme))])
                                                                (if (string? ,key-id)
                                                                    (string->symbol ,key-id)
                                                                    ,key-id))
                                                              ,(send value to-scheme)))
                                                     key-value-pair))
                                            key-values))))))
      
      (super-instantiate ())))
  
  ;; 5.2.6
  (define string-conversion%
    (class expression%
      ;; expression: (is-a?/c expression%)
      (init-field expression)
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(,(py-so 'py-call) ,(py-so 'py-repr) (list ,(send expression to-scheme)))))
      
      (super-instantiate ())))
  
  ;; 5.3.1
  (define attribute-ref%
    (class expression%
      ;; expression: (is-a?/c expression%)
      ;; identifier: (is-a?/c identifier%)
      (init-field expression identifier)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tattribute-ref% expression identifier start-pos end-pos))
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (let ([expr (send expression to-scheme)]
              [id (send identifier to-scheme)])
          (->orig-so `(py-get-attr/obj ,expr (make-py-symbol ',id)))))
                     ;`(if (namespace-variable-value ',expr #t (lambda () #f))
                     ;     (,(py-so 'python-get-member) ,expr ',id)
                     ;     ,(string->symbol (string-append (symbol->string (syntax-e expr))
                     ;                                     "."
                     ;                                     (symbol->string (syntax-e id))))))))
      
      (super-instantiate ())))
  
  ;; 5.3.2
  (define subscription%
    (class expression%
      ;; expression: (is-a?/c expression%)
      ;; subs: (is-a?/c expression%)
      (init-field expression sub)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tsubscription% expression sub start-pos end-pos))
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope)
        (send sub set-bindings! enclosing-scope))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(,(py-so 'python-index) ,(send expression to-scheme)
                                            ,(send sub to-scheme))))
      
      (super-instantiate ())))
  
  ;; 5.3.3
  (define simple-slicing%
    (class expression%
      ;; expression: (is-a?/c expression%)
      ;; lower: (or/f false? (is-a?/c expression%))
      ;; upper: (or/f false? (is-a?/c expression%))
      (init-field expression lower upper)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tsimple-slicing% expression lower upper start-pos end-pos))
      
      (define/override (set-bindings! enclosing-scope)
        (when lower (send lower set-bindings! enclosing-scope))
        (when upper (send upper set-bindings! enclosing-scope))
        (send expression set-bindings! enclosing-scope))
      
      ;;daniel
      (inherit ->orig-so)
      ;; to do - check that expression evals to a python sequence
      (define/override (to-scheme)
        (->orig-so `(,(py-so 'python-method-call)
                     ,(send expression to-scheme)
                     '__getitem__
                     (list (,(py-so 'py-create)
                      ,(py-so 'py-slice%)
                      ,(if lower
                           (send lower to-scheme)
                           `(,(py-so 'number->py-number) 0))
                      ,(if upper
                           (send upper to-scheme)
                           `(,(py-so 'number->py-number) +inf.0)))))))
      
      (super-instantiate ())))
  
  ;; 5.3.4
  (define call%
    (class expression%
      (inherit stx-err)
      
      ;; expression: (is-a?/c expression%)
      (init-field expression)
      
      ;; arg-list: (listof (or/f (list/p (symbols 'dict 'seq 'pos) (is-a?/c expression%))
      ;;                         (list/p (symbols 'key)
      ;;                                 (is-a?/c identifier%)
      ;;                                 (is-a?/c expression%))))
      (init arg-list)
     ; (printf "call arg-list: ~a~n" arg-list)
      ;; pos: (listof (is-a?/c expression%))
      (define pos null)
      ;; key: (listof (cons/p (is-a?/c identifier) (is-a?/c expression%)))
      (define key null)
      ;; seq: (or/f false? (is-a?/c expression%))
      (define seq #f)
      ;; dict: (or/f false? (is-a?/c expression%))
      (define dict #f)
      
      ;;;; from what I understand, seq and dict will always be false... - daniel
      
      (for-each (lambda (arg)
                  (cond
                    ((eq? 'pos (car arg))
                     (cond
                       ((null? key)
                        (set! pos (cons (cadr arg) pos)))
                       (else
                        (send (cadr arg) stx-err
                              "positional argument cannot follow keyword argument"))))
                    ((eq? 'key (car arg))
                     (set! key (cons (cons (cadr arg) (caddr arg)) key)))
                    ((eq? 'seq (car arg))
                     (set! seq (cadr arg)))
                    (else
                     (set! dict (cadr arg)))))
                arg-list)
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope)
        (when seq (send seq set-bindings! enclosing-scope))
        (when dict (send dict set-bindings! enclosing-scope))
        (for-each (lambda (x) (send x set-bindings! enclosing-scope)) pos)
        (for-each (lambda (x) (send (cdr x) set-bindings! enclosing-scope)) key))
      
     ;  (printf "call  pos: ~a~nkey: ~a~nseq: ~a~ndict: ~a~n" pos key seq dict)
     
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so (let ([pos-args (map (lambda (e)
                                          (send e to-scheme))
                                        (reverse pos))]
                         [key-args (map (lambda (e)
                                          `(list (quote ,(send (car e) to-scheme))
                                                 ,(send (cdr e) to-scheme)))
                                        (reverse key))])
                     (if (is-a? expression attribute-ref%)
                         `(,(py-so 'python-method-call)
                                 ,(send ((class-field-accessor attribute-ref% expression) expression)
                                              to-scheme)
                                 ',(send ((class-field-accessor attribute-ref% identifier) expression)
                                              to-scheme)
                                 ,(generate-list pos-args)
                                 ,(generate-list key-args))
                         #|
                         `(,(py-so 'py-call) ,(send expression to-scheme)
                                             ,(generate-list pos-args)
                                             ,(generate-list key-args))))))|#
                         (if (empty? key-args)
                             `(py-apply ,(send expression to-scheme) ,@pos-args)
                             `(py-apply-kw ,(send expression to-scheme) ,key-args ,@pos-args))))))
      
      (super-instantiate ())))
  
  
  ;; 5.4, 5.6, 5.7, 5.8, 5.10
  (define binary%
    (class expression%
      ;; lhs: (is-a?/c expression%)
      ;; op: (symbols 'or 'and '\| '^ '& '<< '>> '+ '- '* '/ '% '// '**)
      ;; rhs: (is-a?/c expression%)
      (init-field lhs op rhs)
      
      (define/override (set-bindings! enclosing-scope)
        (send lhs set-bindings! enclosing-scope)
        (send rhs set-bindings! enclosing-scope))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (if (eq? op 'and)
            (->orig-so `(py-if ,(send lhs to-scheme) ,(send rhs to-scheme) py-none))
            #|(->orig-so (list (py-so 'python-method-call) (send lhs to-scheme)
                             (case op
                               [(+) ''__add__]
                               [(-) ''__sub__]
                               [(*) ''__mul__]
                               [(/) ''__div__]
                               [else (raise (format "binary% op unsupported: ~a" 
                                                    op))])
                             `(list ,(send rhs to-scheme))))))|#
            (->orig-so (let ([lhs-so (send lhs to-scheme)]
                             [rhs-so (send rhs to-scheme)])
                         (case op
                           [(+) `(spy-cpython-add ,lhs-so ,rhs-so)]
                           [(-) `(spy-cpython-sub ,lhs-so ,rhs-so)]
                           [(*) `(spy-cpython-mul ,lhs-so ,rhs-so)]
                           [(/) `(spy-cpython-div ,lhs-so ,rhs-so)]
                           [else (raise (format "binary% op unsupported: ~a" 
                                                    op))])))))
      
      (super-instantiate ())))
  
  ;; 5.5, 5.10
  (define unary%
    (class expression%
      ;; op: (symbols 'not '+ '- '~)
      ;; rhs: (is-a?/c expression%)
      (init-field op rhs)
      
      (define/override (set-bindings! enclosing-scope)
        (send rhs set-bindings! enclosing-scope))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (let ([s-rhs (send rhs to-scheme)])
          (case op
            [(+) s-rhs]
            [(not) (->orig-so `(,(py-so 'py-not) ,s-rhs))]
            [(-) (->orig-so (if (and (is-a? rhs literal%)
                                     (number? (send rhs get-value)))
                                `(make-py-number ,(- (send rhs get-value)))
                                `(spy-cpython-neg ,s-rhs)))]
                    ;;(list (py-so 'python-method-call) s-rhs
                      ;;            ''__neg__))]
            [(~) (raise "bitwise operation ~ not implemented yet")]
            [else (raise (format "unary op unsupported: ~a" op))])))
      
      (super-instantiate ())))
  
  
  ;; 5.9
  (define comparison%
    (class expression%
      
      ;; comps: (listof (or/f (is-a?/c expression) 
      ;;                      (symbols '< '> '== '>= '<= '<> '!= 'in 'notin 'is 'isnot)))
      ;; expression% oper expression% oper ... expression%
      (init-field comps)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x)
                    (unless (symbol? x) (send x set-bindings! enclosing-scope)))
                  comps))

      ;; scheme-op->python-op:  symbol -> syntax-object
      (define (scheme-op->python-op oper)
        (py-so (case oper
                 [(>) 'py>]
                 [(<) 'py<]
                 [else oper])))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so
         (let ([s-comps (map (lambda (e)
                               (if (symbol? e)
                                   (scheme-op->python-op e)
                                   (send e to-scheme)))
                             comps)])
           `(,(py-so 'py-compare) ,(car s-comps)
                                  ,(car (cdr s-comps))
                                  ,(car (cdr (cdr s-comps)))
                                  ,(generate-list (cdr (cdr (cdr s-comps))))))))
 
      
      (super-instantiate ())))
  
  ;; 5.10
  (define lambda%
    (class expression%
      
      ;; parms: (is-a?/c parameters%)
      ;; body: (is-a?/c expression%)
      (init-field parms body)
      
      ;; bindings: (listof (is-a?/c tidentifier%))
      (define bindings null)
      
      (define/override (set-bindings! enclosing-scope)
        (send parms set-bindings! enclosing-scope)
        (send body set-bindings! this))
      
      (define/public (add-binding id)
        (set! bindings (cons id bindings)))
      
      (define/public (get-bindings) bindings)
      
      (define/public (get-parms) parms)
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so (generate-py-lambda 'anonymous-function
                                       parms
                                       (send body to-scheme)))) ;#f #t))))
;                   `(,(py-so 'procedure->py-function%) (lambda ,(send parms to-scheme)
;                                                         ,(send body to-scheme)))))
      
      (define/public (is-global? b) #f)
      
      (super-instantiate ())))
  
  )
