(module compiler-expr mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
	   "compiler.ss"
	   "compiler-target.ss")
  
  (provide (all-defined))
  
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
               (send (cadr param) stx-err "All parameters following a parameter with a default value must have a default value themselves"))))
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
      
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so (map (lambda (p)
                          (send p to-scheme))
                        pos)))
      
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
      ;; name-string: string?
      (init name-string)
      ;; name: symbol?
      (define name (string->symbol name-string))
      
      (define/public (get-symbol) name)
      
      (inherit-field start-pos end-pos)
      (define/override (to-target)
        (make-object tidentifier% name start-pos end-pos))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so (get-symbol)))
      
      (super-instantiate ())))
  
  ;; 5.2.2
  (define literal%
    (class expression%
      
      ;; value: (or/f string? number?)
      (init-field value)
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so value))
      
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
        (->orig-so (if (= (length expressions) 1)
                       (send (car expressions) to-scheme)
                       `(make-tuple (list ,@(map (lambda (e)
                                                   (send e to-scheme))
                                                 expressions))))))
      
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
        (->orig-so `(list ,@(map (lambda (e) (send e to-scheme))
                                 expressions))))
      
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
        (->orig-so
         (let* ([targ (send for get-targ)]
                [scheme-targ (send targ to-scheme)]
                [scheme-vals (send (send for get-vals) to-scheme)]
                [scheme-expr (send expr to-scheme)])
           (cond
             [(is-a? targ tidentifier%)
              `(map (lambda (,scheme-targ)
                      ,scheme-expr)
                    ,scheme-vals)]
             [(is-a? targ ttuple%)
              (let ([tuple-name (gensym)])
                `(map (lambda (,tuple-name) 
                        ;; the cdr eats the "list" part of "`(list x y)"
                        (apply (lambda ,(cdr (syntax-e scheme-targ))
                                        ,scheme-expr)
                               (tuple-list ,tuple-name)))
                      ,scheme-vals))]
             [else (raise "not implemented yet")]))))
      
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
      ;      (inherit ->orig-so)
      ;      (define/override (to-scheme)
      ;        (->orig-so (let ([targ (send targ-exp to-scheme)])
      ;                     `(map (lambda (,targ)
      ;                             (f ,targ))
      ;                           ,(send vals to-scheme)))))
      
      
      (super-instantiate ())))
  
  (define list-if%
    (class ast-node%
      ;; test: (is-a?/c expression%)
      ;; iter: (or/f (is-a?/c list-for%) (is-a?/c list-if%))
      (init-field test iter)
      
      (define/override (set-bindings! enclosing-scope)
        (send test set-bindings! enclosing-scope)
        (when iter (send iter set-bindings! enclosing-scope)))
      
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
        (->orig-so `(make-dictionary (list ,@(map (lambda (key-value-pair)
                                                    (apply (lambda (key value)
                                                             `(list ,(send key to-scheme)
                                                                    ,(send value to-scheme)))
                                                           key-value-pair))
                                                  key-values)))))
      
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
        (->orig-so `(repr ,(send expression to-scheme))))
      
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
      ;; pos: (listof (is-a?/c expression%))
      (define pos null)
      ;; key: (listof (cons/p (is-a?/c identifier) (is-a?/c expression%)))
      (define key null)
      ;; seq: (or/f false? (is-a?/c expression%))
      (define seq #f)
      ;; dict: (or/f false? (is-a?/c expression%))
      (define dict #f)
      
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
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(,(send expression to-scheme) ,@(map (lambda (e)
                                                           (send e to-scheme))
                                                         pos))))
      
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
        (->orig-so (list op (send lhs to-scheme) (send rhs to-scheme))))
      
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
        (->orig-so (list op (send rhs to-scheme))))
      
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
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so
         (letrec ([f (lambda (cmps)
                       (let* ([first-lhs (send (first cmps) to-scheme)]
                              [first-rhs (send (third cmps) to-scheme)]
                              [first-comp `(,(second cmps) ,first-lhs ,first-rhs)]
                              [what-else (rest (rest cmps))])
                         (if (> (length what-else) 2)
                             `(and ,first-comp ,(f what-else))
                             first-comp)))])
           (f comps))))
      
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
      
      (define/public (is-global? b) #f)
      
      (super-instantiate ())))
  
  )