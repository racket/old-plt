(module compiler-stmt mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss") ; build-list
           (lib "plt-match.ss")
	   "compiler.ss"
	   "compiler-expr.ss"
           "compiler-target.ss"
          ; "primitives.ss"
           "runtime-support.ss"
          ; "empty-context.ss"
           )

  (provide (all-defined-except bindings-mixin))
    
  (define statement%
    (class ast-node%
      ;; check-break/cont: (or/f false? (is-a?/c statement%)) ->
      ;; enclosing-loop is the nearest enclosing loop statement to this
      ;; statement, or #f if there is none.  Check that all break
      ;; and continue statements are inside some loop and annotate the
      ;; enclosing-loop that it contains a break or continue.
      (define/public (check-break/cont enclosing-loop) (void))

      ;; collect-globals: -> (listof symbol?)
      ;; Return the list of all variables in "global" statements in the 
      ;; current scope.
      (define/public (collect-globals) null)
      
      ;;daniel
      (inherit stx-err)
      (define/override (to-scheme)
        (stx-err "Invalid usage of to-scheme on a statement% (I'm purely virtual)"))

      ;; do nothing for most statements
      (define/public (set-tail-position!)
        (void))
      
      ;; needs-escape-continuation?: symbol -> bool
      ;; determine whether this is a non-tail return/break/continue statement
      ;; or a compound statement with a non-tail return/break/continue part
      (define/public (needs-escape-continuation? ec)
        #f)
      
      (super-instantiate ())))

  ;; 6.1
  (define expr-stmt%
    (class statement%
      ;; expression: (is-a?/c expression%)
      (init-field expression)
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
      ;;daniel
      (define/override (to-scheme)
        (send expression to-scheme))
      
      (super-instantiate ())))
      
  ;; 6.2
  (define assert%
    (class statement%
      ;; expr1: (is-a?/c expression%)
      ;; expr2: (or/f false? (is-a?/c expression%))
      (init-field expr1 expr2)
      
      (define/override (set-bindings! enclosing-scope)
        (send expr1 set-bindings! enclosing-scope)
        (when expr2 (send expr2 set-bindings! enclosing-scope)))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(,(py-so 'py-if) (,(py-so 'py-not) ,(send expr1 to-scheme))
                       (,(py-so 'py-raise) ,(py-so 'py-assert-error%)
                        ,(if expr2
                             `(,(py-so 'py-object%->string) ,(send expr2 to-scheme))
                             #f)
                        #f))))
      
      (super-instantiate ())))
  
  ;; 6.3
  (define assignment%
    (class statement%
      ;; targ-exps: (listof (is-a?/c target%))
      (init targ-exps)
      ;; expression: (is-a?/c expression%)
      (init-field expression)
      ;; targets: (listof (is-a?/c target%))
      (define targets (map (lambda (e)
                             (send e to-target))
                           targ-exps))
      
      ;;daniel
      (define scope #f)
      (define (top?) (is-a? scope module-scope%))
      
      ;;daniel, temporary for debugging
      (define/public (get-targs) targets)
      
      
      (define/public (get-scope) scope)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (send expression set-bindings! enclosing-scope)
        (for-each (lambda (e) (send e set-bindings! enclosing-scope)) targets))
      
      
      (define (binding-targ? t)
        (or (not (send scope is-bound? t))
            (eq? (send scope binding-tid t) t)))
      
      (define (def-targ? t)
        (and (top?)
             (not (attr-targ? t))
             (binding-targ? t)))
      
      (define (set-targ? t)
        (and (not (attr-targ? t))
             (not (def-targ? t))))
      
      (define (attr-targ? t)
        (is-a? t tattribute-ref%))  
      
      ;; assignment-so: target% syntax-object -> syntax-object
      (define (assignment-so target rhs)
        (->orig-so
         (cond
           [(is-a? target tidentifier%) (let ([target-so (send target to-scheme)])
                                          (if (def-targ? target)
                                              `(define ,target-so ,rhs)
                                              `(set! ,target-so ,rhs)))]
           [(is-a? target tattribute-ref%) (let* ([expr (send ((class-field-accessor tattribute-ref%
                                                                                     expression) target)
                                                              to-scheme)]
                                                  [id (send ((class-field-accessor tattribute-ref%
                                                                                   identifier) target)
                                                            to-scheme)]
                                                  [module-var (string->symbol
                                                               (string-append
                                                                (symbol->string (syntax-e expr))
                                                                "."
                                                                (symbol->string (syntax-e id))))])
                                             `(py-set-attr/sym ,expr ',id ,rhs))]
           ; `(if (namespace-variable-value ',expr #t (lambda () #f))
                                            ;      (,(py-so 'python-set-member!) ,expr ',id ,rhs)
                                            ;      ,(if top?
                                            ;           `(namespace-set-variable-value! ',module-var
                                            ;                                           ,rhs)
                                            ;           `(set! ,module-var ,rhs))))]
           [(is-a? target tsubscription%) `(,(py-so 'python-method-call)
                                            ,(send ((class-field-accessor tsubscription%
                                                                          expression) target)
                                                   to-scheme)
                                            '__setitem__
                                            (list ,(send ((class-field-accessor tsubscription%
                                                                          sub) target)
                                                         to-scheme)
                                                  ,rhs))]
           [(is-a? target tsimple-slicing%) `(,(py-so 'python-method-call)
                                              ,(send ((class-field-accessor tsimple-slicing%
                                                                            expression) target)
                                                     to-scheme)
                                              '__setitem__
                                              (list ,(let ([lower ((class-field-accessor tsimple-slicing%
                                                                                        lower) target)]
                                                          [upper ((class-field-accessor tsimple-slicing%
                                                                                        upper) target)])
                                                      `(,(py-so 'py-create)
                                                        ,(py-so 'py-slice%)
                                                        ,(if lower
                                                             (send lower to-scheme)
                                                             `(,(py-so 'number->py-number%) 0))
                                                        ,(if upper
                                                             (send upper to-scheme)
                                                             `(,(py-so 'number->py-number%) +inf.0))))
                                                    ,rhs))]
           [(or (is-a? target ttuple%)
                (is-a? target tlist-display%))
            `(begin ,@(let ([sub-targets (send target get-sub-targets)])
                        (map (lambda (i)
                               (assignment-so (list-ref sub-targets i)
                                              #`(#,(py-so 'python-index) #,rhs #,i)))
                             (build-list (length sub-targets) identity))))]
           [else (error "target type not supported yet")])))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (let* ([rhs (datum->syntax-object (current-toplevel-context) (gensym 'rhs) #f #f)]
               [body (map (lambda (t)
                            (assignment-so t rhs))
                          targets)])
          (->orig-so (if (top?)
                         `(begin (define ,rhs ,(send expression to-scheme))
                                 ,@body)
                         `(let ([,rhs ,(send expression to-scheme)])
                                   ,@body)
                         )
                     )))
      
      (super-instantiate ())))
  
  ;;6.3.1
  (define aug-assignment%
    (class statement%
      ;; targ-exp: (is-a?/c expression%)
      (init targ-exp)

      ;; op: (symbols '+= '-= '*= '/= '%= '&= '\|= '^= '<<= '>>= '**= '//=)
      ;; expression: expression%
      (init-field op expression)

      ;(define target (send targ-exp to-target))
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
       ; (send target set-bindings! enclosing-scope))

      (define inplace-op (case op
                           [(+=) '__iadd__]
                           [(-=) '__isub__]
                           [(*=) '__imul__]
                           [(/=) '__idiv__]
                           [(%=) '__imod__]
                           [(&=) '__iand__]
                           [(\|=) '__ior__]
                           [(^=) '__inot__]
                           [(<<=) '__ilshift__]
                           [(>>=) '__irshift__]
                           [(**=) '__ipow__]
                           [(//=) '__itruediv__]))

      (define safe-meth (case op
                          [(+=) '__add__]
                          [(-=) '__sub__]
                          [(*=) '__mul__]
                          [(/=) '__div__]
                          [(%=) '__mod__]
                          [(&=) '__and__]
                          [(\|=) '__or__]
                          [(^=) '__not__]
                          [(<<=) '__lshift__]
                          [(>>=) '__rshift__]
                          [(**=) '__pow__]
                          [(//=) '__truediv__]))

      (define safe-op (case op
                        [(+=) '+]
                        [(-=) '-]
                        [(*=) '*]
                        [(/=) '/]
                        [(%=) '%]
                        [(&=) '&]
                        [(\|=) '\|]
                        [(^=) '^]
                        [(<<=) '<<]
                        [(>>=) '>>]
                        [(**=) '**]
                        [(//=) '//]))
      
      ;;daniel
      (inherit ->orig-so)
      (inherit-field start-pos end-pos)
      (define orig-targ-exp targ-exp)
      (define/override (to-scheme)
        (let ([targ (gensym 'targ)]
              [expr (gensym 'expr)])
          (->orig-so `(let ([,targ ,(send orig-targ-exp to-scheme)]
                            [,expr ,(send expression to-scheme)])
                        (if (has-member? ,targ ',inplace-op)
                            (python-method-call ,targ ',inplace-op ,expr)
                            ,(send (make-object assignment%
                                    (list orig-targ-exp)
                                    (make-object binary%
                                      (make-object identifier% targ start-pos end-pos)
                                      safe-op
                                      (make-object identifier% expr start-pos end-pos)
                                      start-pos end-pos) ; start-pos, end-pos
                                    start-pos end-pos)
                                  to-scheme))))))
                                        
        #|
        (->orig-so (send (make-object assignment%
                           (list orig-targ-exp)
                           (make-object binary%
                             orig-targ-exp
                             (case op
                               [(+=) '+]
                               [(-=) '-]
                               [(*=) '*]
                               [(/=) '/]
                               [(%=) '%]
                               [(\|=) '\|]
                               [(^=) '^]
                               [(<<=) '<<]
                               [(>>=) '>>]
                               [(**=) '**]
                               [(//=) '//])
                             expression
                             start-pos end-pos) ; start-pos, end-pos
                           start-pos end-pos)
                         to-scheme))) |#
      
      (super-instantiate ())))

  ;; 6.4
  (define pass%
    (class statement%
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so 'py-none))
      
      (super-instantiate ())))
  
  ;; 6.5
  (define del%
    (class statement%
      ;; targ-exp: (is-a?/c expression%)
      (init targ-exp)
      ;; target: (is-a?/c target%)
      (define target (send targ-exp to-target))

      (define/override (set-bindings! enclosing-scope)
        (send target set-bindings! enclosing-scope))
      
      (inherit ->orig-so stx-err)
      (define/override (to-scheme)
        (->orig-so (cond
                     [(is-a? target tidentifier%) `(namespace-set-variable-value! ,(send target to-scheme) 'undefined)]
                     [else (stx-err "del statement not fully implemented yet.")])))
      
      (super-instantiate ())))

  ;; 6.6
  (define print%
    (class statement%
      ;; to-file?: any?
      ;; expressions: (listof (is-a?/c expression%)
      (init-field to-file? expressions)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (e) (send e set-bindings! enclosing-scope)) expressions))
      
      ;;daniel
      (inherit ->orig-so ->lex-so)
      (define/override (to-scheme)
        (->orig-so `(,(py-so 'py-print)
                     ,(if to-file?
                          (send to-file? to-scheme)
                          #f)
                     (list ,@(map (lambda (e)
                                              (send e to-scheme))
                                            expressions)))))
      
      (super-instantiate ())))
  
  ;; 6.7
  (define return%
    (class statement%
      ;; expression: (or/f false? (is-a?/c expression%)
      (init-field expression)
      
      (inherit stx-err)
      (define scope #f)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (unless (is-a? enclosing-scope function-definition%)
          (stx-err "'return' outside function"))
        (when expression
          (send expression set-bindings! enclosing-scope)))

      (define tail-position? #f)
      
      ;; pleeeease let it be true :)
      (define/override (set-tail-position!)
        ;(printf "TAIL!~n")
        (set! tail-position? #t))
      
      (define/override (needs-escape-continuation? ec)
        ;(printf "return~n")
        (and (not tail-position?)
             (eq? ec (send scope get-return-symbol))))
      
      ;;daniel
      (inherit ->orig-so ->lex-so)
      (define/override (to-scheme); py-return)
        ;(->orig-so `(,py-return ,(send expression to-scheme))))
        (->orig-so (if tail-position?
                       (send expression to-scheme)
                       `(,(send scope get-return-symbol) ,(send expression to-scheme)))))
      
      (super-instantiate ())))

  ;; 6.8
  (define yield%
    (class statement%
      ;; expression: (is-a?/c expression%)
      (init-field expression)

      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
      (super-instantiate ())))
  
  ;; 6.9
  (define raise%
    (class statement%

      ;; type: (or/f (false? is-a?/c expression%))
      ;; parm: (or/f (false? is-a?/c expression%))
      ;; traceback: (or/f (false? is-a?/c expression%))
      (init-field type parm traceback)

      (define/override (set-bindings! enclosing-scope)
        (when type (send type set-bindings! enclosing-scope))
        (when parm (send parm set-bindings! enclosing-scope))
        (when traceback (send traceback set-bindings! enclosing-scope)))
      
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(,(py-so 'py-raise) ,(and type
                                             (send type to-scheme))
                                        ,(and parm
                                             (send parm to-scheme))
                                        ,(and traceback
                                             (send traceback to-scheme)))))
      
      (super-instantiate ())))
  
  ;; 6.10
  (define break%
    (class statement%
      (inherit stx-err)
      
      (define loop #f)
      
      (define/override (check-break/cont enclosing-loop)
        (set! loop enclosing-loop)
        (if enclosing-loop
            (send enclosing-loop set-can-break?)
            (stx-err "Break statement must be within loop")))
      
      

      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(,(send loop get-break-symbol) (void))))
      
      (super-instantiate ())))

  ;; 6.11
  (define continue%
    (class statement%
      (inherit stx-err)

      (define loop #f)
      
      (define/override (check-break/cont enclosing-loop)
        (set! loop enclosing-loop)
        (if enclosing-loop
            (send enclosing-loop set-can-cont?)
            (stx-err "Continue statement must be within loop")))
      
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(,(send loop get-continue-symbol) )))
      
      (super-instantiate ())))
  
  ;(require "empty-context.ss")
  ;; 6.12
  (define import-module%
    (class statement%
      ;; modules: (listof (list/p (listof (is-a?/c identifier%))
      ;;                          (or/f false? (is-a?/c identifier%))))
      (init-field modules)

;      (printf "modules: ~a~n" modules)
      (define scope #f)
      (define (top?) (is-a? scope module-scope%))
      (define/public (get-scope) scope)
      
      (define/override (set-bindings! enclosing-scope)
        (when enclosing-scope
          (set! scope enclosing-scope)
          (for-each (lambda (module)
                      (let ([id (or (cadr module)
                                    (car (car module)))])
                        (unless (send enclosing-scope is-local? id)
                          (send enclosing-scope add-binding id))))
                    modules))
        (map caar modules))
      
      (inherit ->orig-so ->lex-so)
      (define/override (to-scheme)
        (->orig-so `(begin ,@(map (lambda (import)
                                    (let ([ids (map (lambda (id) (car `(',(send id to-scheme))))
                                                    (car import))]
                                          [name (if (cadr import)
                                                    (send (cadr import) to-scheme)
                                                    (send (car (car import)) to-scheme))]
                                          [at-top? (top?)])
                                      `(,(if at-top? 'namespace-set-variable-value! 'set!)
                                        ,(if at-top? (car `(',name)) name)
                                        (call-with-values
                                         (lambda ()
                                           (python-load-module (list ,@ids)))
                                         namespace->py-module%))))
                                  modules))))
      
      (super-instantiate ())))

  (define import-from%
    (class statement%
      ;; module: (listof (is-a?/c identifier%))
      ;; ids: (or/f (symbols '*) (listof (list/p (is-a?/c identifier%)
      ;;                                         (or/f false? (is-a?/c identifier%)))))
      (init-field module ids)
      
      (define/override (set-bindings! enclosing-scope)
        (unless (symbol? ids) (map (lambda (b) (if (cadr b) (cadr b) (car b))) ids)))
      
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(,(py-so 'python-import-from-module)
                     ,(if (eq? ids '*)
                          #f
                          (generate-list (map (lambda (id)
                                                (let ([binding (send (car id) to-scheme)])
                                                  (if (cadr id)
                                                      `(list ',binding ',(send (cadr id) to-scheme))
                                                      `(list ',binding #f))))
                                              ids)))
                     ,@(map (lambda (id) (car `(',(send id to-scheme))))
                            module))))
      
      (super-instantiate ())))
  
  ;; 6.13
  (define global%
    (class statement%
      ;; identifiers: (listof (is-a?/c identifier%))
      (init-field identifiers)

      (define/override (collect-globals) identifiers)
      
      ;; doesn't actually _do_ anything at runtime
      (inherit-field start-pos end-pos)
      (define/override (to-scheme)
        (send (make-object pass% start-pos end-pos) to-scheme))
      
      (super-instantiate ())))

  ;; 6.14
  (define exec%
    (class statement%
      ;; exp1: (or/f false (is-a?/c expression))
      ;; exp2: (or/f false (is-a?/c expression))
      ;; exp3: (or/f false (is-a?/c expression))
      (init-field exp1 exp2 exp3)

      (define/override (set-bindings! enclosing-scope)
        (when exp1 (send exp1 set-bindings! enclosing-scope))
        (when exp2 (send exp2 set-bindings! enclosing-scope))
        (when exp3 (send exp3 set-bindings! enclosing-scope)))
      
      (super-instantiate ())))
  
  (define (bindings->los b)
    (cond
      [(list? b) (map bindings->los b)]
      [else (send b get-symbol)]))
  
  ;; 7
  (define suite%
    (class statement%
      
      ;; statements: (listof (is-a?/c statement%))
      (init-field statements)
      (define (sub-stmt-map f)
        (map f statements))
      
      (define scope #f)
      (define/public (get-scope) scope)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (for-each (lambda (s) (send s set-bindings! enclosing-scope)) statements))

      (define/override (check-break/cont enclosing-loop)
        (void (sub-stmt-map (lambda (s) (send s check-break/cont enclosing-loop)))))

      (define/override (collect-globals)
        (apply append (sub-stmt-map (lambda (s) (send s collect-globals)))))

      (define last-statement (car (last-pair statements)))
      
      (define/override (set-tail-position!)
        (send last-statement set-tail-position!))
      
      ;;daniel
      ; defs-and-exprs:  -> (listof (union sexp (list (or 'field 'method) name value)))
      (define/public (defs-and-exprs)
        ;(printf "DEFS-AND-EXPRS BINDINGS: ~a~n" (bindings->los (send scope get-bindings)))
        (sub-stmt-map (lambda (s)
                        (let* ([so (send s to-scheme)]
                               [so-l (syntax->list so)])
                          (cond
                            [(is-a? s assignment%) 
                             (if (free-identifier=? (first so-l)
                                                    (datum->syntax-object (current-runtime-support-context)
                                                                          'namespace-set-variable-value!))
                                 (list 'field (second (second so-l)) (third so-l))
                                 (list 'field
                                       (second (syntax->list (third (syntax->list (second so-l)))))
                                       (second (syntax->list (first (syntax->list (second (syntax->list (second so-l)))))))))]
                            [(is-a? s function-definition%) (list 'method
                                                                  (let ([quoted-name-so (second so-l)])
                                                                    (second (syntax->list quoted-name-so)))
                                                                  (third so-l))]
                            [else so])))))
;                              (if so-l
;                                       ;;;; either when defining or mutating a class field
;                                  (if (free-identifier=? (first so-l)
;                                                              (datum->syntax-object runtime-context
;                                                                                    'define))
;                                      
;                                          (list (if (is-a? s assignment%)
;                                                    'field
;                                                    'method)
;                                                (second so-l)
;                                                (third so-l))
;                                      (if (and (free-identifier=? (first so-l)
;                                                                  (datum->syntax-object runtime-context
;                                                                                        'begin))
;                                               (free-identifier=? (first (syntax->list (second so-l)))
;                                                                  (datum->syntax-object runtime-context
;                                                                                        'let)))
;                                          (list (second (syntax->list (third (syntax->list (second so-l)))))
;                                                (second (syntax->list (first (syntax->list (second (syntax->list (second so-l))))))))
;                                          so))
;                                  so)))))
      
       

      (define/override (needs-escape-continuation? ec)
        (ormap (lambda (statement)
                 (send statement needs-escape-continuation? ec))
               statements))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override to-scheme
        (opt-lambda ([escape-continuation-symbol #f] [lambda-suite? #f] [def-suite? #f])
          (let* ([return-is-last? (is-a? last-statement return%)]
                 [bodies (let ([statements (sub-stmt-map (lambda (s) (send s to-scheme)))]
                               [insert-void-return? (and def-suite?
                                                         (not return-is-last?))])
                           (if insert-void-return? ; functions that have no return must return None
                               (append statements `(,(py-so 'py-none)))
                               statements))]
                 [bodies-with-let (if (or lambda-suite? def-suite?)
                                      (generate-function-bindings (send scope get-parms) bodies scope)
                                      bodies)])
            (->orig-so (if (and escape-continuation-symbol
                                (needs-escape-continuation? escape-continuation-symbol))
                           `(call-with-escape-continuation
                             (lambda (,escape-continuation-symbol)
                               ,@bodies-with-let))
                           (if (= 1 (length bodies-with-let))
                               (first bodies-with-let)
                               `(begin ,@bodies-with-let)))))))

         
      (super-instantiate ())))
  
  ;; 7.1
  (define if%
    (class statement%
      ;; test-bodies: (listof (list/p (is-a?/c expression%) (is-a?/c suite%)))
      ;; else: (or/f (is-a?/c suite%) false?)
      (init-field test-bodies else)

      (define (sub-stmt-map f)
        (append
         (map (lambda (x) (f (cadr x))) test-bodies)
         (if else
             (list (f else))
             null)))
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x)
                    (send (car x) set-bindings! enclosing-scope)
                    (send (cadr x) set-bindings! enclosing-scope))
                  test-bodies)
        (when else (send else set-bindings! enclosing-scope)))
      
      (define/override (collect-globals)
        (apply append (sub-stmt-map (lambda (s) (send s collect-globals)))))
      
      (define/override (check-break/cont enclosing-loop)
        (void (sub-stmt-map (lambda (s) (send s check-break/cont enclosing-loop)))))

      (define/override (set-tail-position!)
        (for-each (lambda (test&thn)
                    (send (second test&thn) set-tail-position!))
                  test-bodies)
        (when else
          (send else set-tail-position!)))
      
      (define/override (needs-escape-continuation? ec)
        (or (ormap (lambda (test&thn)
                     (send (second test&thn) needs-escape-continuation? ec))
                   test-bodies)
            (and else (send else needs-escape-continuation? ec))))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
;        (let ([s-test-bodies (map (lambda (test-body)
;                                    `[(,(py-so 'py-if) ,(send (car test-body) to-scheme) #t #f)
;                                      ,(send (cadr test-body) to-scheme)])
;                                  test-bodies)])
        (->orig-so (let loop ([test-bodies test-bodies])
                         (match test-bodies
                           ['() (if else
                                    (send else to-scheme)
                                    (void))]
                           [(list (list test then) rest-bodies ...)
                            `(py-if ,(send test to-scheme)
                                    ,(send then to-scheme)
                                    ,(loop rest-bodies))]))))
                   ; (if else
                   ;`(cond
                       ;   ,@s-test-bodies
                       ;   [else ,(send else to-scheme)])
                   ;    `(cond ,@s-test-bodies)))))
      
      (super-instantiate ())))

  ;; 7.2
  (define while%
    (class statement%

      ;; test: (is-a?/c expression%)
      ;; body: (is-a?/c suite%)
      ;; else: (or/f false? (is-a?/c suite%))
      (init-field test body else)
      (define can-break? #f)
      (define can-cont? #f)
      (define/public (set-can-break?) (set! can-break? #t))
      (define/public (set-can-cont?) (set! can-cont? #t))
      
      (define/override (set-bindings! enclosing-scope)
        (send test set-bindings! enclosing-scope)
        (send body set-bindings! enclosing-scope)
        (when else (send else set-bindings! enclosing-scope)))

      (define/override (collect-globals)
        (append (send body collect-globals)
                (if else (send else collect-globals) null)))
      
      (define/override (check-break/cont enclosing-loop)
        (send body check-break/cont this)
        (if else
            (send else check-break/cont enclosing-loop)))

      (define break-symbol (gensym 'break))
      (define continue-symbol (gensym 'continue))
      
      (define/public (get-break-symbol) break-symbol)
      (define/public (get-continue-symbol) continue-symbol)

      (define/override (set-tail-position!)
        (when else
          (send else set-tail-position!)))
      
      (define/override (needs-escape-continuation? ec)
        (and else (send else needs-escape-continuation? ec)))
      
      (inherit ->orig-so)
      (define/override (to-scheme)
        ;(let* ([loop (gensym 'loop)])
        ;  (set! continue-symbol loop)
          (->orig-so (let ([normal-body
                            `(let/ec ,break-symbol
                                (let ,continue-symbol ()
                                  (py-if ,(send test to-scheme)
                                    (begin ,(send body to-scheme continue-symbol)
                                           (,continue-symbol)))))])
                       (if else
                           `(begin ,normal-body ,(send else to-scheme))
                           normal-body))))
      ;)
      
      
      (super-instantiate ())))
  
  ;; 7.3
  (define for%
    (class statement%
      
      ;; targ-exp: (is-a?/c expression)
      (init targ-exp)

      ;; vals: (is-a?/c expression%)
      ;; body: (is-a?/c suite%)
      ;; else: (or/f false? (is-a?/c suite%))
      (init-field vals body else)

      ;; target: (is-a?/c target%)
      (define target (send targ-exp to-target))
      (define can-break? #f)
      (define can-cont? #f)
      (define/public (set-can-break?) (set! can-break? #t))
      (define/public (set-can-cont?) (set! can-cont? #t))
      
      (define scope #f)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (send target set-bindings! enclosing-scope)
        (send body set-bindings! enclosing-scope)
        (when else (send else set-bindings! enclosing-scope)))
      
      (define/override (collect-globals)
        (append (send body collect-globals)
                (if else (send else collect-globals) null)))
      
      (define/override (check-break/cont enclosing-loop)
        (send body check-break/cont this)
        (if else
            (send else check-break/cont enclosing-loop)))
      
      (define break-symbol (gensym 'break))
      (define continue-symbol (gensym 'continue))
      
      (define/public (get-break-symbol) break-symbol)
      (define/public (get-continue-symbol) continue-symbol)

      (define/override (set-tail-position!)
        (when else (send else set-tail-position!)))

      (define/override (needs-escape-continuation? ec)
        (and else (send else needs-escape-continuation? ec)))

      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (let ([parm-names (target->parm-tup target)])
          (->orig-so (let* ([bdy  `(let* ([index -1]
                                          [seq ,(send vals to-scheme)]
                                          [len (python-method-call seq '__len__)])
                                     (let ,continue-symbol ()
                                       (set! index (+ 1 index))
                                       (unless (= index len)
                                         ,(if (is-a? parm-names identifier%)
                                              `(set! ,(send parm-names to-scheme) (python-index seq index))
                                              `(let ([current-tuple (python-index seq index)])
                                                 ,@(map (lambda (parm-index)
                                                          `(set! ,(send (list-ref parm-names parm-index)
                                                                        to-scheme)
                                                                 (python-index current-tuple ,parm-index)))
                                                        (build-list (length parm-names) identity))))
                                         ,(send body to-scheme continue-symbol)
                                         (,continue-symbol))))]
                            [normal-body (if (send body needs-escape-continuation? break-symbol)
                                             `(call-with-escape-continuation
                                               (lambda (,break-symbol)
                                                 ,bdy))
                                             bdy)])
                       (if else
                           `(begin ,normal-body ,(send else to-scheme))
                           normal-body)))))
                      
                                                     ;#f
                                                     
;                                ,(cond
;                                [(is-a? target tidentifier%)
;                                 `(lambda (,(send target to-scheme))
;                                    ,(send body to-scheme))]
;                                [(or (is-a? target ttuple%)
;                                     (is-a? target tlist-display%))
;                                 (let ([item (gensym 'item)])
;                                   `(lambda (,item)
;                                      (apply 
;                                       (lambda (,@(map (lambda (t) (send t to-scheme))
;                                                       (send target get-sub-targets)))
;                                         ,(send body to-scheme))
;                                       (,(py-so 'py-sequence%->list) ,item))))]
;                                [else (error "bad target for a 'for' loop")])
   ;                                (,(py-so 'py-sequence%->list) ,(send vals to-scheme)))))
   ;                   ,(if else
   ;                       (send else to-scheme)))))
      
      (super-instantiate ())))
  
  ;; 7.4
  (define try-except%
    (class statement%
      (inherit stx-err)
      ;; body: (is-a?/c suite%)
      (init-field body)
      ;; excepts: (listof (list/p (or/f false? (is-a?/c expression%))
      ;;                          (or/f false? (is-a?/c expression%))
      ;;                          (is-a?/c suite%)))
      (init excepts)
      ;; else: (or/f false? (is-a?/c suite%))
      (init-field else)
      (define (sub-stmt-map f)
        (cons (f body)
              (append
               (map (lambda (x) (f (caddr x))) exceptions)
               (if else
                   (list (f else))
                   null))))
      
      ;; exceptions: (listof (list/p (or/f false? (is-a?/c expression%))
      ;;                             (or/f false? (is-a?/c target%))
      ;;                             (is-a?/c suite%)))
      (define exceptions
        (map (lambda (x)
               (list (car x) (if (cadr x) (send (cadr x) to-target) #f) (caddr x)))
             excepts))

      (define/override (set-bindings! enclosing-scope)
        (send body set-bindings! enclosing-scope)
        (for-each (lambda (x)
                    (when (car x) (send (car x) set-bindings! enclosing-scope))
                    (when (cadr x) (send (cadr x) set-bindings! enclosing-scope))
                    (send (caddr x) set-bindings! enclosing-scope))
                  exceptions)
        (when else (send else set-bindings! enclosing-scope)))
        
      (define/override (check-break/cont enclosing-loop)
        (void (sub-stmt-map (lambda (s) (send s check-break/cont enclosing-loop)))))
      
      (define/override (collect-globals)
        (apply append (sub-stmt-map (lambda (s) (send s collect-globals)))))
                       
      (define exn (gensym 'exn))
      (inherit ->orig-so)
      (define/override (to-scheme)
        (let ([handled `(with-handlers ,(map (lambda (e&t&s)
                                               `[(lambda (,exn)
                                                   (and (,(py-so 'exn:python?) ,exn)
                                                        ,(if (car e&t&s)
                                                             `(,(py-so 'py-compatible-exn?)
                                                               ,(send (car e&t&s)
                                                                      to-scheme)
                                                               ,exn)
                                                             #t)))
                                                 ,(let ([handler-body (send (caddr e&t&s) to-scheme)])
                                                    `(lambda (,exn)
                                                       ,(if (cadr e&t&s)
                                                            `(,(generate-lambda (target->parameters (cadr e&t&s))
                                                                                handler-body
                                                                                #f)
                                                           (,(py-so 'exn:python-value) ,exn))
                                                         handler-body)))])
                                             exceptions)
                          ;; return the result in a list to signal successful try block
                         (list ,(send body to-scheme)))])
          (->orig-so (if else
                           `(if (list? ,handled)
                                  ,(send else to-scheme))
                         handled))))
      
      (super-instantiate ())
      (let loop ((e exceptions))
        (cond
          ((null? e) (void))
          ((not (null? (cdr e)))
           (if (not (caar e))
               (stx-err "default except clause must be last"))
           (loop (cdr e)))
          (else (loop (cdr e)))))))
  
  
  (define try-finally%
    (class statement%
      ;; body: (is-a?/c suite%)
      ;; finalizer: (is-a?/c suite%)
      (init-field body finalizer)
      
      (define scope #f)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (send body set-bindings! enclosing-scope)
        (send finalizer set-bindings! enclosing-scope))
      
      (define/override (check-break/cont enclosing-loop)
        (send body check-break/cont enclosing-loop)
        (send finalizer check-break/cont enclosing-loop))
      
      (define/override (collect-globals)
        (append (send body collect-globals)
                (send finalizer collect-globals)))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (let ([ec (send scope get-escape-continuation-symbol)]
              [value (gensym 'value)]
              [handled (gensym 'handled)]
              [finally (gensym 'finally)])
          (->orig-so `(let ([,ec (lambda (,value)
                                   (let ([,finally ,(send finalizer to-scheme)])
                                     (if (,(py-so 'exn:python?) ,value)
                                         (raise ,value)
                                         (,ec (if (void? ,finally) ; void unless finalizer returned something
                                                  ,value
                                                  ,finally)))))])
                        (,ec (with-handlers ([,(py-so 'exn:python?) identity])
                               ,(send body to-scheme)))))))
;                     `(let ([,handled (with-handlers ([,(py-so 'exn:python?) identity])
;                                        ,(send body to-scheme))])
;                        (let ([,finally ,(send finalizer to-scheme)])
;                          (if (,(py-so 'exn:python?) ,handled)
;                              (raise ,handled)
;                              ,finally))))))
                   
      (super-instantiate ())))
  
  
  (define (bindings-mixin %)
    (class %
      (super-instantiate ())
      (inherit-field body)
      (define global-table
        (let ((ht (make-hash-table)))
          (for-each
           (lambda (g) 
             (hash-table-put! ht (send g get-symbol) g))
           (send body collect-globals))
          ht))
      
      ;;daniel
      ;;  looks like global-table is a hash-table of key: symbol, value: identifier%
      
      ;; bindings: (listof (is-a?/c tidentifier%))
      (define bindings null)
      
      ;;daniel
      (define/public (get-global-table) global-table)
      (define/public (get-bindings) bindings)
      
      (define/public (add-binding id)
        (unless (hash-table-get global-table (send id get-symbol) (lambda () #f))
          (set! bindings (cons id bindings))))
      
;      (define/public (is-global? id)
;        (cond
;          ((hash-table-get global-table (send id get-symbol) (lambda () #f)) #t)
;          (else #f)))
      ;;daniel
      ;;  modified to return the tidentifier% in case it's in the global table
      (define/public (is-global? id)
        (hash-table-get global-table (send id get-symbol) (lambda () #f)))
      
      ;;daniel
      (define/public (is-local? id)
        (ormap (lambda (b)
                 (and (tidentifier=? id b) b))
               bindings))
      
      ;;daniel
      (define/public (is-bound? id)
        (or (is-global? id) (is-local? id)))
      
      ;;daniel
      ;; binding-tid: (union identifier% tidentifier%) -> (union identifier% tidentifier% false)
      ;; which tidentifier% or identifier% is the first-seen (binding) instance of this id?
      (define/public (binding-tid id)
        (is-bound? id))
      
      ))
  

  ;; daniel
  ;;  use this as the object type of the initial empty environment for a module
  (define module-scope%
    (bindings-mixin
     (class object%
       (field [body (make-object (class object%
                                   (define/public (collect-globals) null)
                                   (super-instantiate ())))])
       (define/public (get-escape-continuation-symbol) (gensym 'unusable))
       (super-instantiate ()))))
  
  ;; 7.5
  (define function-definition%
    (bindings-mixin
     (class statement%
       (inherit stx-err)
       
       ;; name: (is-a?/c identifier%)
       ;; parms: (is-?/c parameters%)
       ;; body: (is-a?/c suite%)
       (init-field name parms body)
       
       ;;daniel -- moved (send name to-target) here to make set-bindings! work correctly
       (define tname (send name to-target))

       (define return-symbol (gensym 'return))
      
       (define/public (get-return-symbol) return-symbol)
       (define/public (get-escape-continuation-symbol) return-symbol)
       
       (define/override (set-bindings! es)
         (when es
           (unless (send es is-local? tname)
             (send es add-binding tname)))
         (send parms set-bindings! es)
         (send body set-bindings! this))
       
       (define/override (check-break/cont enclosing-loop)
         (send body check-break/cont #f))
       
       (define/public (get-parms) parms)
       
       (send body set-tail-position!)
       
       
       ;;daniel
       (inherit ->orig-so ->lex-so)
       (define/override (to-scheme)
         (->orig-so (let ([proc-name (send name to-scheme)])
                      `(define ,proc-name ;(send name get-symbol)
                         ;namespace-set-variable-value! #cs',proc-name
                         ,(generate-py-lambda proc-name
                                              parms
                                              (send body to-scheme return-symbol #t #t))))
                ;   (current-toplevel-context)
                   ))
;                         (,(py-so 'procedure->py-function%)
;                          ;; this is where the outer "let" for default values should
;                          ;; be placed; I'll do that after I finish the rest of the def/call code.
;                          ;; it's bad design anyway...
;                          (opt-lambda ,(send parms to-scheme)
;                            (let ,(append (normalize-assoc-list
;                                           (flatten1
;                                            (map (lambda (tuple)
;                                                   (unpack tuple
;                                                           (send (first-atom tuple) to-scheme)))
;                                                 (filter list? pos))))
;                                          (map (lambda (b)
;                                                 `[,(send b to-scheme) (void)])
;                                               (send this get-bindings)))
;                              ,(send body to-scheme return-symbol #t)))
;                          ',proc-name
;                          (list ,@(map (lambda (p)
;                                         `',(send (first-atom p) to-scheme))
;                                       pos))
;                          (list ,@(map (lambda (k)
;                                         `(cons ',(send (car k) to-scheme)
;                                                ,(send (cdr k) to-scheme)))
;                                       key))
;                          ,(and seq (send seq to-scheme))
;                          ,(and dict (send dict to-scheme)))))))
       
       (super-instantiate ()))))
    
  ;; 7.6
  (define class-definition%
    (bindings-mixin
     (class statement%
       ;; name: (is-a?/c identifier%)
       ;; inherit-expr: (listof (is-a?/c expression%))
       ;; body: (is-a?/c suite%)
       (init-field name inherit-expr body)
       
       ;; daniel -- moved send to-target here
       (define tname (send name to-target))
       
       (define/override (set-bindings! enclosing-scope)
         (when (and enclosing-scope
                    (not (send enclosing-scope is-local? tname)))
           (send enclosing-scope add-binding tname))
         (for-each (lambda (x) (send x set-bindings! enclosing-scope)) inherit-expr)
         (send body set-bindings! this))
       
       (define/override (check-break/cont enclosing-loop)
         (send body check-break/cont #f))
       
       ;;daniel
       (inherit ->orig-so ->lex-so)
       (define/override (to-scheme)
         (let ([class-name (send name to-scheme)]
               [inherit-list (map (lambda (i) (send i to-scheme)) inherit-expr)])
         (->orig-so `(define ,class-name
                       (py-apply cpy-type
                                 #|
                        (list (,(py-so 'symbol->py-string%) #cs',class-name)
                              (,(py-so 'list->py-tuple%) (list ,@(if (empty? inherit-list)
                                                                     `(,(->lex-so 'object (current-toplevel-context)))
                                                                     inherit-list)))
                        ,(let* ([exprs null]
                                [keys null]
                                [defs
                                 (map (lambda (def)
                                        (begin0
                                          `(lambda (this-class)
                                             (list #cs(quote ,(second def))
                                                   ,(if (or (null? keys)
                                                            (eq? 'method (first def)))
                                                        (third def)
                                                        `(let-values ([,keys
                                                                       (values ,@(map (lambda (key)
                                                                                        `(,(py-so 'python-get-member)
                                                                                          this-class
                                                                                          #cs(quote ,key)
                                                                                          #f))
                                                                                      keys))])
                                                           ,(third def)))))
                                          (unless (member (second def) keys)
                                            (set! keys (cons (second def) keys)))))
                                      (filter (lambda (def-or-expr)
                                                (or (list? def-or-expr)
                                                    (begin (set! exprs (cons def-or-expr exprs))
                                                           #f)))
                                              (send body defs-and-exprs)))])
                           `(begin ,@(reverse exprs)
                                   (list ,@defs)) |#
                           (make-py-symbol ',class-name)
                           (make-py-tuple (list ,@(if (empty? inherit-list)
                                                      (list 'cpy-object)
                                                      inherit-list)))
                           (make-py-dict (let ()
                                           ,(send body to-scheme)
                                           (list ,@(map (lambda (b)
                                                        (let ([id (send b to-scheme)])
                                                          `(cons (make-py-symbol ',id) ,id)))
                                                      (send this get-bindings)))))
                           )))))
                           
       
       (super-instantiate ()))))
)
