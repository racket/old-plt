(module compiler-stmt mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss") ; build-list
	   "compiler.ss"
	  ; "compiler-expr.ss"
           "compiler-target.ss"
          ; "primitives.ss"
           "runtime-context.ss"
           "empty-context.ss")

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
                       (error ,(if expr2
                                   `(format "AssertError: ~a"
                                           (,(py-so 'py-object%->string) ,(send expr2 to-scheme)))
                                   "AssertError")))))
      
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
        (and (not (is-a? t tattribute-ref%))
             (binding-targ? t)))
      
      (define (set-targ? t)
        (and (not (is-a? t tattribute-ref%))
             (not (binding-targ? t))))
      
      (define (attr-targ? t)
        (is-a? t tattribute-ref%))  
      
      ;; assignment-so: target% syntax-object -> syntax-object
      (define (assignment-so target rhs)
        (->orig-so
         (cond
           [(is-a? target tidentifier%) `(,(if (binding-targ? target)
                                               'define
                                               'set!) ,(send target to-scheme) ,rhs)]
           [(is-a? target tattribute-ref%) `(,(py-so 'python-set-member!)
                                             ,(send ((class-field-accessor tattribute-ref%
                                                                           expression) target)
                                                    to-scheme)
                                             ',(send ((class-field-accessor tattribute-ref%
                                                                            identifier) target)
                                                     to-scheme)
                                             ,rhs)]
           [(or (is-a? target ttuple%)
                (is-a? target tlist-display%)) (let ([targets (send target get-sub-targets)])
                   (let* ([indexes (build-list (length targets) identity)]
                             [def-targs (filter (lambda (i)
                                                  (def-targ? (list-ref targets i)))
                                                indexes)]
                             [set-targs (filter (lambda (i)
                                                  (set-targ? (list-ref targets i)))
                                                indexes)]
                             [attr-targs (filter (lambda (i)
                                                   (attr-targ? (list-ref targets i)))
                                                 indexes)])
                        `(define-values ,(map (lambda (i)
                                                (send (list-ref targets i) to-scheme))
                                              def-targs)
                           (begin ,@(map (lambda (i)
                                            `(set! ,(send (list-ref targets i) to-scheme)
                                                   (,(py-so 'python-index) ,rhs ,i)))
                                          set-targs)
                                   ,@(map (lambda (i)
                                            `(,(py-so 'python-set-member!)
                                             ,(send ((class-field-accessor tattribute-ref%
                                                                           expression) (list-ref targets i))
                                                    to-scheme)
                                             ',(send ((class-field-accessor tattribute-ref%
                                                                            identifier) (list-ref targets i))
                                                     to-scheme)
                                             (,(py-so 'python-index) ,rhs ,i)))
                                          attr-targs)
                                   (values ,@(map (lambda (i)
                                                    `(,(py-so 'python-index) ,rhs ,i))
                                                  def-targs))))))]
           [else (error "target type not supported yet")])))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (let ([rhs (gensym 'rhs)])
        (->orig-so `(begin (define ,rhs ,(send expression to-scheme))
                             ,@(map (lambda (t)
                                      (assignment-so t rhs))
                                    targets)))))
      
      (super-instantiate ())))
  
  ;;6.3.1
  (define aug-assignment%
    (class statement%
      ;; targ-exp: (is-a?/c expression%)
      (init targ-exp)

      ;; op: (symbols '+= '-= '*= '/= '%= '&= '\|= '^= '<<= '>>= '**= '//=)
      ;; expression: expression%
      (init-field op expression)

      (define target (send targ-exp to-target))
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope)
        (send target set-bindings! enclosing-scope))
      
      (super-instantiate ())))

  ;; 6.4
  (define pass%
    (class statement%
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so '(void)))
      
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
        (->orig-so (if to-file?
                       `(print-to-file blah)
                       `(,(py-so 'py-print) (list ,@(map (lambda (e)
                                              (send e to-scheme))
                                            expressions))))))
      
      (super-instantiate ())))
  
  ;; 6.7
  (define return%
    (class statement%
      ;; expression: (or/f false? (is-a?/c expression%)
      (init-field expression)
      
      (define scope #f)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (when expression
          (send expression set-bindings! enclosing-scope)))
      
      ;;daniel
      (inherit ->orig-so ->lex-so)
      (define/override (to-scheme); py-return)
        ;(->orig-so `(,py-return ,(send expression to-scheme))))
        (->orig-so `(,(send scope get-return-symbol) ,(send expression to-scheme))))
      
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
;        (->orig-so (if type
;                       (let ([type (send type to-scheme)])
;                         (if parm
;                             (let ([parm (send parm to-scheme)])
;                               (if traceback
;                                   (let ([traceback (send traceback to-scheme)])
;                                     `(raise (,(py-so 'py-create) type parm)))
;                                   `(raise (,(py-so 'py-create) type parm))))
;                             `(raise (,(py-so 'py-create) type))))
;                       `(raise (,(py-so 'python-current-exception))))))
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
      
      (define/override (check-break/cont enclosing-loop)
        (if enclosing-loop
            (send enclosing-loop set-can-break?)
            (stx-err "Break statement must be within loop")))
      
      (super-instantiate ())))

  ;; 6.11
  (define continue%
    (class statement%
      (inherit stx-err)

      (define/override (check-break/cont enclosing-loop)
        (if enclosing-loop
            (send enclosing-loop set-can-cont?)
            (stx-err "Continue statement must be within loop")))
      
      (super-instantiate ())))
  
  ;; 6.12
  (define import-module%
    (class statement%
      ;; modules: (listof (list/p (listof (is-a?/c identifier%))
      ;;                          (or/f false? (is-a?/c identifier%))))
      (init-field modules)

      (define/override (set-bindings! enclosing-scope)
        (map caar modules))
      
      (super-instantiate ())))

  (define import-from%
    (class statement%
      ;; module: (listof (is-a?/c identifier%))
      ;; ids: (or/f (symbols '*) (listof (list/p (is-a?/c identifier%)
      ;;                                         (or/f false? (is-a?/c identifier%)))))
      (init-field module ids)
      
      (define/override (set-bindings! enclosing-scope)
        (unless (symbol? ids) (map (lambda (b) (if (cadr b) (cadr b) (car b))) ids)))
      
      (super-instantiate ())))
  
  ;; 6.13
  (define global%
    (class statement%
      ;; identifiers: (listof (is-a?/c identifier%))
      (init-field identifiers)

      (define/override (collect-globals) identifiers)
      
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
    
  ;; 7
  (define suite%
    (class statement%
      
      ;; statements: (listof (is-a?/c statement%))
      (init-field statements)
      (define (sub-stmt-map f)
        (map f statements))
      
      (define scope #f)
      
      (define/override (set-bindings! enclosing-scope)
        (set! scope enclosing-scope)
        (for-each (lambda (s) (send s set-bindings! enclosing-scope)) statements))

      (define/override (check-break/cont enclosing-loop)
        (void (sub-stmt-map (lambda (s) (send s check-break/cont enclosing-loop)))))

      (define/override (collect-globals)
        (apply append (sub-stmt-map (lambda (s) (send s collect-globals)))))
      
      
      ;;daniel
      ; defs-and-exprs:  -> (listof (union sexp (list name value)))
      (define/public (defs-and-exprs)
            (sub-stmt-map (lambda (s)
                            (let* ([so (send s to-scheme)]
                                   [so-l (syntax->list so)])
                              (if (and so-l
                                       (free-identifier=? (first so-l)
                                                          (datum->syntax-object compiler-context
                                                                                'define)))
                                  (list (second so-l) (third so-l))
                                  so)))))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override to-scheme
        (opt-lambda ([escape-continuation-symbol (gensym 'unusable)])
            (->orig-so ;(let ([py-return (gensym 'return)])
                         `(call-with-escape-continuation
                           (lambda (,escape-continuation-symbol) ;py-return)
                             ,@(sub-stmt-map (lambda (s) (send s to-scheme))))))))
;                                               (if (is-a? s return%)
;                                                   (send s to-scheme py-return)
;                                                   (send s to-scheme)))))))))
         
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
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (let ([s-test-bodies (map (lambda (test-body)
                               `[(,(py-so 'py-if) ,(send (car test-body) to-scheme) #t #f)
                                 ,(send (cadr test-body) to-scheme)])
                             test-bodies)])
        (->orig-so (if else
                       `(cond
                          ,@s-test-bodies
                          [else ,(send else to-scheme)])
                       `(cond ,@s-test-bodies)))))
      
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
      (define (set-can-break?) (set! can-break? #t))
      (define (set-can-cont?) (set! can-cont? #t))
      
      (define/override (set-bindings! enclosing-scope)
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

      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(begin
                      (for-each ,(cond
                                [(is-a? target tidentifier%)
                                 `(lambda (,(send target to-scheme))
                                    ,(send body to-scheme))]
                                [(or (is-a? target ttuple%)
                                     (is-a? target tlist-display%))
                                 (let ([item (gensym 'item)])
                                   `(lambda (,item)
                                      (apply 
                                       (lambda (,@(map (lambda (t) (send t to-scheme))
                                                       (send target get-sub-targets)))
                                         ,(send body to-scheme))
                                       (,(py-so 'py-sequence%->list) ,item))))]
                                [else (error "bad target for a 'for' loop")])
                              (,(py-so 'py-sequence%->list) ,(send vals to-scheme)))
                      ,(if else
                          (send else to-scheme)))))
      
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
      
      (define/override (set-bindings! enclosing-scope)
        (send body set-bindings! enclosing-scope)
        (send finalizer set-bindings! enclosing-scope))
      
      (define/override (check-break/cont enclosing-loop)
        (send body check-break/cont enclosing-loop)
        (send finalizer check-break/cont enclosing-loop))
      
      (define/override (collect-globals)
        (append (send body collect-globals)
                (send finalizer collect-globals)))
      
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
       
       ;;daniel
       (inherit ->orig-so)
       (define/override (to-scheme)
         (->orig-so (let ([proc-name (send name to-scheme)])
                      `(define ,proc-name
                         (,(py-so 'py-lambda) ',proc-name ,(send parms to-scheme)
                                            ,(send body to-scheme return-symbol))))))
       
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
                       (,(py-so 'python-method-call) ,(py-so 'py-type%) '__call__
                        (,(py-so 'symbol->py-string%) #cs',class-name)
                        (,(py-so 'list->py-tuple%) (list ,@(if (empty? inherit-list)
                                                               `(,(->lex-so 'object empty-context))
                                                               inherit-list)))
                        ,(let* ([exprs null]
                                [keys null]
                                [values null]
                                [defs
                                 (map (lambda (def)
                                        (begin0
                                          `(lambda (this-class)
                                             (list #cs',(first def)
                                                   ,(if (null? keys)
                                                        (second def)
                                                        `(let-values ([,keys
                                                                       (values ,@(map (lambda (key)
                                                                                        `(,(py-so 'python-get-member)
                                                                                          this-class
                                                                                          #cs',key
                                                                                          #f))
                                                                                      keys))])
                                                           ,(second def)))))
                                          (set! keys (cons (first def) keys))
                                          (set! values (cons (second def) values))))
                                      (filter (lambda (def-or-expr)
                                                (or (list? def-or-expr)
                                                    (begin (set! exprs (cons def-or-expr exprs))
                                                           #f)))
                                              (send body defs-and-exprs)))])
                           `(begin ,@(reverse exprs)
                                   (list ,@defs))))))))
                           
       
       (super-instantiate ()))))
)