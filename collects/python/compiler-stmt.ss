(module compiler-stmt mzscheme
  (require (lib "class.ss")
	   "compiler.ss")

  (provide-all-defined-except bindings-mixin)

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
      (super-instantiate ())))

  ;; 6.1
  (define expr-stmt%
    (class statement%
      ;; expression: (is-a?/c expression%)
      (init-field expression)
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
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
      
      (super-instantiate ())))
  
  ;; 6.3
  (define assignment%
    (class statement%
      ;; targ-exps: (listof (is-a?/c target%))
      (init targ-exps)
      ;; expression: (is-a?/c expression%)
      (init-field expression)
      ;; targets: (listof (is-a?/c target%))
      (define targets (map (lambda (e) (send e to-target)) targ-exps))
      
      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope)
        (for-each (lambda (e) (send e set-bindings! enclosing-scope)) targets))
      
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
      
      (super-instantiate ())))
  
  ;; 6.7
  (define return%
    (class statement%
      ;; expression: (or/f false? (is-a?/c expression%)
      (init-field expression)
      
      (define/override (set-bindings! enclosing-scope)
        (when expression
          (send expression set-bindings! enclosing-scope)))
      
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
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (s) (send s set-bindings! enclosing-scope)) statements))

      (define/override (check-break/cont enclosing-loop)
        (void (sub-stmt-map (lambda (s) (send s check-break/cont enclosing-loop)))))

      (define/override (collect-globals)
        (apply append (sub-stmt-map (lambda (s) (send s collect-globals)))))
      
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
      
      ;; bindings: (listof (is-a?/c tidentifier%))
      (define bindings null)
      
      (define/public (add-binding id)
        (unless (hash-table-get global-table (send id get-symbol) (lambda () #f))
          (set! bindings (cons id bindings))))
      
      (define/public (is-global? id)
        (cond
          ((hash-table-get global-table (send id get-symbol) (lambda () #f)) #t)
          (else #f)))))
  
  ;; 7.5
  (define function-definition%
    (bindings-mixin
     (class statement%
       (inherit stx-err)
       
       ;; name: (is-a?/c identifier%)
       ;; parms: (is-?/c parameters%)
       ;; body: (is-a?/c suite%)
       (init-field name parms body)
       
       (define/override (set-bindings! es)
         (when es (send es add-binding (send name to-target)))
         (send parms set-bindings! es)
         (send body set-bindings! this))
       
       (define/override (check-break/cont enclosing-loop)
         (send body check-break/cont #f))
       
       (super-instantiate ()))))
    
  ;; 7.6
  (define class-definition%
    (bindings-mixin
     (class statement%
       ;; name: (is-a?/c identifier%)
       ;; inherit-expr: (listof (is-a?/c expression%))
       ;; body: (is-a?/c suite%)
       (init-field name inherit-expr body)
       
       (define/override (set-bindings! enclosing-scope)
         (when enclosing-scope (send enclosing-scope add-binding (send name to-target)))
         (for-each (lambda (x) (send x set-bindings! enclosing-scope)) inherit-expr)
         (send body set-bindings! this))
       
       (define/override (check-break/cont enclosing-loop)
         (send body check-break/cont #f))
       
       (super-instantiate ()))))
)