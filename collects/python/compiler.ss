(module compiler mzscheme
  (require (lib "class.ss")
           (lib "lex.ss" "parser-tools")
           (lib "readerr.ss" "syntax"))
           
  (provide (all-defined-except stx-orig-prop))
  
  (define stx-orig-prop
    (read-syntax #f (open-input-string "orig")))
      
  (define ast-node%
    (class object%
      (init-field start-pos end-pos)
      
      (define src-loc
        (list (file-path)
              (position-line start-pos)
              (position-col start-pos)
              (position-offset start-pos)
              (- (position-offset end-pos)
                 (position-offset start-pos))))
      
      ;; stx-err: string? ->
      ;; raises an exception with the source of the expression
      (define/public (stx-err msg)
        (apply raise-read-error (cons msg src-loc)))

      ;; ->orig-so: datum? -> syntax-object?
      ;; converts the datum into a syntax-object using the source
      ;; location of the expression.  Uses the stx-orig-prop
      ;; to make the syntax object look like it appeared in the
      ;; source text.  This way check syntax will highlight it.
      (define/public (->orig-so datum)
        (datum->syntax-object #f datum src-loc stx-orig-prop))        

      ;; set-bindings!: (or/f fales? (is-a?/c def%) (is-a?/c class%) (is-a?/c lambda%)) ->
      ;; Compute all the bindings this statement could introduce into
      ;; its surrounding scope, assuming that the statement is contained
      ;; inside of some lexical scope.  Scopes are created by lambda, def
      ;; and class.
      (define/public (set-bindings! enclosing-scope) null)

      ;; ->so: datum? -> syntax-object?
      ;; Like ->orig-so, but doesn't set the property.
      (define/public (->so datum)
        (datum->syntax-object #f datum src-loc))
      
      ;; compile: -> syntax-object?
      (define/public (compile)
        (stx-err "The compiler does not understand this expression"))
      
      (super-instantiate ())))
  
  ;; 6.3
  (define target%
    (class ast-node%
      (super-instantiate ())))
  
  (define expression%
    (class ast-node%
      (inherit stx-err)

      ;; to-target: -> (is-a?/c target%)
      ;; Raises an exception if the expression is not a valid
      ;; assignment target, otherwise returns a target%
      (define/public (to-target)
        (stx-err "Invalid target"))
      (super-instantiate ())))

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
      
      (super-instantiate ())))
  
  (define tidentifier%
    (class target%
      ;; name: symbol?
      (init-field name)
      
      (define scope #f)
      
      (define/public (get-symbol) name)
      
      (define/override (set-bindings! enclosing-scope)
        (when enclosing-scope 
          (send enclosing-scope add-binding this)
          (set! scope enclosing-scope)))
          
      (super-instantiate ())))

  (define ttuple%
    (class target%
      ;; sub-targets: (listof (is-a?/c target%))
      (init-field sub-targets)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x) (send x set-bindings! enclosing-scope)) sub-targets))
      
      (super-instantiate ())))
  
  (define tlist-display%
    (class target%
      ;; sub-targets: (listof (is-a?/c target%))
      (init-field sub-targets)

      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x) (send x set-bindings! enclosing-scope)) sub-targets))
      (super-instantiate ())))
  
  (define tattribute-ref%
    (class target%
      ;; expression: (is-a?/c expression%)
      ;; identifier: (is-a?/c identifier%)
      (init-field expression identifier)

      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
      (super-instantiate ())))
  
  (define tsubscription%
    (class target%
      ;; expression: (is-a?/c expression%)
      ;; sub: (is-a?/c expression%)
      (init-field expression sub)

      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope)
        (send sub set-bindings! enclosing-scope))
      
      (super-instantiate ())))
  
  (define tsimple-slicing%
    (class target%
      ;; expression: (is-a?/c expression%)
      ;; lower: (or/f false? (is-a?/c expression%))
      ;; upper: (or/f false? (is-a?/c expression%))
      (init-field expression lower upper)

      (define/override (set-bindings! enclosing-scope)
        (when expression (send expression set-bindings! enclosing-scope))
        (when lower (send lower set-bindings! enclosing-scope))
        (when upper (send upper set-bindings! enclosing-scope)))
      
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
      
      (super-instantiate ())))

  ;; 5.2.2
  (define literal%
    (class expression%

      ;; value: (or/f string? number?)
      (init-field value)

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
      
      (super-instantiate ())))

  (define list-comprehension%
    (class expression%
      ;; expr: (is-a?/c expression%)
      ;; for: (is-a?/c list-for%)
      (init-field expr for)

      (define/override (set-bindings! enclosing-scope)
        (send expr set-bindings! enclosing-scope)
        (send for set-bindings! enclosing-scope))
      
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
      
      (super-instantiate ())))
  
  ;; 5.2.6
  (define string-conversion%
    (class expression%
      ;; expression: (is-a?/c expression%)
      (init-field expression)

      (define/override (set-bindings! enclosing-scope)
        (send expression set-bindings! enclosing-scope))
      
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
      
      (super-instantiate ())))

  ;; 5.5, 5.10
  (define unary%
    (class expression%
      ;; op: (symbols 'not '+ '- '~)
      ;; rhs: (is-a?/c expression%)
      (init-field op rhs)

      (define/override (set-bindings! enclosing-scope)
        (send rhs set-bindings! enclosing-scope))
      
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