(module compiler-target mzscheme
  (require (lib "class.ss")
           "runtime-support.ss"
	   "compiler.ss")
           ;"empty-context.ss")

  (provide (all-defined))

  ;; 6.3
  (define target%
    (class ast-node%
	(super-instantiate ())))
  
  (define tidentifier%
    (class target%
      ;; name: symbol?
      (init-field name)
      
      (define scope #f)
      
      (define/public (get-symbol) name)
      
      (define/override (set-bindings! enclosing-scope)
        (when enclosing-scope 
          (unless (send enclosing-scope is-local? this)
            (send enclosing-scope add-binding this))
          (set! scope enclosing-scope)))
      
      ;;daniel
      (define/public (get-sub-targets) (list this)) ; I am my subtargets :P
      
      ;;daniel
      (inherit ->orig-so ->lex-so)
      (define/override (to-scheme)
        (->lex-so (get-symbol) (current-toplevel-context)))
      
      ;;; had to change that context from an empty context defined in some module to no context.
      ;;; starting with mzscheme 203.4, top-level definitions cannot define variables with the context
      ;;;   of another module.
      ;;; the burden of namespace management is then shifted from the compiler (through contexts)
      ;;;   to the evaluator (through namespaces).
      ;;; i.e., make sure eval is called within an empty namespace (plus python built-ins).
      
      (super-instantiate ())))
  
  (define (tidentifier=? a b)
    (eq? (send a get-symbol)
         (send b get-symbol)))

  (define ttuple%
    (class target%
      ;; sub-targets: (listof (is-a?/c target%))
      (init-field sub-targets)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x) (send x set-bindings! enclosing-scope)) sub-targets))
      
      ;;daniel
      (define/public (get-sub-targets) sub-targets)
      
;      ;;daniel
;      (inherit ->orig-so)
;      (define/override (to-scheme)
;        (->orig-so `(list ,@(map (lambda (t) (send t to-scheme))
;                                 sub-targets))))
      
      (super-instantiate ())))
  
  (define tlist-display%
    (class target%
      ;; sub-targets: (listof (is-a?/c target%))
      (init-field sub-targets)

      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x) (send x set-bindings! enclosing-scope)) sub-targets))

      ;;daniel
      (define/public (get-sub-targets) sub-targets)
      
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
  )
  