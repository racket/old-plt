(module compiler-target mzscheme
  (require (lib "class.ss")
	   "compiler.ss")

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
          (send enclosing-scope add-binding this)
          (set! scope enclosing-scope)))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so (get-symbol)))
      
      (super-instantiate ())))

  (define ttuple%
    (class target%
      ;; sub-targets: (listof (is-a?/c target%))
      (init-field sub-targets)
      
      (define/override (set-bindings! enclosing-scope)
        (for-each (lambda (x) (send x set-bindings! enclosing-scope)) sub-targets))
      
      ;;daniel
      (inherit ->orig-so)
      (define/override (to-scheme)
        (->orig-so `(list ,@(map (lambda (t) (send t to-scheme))
                                 sub-targets))))
      
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
  )
  