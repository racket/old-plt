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


  )