(module compiler mzscheme
  (require (lib "class.ss")
          ; (lib "list.ss")
           (lib "lex.ss" "parser-tools")
           (lib "readerr.ss" "syntax"))
           
  (provide (all-defined-except stx-orig-prop))
  
  (define stx-orig-prop
    (read-syntax #f (open-input-string "orig")))
  
  (define compiler-context #'here)
  
  (define new-context null)
  (define (set-context! ctx)
    (set! new-context ctx))
  
  
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
        (datum->syntax-object compiler-context datum src-loc stx-orig-prop))
      
      ;; ->lex-so: datum context -> syntax-object
      ;; same as ->orig-so, but with lexical information
      (define/public (->lex-so datum context)
        (datum->syntax-object context datum src-loc stx-orig-prop))

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
      
      
      ;;daniel
      ;; to-scheme: -> syntax-object?
      (define/public (to-scheme)
        (stx-err (format "Invalid usage of to-scheme on an ast-node% (I'm purely virtual) ~a" this)))
      
      (super-instantiate ())))

  )