(module python mzscheme
  (require (lib "class.ss")
          ; (lib "list.ss")
          ; (lib "etc.ss")
          ; "compiler.ss"
          ; "python-node.ss"
          ; "primitives.ss" ;; need py-object%->string
          ; "read-python.ss"
           "compile-python.ss"
           "python-import.ss")
           ;"base.ss")
  
  ;;;; temporary Python Evaluation module by Daniel ;;;;;;;
  
  (provide python
           ;read-python
           ;python-to-scheme
           ;compile-python
           ;compile-python-ast
           ;parse-python-port
           ;parse-python-file
           render-python-value
           render-python-value/format)
  
  (define (convert-value value)
    ((dynamic-require '(lib "primitives.ss" "python") 'py-object%->string) value))
  
  (define (render-python-value/format value port port-write)
    (render-python-value value port port-write))
  
  (define (render-python-value value port port-write)
            (let ([to-render (convert-value value)])
                             ;(if (python-node? value)
                             ;   (format "~a" (py-object%->string value))
                             ;   value)])
              (if #f ;port-write
                  (port-write to-render)
                  (display to-render port))))
  


  
    
  (define (python path)
    (let ([results (eval-python&copy (python-to-scheme path) (make-python-namespace))])
      ;(let ([port (current-output-port)])
      ;  (for-each (lambda (value)
      ;              (render-python-value value port printf))
      ;            results))
      (map convert-value results)))
        
                     
;  (define (python path)
;    ;; setup initial eval namespace
;    (let ([m-path ((current-module-name-resolver) '"base.ss" #f #f)]
;          [empty-namespace (make-namespace 'empty)]
;          [n (current-namespace)])
;      (dynamic-require m-path #f)
;      ;; eval in new namespace
;      (let ([results (parameterize ([current-namespace empty-namespace])
;                       (namespace-attach-module n m-path)
;                       (namespace-require m-path)
;                       (map eval
;                            (python-to-scheme path)))])
;        ;; copy all new bindings to the first namespace
;        (for-each (lambda (symbol)
;                    (unless (namespace-variable-value symbol #t
;                                                      (lambda () #f))
;                      (with-handlers ([exn? (lambda (exn) #f)])
;                        (namespace-set-variable-value! symbol
;                                                       (parameterize ([current-namespace empty-namespace])
;                                                         (namespace-variable-value symbol))))))
;                  (parameterize ([current-namespace empty-namespace])
;                    (namespace-mapped-symbols)))
;        ;; return the results in a nice format
;        (map (lambda (result)
;               (if (python-node? result)
;                   (py-object%->string result)
;                   result))
;             (filter (lambda (r) (not (void? r)))
;                     results)))))
  



 ; (require "compiler-stmt.ss")
  
;  (define ast (fifth (read-python "mini.py")))
;  (define suite ((class-field-accessor function-definition% body) ast))
;  (define statements ((class-field-accessor suite% statements) suite))
;  (define x1 (cadr statements))
;  (define x2 (caddr statements))
;  (define scope1 (send x1 get-scope))
;  (define scope1-globals (send scope1 get-global-table))
;  (define scope1-bindings (send scope1 get-bindings))
  
;(define g (fourth statements))
;  (define g-suite ((class-field-accessor function-definition% body) g))
;(define g-statements ((class-field-accessor suite% statements) g-suite))
; (define b (car g-statements))
; (define b-scope (send b get-scope))
; (define b-bindings (send b-scope get-bindings)  )
  
;  (let* ([ast-l (read-python "mini.py")]
;         [ax (first ast-l)]
;         [bx (second ast-l)]
;         [atarg (first (send ax get-targs))]
;         [btarg (first (send bx get-targs))]
;         [ascope (send ax get-scope)]
;         [bscope (send bx get-scope)])
;    (list (send ascope is-bound? atarg)
;          (eq? (send ascope binding-tid atarg) atarg)
;          (eq? (send ascope binding-tid atarg) btarg)
;          (eq? (send ascope binding-tid btarg) atarg)
;          (eq? (send ascope binding-tid btarg) btarg))
;    (equal? (send ascope binding-tid atarg) atarg)
;    (eq? btarg (send ascope is-local? atarg))
;    (eq? (first (send ax get-targs)) (first (send ax get-targs)))
;    (eq? (send ascope binding-tid btarg)
;         atarg))
  )