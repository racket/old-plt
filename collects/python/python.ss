(module python mzscheme
  (require (lib "class.ss")
          ; "compiler.ss"
           "read-python.ss"
           "base.ss")
  
  ;;;; temporary Python Evaluation module by Daniel ;;;;;;;
  
  (provide python
           read-python
           python-to-scheme
           compile-python
           compile-python-ast
           parse-python-port
           parse-python-file)
  
  (define (python-to-scheme path)
    (compile-python (read-python path)))
  
  (define (compile-python ast-list)
    ;    (set-context! #'here)
    (map (lambda (ast)
           (send ast to-scheme))
         ast-list))
  
  (define (compile-python-ast ast)
    (send ast to-scheme))
  
  (define parse-python-port read-python-port)
  
  (define parse-python-file read-python)
  
  (define (python path)
    ;; setup initial eval namespace
    (let ([m-path ((current-module-name-resolver) '(lib "base.ss" "python") #f #f)]
          [empty-namespace (make-namespace 'empty)]
          [n (current-namespace)])
      (dynamic-require m-path #f)
      ;; eval in new namespace
      (let ([results (parameterize ([current-namespace empty-namespace])
                       (namespace-attach-module n m-path)
                       (namespace-require m-path)
                       (map eval
                            (python-to-scheme path)))])
        ;; copy all new bindings to the first namespace
        (for-each (lambda (symbol)
                    (unless (namespace-variable-value symbol #t
                                                      (lambda () #f))
                      (with-handlers ([exn? (lambda (exn) #f)])
                        (namespace-set-variable-value! symbol
                                                       (parameterize ([current-namespace empty-namespace])
                                                         (namespace-variable-value symbol))))))
                  (parameterize ([current-namespace empty-namespace])
                    (namespace-mapped-symbols)))
        ;; return the results in a nice format
        (map (lambda (result)
               (if (python-node? result)
                   (py-object%->string result)
                   result))
             (filter (lambda (r) (not (void? r)))
                     results)))))
  
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