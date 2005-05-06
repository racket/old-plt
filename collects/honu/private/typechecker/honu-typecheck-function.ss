(module honu-typecheck-function mzscheme
  (require (lib "struct.ss"))
  (require (lib "list.ss" "srfi" "1"))
  
  (require "../../ast.ss")
  (require "honu-type-utils.ss")
  (require "honu-typecheck-exp.ss")
  (require "../../read-error-with-stx.ss")
    
  (provide honu-typecheck-function)
  (define (honu-typecheck-function pgm defn)
    (let ((env (fold (lambda (n t e)
                       (extend-env e n t))
                     (get-initial-env pgm)
                     (honu-function-arg-names defn)
                     (honu-function-arg-types defn))))
      (let-values (((e1 t1) ((honu-typecheck-exp pgm env (empty-env))
                             (honu-function-body defn))))
        (if (<:_P pgm t1 (honu-function-type defn))
            (copy-struct honu-function defn
              (honu-function-body e1))
            (raise-read-error-with-stx
              "Return type for function does not match body's type"
              (honu-ast-src-stx (honu-function-type defn)))))))
  )