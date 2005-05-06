(module honu-typecheck-function mzscheme
  (require (lib "struct.ss")
           (lib "plt-match.ss")
           (lib "list.ss" "srfi" "1"))
  
  (require "../../ast.ss")
  (require "honu-type-utils.ss")
  (require "honu-typecheck-exp.ss")
  (require "../../read-error-with-stx.ss")
    
  (provide honu-typecheck-function)
  (define (honu-typecheck-function pgm defn)
    (match-let ([(struct honu-function (stx name type arg-names arg-types body)) defn])
      (let ((env (fold (lambda (n t e)
                         (extend-env e n t))
                       (get-initial-env pgm)
                       arg-names
                       arg-types)))
        (let-values ([(e1 t1) ((honu-typecheck-exp pgm env (empty-env)) body type)])
          (copy-struct honu-function defn
            (honu-function-body e1))))))
  )
