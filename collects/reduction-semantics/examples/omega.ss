(module omega mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss")
  
  (reduction-steps-cutoff 10)
  
  (define lang
    (language (e (e e)
                 variable
                 v)
              (c (v c)
                 (c e)
                 hole)
              (v (lambda (variable) e))))
  
  (define reductions
    (list
     (reduction/context lang
                        c
                        ((lambda ((name x variable)) (name body e)) (name arg v))
                        (lc-subst x arg body))))
  
  (define lc-subst
    (subst
     [(? symbol?) (variable)]
     [`(lambda (,x) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list x) b)]
     [`(,f ,x)
      (all-vars '())
      (build (lambda (vars f x) `(,f ,x)))
      (subterm '() f)
      (subterm '() x)]))
      
  
  (gui lang reductions '((lambda (x) (x x)) (lambda (x) (x x)))))
