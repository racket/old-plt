(module control mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss")
  
  (reduction-steps-cutoff 100)
  
  (define lang
    (language (e (e e)
                 (\# e)
                 (f e)
                 variable
                 (+ e e)
                 v)
              (c (v c)
                 (c e)
                 (\# c)
                 (f c)
                 (+ v c)
                 (+ c e)
                 hole)
              (c# (v c#)
                  (c# e)
                  (f c#)
                  (+ v c#)
                  (+ c# e)
                  hole)
              (v (lambda (variable) e)
                 number)))
  
  (define reductions
    (list
     (reduction lang
                (in-hole (name c c)
                         (\# (in-hole*
                              hole#
                              (name c# c#)
                              (f (name v v)))))
                (let ([x (variable-not-in c# 'x)])
                  (replace c hole `(,v (lambda (,x) ,(replace c# hole# x))))))
     (reduction lang
                (in-hole (name c# c#)
                         (f (name v v)))
                (let ([x (variable-not-in c# 'x)])
                  `(,v (lambda (,x) ,(replace c# hole x)))))
     (reduction/context lang
                        c
                        ((lambda ((name x variable)) (name body e)) (name arg v))
                        (lc-subst x arg body))
     (reduction/context lang
                        c
                        (+ (name n1 number) (name n2 number))
                        (+ n1 n2))))
  
  (define lc-subst
    (subst
     [`(\# ,e)
      (all-vars '())
      (build (lambda (vars body) `(\# ,body)))
      (subterm '() e)]
     [`(f ,e)
      (all-vars '())
      (build (lambda (vars body) `(f ,body)))
      (subterm '() e)]
     [`(+ ,e1 ,e2)
      (all-vars '())
      (build (lambda (vars e1 e2) `(+ ,e1 ,e2)))
      (subterm '() e1)
      (subterm '() e2)]
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list x) b)]
     [`(,f ,x)
      (all-vars '())
      (build (lambda (vars f x) `(,f ,x)))
      (subterm '() f)
      (subterm '() x)]))
      
  (gui/multiple lang reductions
                (list '(+ 1 (+ 2 (f (lambda (d) (d (d 0))))))
                      '(+ 1 (\# (+ 2 (f (lambda (d) (d (d 0))))))))))