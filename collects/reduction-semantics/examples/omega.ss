(module omega mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss")
  
  (reduction-steps-cutoff 10)
  
  (define lang
    (language (e (e e)
                 (abort e)
                 (variable-except lambda call/cc abort)
                 v)
              (c (v c)
                 (c e)
                 hole)
              (v call/cc
                 number
                 (lambda (variable) e))))
  
  (define reductions
    (list
     (reduction lang
                (in-hole (name c c) (call/cc (name arg v)))
                (let ([v (variable-not-in (term c) 'x)])
                  (replace (term c) 
                           (term hole)
                           (term (arg (lambda (,v) (abort ,(replace (term c) (term hole) v))))))))
     (reduction lang
                (in-hole c (abort (name e e)))
                (term e))
     (reduction/context lang
                        c
                        ((lambda ((name x variable)) (name body e)) (name arg v))
                        (lc-subst (term x) (term arg) (term body)))))
  
  (define lc-subst
    (plt-subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list x) b)]
     [`(call/cc ,v)
      (all-vars '())
      (build (lambda (vars arg) `(call/cc ,arg)))
      (subterm '() v)]
     [`(,f ,x)
      (all-vars '())
      (build (lambda (vars f x) `(,f ,x)))
      (subterm '() f)
      (subterm '() x)]))
      
  
  ;(gui lang reductions '((lambda (x) (x x)) (lambda (x) (x x))))
  
  (gui lang reductions '((call/cc call/cc) (call/cc call/cc)))
  ;(gui lang reductions '((lambda (x) ((call/cc call/cc) x)) (call/cc call/cc)))
  
  )
