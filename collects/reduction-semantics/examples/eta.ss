(module eta mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss")
  
  (reduction-steps-cutoff 100)
  
  (define lang
    (language (e (e e)
                 variable
                 (+ e e)
                 v)
              (c (v c)
                 (c e)
                 (+ v c)
                 (+ c e)
                 hole)
              (v (lambda (variable) e)
                 number)))
  
  (define reductions
    (list
     (reduction/context lang
                        c
                        ((lambda ((name x variable)) (name body e)) (name arg v))
                        (lc-subst x arg body))
     (reduction/context lang
                        c
                        (+ (name n1 number) (name n2 number))
                        (+ n1 n2))
     (reduction/context lang
                        c
                        (side-condition (lambda ((name x variable)) ((name e e) (name x variable)))
                                        (equal? e (lc-subst x 1234 e)))
                        e)
     
     (reduction lang
                (in-hole c ((name n number) (name arg v)))
                (format "procedure application: expected procedure, given: ~a; arguments were: ~a" n arg))
     (reduction lang
                (in-hole c (+ (name non-num (lambda (variable) e)) (name arg2 v)))
                (format "+: expects type <number> as 1st argument, given: ~s; other arguments were: ~s"
                        non-num arg2))
     (reduction lang
                (in-hole c (+ (name arg1 v) (name non-num (lambda (variable) e))))
                (format "+: expects type <number> as 2nd argument, given: ~s; other arguments were: ~s"
                        arg1 non-num))))
  
  (define lc-subst
    (subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list x) b)]
     [`(+ ,n2 ,n1)
      (all-vars '())
      (build (lambda (vars n1 n2) `(+ ,n1 ,n1)))
      (subterm '() n1)
      (subterm '() n2)]
     [`(,f ,x)
      (all-vars '())
      (build (lambda (vars f x) `(,f ,x)))
      (subterm '() f)
      (subterm '() x)]))
      
  
  (gui lang reductions '(+ (lambda (x) ((+ 1 2) x)) 1)))