(module call-by-need mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss"
           (lib "match.ss"))
  
  (reduction-steps-cutoff 10)
  
  (define lang
    (language (e (e e)
                 variable
                 v)
              (v (lambda (variable) e))
              (a v 
                 ((lambda (variable) a) e))
              (ac hole
                  (side-condition ((lambda ((name qq variable)) (name ac ac)) e)
                                  (qq . used-in . ac))
                  ;((lambda (variable) ac) e)
                  )
              (c (c e)
                 ((lambda (variable) c) e)
                 ((lambda ((name z variable)) (in-hole+ c (name z variable))) c)
                 hole)))
  
  (define reductions
    (list
     (reduction lang
                (in-hole (name c c) ((lambda ((name x variable)) (in-hole* h2 (name c2 c) (name x variable))) (name v v)))
                (replace c hole `((lambda (,x) ,(replace c2 h2 v)) ,v)))
     (reduction lang
                (in-hole (name c c)
                         (((lambda ((name x variable)) (name a a)) (name n e)) (name m e)))
                (if (x . used-in . n)
                    (let ([new-var (variable-not-in (cons a n) x)])
                      (replace c hole `((lambda (,x) ,(lc-subst x new-var `(,a ,n))) ,m)))
                    (replace c hole `((lambda (,x) (,a ,n)) ,m))))
     (reduction lang
                (in-hole
                 (name c c)
                 ((name f (lambda ((name x variable)) (in-hole+ c (name x variable))))
                  ((lambda ((name y variable)) (name a a)) (name e e))))
                (if (y . used-in . e)
                    (let ([new-var (variable-not-in (cons f a) y)])
                      (replace c hole `((lambda (,y) ,(lc-subst y new-var `(,f ,a))) ,e)))
                    (replace c hole `((lambda (,y) (,f ,a)) ,e))))
     
     ;; gc
     (reduction lang
                (in-hole (name ac ac)
                         (side-condition ((lambda ((name x variable)) (name a a)) (name e e))
                                         (x . not-free-in . a)))
                (replace ac hole a))
     ))
  
  (define (used-in x e) (not (not-free-in x e)))
  (define (not-free-in x e) (equal? e (lc-subst x (symbol-append x '*) e)))
  
  (define (symbol-append x y)
    (string->symbol
     (string-append
      (symbol->string x)
      (symbol->string y))))
  
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
  
  (gui lang reductions 
       '((lambda (f) ((f (lambda (a) a)) (f (lambda (b) b))))
         ((lambda (x) (lambda (w) (x w)))
          ((lambda (z) z) (lambda (q) q))))))