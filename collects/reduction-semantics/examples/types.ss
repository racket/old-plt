(module types mzscheme
  (require (lib "reduction-semantics.ss" "reduction-semantics")
           (lib "gui.ss" "reduction-semantics")
           (lib "subst.ss" "reduction-semantics"))
  
  (reduction-steps-cutoff 10)
  
  (define lang
    (language (e (e e)
                 x
                 number
                 (lambda (x t) e)
                 (if e e e)
                 (= e e)
		 (-> e e)
                 num
                 bool)
              (c (t c)
                 (c e)
                 (-> t c)
                 (-> c e)
                 (= t c)
                 (= c e)
                 (if c e e)
                 (if t c e)
                 (if t t c)
                 hole)
	      (x (variable-except lambda -> if =))
              (t num bool (-> t t))))
  
  (define-syntax (r--> stx)
    (syntax-case stx ()
      [(_ i r) (syntax (reduction/context lang c i r))]))

  (define-syntax (e--> stx)
    (syntax-case stx ()
      [(_ i msg) (syntax (reduction lang (in-hole c i) msg))]))
	 
  (define reductions
    (list
     (r--> number 
           'num)
     
     (r--> (lambda ((name x x) (name t t)) (name body e))
           `(-> ,t ,(lc-subst x t body)))
     
     (r--> ((-> (name t1 t) (name t2 t)) (name t1 t))
           t2)
     
     (e--> (side-condition ((-> (name t1 t) t) (name t2 t))
                           (not (equal? t1 t2)))
           (format "app: domain error ~s and ~s" t1 t2))
     
     (e--> (num (name t t))
           (format "app: non function error ~s" t))
     
     (r--> (if bool (name t t) (name t t))
           t)
     (e--> (side-condition (if bool (name t1 t) (name t2 t))
                           (not (equal? t1 t2)))
           (format "if: then and else clause mismatch ~s and ~s" t1 t2))
     (e--> (side-condition (if (name t t) t t)
                           (not (equal? t 'bool)))
           (format "if: test not boolean ~s" t))
     
     (r--> (= num num) 'bool)
     (e--> (side-condition (= (name t1 t) (name t2 t))
                           (or (not (equal? t1 'num))
                               (not (equal? t2 'num))))
           (format "=: not comparing numbers ~s and ~s" t1 t2))))
  
  (define lc-subst
    (subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x ,t) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars) ,t) ,body)))
      (subterm (list x) b)]
     [`(,f ,@(xs ...))
      (all-vars '())
      (build (lambda (vars f . xs) `(,f ,@xs)))
      (subterm '() f)
      (subterms '() xs)]))
  
  (define term '((lambda (x num) (lambda (y num) (if (= x y) 0 x))) 1))
  (gui lang reductions term)
  )
