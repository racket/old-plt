#|

This is an adaptation of Cormac Flanagan's future semantics
to a scheme where each term only has a single hole, but
there are multiple decompositions for each term.

|#

(module future mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss")
  
  (define lang
    (language
     (state (flet (variable state) state)
            m
            error)
     (m (let (variable (future m)) m)
        (let (variable (car v)) m)
        (let (variable (cdr v)) m)
        (let (variable (if v m m)) m)
        (let (variable (apply v v)) m)
        (let (variable v) m)
        v)
     (v number
        variable
        (cons v v)
        (lambda (variable) m))
     
     (e-state (flet (variable e-state) state)
              (flet (variable state) e-state)
              e)
     (e hole
        (let (variable e) m)
        (let (variable (future e)) m))))
  
  (define reductions
    (list
     (reduction lang
                (in-hole (name e e-state)
                         (let ((name var variable) (name val v))
                           (name exp m)))
                (replace (term e)
                         (term hole) 
                         (future-subst (term var) (term val) (term exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let ((name var variable) (car (cons (name val v) v)))
                           (name exp m)))
                (replace (term e) (term hole) (future-subst (term var) (term val) (term exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let ((name var variable) (cdr (cons v (name val v))))
                           (name exp m)))
                (replace (term e) (term hole) (future-subst (term var) (term val) (term exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let ((name var variable) (if true (name thn m) m))
                           (name exp m)))
                (replace (term e) (term hole) (term (let (var thn) exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let ((name var variable) (if false m (name els m)))
                           (name exp m)))
                (replace (term e) (term hole) (term (let (var els) exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let ((name var variable) 
                               (apply (lambda ((name formal variable)) (name body m))
                                      (name actual v)))
                           (name exp m)))
                (replace 
                 (term e) (term hole)
                 (term (let (var ,(future-subst (term format) (term actual) (term body)) exp)))))
     (reduction lang
                (in-hole (name e e-state)
                         (let ((name x variable) (future (name m1 m))) (name m2 m)))
                (let ([p (variable-not-in (list (term e) (term m1) (term m2)) 'p)])
                  (term (flet (,p m1) (let (x ,p) m2)))))
     (reduction lang
                (flet ((name p variable) (name v v)) (name body state))
                (future-subst (term p) (term v) (term body)))
     (reduction lang
                (flet ((name p2 variable) (flet ((name p1 variable) (name s1 state)) 
                                                (name s2 state))) 
                      (name s3 state))
                (term (flet (p1 s1) (flet (p2 s2) s3))))))
  
  (define future-subst
    (subst
     [`(let (,a-var ,rhs-exp) ,body-exp)
      (all-vars (list a-var))
      (build (lambda (vars rhs-exp body-exp) `(let (,(car vars) ,rhs-exp) ,body-exp)))
      (subterm '() rhs-exp)
      (subterm (list a-var) body-exp)]
     [`(lambda (,a-var) ,exp)
      (all-vars (list a-var))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list a-var) exp)]
     [(? number?) (constant)]
     [(? symbol?) (variable)]
     [`(cons ,hd ,tl)
      (all-vars '())
      (build (lambda (vars hd tl) `(cons ,hd ,tl)))
      (subterm '() hd)
      (subterm '() tl)]))
  
  (define (copy-sexp x) (if (pair? x) (cons (copy-sexp (car x)) (copy-sexp (cdr x))) x))
  
  '(gui lang reductions '(let (x (future (let (y (cons 1 2))
                                           (let (z (car y))
                                             z))))
                           (let (p (cons 3 4))
                             (let (q (car p))
                               (cons x q)))))
  
  (gui lang reductions '(let (x (future (let (y 1)
                                          y)))
                          x)))