(module macro mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss")
  
  (define lang
    (language 
     (e (lambda (variable) e)
        (app e e)
        variable)
     (e-ctxt (lambda (variable) e-ctxt)
             (app e-ctxt any)
             (app e e-ctxt)
             hole)))
  
  (define reductions
    (list
     (reduction/context lang
                        e-ctxt
                        (or (name e1 any) (name e2 any))
                        (let ([var (variable-not-in (list e1 e2) 'x)])
                          `(let (,var ,e1) (if ,var ,var ,e2))))
     (reduction/context lang
                        e-ctxt
                        (let ((name var variable) (name rhs any)) 
                          (name body any))
                        `(app (lambda (,var) ,body) ,rhs))
     (reduction/context lang
                        e-ctxt
                        (if (name test any)
                            (name thn any)
                            (name els any))
                        `(app (app ,test ,thn) ,els))
     (reduction/context lang
                        e-ctxt
                        (true)
                        `(lambda (x) (lambda (y) x)))
     (reduction/context lang
                        e-ctxt
                        (false)
                        `(lambda (x) (lambda (y) y)))))
  
  (gui lang reductions '(or (false) (true))))
