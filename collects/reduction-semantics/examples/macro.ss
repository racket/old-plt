(module macro mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss")
  
  (define lang
    (language 
     (e (lambda (variable) e)
        (app e e)
        number
        variable)
     (e-ctxt (lambda (variable) e-ctxt)
             (app e-ctxt any)
             (app e e-ctxt)
             hole)))
  
  (define macros '(or let if true false id))
  
  (define-syntax (--> stx)
    (syntax-case stx ()
      [(_ frm to)
       (syntax (reduction/context lang e-ctxt frm to))]))
  
  (define reductions
    (list
     (--> (id (name e any))
          e)
     (--> (side-condition ((name e1 any) (name e2 any))
                          (not (memq e1 macros)))
          `(app ,e1 ,e2))
     (--> (or (name e1 any) (name e2 any))
          (let ([var (variable-not-in (list e1 e2) 'x)])
            `(let (,var ,e1) (if ,var ,var ,e2))))
     (--> (let ((name var variable) (name rhs any)) 
            (name body any))
          `((lambda (,var) ,body) ,rhs))
     (--> (if (name test any)
              (name thn any)
              (name els any))
          `((,test ,thn) ,els))
     (--> (true)
          `(lambda (x) (lambda (y) x)))
     (--> (false)
          `(lambda (x) (lambda (y) y)))))
  
  (gui lang reductions '((id id) 5))
  (gui lang reductions '(id 5))
  (gui lang reductions '(or (false) (true))))
