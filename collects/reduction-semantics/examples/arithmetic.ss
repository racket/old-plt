(module arithmetic mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss")
  
  (define lang
    (language 
     (e (binop e e)
        (sqrt e)
        number)
     (binop +
            -
            *
            /)
     
     (e-ctxt (binop e e-ctxt)
             (binop e-ctxt e)
             (sqrt e-ctxt)
             hole)
     (v number)))
  
  (define reductions
    (list
     (reduction lang
                (in-hole (name c e-ctxt) 
                         (+ (name n1 number)
                            (name n2 number)))
                (replace c hole (+ n1 n2)))
     (reduction/context lang
                        e-ctxt
                        (- (name n1 number)
                           (name n2 number))
                        (- n1 n2))
     (reduction/context lang
                        e-ctxt
                        (* (name n1 number)
                           (name n2 number))
                        (* n1 n2))
     (reduction/context lang
                        e-ctxt
                        (/ (name n1 number)
                           (name n2 number))
                        (/ n1 n2))
     (reduction/context lang
                        e-ctxt
                        (sqrt (name n number))
                        (sqrt n))))
     
  
  (gui lang reductions '(- (* (sqrt 36) (/ 1 2)) (+ 1 2))))

