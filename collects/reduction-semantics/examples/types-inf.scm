(module types-inf mzscheme
  (require (lib "reduction-semantics.ss" "reduction-semantics")
           (lib "gui.ss" "reduction-semantics")
           (lib "subst.ss" "reduction-semantics"))
  
  (reduction-steps-cutoff 10)

  #|
  
  The simplification rules in here aren't right yet.
  
  |#
  
  (define lang
    (language 
     (p ((a t) ... e))
     (e (e e)
        x
        number
        (lambda (x) e)
        (-> e e)
        num)
     (c (t c)
        (c e)
        (-> t c)
        (-> c e)
        hole)
     (x (side-condition (name x (variable-except lambda -> if =)) (not (ty-var? x))))
     (a (side-condition (name a variable) (ty-var? a)))
     (t num (-> t t) a)
     (st num 
         (-> a a)
         (-> num num)
         (-> a num)
         (-> num a)
         a)))
  
  (define (ty-var? x)
    (let ([s (symbol->string x)])
      (and ((string-length s) . > . 0)
           (char=? (string-ref s 0) #\:))))
  
  (define-syntax (p--> stx)
    (syntax-case stx ()
      [(_ i r) (syntax (reduction lang i r))]))
  
  (define reductions
    (list

     ;; move to all st.
     (p--> ((name before (a st)) ...
            ((name a1 a) (name t (-> (-> (name t1 t) (name t2 t)) (name t3 t))))
            (name after (a t)) ...
            (name a2 a))
           (let ([nt (variable-not-in (list before t1 t2 t3 a1 after a2) ':t)])
             `(,@before
               (,a1 (-> ,nt ,t3))
               (,nt (-> ,t1 ,t2))
               ,@after
               ,a2)))
     
     ;; move to all st
     (p--> ((name before (a st)) ...
            ((name a1 a) (name t (-> (name t1 t) (-> (name t2 t) (name t3 t)))))
            (name after (a t)) ...
            (name a2 a))
           (let ([nt (variable-not-in (list before t1 t2 t3 a1 after a2) ':t)])
             `(,@before
               (,a1 (-> ,t1 ,nt))
               (,nt (-> ,t2 ,t3))
               ,@after
               ,a2)))
     
     ;; eliminate var = var
     (p--> (name p ((name before (a st)) ...
                    ((name a1 a) (name a2 a))
                    (name after (a st)) ...
                    (name a a)))
           `(,@(put-in a1 a2 before)
             ,@(put-in a1 a2 after)
             ,(put-in a1 a2 a)))
     
     ;; eliminate var = num
     (p--> ((name before (a st)) ...
            ((name a1 a) num)
            (name after (a st)) ...
            (name a a))
           `(,@(put-in a1 'num before)
             ,@(put-in a1 'num after)
             ,(put-in a1 'num a)))
     
     (p--> (name p ((name before (a st)) ...
                    ((name a1 a) (-> (name a2 a) (name a3 a)))
                    (name during (a st)) ...
                    ((name a1 a) (-> (name a4 a) (name a5 a)))
                    (name after (a st)) ...
                    (name a6 a)))
           (put-in a2 a4 p))
     
     (p--> (name p ((name before (a st)) ...
                    ((name a1 a) (-> (name a2 a) (name a3 a)))
                    (name during (a st)) ...
                    ((name a1 a) (-> (name a4 a) (name a5 a)))
                    (name after (a st)) ...
                    (name a6 a)))
           (put-in a3 a5 p))
            
     (p--> ((name before (a st)) ...
            (name same (a st))
            (name during (a st)) ...
            (name same (a st))
            (name after (a st)) ...
            (name a a))
           `(,@before
             ,same
             ,@after
             ,a))
     
     (p--> ((name bindings (a t)) ... (in-hole (name c c) number))
           (let ([nt (variable-not-in (list c bindings) ':t)])
             `(,@bindings (,nt num) ,(replace c hole nt))))
     
     (p--> ((name bindings (a t)) ... 
            (in-hole (name c c) (lambda ((name b x)) (name body e))))
           (let ([nt (variable-not-in (list body c bindings) ':t)])
             `(,@bindings
               ,(replace c hole `(-> ,nt ,(lc-subst b nt body))))))
     
     (p--> ((name bindings (a t)) ... 
            (in-hole (name c c) ((name t1 t) (name t3 t))))
           (let* ([nt1 (variable-not-in (list c bindings) ':t)]
                  [nt2 (variable-not-in (list nt1 c bindings) ':t)])
             `(,@bindings (,nt1 ,t1) (,nt1 (-> ,t3 ,nt2)) ,(replace c hole nt2))))))

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

  (define (put-in old new sexp)
    (let loop ([sexp sexp])
      (cond
        [(pair? sexp) (cons (loop (car sexp)) (loop (cdr sexp)))]
        [(eq? old sexp) new]
        [else sexp])))
  
  ;(define term '(((lambda (x) (x x)) (lambda (x) (x x)))))
  (define term '((((lambda (x) x) (lambda (x) x)) 1)))
  (gui lang reductions term)
  )
