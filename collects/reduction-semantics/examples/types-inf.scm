(module types-inf mzscheme
  (require (lib "reduction-semantics.ss" "reduction-semantics")
           (lib "gui.ss" "reduction-semantics")
           (lib "subst.ss" "reduction-semantics"))
  
  (reduction-steps-cutoff 10)

  #|
  
  The simplification rules in here aren't right yet.
  They don't detect loops.
  
  |#
  
  (define lang
    (language 
     (p ((a t) ... e))
     (e (e e)
        x
        a
        number
        (lambda (x) e)
        (-> e e)
        num)
     (c (t c)
        (c e)
        (-> t c)
        (-> c e)
        hole)
     (x (side-condition (name x (variable-except lambda -> if =)) (not (ty-var? (term x)))))
     (a (side-condition (name a variable) (ty-var? (term a))))
     (t num (-> t t) a)
     (st num (-> a a))))
  
  (define (ty-var? x)
    (let ([s (symbol->string x)])
      (and ((string-length s) . > . 0)
           (char=? (string-ref s 0) #\:))))
  
  (define-syntax (p--> stx)
    (syntax-case stx ()
      [(_ i r) (syntax (reduction lang i r))]))
  
  (define reductions
    (list

     ;; move to all st (eliminate left nested arrow)
     (p--> ((name before (a st)) ...
            (a_1 (name t (-> (-> t_1 t_2) t_3)))
            (name after (a t)) ...
            a_2)
           (let ([nt (variable-not-in (term (before ... t_1 t_2 t_3 a_1 after ... a_2)) ':t)])
             (term 
              (before ...
               (a_1 (-> ,nt t_3))
               (nt (-> t1 t_2))
               after ...
               a_2))))
     
     ;; move to all st (eliminate right nested arrow)
     (p--> ((name before (a st)) ...
            (a_1 (name t (-> t_1 (-> t_2 t_3))))
            (name after (a t)) ...
            a_2)
           (let ([nt (variable-not-in (term (before ... t_1 t_2 t_3 a_1 after ... a_2)) ':t)])
             (term 
              (before ...
               (a_1 (-> t_1 ,nt))
               (,nt (-> t_2 t_3))
               after ...
               a_2))))
     
     ;; move to all st (eliminate var = var)
     (p--> (name p ((name before (a st)) ...
                    (a_1 a_2)
                    (name after (a t)) ...
                    a_3))
            
           `(,@(put-in (term a_1) (term a_2) (term (before ...)))
             ,@(put-in (term a_1) (term a_2) (term (after ...)))
             ,(put-in (term a_1) (term a_2) (term a_3))))
     
     ;; next 4: propogate nested constraints
     
     (p--> (side-condition
            ((name before (a st)) ...
             (a_1 (-> a_2 a_3))
             (name during (a st)) ...
             (a_1 (-> a_4 a_5))
             (name after (a st)) ...
             a_6)
            (and (not (eq? (term a_2) (term a_4)))
                 (not (eq? (term a_3) (term a_5)))))
           (term
            (before ...
             (a_1 (-> a_2 a_3))
             during ...
             (a_1 (-> a_4 a_5))
             after ...
             (a_2 a_4)
             (a_3 a_5)
             a_6)))
     
     (p--> (side-condition
            ((name before (a st)) ...
             (a_1 (-> a_2 a_3))
             (name during (a st)) ...
             (a_1 (-> a_4 a_5))
             (name after (a st)) ...
             a_6)
            (and (eq? (term a_2) (term a_4))
                 (not (eq? (term a_3) (term a_5)))))
           (term 
            (before ...
             (a_1 (-> a_2 a_3))
             during ...
             (a_1 (-> a_4 a_5))
             after ...
             (a_3 a_5)
             a_6)))
     
     (p--> (side-condition
            ((name before (a st)) ...
             (a_1 (-> a_2 a_3))
             (name during (a st)) ...
             (a_1 (-> a_4 a_5))
             (name after (a st)) ...
             a_6)
            (and (not (eq? (term a_2) (term a_4)))
                 (eq? (term a_3) (term a_5))))
           (term 
            (before ...
             (a_1 (-> a_2 a_3))
             during ...
             (a_1 (-> a_4 a_5))
             after ...
             (a_2 a_4)
             a_6)))
     
     (p--> (side-condition
            ((name before (a st)) ...
             (a_1 (-> a_2 a_3))
             (name during (a st)) ...
             (a_1 (-> a_4 a_5))
             (name after (a st)) ...
             a_6)
            (and (eq? (term a_2) (term a_4))
                 (eq? (term a_3) (term a_5))))
           (term 
            (before ...
             (a_1 (-> a_2 a_3))
             during ...
             after ...
             a_6)))
     
     ;; detect type errors
     (p--> ((name before (a st)) ...
            ((name a1 a) (-> a a))
            (name during (a st)) ...
            ((name a1 a) num)
            (name after (a st)) ...
            (name a2 a))
           "type mismatch")
 
     (p--> ((name bindings (a t)) ... (in-hole (name c c) number))
           (let ([nt (variable-not-in (term (c bindings ...)) ':t)])
             (term (bindings ... (,nt num) ,(replace (term c) (term hole) nt)))))
     
     (p--> ((name bindings (a t)) ... 
            (in-hole (name c c) (lambda ((name b x)) (name body e))))
           (let ([nt (variable-not-in (list (term body) (term c) (term (bindings ...))) ':t)])
             (term 
              (bindings ...
               ,(replace (term c)
                         (term hole) 
                         (term (-> ,nt ,(lc-subst (term b) nt (term body)))))))))
     
     (p--> ((name bindings (a t)) ... 
            (in-hole (name c c) ((name t1 t) (name t3 t))))
           (let* ([nt1 (variable-not-in (term (c bindings ...)) ':t)]
                  [nt2 (variable-not-in (term (,nt1 c bindings ...)) ':t)])
             (term (bindings ... 
                    (,nt1 t1)
                    (,nt1 (-> t3 ,nt2))
                    ,(replace (term c) (term hole) nt2)))))))

  (define lc-subst
    (subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
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
  
  (define theterm '(((lambda (x) (x x)) (lambda (x) (x x)))))
  ; (define theterm '((((lambda (x) x) (lambda (x) x)) 1)))
  
  ;; big one: goes to type mistmatch
  ;(define theterm '(((lambda (x) ((x (lambda (x) x)) (x 1)))
  ;                (lambda (x) x))))
  
  (traces lang reductions theterm))
