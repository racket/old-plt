(module iswim mzscheme
  (require (lib "reduction-semantics.ss" "reduction-semantics")
           (lib "subst.ss" "reduction-semantics"))
  
  (provide iswim-grammar
           is-in-V?
           iswim-subst
           beta_v delta
           ->v :->v

           if0 true false
           mkpair fst snd
           Y_v sum)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Expression grammar:
  
  (define iswim-grammar
    (language (M (M M)
		 (o1 M)
		 (o2 M M)
		 V)
	      (V variable
		 (lambda variable M)
		 b)
	      (b number)
	      (o1 "add1" "sub1" "iszero")
	      (o2 "+" "-" "*" "^")
	      
	      ;; Evaluation contexts:
	      (E hole
		 (E M)
		 (V E)
		 (o1 E)
		 (o2 E M)
		 (o2 V E))))

  (define is-in-V?
    ;; to check whether an expression is in V, apply a reduction that
    ;; matches only Vs, and see whether it generates any results
    (let ([v-to-7 (list (reduction iswim-grammar V 7))])
      (lambda (M)
        (pair? (reduce v-to-7 M)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Substitution:
  
  ;; The subst form makes implemention of capture-avoiding
  ;; easier. We just have to describe how variables bind
  ;; in our language's forms.
  
  (define iswim-subst/backwards
    (subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda ,X ,M)
      (all-vars (list X))
      (build (lambda (X-list M) `(lambda ,(car X-list) ,M)))
      (subterm (list X) M)]
     [`(,(and o (or "add1" "sub1" "iszero")) ,M1)
      (all-vars '())
      (build (lambda (vars M1) `(,o ,M1)))
      (subterm '() M1)]
     [`(,(and o (or "+" "-" "*" "^")) ,M1 ,M2)
      (all-vars '())
      (build (lambda (vars M1 M2) `(,o ,M1 ,M2)))
      (subterm '() M1)
      (subterm '() M2)]
     [`(,M1 ,M2)
      (all-vars '())
      (build (lambda (empty-list M1 M2) `(,M1 ,M2)))
      (subterm '() M1)
      (subterm '() M2)]))

  ;; the argument order for the subst-generated function
  ;; doesn't match the order in the notes:
  (define (iswim-subst M Xr Mr)
    (iswim-subst/backwards Xr Mr M))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Reductions:
  
  ;; beta_v reduction
  (define beta_v
    (reduction iswim-grammar
               ((lambda (name X1 variable) (name M1 M)) (name V1 V))
               (iswim-subst M1 X1 V1)))
  
  
  (define delta
    (list
     (reduction iswim-grammar ("add1" (name b1 b)) (add1 b1))
     (reduction iswim-grammar ("sub1" (name b1 b)) (sub1 b1))
     (reduction iswim-grammar ("iszero" (name b1 b))
                (if (zero? b1) 
                    '(lambda x (lambda y x))
                    '(lambda x (lambda y y))))
     (reduction iswim-grammar ("+" (name b1 b) (name b2 b)) (+ b1 b2))
     (reduction iswim-grammar ("-" (name b1 b) (name b2 b)) (- b1 b2))
     (reduction iswim-grammar ("*" (name b1 b) (name b2 b)) (* b1 b2))
     (reduction iswim-grammar ("^" (name b1 b) (name b2 b)) (expt b1 b2))))
  
  ;; ->v
  (define ->v (map (lambda (red)
                     (compatible-closure red iswim-grammar 'E))
                   (cons beta_v delta)))
  
  ;; :->v
  (define :->v (map (lambda (red)
                      (context-closure red iswim-grammar 'E))
                    (cons beta_v delta)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Abbreviations:
  
  (define (if0 test then else)
    (let ([X (variable-not-in `(,then ,else) 'X)])
      `(((("iszero" ,test) (lambda ,X ,then)) (lambda ,X ,else)) 77)))
  
  (define true '(lambda x (lambda y x)))
  (define false '(lambda x (lambda y y)))
  
  (define mkpair '(lambda x (lambda y (lambda s ((s x) y)))))
  (define fst '(lambda p (p (lambda x (lambda y x)))))
  (define snd '(lambda p (p (lambda x (lambda y y)))))
  
  (define Y_v '(lambda f (lambda x
                           (((lambda g (f (lambda x ((g g) x))))
                             (lambda g (f (lambda x ((g g) x)))))
                            x))))
  
  (define mksum `(lambda s
                   (lambda x 
                     ,(if0 'x 0 '("+" x (s ("sub1" x)))))))
  (define sum `(,Y_v ,mksum)))
