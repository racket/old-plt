(module iswim mzscheme
  (require (lib "reduction-semantics.ss" "reduction-semantics")
           (lib "subst.ss" "reduction-semantics")
	   (lib "contracts.ss"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Expression grammar:
  
  (define iswim-grammar
    (language (M (M M)
		 (o1 M)
		 (o2 M M)
		 V
		 ("letcc" X M)
		 ("cc" M M))
	      (V X
		 ("lam" variable M)
		 b
		 ("[" M "]"))
	      (X variable)
	      (b number)
	      (o1 "add1" "sub1" "iszero")
	      (o2 "+" "-" "*" "^")
	      (on o1 o2)
	      
	      ;; Evaluation contexts:
	      (E hole
		 (E M)
		 (V E)
		 (o1 E)
		 (o2 E M)
		 (o2 V E)
		 ("cc" E M)
		 ("cc" V E))

	      ;; Continuations (CK machine):
	      (k "mt"
		 ("fun" V k)
		 ("arg" M k)
		 ("narg" (V ... on) (M ...) k))

	      ;; Environments and closures (CEK):
	      (env ((X = vcl) ...))
	      (cl (M : env))
	      (vcl (V- : env))
	      
	      ;; Values that are not variables:
	      (V- ("lam" variable M)
		  b)

	      ;; Continuations with closures (CEK):
	      (k- "mt"
		  ("fun" vcl k-)
		  ("arg" cl k-)
		  ("narg" (vcl ... on) (cl ...) k-))))

  (define M? (language->predicate iswim-grammar 'M))
  (define V? (language->predicate iswim-grammar 'V))
  (define o1? (language->predicate iswim-grammar 'o1))
  (define o2? (language->predicate iswim-grammar 'o2))
  (define on? (language->predicate iswim-grammar 'on))
  (define k? (language->predicate iswim-grammar 'k))

  (define env? (language->predicate iswim-grammar 'env))
  (define cl? (language->predicate iswim-grammar 'cl))
  (define vcl? (language->predicate iswim-grammar 'vcl))
  (define k-? (language->predicate iswim-grammar 'k-))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Substitution:
  
  ;; The subst form makes implemention of capture-avoiding
  ;; easier. We just have to describe how variables bind
  ;; in our language's forms.
  
  (define iswim-subst/backwards
    (subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`("lam" ,X ,M)
      (all-vars (list X))
      (build (lambda (X-list M) `("lam" ,(car X-list) ,M)))
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
      (subterm '() M2)]
     [`("letcc" ,X ,M)
      (all-vars (list X))
      (build (lambda (X-list M) `("letcc" ,(car X-list) ,M)))
      (subterm (list X) M)]
     [`("cc" ,M1 ,M2)
      (all-vars '())
      (build (lambda (vars M1 M2) `("cc" ,M1 ,M2)))
      (subterm '() M1)
      (subterm '() M2)]
     [`("[" ,E "]")
      (all-vars '())
      (build (lambda (vars) `("[" ,E "]")))]))
     

  ;; the argument order for the subst-generated function
  ;; doesn't match the order in the notes:
  (define (iswim-subst M Xr Mr)
    (iswim-subst/backwards Xr Mr M))

  (define empty-env '())

  ;; Environment lookup
  (define (env-lookup env X)
    (let ([m (assq X env)])
      (and m (caddr m))))

  ;; Environment extension
  (define (env-extend env X vcl)
    (cons (list X '= vcl) env))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Reductions:
  
  ;; beta_v reduction
  (define beta_v
    (reduction iswim-grammar
               (("lam" (name X1 variable) (name M1 M)) (name V1 V))
               (iswim-subst M1 X1 V1)))
  
  
  (define delta
    (list
     (reduction iswim-grammar ("add1" (name b1 b)) (add1 b1))
     (reduction iswim-grammar ("sub1" (name b1 b)) (sub1 b1))
     (reduction iswim-grammar ("iszero" (name b1 b))
                (if (zero? b1) 
                    '("lam" x ("lam" y x))
                    '("lam" x ("lam" y y))))
     (reduction iswim-grammar ("+" (name b1 b) (name b2 b)) (+ b1 b2))
     (reduction iswim-grammar ("-" (name b1 b) (name b2 b)) (- b1 b2))
     (reduction iswim-grammar ("*" (name b1 b) (name b2 b)) (* b1 b2))
     (reduction iswim-grammar ("^" (name b1 b) (name b2 b)) (expt b1 b2))))
  
  ;; ->v
  (define ->v (map (lambda (red)
                     (compatible-closure red iswim-grammar 'M))
                   (cons beta_v delta)))
  
  ;; :->v
  (define :->v (map (lambda (red)
                      (context-closure red iswim-grammar 'E))
                    (cons beta_v delta)))

  ;; :->v+letcc
  (define :->v+letcc (append
		      :->v
		      (list

		       ;; letcc rule:
		       (reduction
			iswim-grammar
			(in-hole (name E E) ("letcc" (name X X) (name M M)))
			(replace E hole (iswim-subst M X `("[" ,(replace E hole '||) "]"))))

		       ;; cc rule:
		       (reduction
			iswim-grammar
			(in-hole E ("cc" ("[" (in-hole* inner-hole (name E2 E) ||) "]") (name V V)))
			(replace E2 inner-hole V)))))
			
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helpers:
  
  (define (delta*n on Vs)
    (let ([l (reduce delta `(,on ,@Vs))])
      (if (null? l)
	  #f
	  (car l))))

  (define (delta*1 o1 V)
    (delta*n o1 (list V)))

  (define (delta*2 o2 V1 V2)
    (delta*n o2 (list V1 V2)))

  ;; function-reduce*
  (define (function-reduce* reds expr done?)
    (cons 
     expr
     (if (done? expr)
	 null
	 (let ([l (reduce reds expr)])
	   (cond
	    [(null? l) null]
	    [(= 1 (length l))
	     (function-reduce* reds (car l) done?)]
	    [else
	     (error 'function-reduce* 
		    "found ~a possible steps from ~e"
		    (length l)
		    expr)])))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Abbreviations:
  
  (define (if0 test then else)
    (let ([X (variable-not-in `(,then ,else) 'X)])
      `(((("iszero" ,test) ("lam" ,X ,then)) ("lam" ,X ,else)) 77)))
  
  (define true '("lam" x ("lam" y x)))
  (define false '("lam" x ("lam" y y)))
  (define boolean-not `("lam" x ((x ,false) ,true)))
  
  (define mkpair '("lam" x ("lam" y ("lam" s ((s x) y)))))
  (define fst '("lam" p (p ("lam" x ("lam" y x)))))
  (define snd '("lam" p (p ("lam" x ("lam" y y)))))
  
  (define Y_v '("lam" f ("lam" x
			 ((("lam" g (f ("lam" x ((g g) x))))
			   ("lam" g (f ("lam" x ((g g) x)))))
			  x))))
  
  (define mksum `("lam" s
		  ("lam" x 
		   ,(if0 'x 0 '("+" x (s ("sub1" x)))))))
  (define sum `(,Y_v ,mksum))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Exports:
  
  (provide/contract (iswim-grammar compiled-lang?)
		    (M? (any? . -> . boolean?))
		    (V? (any? . -> . boolean?))
		    (o1? (any? . -> . boolean?))
		    (o2? (any? . -> . boolean?))
		    (on? (any? . -> . boolean?))
		    (k? (any? . -> . boolean?))
		    (env? (any? . -> . boolean?))
		    (cl? (any? . -> . boolean?))
		    (vcl? (any? . -> . boolean?))
		    (iswim-subst (M? symbol? M? . -> . M?))
		    (env-lookup (env? symbol? . -> . (union false? vcl?)))
		    (env-extend (env? symbol? vcl? . -> . env?))
		    (empty-env env?)
		    (beta_v red?)
		    (delta (listof red?))
		    (delta*1 (o1? V?  . -> . (union false? V?)))
		    (delta*2 (o2? V? V? . -> .  (union false? V?)))
		    (delta*n (on? (listof V?) . -> .  (union false? V?)))
		    (->v (listof red?))
		    (:->v (listof red?))
		    (:->v+letcc (listof red?))
		    (function-reduce* ((listof red?) any? (any? . -> . boolean?) 
				       . -> . (listof any?)))
		    (if0 (M? M? M? . -> . M?))
		    (true M?)
		    (false M?)
		    (boolean-not M?)
		    (mkpair M?)
		    (fst M?)
		    (snd M?)
		    (Y_v M?)
		    (sum M?)))
