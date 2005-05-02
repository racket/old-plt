(module aux mzscheme 

  ;; auxiliary functions 

  (require (file "Testing/testing.scm")
           (lib "contract.ss")
           (lib "list.ss"))
  
  (provide/contract 
   [set=?  (procedure? . -> . (list? list? . -> . boolean?))]
   [set>   (list? procedure? . ->d . (lambda (l f) (lambda (r) (<= (length r) (length l)))))]
   [choose (cons? . ->d . (lambda (g) (lambda (r) (pair? (member r g)))))])
  
  (provide
   rotate       ;; Listof[X] -> Listof[X]
   and*         ;; Boolean *-> Boolean 
   member*      ;; X Listof[X] (X X -> Boolean) -> Boolean 
   list-object= ;; (X X -> Boolean) -> (Listof[X] Listof[X] -> Boolean)
   ; set>         ;; Listof[X] (X X -> Boolean) -> Listof[X]
   )
  
  (define (set=? p)
    (lambda (l1 l2)
      (and (andmap (lambda (x2) (member* x2 l1 p)) l2)
           (andmap (lambda (x1) (member* x1 l2 p)) l1))))

  (define (choose l) (list-ref l (random (length l))))
    
  (define (set> lc p=)
    (foldr (lambda (f r) (if (member* f r p=) r (cons f r))) '() lc))

  (define (and* . l) (foldl (lambda (x y) (and x y)) #t l))

  (define (member* x lox eqx)
    (cond
      [(null? lox) #f]
      [else (or (eqx (car lox) x) (member* x (cdr lox) eqx))]))

  (define (list-object= eqx)
    (lambda (lox loy)
      (and (= (length lox) (length loy)) (andmap eqx lox loy))))
  
  (define (rotate l) (if (null? l) '() (append (cdr l) (list (car l)))))
 
  (test== (set> '(a b c b) eq?) '(a c b))
  
  (test== (rotate '()) '())
  (test== (rotate '(1 2 3)) '(2 3 1))

  )
