;; Library of functions for the compiler

(unit/sig
 compiler:library^
 (import (zodiac : zodiac:system^)
	 mzlib:function^)

(define logical-inverse
  (lambda (fun)
    (lambda (obj)
      (not (fun obj)))))

(define one-of
  (lambda preds
    (lambda (obj)
      (ormap (lambda (p) (p obj)) preds))))

(define all-of
  (lambda preds
    (lambda (obj)
      (andmap (lambda (p) (p obj)) preds))))

(define none-of
  (lambda preds
    (logical-inverse (apply one-of preds))))

(define vector-map
  (lambda (f vec)
    (let ((x (make-vector (vector-length vec))))
      (let loop ((i 0))
	(if (= i (vector-length vec))
	    vec
	    (begin (vector-set! vec i (f (vector-ref vec i)))
		   (loop (add1 i))))))))

(define improper-map
  (lambda (f ilist)
    (cond 
     ((pair? ilist) (#%cons (f (car ilist)) (improper-map f (cdr ilist))))
     ((null? ilist) null)
     (else (f ilist)))))

(define begin-map!
  (lambda (non-tail tail list)
    (if (null? list)
	null
	(begin
	  (let loop ([list list] [next (cdr list)])
	    (let ([tail? (null? next)])
	      (set-car! list ((if tail? tail non-tail) (car list)))
	      (unless tail? (loop next (cdr next)))))
	  list))))

(define begin-map
  (lambda (non-tail tail list)
    (if (null? list)
	null
	(let ([tail? (null? (cdr list))])
	  (cons ((if tail? tail non-tail) (car list)) 
		(begin-map non-tail tail (cdr list)))))))

(define map!
  (lambda (fun list)
    (let loop ([l list])
      (if (null? l)
	  list
	  (begin (set-car! l (fun (car l))) (loop (cdr l)))))))

(define list-index
  (lambda (obj list)
    (cond
     [(null? list) (error 'list-index "~a not found int ~a" obj list)]
     [(eq? obj (car list)) 0]
     [else (add1 (list-index obj (cdr list)))])))

(define list-last
  (lambda (list)
    (if (null? list)
	(error 'list-last "~a is empty!" list)
	(let loop ([a list] [b (cdr list)])
	  (if (null? b)
	      (car a)
	      (loop b (cdr b)))))))

;; Set operations
(define-struct set (%m))
(define empty-set (make-set null))
(define make-singleton-set (compose make-set list))
(define list->set
  (lambda (l)
    (unless (list? l) (error 'list->set "~a not a list"))
    (make-set l)))
(define set->list set-%m)
(define improper-list->set
  (lambda (l)
    (let loop ([l l] [acc null])
      (cond
	[(null? l) (list->set acc)]
	[(pair? l) (loop (cdr l) (cons (car l) acc))]
	[else (list->set (cons l acc))]))))
(define set-list-predicate
  (lambda (pred)
    (lambda (obj set)
      (pred obj (set->list set)))))
(define set-member? (set-list-predicate member))
(define set-memv? (set-list-predicate memv))
(define set-memq? (set-list-predicate memq))
(define set-empty? (compose null? set->list))

(define set-union ; O(|a|*|b|)
  (lambda (a b)
    (let union ([a (set->list a)]
		[b (set->list b)])
      (cond
       [(null? a) (list->set b)]
       [(memq (car a) b) (union (cdr a) b)]
       [else (union (cdr a) (cons (car a) b))]))))

(define set-union-singleton
  (lambda (set obj)
    (if (memq obj (set->list set))
	set
	(list->set (cons obj (set->list set))))))

(define set-minus ; O(|a|*|b|)
  (lambda (a b)
    (let minus ([a (set->list a)]
		[b (set->list b)]
		[acc null])
      (cond
       [(null? a) (list->set acc)]
       [(memq (car a) b) (minus (cdr a) b acc)]
       [else (minus (cdr a) b (cons (car a) acc))]))))

(define set-intersect ; O(|a|*|b|)
  (lambda (a b)
    (let intersect ([a (set->list a)]
		    [acc null])
      (cond
       [(null? a) (list->set acc)]
       [(set-memq? (car a) b) (intersect (cdr a) (cons (car a) acc))]
       [else (intersect (cdr a) acc)]))))

(define symbol-append
  (lambda s
    (let loop ([str ""] [s s])
      (if (null? s)
	  (string->symbol str)
	  (loop (string-append str (symbol->string (car s))) (cdr s))))))

(define compiler:formals->arity
  (lambda (f)
    (let ([L (length (zodiac:arglist-vars f))])
      (cond
	[(zodiac:sym-arglist? f) (values 0 -1)]
	[(zodiac:list-arglist? f) (values L L)]
	[(zodiac:ilist-arglist? f) (values (- L 1) -1)]))))

(define compiler:paroptformals->arity
  (lambda (f)
    (let ([L (length (zodiac:paroptarglist-vars f))]
	  [non-defs (let loop ([l (zodiac:paroptarglist-vars f)])
		      (if (or (null? l) (pair? (car l)))
			  0
			  (add1 (loop (cdr l)))))])
      (cond
	[(zodiac:sym-paroptarglist? f) (values 0 -1)]
	[(zodiac:list-paroptarglist? f) (values non-defs L)]
	[(zodiac:ilist-paroptarglist? f) (values non-defs -1)]))))

(define compiler:formals->arity*
  (lambda (fs)
    (cond
     [(null? fs) (values -1 0)]
     [(null? (cdr fs)) (compiler:formals->arity (car fs))]
     [else (let-values ([(a- a+) (compiler:formals->arity (car fs))]
			[(b- b+) (compiler:formals->arity* (cdr fs))])
		 (values (min a- b-)
			 (if (or (negative? b+) (negative? a+))
			     -1
			     (max a+ b+))))])))
              
(define compiler:gensym gensym)
(define compiler:label-number 0)
(define (compiler:reset-label-number!)
  (set! compiler:label-number 0))
(define compiler:genlabel
  (lambda ()
    (begin0 compiler:label-number
	    (set! compiler:label-number (add1 compiler:label-number)))))

(define compiler:bad-chars
  (string->list "#+-.*/<=>!?:$%_&~^@;^()[]{}|\\,~\"`' "))

(define (compiler:clean-string s)
  (let* ((str (string->list s)))
    (list->string
     (map (lambda (c) (if (member c compiler:bad-chars)
			  #\_
			  c))
	  str))))
     
(define (global-defined-value* v)
  (and v (global-defined-value v)))

)
