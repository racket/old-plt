(unit/sig queue^
  (import)
  
  ;; Queue = (Cons #f #f) | (Cons (listof el) (Cons el null))
  
  ;; enq! : el Queue(el) -> el
  (define (enq! el q)
    (let ([tail (cdr q)]
	  [cell (cons el null)])
      (cond
	[tail
	 (set-cdr! (cdr q) cell)]
	[else
	 (set-car! q cell)])
      (set-cdr! q cell)
      el))
  
  ;; deq! : Queue(el) -> el
  ;; fails if q is empty
  (define (deq! q)
    (let ([result (caar q)])
      (cond
	[(null? (cdar q))
	 (set-car! q #f)
	 (set-cdr! q #f)]
	[else
	 (set-car! q (cdar q))])
      result))
  
  ;; dupq : Queue(el) -> Queue(el)
  (define (dupq q)
    (let ([newq (mtq)]
	  [head (car q)])
      (when head
	(let loop ([p head])
	  (unless (null? p)
	    (enq! (car p) newq);inefficient, but correct
	    (loop (cdr p)))))
      newq))
  
  ;; quick-dupq : Queue(el) -> Queue(el)
  ;; The list is shared
  (define (quick-dupq q)
    (cons (car q) (cdr q)))
  
  ;; mtq : -> Queue
  (define (mtq) (cons #f #f))
  
  ;; mtq? : Queue -> Boolean
  (define (mtq? q) (not (car q)))
  
  ;; last-q : Queue(el) -> el
  ;; q must not be empty.
  (define (last-q q) (cadr q))
  
  ;; first-q : Queue(el) -> el
  ;; q must not be empty.
  (define (first-q q) (caar q))
  
  ;; q-length : Queue(el) -> Nat
  (define (q-length q)
    (if (mtq? q)
	0
	(length (car q)))))