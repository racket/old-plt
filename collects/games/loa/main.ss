(unit/sig ()
  (import mred^
	  loa^
	  loa:utils^)

  (define frame (make-object frame% "Lines of Action" #f 300 300))
  (define loa-pasteboard (make-object loa-pasteboard% 8 8))
  (define loa-canvas (make-object loa-canvas% frame loa-pasteboard))

  (define (make color x y)
    (send loa-pasteboard insert
	  (make-object loa-checker% color x y)))

  (let loop ([n 6])
    (unless (zero? n)
      (make 'white 0 n)
      (make 'white 7 n)
      (make 'black n 0)
      (make 'black n 7)
      (loop (- n 1))))

  (send frame show #t))
  