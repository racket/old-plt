(module constants mzscheme
  (provide
   dummy
   dummy-thunk
   undefined
   fail-empty
   fail-false
   test-true
   test-false
   id
   select-right
   select-left
   )
  
  (define dummy (void))
  (define dummy-thunk (lambda () dummy))
  (define undefined (letrec ([x x]) x))
  (define fail-empty (lambda () '()))
  (define fail-false (lambda () #f))
  (define test-true (lambda (x) #t))
  (define test-false (lambda (x) #f))
  (define id (lambda (x) x))
  (define select-right (lambda (x y) y))
  (define select-left (lambda (x y) x))
  )
