(module gvec-tests mzscheme
  (require (file "/home/jacobm/tester/tester.ss"))
  (require (file "/home/jacobm/growable-vector.ss"))
  
(run-tests "Growable vector tests"
           
           (test "basic gvector-ref test"
	    (gvector-ref (make-gvector 1 'a) 0)
	    'a)
           (test-error "does a bad dereference properly signal an error?"
	    (gvector-ref (make-gvector 1 'a) 1)
            "gvector-ref: Index out of bounds: 1")
	   
           (test "gvector->list test"
	    (gvector->list (make-gvector 5 'a))
            '(a a a a a))
	   
           (test "are gvector->list and list->gvector inverse functions?"
	    (gvector->list (list->gvector '(a b c d e)))
            '(a b c d e))

           (test "test gvector-add! for basic functionality -- relies on gvector->list"
	    (let ((gvec (list->gvector '(a b c))))
              (begin (gvector-add! gvec 'd)
                     (gvector->list gvec)))
            '(a b c d))

           (test "test gvector-set! and gvector-add!"
	    (let ((gvec (list->gvector '(a b c))))
              (begin
                (gvector-set! gvec 0 'c)
                (gvector-add! gvec 'd)
                (gvector->list gvec)))
            '(c b c d))

           (test "large number of sequential gvector-add! calls"
	    (let ((gvec (list->gvector '())))
              (begin
                (for-each (lambda (x) (gvector-add! gvec x))
                          '(a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q
			       r  s  t  u  v  w  x  y  z  1  2  3  4  5  6  7  8
			       9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
                (gvector->list gvec)))
            '(a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q
		 r  s  t  u  v  w  x  y  z  1  2  3  4  5  6  7  8
		 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))

(test-manifest '((file "/home/jacobm/tester/gvec-tests.ss")))

)
  
