
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))


(SECTION 'class/d)

(import (lib "class.ss"))
(import (lib "classd.ss"))

(syntax-test #'(class/d object% ((public x)) (define (x) 1)))
(syntax-test #'(class/d object% () ((public x))))
;; Should this be an error?
; (syntax-test '(class/d object% () ((public x x)) (define x 10)))

(define-syntax class/d-test-macro
  (lambda (stx)
    (syntax-case stx ()
      [(_ res test-name exp)
       (with-syntax ([sym (datum->syntax (gensym (format "~a:" (syntax-e (syntax test-name)))) #f #f)])
	 (syntax
	  (test
	   res
	   (let ([sym (lambda () exp)])
	     sym))))])))

(class/d-test-macro
 1
 'test-0
 (send (make-object (class/d object% () ((public y)) (define-values (y) (lambda () 1)) (super-init))) y))

(class/d-test-macro
 1
 'test-1
 (send (make-object (class/d object% () ((public y)) (define (y) 1) (super-init))) y))

(class/d-test-macro
 1
 'test-2
 (send (make-object (class/d object% () ((public y)) (define (y) 1) (define (z) 1) (super-init))) y))

(class/d-test-macro
 3
 'test-3
 (let ([x 1])
   (make-object 
       (class/d object% () ()
         (set! x 2)
         (set! x 3)
         (super-init)))
   x))

(class/d-test-macro
 2
 'test-4
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                      ()
                      ((override x))
                      (super-init)
                      (define (x) 2)))
       x))


(class/d-test-macro
 2
 'test-5
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                      ()
                      ((inherit x)
                       (public y))
                      (super-init)
                      (define (y) (+ (x) (x)))))
       y))

(class/d-test-macro
 2
 'test-6
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                      ()
                      ((rename [super-x x])
                       (public y))
                      (super-init)
                      (define (y) (+ (super-x) (super-x)))))
       y))

(class/d-test-macro
 2
 'test-7
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                      ()
                      ((rename [super-x x])
                       (override x))
                      (super-init)
                      (define (x) (+ (super-x) (super-x)))))
       x))

(class/d-test-macro
 2
 'test-8
 (send (make-object (class/d object% (xxx)
                      ((public x))
                      (define (x) xxx)
                      (super-init))
         2)
       x))

(class/d-test-macro
 1
 'test-9
 (send (make-object (class/d*/names (local-this local-super-init)
                                    object%
                                    ((interface ()))
                                    ()
                                    ((public x))
                                    (define (x) 1)
                                    (local-super-init)))
       x))

(class/d-test-macro
 1
 test-10
 (send (make-object (class/d* object%
                      ((interface ()))
                      ()
                      ((public x))
                      (define (x) 1)
                      (super-init)))
       x))

(class/d-test-macro
 77
 test-11
 (ivar (make-object (class/d object% ()
                      ((public x))
                      (define y 77)
                      (define x y)
                      (super-init)))
       x))

(class/d-test-macro
 (cons 78 16)
 test-12
 (ivar (make-object (class/d (class object% () (public [x 16]) (sequence (super-init))) ()
                      ((override x)
                       (rename [super-x x]))
                      (super-init)
                      (define y 78)
                      (define x (cons y super-x))))
       x))

(class/d-test-macro
 #t
 test-13
 (ivar (make-object (class/d object% ()
                      ((public x))
                      
                      (define-struct x (a))
                      
                      (define x (x? (make-x 1)))
                      
                      (super-init)))
       x))

(class/d-test-macro
 #t
 test-14
 (ivar (make-object (class/d object% ()
                      ((public x))
                      
                      (define-struct x (a))
                      (define x (x-a (make-x #t)))
                      
                      (super-init)))
       x))

(class/d-test-macro
 #t
 test-15
 (ivar (make-object (class/d object% ()
                      ((public x))
                      
                      (define-struct x (a b))
                      (define x (x-b (make-x #f #t)))
                      
                      (super-init)))
       x))

(class/d-test-macro
 #t
 test-16
 (ivar (make-object (class/d object% ()
                      ((public x))
                      
                      (define-struct x (a))
                      (define x1 (make-x #f))
                      (set-x-a! x1 #t)
                      (define x (x-a x1))
                      
                      (super-init)))
       x))

(class/d-test-macro
 #t
 test-17
 (ivar (make-object (class/d object% ()
                      ((public x))
                      
                      (define-struct x (a b))
                      (define x1 (make-x #f #f))
                      (set-x-b! x1 #t)
                      (define x (x-b x1))
                      
                      (super-init)))
       x))

(class/d-test-macro
 #t
 test-18
 (ivar (make-object (class/d object% ()
                      ((public x))
                      
                      (define-struct x (a))
                      (define-struct (y struct:x) (b))
                      (define x (y-b (make-y #f #t)))
                      
                      (super-init)))
       x))

(class/d-test-macro
 #t
 test-18
 (ivar (make-object (class/d object% ()
                      ((public x))
                      
                      (define-struct x (a))
                      (define-struct (y struct:x) (b))
                      (define x1 (make-y #f #f))
                      (set-y-b! x1 #t)
                      (define x (y-b x1))
                      (super-init)))
       x))

(report-errs)
