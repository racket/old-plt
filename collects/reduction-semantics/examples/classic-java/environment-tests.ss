;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; environment-tests
;;
;; $Id: environment-tests.ss,v 1.2 2004/12/31 22:12:15 cobbe Exp $
;;
;; tests for the environment model.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module environment-tests mzscheme

  (require "test.ss")
  (provide environment-tests)
  (require/expose "environment.ss" ())

  (define environment-tests
   (make-test-suite
    "Tests for basic environments"

    (make-test-case
     "lookup-in-empty"
     (assert = 3 (lookup (make-empty-env) 'x (lambda () 3))))

    (make-test-case
     "id present"
     (assert = 42 (lookup (extend-env (make-empty-env)
                                      '(x y z)
                                      '(16 42 53))
                          'y (lambda () 0))))
    (make-test-case
     "id not present"
     (assert = 0 (lookup (extend-env (make-empty-env)
                                     '(a b c) '(1 2 3)) 'x (lambda () 0))))

    (make-test-case "env->alist"
                    (assert-equal? (env->alist
                                    (extend-env (extend-env (make-empty-env)
                                                            '(x y)
                                                            '(1 2))
                                                '(a b x)
                                                '(4 5 6)))
                                   '((a 4) (b 5) (x 6) (x 1) (y 2))))

    (make-test-case "extend: mismatch"
      (assert-exn
       (lambda (exn)
         (and (exn:fail:contract? exn)
              (string=? (exn-message exn)
                        "extend-env: IDs and bindings must have same length")))
       (lambda () (extend-env (make-empty-env) '(a b c) '(3 4)))))
    )))
