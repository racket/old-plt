#cs
(module proxy-auxs-test mzscheme
  
  (require
   "if.scm"
   "tiles.scm"
   "proxy-auxs.scm"
   (planet "test.ss" ("schematics" "schemeunit.plt" 1 0))
   (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 0))
   (lib "class.ss"))
  
  (require (planet "util.ss" ("schematics" "schemeunit.plt" 1 0)))
  
  (require/expose 
   "proxy-auxs.scm" 
   (create-call parse-call create-return parse-return read-message))
  
  ;; -- testing: create-call followed by parse-call 
  
  (define-syntax test-cr
    (syntax-rules (comp:)
      [(_ label method args ... comp: equal)
       (make-test-case 
        label
        (assert equal
                (parse-call (create-call 'method (list args ...)))
                (list 'method args ...)))]
      [(_ label method args ...) (test-cr label method args ... comp: equal?)]))
  
  (define tc2 (test-cr "register" register "m"))  
  (define tc3 (test-cr "inform" inform "hello\nworld"))  
  (define tc4 (test-cr "p-l-t" potential-locations-for-tile))  
  (define tc5 (test-cr "p-t" place-tile (make-tile "1" 1 1 90) 
                       comp: (lambda (x y)
                               (and (eq? (car x) (car y))
                                    (tile= (cadr x) (cadr y))))))
  (define tc6 (test-cr "p-l-f" potential-locations-for-followers))  
  (define tc7 (test-cr "p-f" place-follower (make-tile "1" 1 1 90) INNER
                       comp: (lambda (x y)
                               (and (eq? (car x) (car y))
                                    (tile=  (cadr x) (cadr y))
                                    (equal? (caddr x) (caddr y))))))
  
  (test/text-ui 
   (make-test-suite "call followed by parse" tc2 tc3 tc4 tc5 tc6 tc7))
  
  (define tc8 (test-cr "take turn" take-turn))
  (define tc9 (test-cr "score and token" score-and-token 3 2))
  (define tc10 (test-cr "inform" inform "hello\ngood bye"))
  (define tc11 (test-cr "other score and token" other-score-and-token RED 3 2))
  (define tc12 (test-cr "placed tile" placed-tile (make-tile "1" 1 1 90) 
                        comp: 
                        (lambda (x y)
                          (and (eq? (car x) (car y))
                               (tile= (cadr x) (cadr y))))))
  (define tc13 (test-cr "placed follower" placed-follower (make-tile "1" 1 1 90) 
                        RED INNER
                        comp: 
                        (lambda (x y)
                          (and (eq? (car x) (car y))
                               (tile=  (cadr x) (cadr y))
                               (equal? (caddr x) (caddr y))
                               (equal? (cadddr x) (cadddr y))))))
  
  (test/text-ui
   (make-test-suite "call follower by parse for proxy-player" 
                    tc8 tc9 tc10 tc11 tc12 tc13))
  
  ;; --- testing: create-return followed by parse-return 
  
  (define-syntax test-rp
    (syntax-rules (comp:)
      [(_ label v) (test-rp label v comp: equal?)]
      [(_ label v comp: equal)
       (make-test-case label (assert equal
                                     (car (parse-return (create-return v)))
                                     v))]))
  
  (define rt0 (test-rp "void" (void)))
  (define rt2 (test-rp "red" RED))
  (define rt3 (test-rp "empty" '()))
  (define rt4 (test-rp "list of tiles" 
                       (list (make-tile "1" 1 1 0)
                             (make-tile "2" 2 2 0))
                       comp: list-tile=))
  (define rt5 (test-rp "list of tile followers"
                       (list (list (make-tile "1" 1 1 0) INNER)
                             (list (make-tile "2" 2 2 270) EAST))
                       comp: (lambda (x y)
                               (define (comp x y)
                                 (and (tile= (car x) (car y))
                                      (eq? (cadr x) (cadr y))))
                               (andmap comp x y))))
  
  
  (test/text-ui 
   (make-test-suite "return followed by parse" rt0 rt2 rt3 rt4 rt5))
  
  ;; --- testing: the conspiracy of read-message calls 
  
  (define-syntax test-io
    (syntax-rules (comp:)
      [(_ label method args ... comp: equal)
       (make-test-case 
        label 
        (assert equal 
                (let ()
                  (define out (open-output-string))
                  (define call (make-call out))
                  (call 'method args ...)
                  (close-output-port out)
                  (parse-call 
                   (read-message (open-input-string (get-output-string out)))))
                (list 'method args ...)))]
      [(_ label method args ...) (test-io label method args ... comp: equal?)]))
  
  (define rmcall1 (test-io "register" register "m"))
  (define rmcall3 (test-io "inform" inform "hello\nworld"))  
  (define rmcall4 (test-io "p-l-t" potential-locations-for-tile))  
  (define rmcall5 (test-io "p-t/rmcall" place-tile (make-tile "1" 1 1 90) 
                           comp: (lambda (x y)
                                   (and (eq? (car x) (car y))
                                        (tile= (cadr x) (cadr y))))))
  (define rmcall6 (test-io "p-l-f" potential-locations-for-followers))  
  (define rmcall7 (test-io "p-f" place-follower (make-tile "1" 1 1 90) INNER
                           comp: (lambda (x y)
                                   (and (eq? (car x) (car y))
                                        (tile=  (cadr x) (cadr y))
                                        (equal? (caddr x) (caddr y))))))
  
  (test/text-ui 
   (make-test-suite "call followed by parse via i/o"
                    rmcall1 rmcall3 rmcall4 rmcall5 rmcall6 rmcall7))
  
  (define rmcall8 (test-io "take turn" take-turn))
  (define rmcall9 (test-io "score and token" score-and-token 3 2))
  (define rmcall10 (test-io "inform" inform "hello\ngood bye"))
  (define rmcall11 (test-io "other score and token" other-score-and-token RED 3 2))
  (define rmcall12 (test-io "placed tile" placed-tile (make-tile "1" 1 1 90) 
                            comp: 
                            (lambda (x y)
                              (and (eq? (car x) (car y))
                                   (tile= (cadr x) (cadr y))))))
  (define rmcall13 (test-io "placed follower" placed-follower (make-tile "1" 1 1 90) 
                            RED INNER
                            comp: 
                            (lambda (x y)
                              (and (eq? (car x) (car y))
                                   (tile=  (cadr x) (cadr y))
                                   (equal? (caddr x) (caddr y))
                                   (equal? (cadddr x) (cadddr y))))))
  
  (test/text-ui
   (make-test-suite "call follower by parse for proxy-player" 
                    rmcall8 rmcall9 rmcall10 rmcall11 rmcall12 rmcall13))
  
  (test/text-ui 
   (make-test-suite
    "return followed by parse via i/o"
    (make-test-case 
     "void"
     (assert equal? 
             (let ()
               (define out (open-output-string))
               (define return (make-return out))
               (return (void))
               (close-output-port out)
               (parse-return 
                (read-message (open-input-string (get-output-string out)))))
             (list (void))))))
  )