#cs
(module proxy-player-test mzscheme 
  
  (require "if.scm"
           "proxy-player.scm"
           "proxy-auxs.scm"
           "tiles.scm"
           (lib "class.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 0))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 0)))
  
  (require (planet "util.ss" ("schematics" "schemeunit.plt" 1 0)))
  
  (require/expose 
   "proxy-auxs.scm" 
   (create-call parse-call create-return parse-return read-message))
  
  (define-syntax test-pp
    (syntax-rules ()
      [(_ label fake-call* fake-turn% fake-client result)
       (test-pp label fake-call* fake-turn% fake-client result comp: equal?)]
      [(_ label fake-call* fake-turn% fake-client result comp: equal)
       (make-test-case 
        label
        (assert equal
                (let ()
                  (define callp  (open-output-string))
                  (define call   (make-call callp))
                  (define return (make-return callp))
                  (define calls  (begin
                                   (fake-call* call return)
                                   (get-output-string callp)))                 
                  (define in     (open-input-string calls))
                  (define ou     (open-output-string))
                  (define pp     (new proxy-player% [input in] [output ou] [name "m"]))
                  (define _      (send pp take-turn (new fake-turn%)))
                  (fake-client   (open-input-string (get-output-string ou))))
                result))]))
  
  ;; --- just check a potential-locations-for-tile --- 
  
  (define tc0 
    (test-pp "take turn 0" fake-call0 fake-turn0% fake-client0
             (list (list 'take-turn)
                   '())))
  
  (define (fake-call0 call return)
    (call 'potential-locations-for-tile)
    (return (void)))
  
  (define (fake-client0 prt)
    (define listen (make-listen prt void void void))
    (define results '())
    (reverse
     (cons (listen (lambda (m args)
                     (set! results (cons `(,m . ,args) results))
                     (void)))
           results)))
  
  (define fake-turn0%
    (class object%
      (super-new)
      (define/public (potential-locations-for-tile) '())))
  
  (test/text-ui tc0) 
  
  ;; --- potenital-locations-for-tile with result and place-tile --- 
  
  (define tc1 
    (test-pp "take turn 1" fake-call1 fake-turn1% fake-client1
             (list the-tile)
             comp: list-tile=))
  
  (define the-tile (make-tile "1" 1 1 90))
  
  (define (fake-call1 call return)
    (call 'potential-locations-for-tile)
    (call 'place-tile the-tile)
    (return (void)))
  
  (define (fake-client1 prt)
    (define listen (make-listen prt void void void))
    (define results '())
    (cadr 
     (reverse
      (cons (listen (lambda (m args) (set! results (cons `(,m . ,args) results))))
            results))))
  
  (define fake-turn1%
    (class object%
      (super-new)
      (define/public (potential-locations-for-tile) (list the-tile))
      (define/public (place-tile t)
        (unless (is-a? t tile<%>) (error 'fake-turn1 "not a tile: ~e" t)))))      
  
  (test/text-ui tc1) 
  
  ;; --- potenital-locations-for-followers ---
  
  (define tc2 
    (test-pp "take turn 2" fake-call2 fake-turn2% fake-client2 '()))
  
  
  (define (fake-call2 call return)
    (call 'potential-locations-for-followers)
    (return (void)))
  
  (define (fake-client2 prt)
    (define listen (make-listen prt void void void))
    (define results '())
    (cadr 
     (reverse
      (cons (listen (lambda (m args) (set! results (cons `(,m . ,args) results))))
            results))))
  
  (define fake-turn2%
    (class object%
      (super-new)
      (define/public (potential-locations-for-followers) '())))
  
  (test/text-ui tc2) 
  
  ;; --- potenital-locations-for-followers, place follower ---
  
  (define tc3 
    (test-pp "take turn 3" fake-call3 fake-turn3% fake-client3 
             (list the-tile INNER)
             comp:
             (lambda (x y)
               (and (tile= (car x) (car y))
                    (eq? (cadr x) (cadr y))))))
  
  
  (define (fake-call3 call return)
    (call 'potential-locations-for-followers)
    (call 'place-follower the-tile INNER)
    (return (void)))
  
  (define (fake-client3 prt)
    (define listen (make-listen prt void void void))
    (define results '())
    (caadr 
     (reverse
      (cons (listen (lambda (m args) (set! results (cons `(,m . ,args) results))))
            results))))
  
  (define fake-turn3%
    (class object%
      (super-new)
      (define/public (potential-locations-for-followers) (list (list the-tile INNER)))
      (define/public (place-follower t f) (void))))
  
  (test/text-ui tc3) 
  
  )