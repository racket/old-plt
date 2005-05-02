#cs
(module proxy-admin-test mzscheme 
  
  (require "if.scm"
           "proxy-admin.scm"
           "proxy-auxs.scm"
           "tiles.scm"
           (lib "class.ss")
           (lib "pretty.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 0))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 0)))
  
  (require (planet "util.ss" ("schematics" "schemeunit.plt" 1 0)))
  
  (require/expose 
   "proxy-auxs.scm" 
   (create-call parse-call create-return parse-return read-message))
  
  (define-syntax test-admin
    (syntax-rules (comp:)
      [(_ label player% server expected-result)
       (test-admin label player% server expected-result comp: equal?)]
      [(_ label player% server expected-result comp: equal)
       (make-test-case 
        label
        (assert equal
                (let ()
                  (define-values (ain aout) (make-pipe))
                  (define-values (sin sout) (make-pipe))
                  ;; --- setting up the fake server 
                  (define call  (make-call aout))
                  (define retn  (make-return aout))
                  (define lisn  (make-listen sin void void void))
                  ;; --- from server to admin 
                  (define stuff0 (retn RED))
                  ;; --- registering a player with the proxy-admin, run-game
                  (define ad  (new proxy-admin% [input ain][output sout]))
                  (define pl  (new player%))
                  (define cl  (send ad register pl))
                  (define akn (parse-call (read-message sin (lambda () (printf "eof encountered")))))
                  (define th  (send ad run-game))
                  ;; --- calls and lisns from the server to the proxy-admin
                  (define results (server lisn call retn))
                  ;; it's all over now 
                  (kill-thread th)
                  results)
                expected-result))]))
  
  ;; --- testing : take turn w/o interaction 
  
  (define tc1 (test-admin "take-turn" tp1% sv1 (void)))
  
  (define (sv1 listen call return)
    (call 'take-turn)
    (listen))
  
  (define tp1% 
    (class object% 
      (super-new)
      (field [name "m"])
      (define/public (take-turn t)
        (unless (is-a? t turn<%>) (error 'take-turn "not a turn: ~e" t)))
      (define/public (inform s)
        (unless (string? s) (error 'inform "not a string: ~e" s)))))
  
  ;; --- testing : inform 
  
  (define tc2 (test-admin "inform" tp1% sv2 (void)))
  
  (define (sv2 listen call return)
    (call 'inform "hello\nworld")
    (listen))
  
  ;; --- testing take turn with request for tiles 
  
  (define tc3 (test-admin "take-turn 2" tp3% sv3 (void)))
  
  (define (sv3 listen call return)
    (define (cb m args)
      (if (eq? 'potential-locations-for-tile m)
          (return '())
          (error 'sv3 "bad call: ~e" `(,m . ,args))))
    (define _ (call 'take-turn))
    (define r (listen cb))
    r)
  
  (define tp3% 
    (class object% 
      (super-new)
      (field [name "m"])
      (define/public (take-turn t)
        (define ptl (send t potential-locations-for-tile))
        (unless (null? ptl) (error 'tp3 "expected (), given ~e" ptl)))        
      (define/public (inform s)
        (unless (string? s) (error 'take-turn "not a string: ~e" s)))))
  
  ;; --- testing take turn with request for tiles and tile placement 
  
  (define tc4 (test-admin "take-turn with tile placement" tp4% sv4 the-tile
                          comp: tile=))
  
  (define the-tile (make-tile "1" 1 1 90))
  
  (define (sv4 listen call return)
    (define r #f)
    (define (cb m args)
      (case m 
        [(potential-locations-for-tile) (return (list the-tile))]
        [(place-tile) (set! r (car args)) (return (void))]
        [else (error 'sv3 "bad call: ~e" `(,m . ,args))]))
    (define _ (call 'take-turn))
    (listen cb)
    r)
  
  (define tp4% 
    (class object% 
      (super-new)
      (field [name "m"])
      (define/public (take-turn t)
        (define plt (send t potential-locations-for-tile))
        (unless (pair? plt) (error 'tp3 "expected nel, given ~e" plt))
        (send t place-tile (car plt)))
      (define/public (inform s)
        (unless (string? s) (error 'take-turn "not a string: ~e" s)))))
  
  
  ;; --- testing take turn with request for tiles and tile placement 
  
  (define tc5 (test-admin "take-turn with tile and follower placement" tp5% sv5
                          (list INNER the-tile the-tile)
                          comp: 
                          (lambda (x y)
                            (and (eq? (car x) (car y))
                                 (tile= (cadr x) (cadr y))
                                 (tile= (caddr x) (caddr y))))))
  
  (define (sv5 listen call return)
    (define r #f)
    (define (cb m args)
      (case m 
        [(potential-locations-for-tile) (return (list the-tile))]
        [(place-tile) (set! r (car args))
                      (return (void))]
        [(potential-locations-for-followers) (return (list (list the-tile INNER)))]
        [(place-follower)
         (let* ([t (car args)]
                [p (cadr args)])
           (set! r (cons  p (cons t (list r))))
           (return (void)))]
        [else (error 'sv3 "bad call: ~e" `(,m . ,args))]))
    (define _ (call 'take-turn))
    (listen cb)
    r)
  
  (define tp5% 
    (class object% 
      (super-new)
      (field [name "m"])
      (define/public (take-turn t)
        (define plt (send t potential-locations-for-tile))
        (unless (pair? plt) (error 'tp5 "expected nel, given ~e" plt))
        (send t place-tile (car plt))
        (let ([plf (send t potential-locations-for-followers)])
          (unless (and (pair? plf) (pair? (car plf)) (is-a? (caar plf) tile<%>))
            (error 'tp5 "expected plf, given ~e" plf))
          (send t place-follower (caar plf) (cadar plf))))
      (define/public (inform s)
        (unless (string? s) (error 'take-turn "not a string: ~e" s)))))
  
  ;; --- testing Chris's problem 
  
  (define tc6 (test-admin "Chris's test" tp6-a% sv6 '()))
  
  (define (sv6 listen call return)
    (define r #f)
    (define iis (list "6" "21"))
    (define tiles (list (list (make-tile "6" 0 -1 90)
                              (make-tile "6" 0 -1 180)
                              (make-tile "6" 0 -1 270)
                              (make-tile "6" 1  0  90)
                              (make-tile "6" -1 0 270))
                        (list (make-tile "21" 0 1  90)
                              (make-tile "21" 0 1 270)
                              (make-tile "21" -1 0  90)
                              (make-tile "21" -1 0 270))))
    (define ffs (list (list (list (make-tile "6" 1  0  90) -100)
                            (list (make-tile "6" 1  0  90)  270)
                            (list (make-tile "00" 0 0 0) 0)
                            (list (make-tile "00" 0 0 0) 90))
                      (list (list (make-tile "21" 0 1 90) 90)
                            (list (make-tile "00" 0 0  0) 90)
                            (list (make-tile "6" 1  0  90) -100)
                            (list (make-tile "6" 1  0  90)  270))))
    (define (cb m args)
      (case m
        [(get-index) (return "xx")]
        [(potential-locations-for-tile)
         (let ([x (car tiles)])
           (set! tiles (cdr tiles))
           (return x))]
        [(place-tile) (return (void))]
        [(potential-locations-for-followers) 
         (let ([x (car ffs)])
           (set! ffs (cdr ffs))
           (return x))]
        [(place-follower) (return (void))]
        [else (error 'sv\6 "bad call: ~e" `(,m . ,args))]))
    (call 'take-turn)
    (listen cb)
    (call 'take-turn) ;; 2
    (listen cb)
    '())
  
  (define tp6-a% 
    (class object% 
      (super-new)
      (field [name "m"])
      (define (pp t) (send t to-string))
      (define/public (take-turn t)
        (define plt (send t potential-locations-for-tile))
        (define _1 (pretty-print (map pp plt)))
        (define i (send t get-index))
        (if (string=? i "6")
            (let ()
              (define _2 (send t place-tile (car (cdddr plt))))
              (define plf (send t potential-locations-for-followers))
              (define ff  (caddr plf))
              (pretty-print (map (lambda (t) (list (pp (car t)) (cadr t))) plf))
              (send t place-follower (car ff) (cadr ff)))
            (let ()
              (define _2 (send t place-tile (car plt)))
              (define plf (send t potential-locations-for-followers))
              (pretty-print (map (lambda (t) (list (pp (car t)) (cadr t))) plf)))))
      (define/public (inform s)
        (unless (string? s) (error 'take-turn "not a string: ~e" s)))))
 
  (test/text-ui 
   (make-test-suite 
    "proxy admin calls" 
    tc1 tc2 tc3 tc4 tc5 tc6))
  
  )

