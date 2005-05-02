#cs
(module run-proxy-bad2 mzscheme
  
  (require "if.scm" "run-proxy-aux.scm" "run-proxy-bad-players.scm")
  
  (max-turn-time 4)

  (go (list ; "Matthias"
            ; "Robby"
            bad-register
            bad-register2
            good-register-no-return
            ; qos-attack
            (make-bad (string-append "long-name" (make-string 2000 #\a))
                      (lambda (call _ out)
                        (call 'place-tile)))
            (make-bad 'place-tile 
                      (lambda (call _ out)
                        (call 'place-tile)))
            (make-bad 'send-null
                      (lambda (call listen out)
                        (define x (begin (call 'potential-locations-for-tile)
                                         (listen)))
                        (write #\000 out)
                        (call 'place-tile (car x))
                        (printf "send null char: ~s~n" (listen))))
            (make-bad 'send-long-bad-string
                      (lambda (call listen out)
                        (define x (begin (call 'potential-locations-for-tile)
                                         (listen)))
                        (call 'place-tile (make-string 1000 #\b))))
            (make-bad 'one-call-good-second-call-bad
                      (lambda (call listen out)
                        (define _ 
                          (call 'potential-locations-for-tile))
                        (define x (listen))
                        (call 'place-tile (car x))
                        (call 'place-follower)))
            ; (make-bad 'omega (lambda (call _1 _2) (let L () (L))))
            (make-bad 'broken-message
                      (lambda (call _ out)
                        (fprintf out "<call dummy~n"))))
      #f)
  
  )
