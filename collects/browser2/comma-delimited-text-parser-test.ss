(load "cdata-parser.ss")

(define system-faces (get-face-list))

(define font-list 
  (foldl 
   (lambda (f x) 
     (cond [#t (string-append f " , " x)]))
   ""
   system-faces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VARIABLES
; system-faces => list of all recognizable system fonts
; font-list => system-faces reformatted as cdata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEST CASES:

(define (test-parser input output)
  (let ([got (cdata-parser input)])
    (unless (equal? got output)
      (error 'test-parser "expected ~s, got ~s for ~s"
             output got input))))

(test-parser font-list system-faces)
(test-parser "" ())
(test-parser "," ())
(test-parser ", a" (list "a"))
(test-parser " , a" (list "a"))
(test-parser "a ," (list "a"))
(test-parser "a , " (list "a"))
(test-parser "a , , a" (list "a" "a"))
