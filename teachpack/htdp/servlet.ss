; Author: Paul Graunke
(module servlet mzscheme
  (require (lib "servlet-primitives.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server")
           (lib "servlet-sig.ss" "web-server")
           (lib "error.ss" "htdp")
           (lib "xml.ss" "xml")
           (lib "list.ss"))
  (provide (all-from (lib "servlet-primitives.ss" "web-server"))
           (all-from-except (lib "servlet-sig.ss" "web-server") servlet^)
           (all-from-except (lib "servlet-helpers.ss" "web-server") build-suspender)
           (rename wrapped-build-suspender build-suspender))

  (define wrapped-build-suspender
    (case-lambda
      [(title content)
       (check-suspender2 title content)
       (build-suspender title content)]
      [(title content body-attributes)
       (check-suspender3 title content body-attributes)
       (build-suspender title content body-attributes)]
      [(title content body-attributes head-attributes)
       (check-suspender3 title content body-attributes head-attributes)
       (build-suspender title content body-attributes head-attributes)]))
  
  ; : tst tst -> void
  (define (check-suspender2 title content)
    (check-arg 'build-suspender (html-list? title) "(listof xexpr[HTML])" "1st" title)
    (check-arg 'build-suspender (html-list? content) "(listof xexpr[HTML])" "2nd" content))
  
  ; : tst tst tst -> void
  (define (check-suspender3 title content body-attributes)
    (check-suspender2 title content)
    (check-arg 'build-suspender (attribute-list? body-attributes)
               "(listof (cons sym str))" "3rd" body-attributes))
  
  ; : tst tst tst tst -> void
  (define (check-suspender4 title content body-attributes head-attributes)
    (check-suspender3 title content body-attributes)
    (check-arg 'build-suspender (attribute-list? head-attributes)
               "(listof (cons sym str))" "4th" head-attributes))
  
  ; : tst -> bool
  (define (html-list? x)
    (and (list? x)
         (andmap (lambda (h)
                   (with-handlers ([void (lambda (exn) #f)])
                     (xexpr->xml x)))
                 x)
         #t))
  
  ; : tst -> bool
  (define (attribute-list? x)
    (and (list? x)
         ; more here - list? isn't enough to protect andmap because of improper lists
         (andmap (lambda (b)
                   (and (pair? b) (symbol? (car b)) (string? (cdr b))))
                 x)))
  )
