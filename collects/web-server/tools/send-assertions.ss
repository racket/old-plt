;; Drive a servlet. Pretend to be a Web browser, and send a request to the
;; server. Produce the response.

(module send-assertions mzscheme
  (require (lib "test.ss" "schemeunit")
           "servlet-testing-framework.ss")

  (provide
    assert-output-response/suspended
    (struct unknown ()))

  ;; The unknown value
  (define-struct unknown () (make-inspector))

  ;; Ensure output-response produces the right value. Feed the send/suspends
  ;; (-> response)
  ;; (listof (cons/p (xexpr? . -> . string?) (cons/p symbol? string?)))
  ;; xexpr? . -> . boolean?
  (define (assert-output-response/suspended outputter ins out)
    (special-equal?
      (let loop ((i ins)
                 (resp (start-servlet outputter)))
        (if (null? i)
          resp
          (let ((in (car i)))
            (loop (cdr i) (resume-servlet ((car in) resp) (cdr in))))))
      out))

  ;; True if:
  ;;  a or b is `unknown'
  ;;  a and b are not pairs and are equal?
  ;;  a and b are pairs and their car and cdr are special-equal?
  (define (special-equal? a b)
    (cond
      ((or (unknown? a) (unknown? b)) #t)
      ((and (not (pair? a)) (not (pair? b))) (equal? a b))
      ((and (pair? a) (pair? b)) (and (special-equal? (car a) (car b))
                                      (special-equal? (cdr a) (cdr b))))
      (else (equal? a b))))

 )
