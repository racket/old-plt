;; Test send/suspend, send/back, send/finish, and send/forward
;;; TODO: test send/suspend/callback, send/suspend/dispatch
(module test-send mzscheme
  (require (lib "test.ss" "schemeunit")
           (lib "servlet.ss" "web-server")
           "backend-servlet-testing.ss"
           )

  (provide test-send)

  (define test-send
    (make-test-suite
      "Test send/suspend, send/back, send/finish, and send/forward"

      ;; send/back
      (make-test-case
        "send/back"
        (equal?
          (run-servlet '() '() (lambda () (send/back '(result "The result"))))
          '(result () "The result")))

      ;; send/finish
      (make-test-case
        "send/finish"
        (equal?
          (run-servlet '() '() (lambda () (send/finish '(result "The result"))))
          '(result () "The result")))

      ;; send/suspend
      (make-test-case
        "send/finish + send/suspend"
        (equal?
          (run-servlet
            (list '(n . "2"))
            (list (list '(n . "2")))
            (lambda ()
              (let ((get-num
                      (lambda ()
                        (string->number
                          (extract-binding/single
                            'n
                            (request-bindings
                              (send/suspend
                                (lambda (k-url)
                                  `(form ((action ,k-url)))))))))))
                (send/finish
                  `(result ,(number->string
                              (+ (get-num) (get-num))))))))
          '(result () "4")))

      ;; send/forward
      (make-test-case
        "send/finish + send/forward"
        (equal?
          (run-servlet
            (list '(n . "2"))
            (list (list '(n . "2")))
            (lambda ()
              (let ((get-num
                      (lambda ()
                        (string->number
                          (extract-binding/single
                            'n
                            (request-bindings
                              (send/forward
                                (lambda (k-url)
                                  `(form ((action ,k-url)))))))))))
                (send/finish
                  `(result ,(number->string
                              (+ (get-num) (get-num))))))))
          '(result () "4")))

      ))
  )
