;; Provide send/suspend/callback, until it's publicly provided.

(module ssc mzscheme
  (require (lib "contract.ss")
           (lib "servlet.ss" "web-server")
           (lib "xml.ss" "xml"))

  ;; Is it a Xexpr, or an Xexpr with procedures?
  (define (xexpr-callback? x)
    (correct-xexpr-callback? x #t #f))

  (define xexpr/callback? xexpr-callback?)

  (provide/contract
    (send/suspend/callback (xexpr-callback? . -> . any))
    (send/forward/callback (xexpr-callback? . -> . any))
    (xexpr-callback? (any? . -> . boolean?))
    (xexpr/callback? (any? . -> . boolean?)))

  ;; send/suspend/callback : Response -> a
  ;; HTML with callbacks.
  (define (replace-procedures p-exp p->a)
    (cond
      ((list? p-exp) (map (lambda (p-e) (replace-procedures p-e p->a))
                          p-exp))
      ((procedure? p-exp) (p->a p-exp))
      (else p-exp)))

  (define (send/suspend/callback p-exp)
    (let/cc k0
      (send/back
        (replace-procedures
          p-exp (lambda (proc)
                  (let/cc k1 (k0 (proc (send/suspend k1)))))))))

  ;; Clear the continuation table first
  (define (send/forward/callback p-exp)
    (let/cc k0
      (send/back
        (begin
          (let/ec k7
            (send/forward (lambda (k-url) (k7 #f))))
          (replace-procedures
            p-exp (lambda (proc)
                    (let/cc k1 (k0 (proc (send/suspend k1))))))))))

  ;;; Copied and pasted from xml/private/xexpr.ss, then modified to include
  ;;; procedures.

  ;; correct-xexpr? : any any any -> any
  (define (correct-xexpr-callback? x true false)
    (cond
      ((string? x) true)
      ((symbol? x) true)
      ((number? x) true)
      ((comment? x) true)
      ((pi? x) true)
      ;;;
      ((procedure? x) true)
      ;;;
      ((list? x)
       (or (null? x)
           (if (symbol? (car x))
             (if (has-attribute? x)
               (and (attribute-pairs? (cadr x) true false)
                    (andmap (lambda (part)
                              (correct-xexpr-callback? part true false))
                            (cddr x))
                    true)
               (andmap (lambda (part)
                         (correct-xexpr-callback? part true false))
                       (cdr x)))
             false)))
      (else false)))

  ;; has-attribute? : List -> Boolean
  ;; True if the Xexpr provided has an attribute list.
  (define (has-attribute? x)
    (and (> (length x) 1)
         (list? (cadr x))
         (andmap (lambda (attr)
                   (pair? attr))
                 (cadr x))))

  ;; attribute-pairs? : List any any -> any
  ;; True if the list is a list of pairs.
  (define (attribute-pairs? attrs true false)
    (or (and (null? attrs) true)
        (let ((attr (car attrs)))
          (if (pair? attr)
            (and (attribute-symbol-string? attr true false)
                 (attribute-pairs? (cdr attrs) true false)
                 true)
            false))))

  ;; attribute-symbol-string? : List any any
  ;;                            -> any
  ;; True if the list is a list of String,Symbol pairs.
  (define (attribute-symbol-string? attr true false)
    (if (symbol? (car attr))
      (or (and (or (string? (cadr attr))
                   ;;;
                   (procedure? (cadr attr)))
                   ;;;
               true)
          false)
      false))
  )
