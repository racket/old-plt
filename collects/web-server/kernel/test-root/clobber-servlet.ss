(module clobber-servlet mzscheme
  (require "../servlet.ss")

  (define counter 0)
  (define clobbered? #f)

  (define (clobber-it!)
    (let ([counter-snapshot (add1 counter)])
      (set! counter counter-snapshot)
      (sleep 1)
      (when (not (= counter counter-snapshot))
        (set! clobbered? #t))))

  (let loop ()

    (clobber-it!)

    (send/suspend
     (lambda (a-url)
       `(html (head (title "Clobber Servlet"))
              (body
               (h1 "Clobber Servlet")
               (p ,(format "clobbered? = ~a" clobbered?))
               (p ,(format "counter = ~a" counter))
               (p (a ([href ,a-url]) "Again"))))))

    (loop)))


