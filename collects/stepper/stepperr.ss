(unit/sig stepper:stepper^
  (import mzlib:pretty-print^
          [a : stepper:annotate^]
          [r : stepper:reconstruct^])
  
  ; this will be good only for a single debugging thread, for now
  
  (define exprs-list #f)


  (define break-info #f)
  (define break-sema (make-semaphore))
  (define resume-sema (make-semaphore))
  
  (define (break mark-list all-defs current-def)
    (set! break-info (list mark-list all-defs current-def))
    (semaphore-post break-sema)
    (semaphore-wait resume-sema))

  (define (stepper-start text)
    (let-values ([(annotated exprs)
                  (a:annotate text break)])
      (set! exprs-list exprs)
      (thread (lambda ()
                (printf "result: ~a~n" (eval annotated))
                (semaphore-post break-sema)))
      (semaphore-wait break-sema)
      (display-break-info)))
  
  (define (stepper-step)
    (semaphore-post resume-sema)
    (semaphore-wait break-sema)
    (display-break-info))
  
  (define (display-break-info)
    (pretty-print (apply r:reconstruct expr-list break-info)))
  
  )