(module buginfo mzscheme

  (provide set-bug-report-info!
           get-bug-report-items)

  ; update with symbol/string assoc list
  (define bug-report-info 
    (lambda () '(("Computer language" "unknown"))))

  (define (set-bug-report-info! thunk)
    (set! bug-report-info thunk))

  (define (get-bug-report-items)
    (bug-report-info)))




