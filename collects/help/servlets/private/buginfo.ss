(module buginfo mzscheme

  (provide set-bug-report-info!
           get-bug-report-item)

  ; update with symbol/string assoc list
  (define bug-report-info #f)

  (define (set-bug-report-info! thunk)
    (set! bug-report-info (thunk)))

  (define (get-bug-report-item sym)
    (and bug-report-info
	 (let ([result (assq sym bug-report-info)])
	   (and result
                (cadr result))))))


