(module drbug mzscheme

  (provide set-dr-bug-report-info!
           get-dr-bug-report-item)

  ; update with symbol/string assoc list
  (define dr-bug-report-info #f)

  (define (set-dr-bug-report-info! thunk)
    (set! dr-bug-report-info (thunk)))

  (define (get-dr-bug-report-item sym)
    (and dr-bug-report-info
	 (let ([result (assq sym dr-bug-report-info)])
	   (and result
                (cadr result))))))


