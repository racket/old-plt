(unit/sig mrspidey:interaction^
  (import (compiler:option : compiler:option^)
	  (zodiac : zodiac:system^)
	  compiler:zlayer^
	  compiler:driver^)

  (define (xloc o)
    (if (or (not o)
	    (zodiac:zodiac? o)
	    (zodiac:location? o))
	o
	(begin
	  (compiler:warning 
	   #f 
	   (format "internal error: not a location: ~a" o))
	  #f)))
  
  (define mrspidey:error
    (case-lambda
     [(message object)
      ; (printf "Error~n")
      (analysis-error (xloc object) message)]
     [(message)
      (mrspidey:error message #f)]))

  (define mrspidey:internal-error
    (case-lambda
     [(message object)
      ; (printf "IError~n")
      (analysis-internal-error (xloc object) message)]
     [(message)
      (mrspidey:internal-error message #f #f)]))

  (define mrspidey:warning
    (case-lambda
     [(str loc word-no)
      ; (printf "Warning~n")
      (compiler:warning (xloc loc) str)]
     [(str) 
      (mrspidey:warning str #f #f)]))

  (define mrspidey:add-summary
    (case-lambda
     [(str loc word-no) 
      ; (printf "Summary~n")
      (when (compiler:option:verbose)
	(when (xloc loc)
	  (zodiac:print-start! loc))
	(printf "~a~n" str))]
     [(str) 
      (mrspidey:add-summary str #f #f)]))
  
  (define mrspidey:add-summary-handler
    (make-parameter
     (lambda args
       (apply mrspidey:add-summary args))))

  (define mrspidey:progress
    (lambda (str . rest)
      (when (and #f (compiler:option:verbose))
	(printf "   ~a~n" str))))

  (define mrspidey:progress-handler
    (make-parameter
     (lambda args
       (apply mrspidey:progress args))))

  (define record-analyzed-file
    (lambda args
      (apply (record-analyzed-file-hook) args)))

  (define record-analyzed-file-hook
    (make-parameter void)))
