(unit/sig during^
  (import before^)
  (define remember
    (opt-lambda (name second [minute #f] [hour #f] [day #f] [month #f] [year #f])
      (send edit new-counter name second minute hour day month year)))
  
  (define remember-around
    (opt-lambda (name first second)
      (let ([open (apply (ivar edit new-counter)
			 (if (string? name)
			     (string-append name " start")
			     name)
			 first)]
	    [closed (apply (ivar edit new-counter)
			   (if (string? name)
			       (string-append name " end")
			       name)
			   second)])
	(send open set-open)
	(send closed set-close)))))
  