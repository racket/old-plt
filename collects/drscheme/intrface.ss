  (unit/sig drscheme:interface^
    (import [zodiac : zodiac:system^]
	    [mred : mred^])

    (mred:debug:printf 'invoke "drscheme:zodiac-interface@")

    (define-struct (zodiac-exn struct:exn) (start-location end-location type))

    (define dispatch-report
      (lambda (type string start-location end-location)
	(let* ([start (zodiac:location-offset start-location)]
	       [finish (add1 (zodiac:location-offset end-location))]
	       [file (zodiac:location-file start-location)])
	  (raise (make-zodiac-exn string ((debug-info-handler))
				  start-location end-location type)))))

    (define report-error
      (lambda (type)
	(lambda (z s . args)
	  (let ([string (apply format (if (eq? type 'internal)
					  (string-append "Internal error: "
							 s)
					  s)
			       args)])
	    (printf "error: ~s ~s~n" string s)
	    (cond
	     [(zodiac:zodiac? z) (dispatch-report type string (zodiac:zodiac-start z) (zodiac:zodiac-finish z))]
	     [(zodiac:eof? z) (dispatch-report type string (zodiac:eof-location z) (zodiac:eof-location z))]
	     [(zodiac:period? z) (dispatch-report type string (zodiac:period-location z) (zodiac:period-location z))]
	     [else (mred:message-box (format "~a: ~a" z string) "Error")])
	    (printf "report-error: cannot escape: ~a~n" string)))))

    (define static-error (report-error 'static))
    (define dynamic-error (report-error 'dynamic))
    (define internal-error (report-error 'internal)))
