(unit/sig drscheme:interface^
  (import [zodiac : drscheme:zodiac^])
  
  (define-struct zodiac-exn (message start-location end-location type))
  
  ;; dispatch-report : symbol string number number -> ALPHA 
  ;; escapes
  (define dispatch-report
    (lambda (string object)
      (raise (make-exn:syntax string object #f))))
  
  ;; report-error : symbol -> (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define report-error
    (lambda (type)
      (lambda (z s . args)
	(let ([string (apply format (if (eq? type 'internal)
					(string-append "Internal error: "
						       s)
					s)
			     args)])
	  (cond
	    [(zodiac:zodiac? z) (dispatch-report string z)]
	    [(zodiac:eof? z) (dispatch-report string (zodiac:make-zodiac 'origin (zodiac:eof-location z) (zodiac:eof-location z)))]
	    [(zodiac:period? z) (dispatch-report string (zodiac:make-zodiac 'origin (zodiac:period-location z) (zodiac:period-location z)))]
	    [else ((error-display-handler) (format "internal-error.report-error: ~a: ~a" z string))])))))
  
  ;; static-error : (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define static-error (report-error 'static))

  ;; dynamic-error : (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define dynamic-error (report-error 'dynamic))

  ;; internal-error : (+ zodiac:zodiac zodiac:eof zodiac:period) string (listof TST) ->* ALPHA
  ;; escapes
  (define internal-error (report-error 'internal)))
