(define drscheme:zodiac-interface@
  (unit/sig zodiac:interface^
    (import [zodiac : zodiac:system^]
	    [mred : mred^])

    (mred:debug:printf 'invoke "drscheme:zodiac-interface@")

    (define dispatch-report
      (lambda (type string start-location end-location)
	(let* ([start (zodiac:location-offset start-location)]
	       [finish (add1 (zodiac:location-offset end-location))]
	       [file (zodiac:location-file start-location)])
	  (cond
	   [(is-a? file wx:media-edit%)
	    (let* ([frame (send file get-frame)]
		   [console-edit (ivar frame interactions-edit)]
		   [console-end-position (send console-edit get-end-position)]
		   [escape (send console-edit get-escape)])
	      (send frame ensure-interactions-shown)
	      (send console-edit this-err-write string)
	      (send (send file get-canvas) set-focus)
	      (send file set-position start finish)
	      (send file scroll-to-position start #f (send file last-position) -1)
	      (if escape
		  (escape)
		  ((error-escape-handler))))]
	   [else
	    (mred:message-box
	     (format "~a: ~a.~a-~a.~a: ~a" file
			   (zodiac:location-line start-location)
			   (zodiac:location-column start-location)
			   (zodiac:location-line end-location)
			   (zodiac:location-column end-location)
			   string)
	     "Error")
	    ((error-escape-handler))]))))

    (define report-error
      (lambda (type)
	(lambda (z s . args)
	  (let ([string (apply format (if mred:debug:on?
					  (string-append type s)
					  s)
			       args)])
	    (cond
	     [(zodiac:zodiac? z) (dispatch-report type string (zodiac:zodiac-start z) (zodiac:zodiac-finish z))]
	     [(zodiac:eof? z) (dispatch-report type string (zodiac:eof-location z) (zodiac:eof-location z))]
	     [(zodiac:period? z) (dispatch-report type string (zodiac:period-location z) (zodiac:period-location z))]
	     [else (mred:message-box string "Error")])
	    (printf "report-error: cannot escape: ~a~n" string)))))

    (define static-error (report-error "static error: "))
    (define dynamic-error (report-error "dynamic error: "))
    (define internal-error (report-error "internal error: "))))
