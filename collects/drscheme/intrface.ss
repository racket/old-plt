(define drscheme:zodiac-interface@
  (unit/sig zodiac:interface^
    (import [zodiac : zodiac:system^]
	    [mred : mred^])

    (mred:debug:printf 'invoke "drscheme:zodiac-interface@")

    (define report-in-edit
      (lambda (type string start-location end-location)
	(let* ([start (zodiac:location-offset start-location)]
	       [finish (add1 (zodiac:location-offset end-location))]
	       [edit (zodiac:location-file start-location)]
	       [frame (send edit get-frame)]
	       [console-edit (send frame get-console-edit)]
	       [console-end-position (send console-edit get-end-position)]
	       [escape (send console-edit get-escape)])
	  (send (send edit get-frame) set-show-mode 'both)
	  (send console-edit this-err-write string)
	  (send (send edit get-canvas) set-focus)
	  (send edit set-position start finish)
	  (send edit scroll-to-position start #f (send edit last-position) -1)
	  (send console-edit user-eval '(set-box! ((debug-info-handler)) #f))
	  (when escape
	    (escape)))))

    (define report-error
      (lambda (type)
	(lambda (z s . args)
	  (let ([string (apply format (if mred:debug:on?
					  (string-append type s)
					  s)
			       args)])
	    (cond
	     [(zodiac:zodiac? z) (report-in-edit type string (zodiac:zodiac-start z) (zodiac:zodiac-finish z))]
	     [(zodiac:eof? z) (report-in-edit type string (zodiac:eof-location z) (zodiac:eof-location z))]
	     [(zodiac:period? z) (report-in-edit type string (zodiac:period-location z) (zodiac:period-location z))]
	     [else (wx:message-box string "Error")])
	    (printf "report-error: cannot escape: ~a~n" string)))))

    (define static-error (report-error "static error: "))
    (define dynamic-error (report-error "dynamic error: "))
    (define internal-error (report-error "internal error: "))))
