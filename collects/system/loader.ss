(current-load
 (let* ([old (current-load)]
	[indent ""]
	[step " "]
	[right
	 (lambda () (set! indent (string-append indent step)))]
	[left
	 (lambda () (set! indent
			  (substring indent 0 (- (string-length indent)
						 (string-length step)))))]
	[show
	 (lambda (s . rest)
	   (apply printf (string-append indent s "~n") rest))])
   (lambda (s)
     (dynamic-wind
      (lambda ()
	(right))
      (lambda ()
	(with-handlers ([(lambda (x) #t)
			 (lambda (exn)
			   (show "loaded ~a" s)
			   (raise exn))])
	  (old s)))
      (lambda ()
	(left))))))