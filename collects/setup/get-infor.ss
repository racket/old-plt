(unit/sig setup:info^
  (import)

  (define (warning s x)
    (printf (string-append s "~n") (if (exn? x) (exn-message x) x)))

  ;; get-info : (listof string) -> (union #f (string (-> TST) -> TST))
  (define get-info
    (lambda (collection-p)
      (with-handlers ([void (lambda (x) #f)])
	(let ([dir (apply collection-path collection-p)])
	  (with-handlers ([(lambda (x)
			     (and (exn:i/o:filesystem? x)
				  (string=? (exn:i/o:filesystem-pathname x)
					    (build-path dir "info.ss"))))
			   (lambda (x) #f)]
			  [void
			   (lambda (x)
			     (warning "Warning: error loading info.ss: ~a" x)
			     #f)])
	    (let* ([info (parameterize ([use-compiled-file-kinds 'none])
			   (apply require-library/proc "info.ss" collection-p))])
	      (lambda (key default)
		(with-handlers ([(lambda (x) #t)
				 (lambda (x)
				   (warning "Warning: error calling info.ss proc: ~a" x)
				   (default))])
		  (info key default))))))))))
