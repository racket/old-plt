(unit/sig setup:info^
  (import)

  (define (warning s x)
    (printf (string-append s "~n") (if (exn? x) (exn-message x) x))
    (when (and (global-defined-value 'print-error-trace) (exn? x))
      ((global-defined-value 'print-error-trace) (current-output-port) x)))

  ;; get-info : (listof string) -> (union #f (string (-> TST) -> TST))
  (define get-info
    (lambda (collection-p)
      (with-handlers ([(lambda (x) #t) (lambda (x) #f)])
	(let ([dir (apply collection-path collection-p)])
	  (with-handlers ([(lambda (x)
			     (and (exn:i/o:filesystem? x)
				  (or (string=? (exn:i/o:filesystem-pathname x)
						(build-path dir "info.ss"))
				      (string=? (exn:i/o:filesystem-pathname x)
						(build-path dir "info-table.ss")))))
			   (lambda (x) #f)]
			  [(lambda (x) #t)
			   (lambda (x)
			     (warning "Warning: error loading info: ~a" x)
			     #f)])
	    (let* ([info-table (build-path dir "info-table.ss")]
		   [info (build-path dir "info.ss")]
		   [info-table-proc
		    (if (file-exists? info-table)
			(let ([table (call-with-input-file info-table read)])
			  (if (and (list? table)
				   (andmap (lambda (x)
					     (and (list? x)
						  (= 2 (length x))
						  (symbol? (car x))))
					   table))
			      (lambda (key default)
				(let ([ans (assq key table)])
				  (if ans
				      (cadr ans)
				      (default))))
			      (begin
				(printf "Warning: info-table.ss wrong form: ~a~n" table)
				#f)))
			#f)]
		   [info-proc
		    (if (file-exists? info)
			(let* ([info
				(parameterize ([use-compiled-file-kinds 'none])
				  (apply require-library/proc "info.ss" collection-p))])
			  (lambda (key default)
			    (with-handlers ([(lambda (x) #t)
					     (lambda (x)
					       (warning (format 
							 "Warning: error calling info.ss proc with ~a: ~~a" 
							 key)
							x)
					       (default))])
			      (info key default))))
			#f)])
	      (if (or info-proc info-table-proc)
		  (lambda (key default)
		    (if info-table-proc
			(info-table-proc
			 key
			 (lambda ()
			   (if info-proc
			       (info-proc key default)
			       (default))))
			(info-proc key default)))
		  #f))))))))
