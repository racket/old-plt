(unit/sig quasiquote:quotester^
  (import
    quasiquote:graphical-interface^
    (url : mzlib:url^))

  (define-struct symbol (name))
  (define-struct (stock struct:symbol) ())
  (define-struct (fund struct:symbol) ())

  (define get-chart
    (lambda (symbol)
      (define base-directory-for-stocks "/sm/pg/")
      ;; Rule: append <capital initial of symbol>/<symbol>.gif
      (define base-directory-for-funds "/sm/trmfg/")
      ;; Rule: append <capital initial of symbol>/<symbol>.gif
      (define handle-processing
	(lambda (base-dir)
	  (let ((s (symbol-name symbol)))
	    (display-image-stream
	      (url:get-pure-port
		(url:make-url "http" "www.stockmaster.com" #f
		  (string-append base-dir "/"
		    (string (string-ref s 0))
		    "/" s ".gif")
		  #f #f #f))
	      s))))
      (cond
	((stock? symbol)
	  (handle-processing base-directory-for-stocks))
	((fund? symbol)
	  (handle-processing base-directory-for-funds))
	(else
	  (error 'get-chart
	    "~s is not a stock or fund" symbol)))))

  ;; http://www.stocksmart.com/ows-bin/owa/sq.returnPrice?symbols=<SYMBOL>

  (define extract-quote-amount
    (let ((pattern (regexp "<TD ALIGN=\"RIGHT\">\\$(.+)</TD>")))
      (lambda (port symbol)
	(let loop ()
	  (let ((line (read-line port)))
	    (if (eof-object? line)
	      (error 'get-quote
		"No quote found for ~s" (symbol-name symbol))
	      (let ((matched (regexp-match pattern line)))
		(if matched
		  (let ((value (let (($string (cadr matched)))
				 (let ((p (open-input-string $string)))
				   (let loop ((sum 0))
				     (let ((r (read p)))
				       (if (eof-object? r)
					 sum
					 (loop (+ r sum)))))))))
		    (let finish-loop ()
		      (let ((line (read-line port)))
			(unless (eof-object? line)
			  (finish-loop))))
		    value)
		  (loop)))))))))

  (define get-quote
    (lambda (symbol)
      (extract-quote-amount
	(url:get-pure-port
	  (url:make-url "http" "www.stocksmart.com" #f
	    "ows-bin/owa/sq.returnPrice"
	    #f
	    (string-append "symbols=" (symbol-name symbol))
	    #f))
	symbol)))

  (define stock make-stock)
  (define fund make-fund)

  )
