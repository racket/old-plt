(current-directory "/home/robby/plt/misc")
(load "hierarchy.ss")

(define re:class "^@class ([^ ]*)")
(define re:super "^@super[ ]?([^ ]*)")
(define re:cdb "(.*)(cdb|dbi)")

(define process-cdb
  (lambda (port)
    (let loop ()
      (let ([line (read-line port)])
	(unless (eof-object? line)
	  (cond
	    [(regexp-match re:class line)
	     =>
	     (lambda (match-class)
	       (let ([line (read-line port)])
		 (unless (eof-object? line)
		   (let ([match-super (regexp-match re:super line)])
		     (when match-super
		       (let ([super (let ([name (second match-super)])
				      (if (string=? "" name)
					  #f
					  (string->symbol name)))]
			     [class (string->symbol (second match-class))])
			 (add-relation class super)))))))]
	    [else (void)])
	  (loop))))))

(current-directory  "~robby/proj/doc/")
(let loop ([files (directory-list)])
  (cond
   [(null? files) (void)]
   [else (let ([file (car files)])
	   (when (regexp-match re:cdb file)
	     (call-with-input-file file process-cdb)))
	 (loop (cdr files))]))

(define frame (build-canvas-frame))
;(output-postscript "toolbox-hierarchy.ps" 1/2)
