(module read-doc mzscheme

  (require (lib "etc.ss"))
  (require (lib "getinfo.ss" "setup"))

  (require "util.ss")
  (require "read-lines.ss")
  (require "hd-css.ss")

  (provide read-doc)

  ; extracts help desk message
  (define (get-message coll)
    (with-handlers ; collection may not exist
     ((void (lambda _ #f)))
     ((get-info (list coll)) 
      'help-desk-message
      (lambda () #f))))

  (define no-offset-format "file=~a&caption=~a")
  (define offset-format (string-append no-offset-format "&offset=~a#temp"))

  (define (build-page file caption coll offset)
    (let ([msg (get-message coll)])
      (if msg
	  `(HTML 
            (HEAD ,hd-css)
	    (FRAMESET ((ROWS "36,*")
		       (BORDER "0"))
	      (FRAME ((NAME "message")
		      (MARGINHEIGHT "2")
		      (MARGINWIDTH "4")
		      (BORDERCOLOR "whitesmoke")
		      (SCROLLING "no")
		      (SRC ,(format "/servlets/doc-message.ss?msg=~a" 
				    (hexify-string msg)))))
	      (FRAME ((NAME "content")	
		      (SRC ,(format 
			     "/servlets/doc-content.ss?~a"
			     (if offset
				  (format offset-format
					  (hexify-string file) caption offset)
				  (format no-offset-format
					  (hexify-string file) caption))))))))
	  (read-lines file caption offset))))

  (define read-doc 
    (opt-lambda (file caption coll [offset #f])
      (build-page file caption coll offset))))




