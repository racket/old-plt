
;; These regexps are sensitive to the precise output of tex2page:
(define re:section (regexp "<h[2-9]>(<[^>]*>)*[0-9.]+(.*)</h[2-9]>"))
(define re:chapter (regexp "^(.*)</h1>"))
(define re:method (regexp "<a name=\"node_tag_Temp_[0-9]*\"></a><code class=scheme>(.*)</code> in "))

(define (clean-up s)
  (regexp-replace*
   "^ +"
   (regexp-replace*
    "&[^;]*;"
    (regexp-replace*
     "<[^>]*>"
     (regexp-replace* 
      "(&nbsp;)+"
      (regexp-replace* 
       "&amp;"
       (regexp-replace* 
	"&gt;"
	(regexp-replace*
	 "&lt;"
	 s
	 "<")
	">")
       "\\&")
      " ")
     "")
    "")
   ""))

;; Find the closest section title to the anchor
(define (get-title page anchor)
  (let ([re:anchor (regexp (regexp-quote anchor))])
    (with-handlers ([void (lambda (x) "???")])
      (with-input-from-file (build-path dir-to-read page)
	(lambda ()
	  (let loop ([title-so-far "???"])
	    (let ([r (read-line)])
	      (if (eof-object? r)
		  "???"
		  (let ([title-so-far
			 (cond
			  [(regexp-match re:section r)
			   => (lambda (m) (caddr m))]
			  [(regexp-match re:chapter r)
			   => (lambda (m) (cadr m))]
			  [(regexp-match re:method r)
			   => (lambda (m) (cadr m))]
			  [else title-so-far])])
		    (if (regexp-match re:anchor r)
			(if (equal? title-so-far "???")
			    ;; try section on one more line; needed for class sections in MrEd:
			    (let* ([r (read-line)]
				   [m (and (string? r)
					   (regexp-match re:section r))])
			      (if m
				  (caddr m)
				  title-so-far))
			    title-so-far)
			(loop title-so-far)))))))))))
