(require-library "macro.ss")

; Input ports have three statuses:
;   "raw" = they've just been opened
;   "impure" = they have text waiting
;   "pure" = the MIME headers have been read

; Things we still need:
; combine-url/relative : url x str -> url
; lookup-mime-header : list mime-header -> str + #f

(define-signature mred:url^
  (http/get-impure-port    ; url [x list (str)] -> in-port
    display-pure-port      ; in-port -> ()
    string->url            ; str -> url
    call/input-url         ; url x (in-port -> ()) [x list (str)] -> ()
    purify-port            ; in-port -> ()
))

(define mred:url@
  (unit/sig mred:url^
    (import)

    ; method : str + #f    path : str
    ; search : str + #f    fragment : str + #f
    (define-struct url (method path search fragment))

    ; name : str (all lowercase; not including the colon)
    ; value : str (doesn't have the eol delimiter)
    (define-struct mime-header (name value))

    ; host : str + #f    port : num + #f    path : str
    (define-struct accessor (host port path))

    ; url->default-port : url -> num
    (define url->default-port
      (lambda (url)
	(let ((method (url-method url)))
	  (cond
	    ((string=? method "http") 80)
	    ((not method) 80)
	    (else
	      (error 'url->default-port "Method ~s not supported"
		(url-method url)))))))

    ; make-ports : url x accessor -> in-port x out-port
    (define make-ports
      (lambda (url accessor)
	(let ((port-number (or (accessor-port accessor)
			     (url->default-port url))))
	  (tcp-connect (accessor-host accessor) port-number))))

    ; http/get-impure-port : url [x list (str)] -> in-port
    (define http/get-impure-port
      (opt-lambda (url (strings '()))
	(let ((accessor (url->accessor url)))
	  (let-values (((server->client client->server)
			 (make-ports url accessor)))
	    (for-each (lambda (s)
			(display s client->server))
	      (cons (format "GET ~a HTTP/1.0~n" (accessor-path accessor))
		strings))
	    (newline client->server)
	    (close-output-port client->server)
	    server->client))))

    ; display-pure-port : in-port -> ()
    (define display-pure-port
      (lambda (server->client)
	(let loop ()
	  (let ((c (read-char server->client)))
	    (unless (eof-object? c)
	      (display c)
	      (loop))))
	(close-input-port server->client)))

    ; call/input-url : url x (in-port -> ()) [x list (str)] -> ()
    (define call/input-url
      (opt-lambda (url handler (params '()))
	(let ((server->client (http/get-impure-port url params)))
	  (dynamic-wind
	    (lambda () 'do-nothing)
	    (lambda () (handler server->client))
	    (lambda () (close-input-port server->client))))))

    (define empty-line?
      (lambda (chars)
	(or (null? chars)
	  (and (memv (car chars) '(#\return #\linefeed #\tab #\space))
	    (empty-line? (cdr chars))))))

    (define extract-mime-headers-as-char-lists
      (lambda (port)
	(let headers-loop ((headers '()))
	  (let char-loop ((header '()))
	    (let ((c (read-char port)))
	      (if (eof-object? c)
		(reverse headers)	; CHECK: INCOMPLETE MIME: SERVER BUG
		(if (char=? c #\newline)
		  (if (empty-line? header)
		    (reverse headers)
		    (begin
		      (headers-loop (cons (reverse header) headers))))
		  (char-loop (cons c header)))))))))

    ; purify-port : in-port -> list mime-header
    (define purify-port
      (lambda (port)
	(let ((headers-as-chars (extract-mime-headers-as-char-lists port)))
	  (let header-loop ((headers headers-as-chars))
	    (if (null? headers)
	      '()
	      (let ((header (car headers)))
		(let char-loop ((pre '()) (post header))
		  (if (null? post)
		    (header-loop (cdr headers))
		    (if (char=? #\: (car post))
		      (cons (make-mime-header
			      (list->string (reverse pre))
			      (list->string post))
			(header-loop (cdr headers)))
		      (char-loop (cons (char-downcase (car post)) pre)
			(cdr post)))))))))))

    (define character-set-size 256)

    (define marker-list
      '(#\: #\? #\#))

    (define ascii-marker-list
      (map char->integer marker-list))

    (define marker-locations
      (make-vector character-set-size))

    (define first-position-of-marker
      (lambda (c)
	(vector-ref marker-locations (char->integer c))))

    ; string->url : str -> url
    (define string->url
      (lambda (string)
	(let loop ((markers ascii-marker-list))
	  (unless (null? markers)
	    (vector-set! marker-locations (car markers) #f)
	    (loop (cdr markers))))
	(let loop ((chars (string->list string)) (index 0))
	  (unless (null? chars)
	    (let ((first (car chars)))
	      (when (memq first marker-list)
		(let ((posn (char->integer first)))
		  (unless (vector-ref marker-locations posn)
		    (vector-set! marker-locations posn index)))))
	    (loop (cdr chars) (add1 index))))
	(let
	  ((first-colon (first-position-of-marker #\:))
	    (first-question (first-position-of-marker #\?))
	    (first-hash (first-position-of-marker #\#)))
	  (let
	    ((scheme-start (and first-colon 0))
	      (path-start (if first-colon (add1 first-colon) 0))
	      (search-start (and first-question (add1 first-question)))
	      (fragment-start (and first-hash (add1 first-hash))))
	    (let ((total-length (string-length string)))
	      (let*
		((scheme-finish (and scheme-start first-colon))
		  (path-finish (if first-question first-question
				 (if first-hash first-hash
				   total-length)))
		  (fragment-finish (and fragment-start total-length))
		  (search-finish (and search-start
				   (if first-hash first-hash
				     total-length))))
		(make-url
		  (and scheme-start
		    (substring string scheme-start scheme-finish))
		  (substring string path-start path-finish)
		  (and search-start
		    (substring string search-start search-finish))
		  (and fragment-start
		    (substring string fragment-start fragment-finish)))))))))

    ; url->accessor : url -> accessor
    (define url->accessor
      (lambda (url)
	(let* ((path (url-path url))
		(begin-point 2)		; skip over leading "//"
		(end-point (string-length path)))
	  (let loop ((index begin-point)
		      (first-colon #f)
		      (first-slash #f))
	    (cond
	      ((>= index end-point)
		(make-accessor #f #f (substring path begin-point end-point)))
	      ((char=? #\: (string-ref path index))
		(loop (add1 index) (or first-colon index) first-slash))
	      ((char=? #\/ (string-ref path index))
		(if first-colon
		  (make-accessor
		    (substring path begin-point first-colon)
		    (string->number (substring path (add1 first-colon) index))
		    (substring path index end-point))
		  (make-accessor
		    (substring path begin-point index)
		    #f
		    (substring path index end-point))))
	      (else
		(loop (add1 index) first-colon first-slash)))))))

      ))

; This is commented out; it's here for debugging.
(quote
  (begin
    (invoke-open-unit/sig mred:url@ #f)
    (define url:cs "http://www.cs.rice.edu/")
    (define url:me "http://www.cs.rice.edu/~shriram/")
    (define (test str)
      (call/input-url
	(string->url str)
	(lambda (p)
	  (purify-port p)
	  (display-pure-port p))))))
