(define mred:url@
  (unit/sig mred:url^
    (import)

    (define-struct url (method path search fragment))

    (define character-set-size 256)

    (define marker-list
      '(#\: #\? #\#))

    (define default-port-number/http 80)

    (define ascii-marker-list
      (map char->integer marker-list))

    (define marker-locations
      (make-vector character-set-size))

    (define first-position-of-marker
      (lambda (c)
	(vector-ref marker-locations (char->integer c))))

    (define parse-url
      (lambda (url)
	(let loop ((markers ascii-marker-list))
	  (unless (null? markers)
	    (vector-set! marker-locations (car markers) #f)
	    (loop (cdr markers))))
	(let loop ((chars (string->list url)) (index 0))
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
	    (let ((total-length (string-length url)))
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
		    (cons scheme-start scheme-finish))
		  (cons path-start path-finish)
		  (and search-start
		    (cons search-start search-finish))
		  (and fragment-start
		    (cons fragment-start fragment-finish)))))))))

    (define decompose-path
      (lambda (sub-url)
	(let loop ((chars (string->list sub-url))
		    (this-string '())
		    (strings '()))
	  (if (null? chars)
	    (reverse
	      (cons (list->string (reverse this-string)) strings))
	    (if (char=? #\/ (car chars))
	      (loop (cdr chars) '()
		(cons (list->string (reverse this-string)) strings))
	      (loop (cdr chars) (cons (car chars) this-string)
		strings))))))

    (define normalize-path
      (lambda (paths)
	(let loop ((paths paths) (result '()))
	  (cond
	    ((null? paths) (reverse result))
	    ((string=? "" (car paths))
	      (loop (cdr paths) result))
	    ((string=? "." (car paths))
	      (if (null? result)
		(loop (cdr paths) (cons (car paths) result))
		(loop (cdr paths) result)))
	    ((string=? ".." (car paths))
	      (if (null? result)
		(loop (cdr paths) (cons (car paths) result))
		(loop (cdr paths) (cdr result))))
	    (else
	      (loop (cdr paths) (cons (car paths) result)))))))

    (define path->host/port/path
      (lambda (boundaries url)
	(let ((begin-point (+ 2 (car boundaries)))  ; skip over "//" after http:
	       (end-point (cdr boundaries)))
	  (let loop ((index begin-point)
		      (first-colon #f)
		      (first-slash #f))
	    (cond
	      ((>= index end-point)
		(values #f #f (substring url begin-point end-point)))
	      ((char=? #\: (string-ref url index))
		(loop (add1 index) (or first-colon index) first-slash))
	      ((char=? #\/ (string-ref url index))
		(if first-colon
		  (values
		    (substring url begin-point first-colon)
		    (string->number (substring url (add1 first-colon) index))
		    (substring url index end-point))
		  (values
		    (substring url begin-point index)
		    #f
		    (substring url index end-point))))
	      (else
		(loop (add1 index) first-colon first-slash)))))))

    (define get-url-i/o-ports
      (lambda (url)
	(let ((url (parse-url url)))
	  (let ((method (url-method url))
		 (path (url-path url))
		 (search (url-search url))
		 (fragment (url-fragment url)))
	    (let-values
	      (((hostname port-number access-path)
		 (path->host/port/path path url)))
	      (let-values
		(((input-port output-port)
		   (let ((port-number (or port-number default-port-number/http)))
		     (tcp-connect hostname port-number))))
		(fprintf output-port "GET ~a HTTP/1.0~n" access-path)
		(values input-port output-port)))))))

    (define print-text-at-url
      (lambda (url)
	(let ((server->client (get-port-for-url url)))
	  (let loop ()
	    (let ((c (read-char server->client)))
	      (unless (eof-object? c)
		(display c)
		(loop))))
	  (close-input-port server->client))))

    (define get-port-for-url
      (lambda (url)
	(let-values
	  (((server->client client->server)
	     (get-url-i/o-ports url)))
	  (newline client->server)
	  (close-output-port client->server)
	  (let* ((protocol (read server->client))
		  (code (read server->client)))
	    (let loop ()
	      (let ((r (read-line server->client)))
		(unless (string=? r "")
		  (loop))))
	    (close-input-port server->client)
	    server->client))))

    (define call-with-input-url
      (lambda (url handler)
	(let ((p (get-port-for-url url)))
	  (dynamic-wind
	    (lambda () 'do-nothing)
	    (lambda () (handler p))
	    (lambda () (close-input-port p))))))

      ))
