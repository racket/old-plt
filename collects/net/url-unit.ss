;; To do:
;;   Handle HTTP/file errors.
;;   Not throw away MIME headers.
;;     Determine file type.

;; ----------------------------------------------------------------------

;; Input ports have two statuses:
;;   "impure" = they have text waiting
;;   "pure" = the MIME headers have been read

(module url-unit mzscheme
  (require (lib "file.ss")
           (lib "unitsig.ss")
	   (lib "thread.ss")
	   "uri-codec.ss"
           "url-sig.ss"
           "tcp-sig.ss")
  (provide url@)
  
  (define url@
    (unit/sig net:url^
      (import net:tcp^)
      
      (define-struct (url-exception exn) ())
      
      (define current-proxy-servers
	(make-parameter null (lambda (v)
			       (unless (and (list? v)
					    (andmap
					     (lambda (v)
					       (and (list? v)
						    (= 3 (length v))
						    (equal? (car v) "http")
						    (string? (car v))
						    (number? (caddr v))
						    (exact? (caddr v))
						    (integer? (caddr v))
						    (<= 1 (caddr v) 65535)))
					     v))
				 (raise-type-error
				  'current-proxy-servers
				  "list of list of scheme, string, and exact integer in [1,65535]"
				  v))
			       (apply
				list-immutable
				(map (lambda (v)
				       (list-immutable (string->immutable-string (car v))
						       (string->immutable-string (cadr v))
						       (caddr v)))
				     v)))))


      ;; This is commented out; it's here for debugging.
      ;; It used to be outside the unit.
      
      (quote
       (begin
         (invoke-open-unit/sig mzlib:url@ #f)
         (define url:cs (string->url "http://www.cs.rice.edu/"))
         (define url:me (string->url "http://www.cs.rice.edu/~shriram/"))
         (define (test url)
           (call/input-url url
                           get-pure-port
                           display-pure-port))))
      
      (define url-error
        (lambda (fmt . args)
          (let ((s (apply format fmt (map (lambda (arg)
                                            (if (url? arg)
                                                (url->string arg)
                                                arg))
                                          args))))
            (raise (make-url-exception s (current-continuation-marks))))))
      
      ;; scheme : str + #f
      ;; host : str + #f
      ;; port : num + #f
      ;; path : str
      ;; params : str + #f
      ;; query : str + #f
      ;; fragment : str + #f
      (define-struct url (scheme host port path params query fragment))
      (define-struct (url/user url) (user))
      
      (define url->string 
        (lambda (url)
          (let ((scheme (url-scheme url))
                (host (url-host url))
                (port (url-port url))
                (path (url-path url))
                (params (url-params url))
                (query (url-query url))
                (fragment (url-fragment url)))
            (cond
	     ((and scheme (string=? scheme "file"))
	      (string-append "file:" path
                             (or (and (not fragment) "")
                                 (string-append "#" fragment))))
             (else
	      (let ((sa string-append))
		(sa (if scheme (sa scheme "://") "")
		    (if host host "")
		    (if port (sa ":" (number->string port)) "")
					; There used to be a "/" here, but that causes an
					; extra leading slash -- wonder why it ever worked!
		    path
		    (if params (sa ";" params) "")
		    (if query (sa "?" query) "")
		    (if fragment (sa "#" fragment) ""))))))))
      
      ;; url->default-port : url -> num
      (define url->default-port
        (lambda (url)
          (let ((scheme (url-scheme url)))
            (cond
	     ((not scheme) 80)
	     ((string=? scheme "http") 80)
	     (else
	      (url-error "Scheme ~a not supported" (url-scheme url)))))))
      
      ;; make-ports : url -> in-port x out-port
      (define make-ports
        (lambda (url proxy)
          (let ((port-number (if proxy
				 (caddr proxy)
				 (or (url-port url)
				     (url->default-port url))))
		(host (if proxy
			  (cadr proxy)
			  (url-host url))))
            (tcp-connect host port-number))))
      
      ;; http://getpost-impure-port : bool x url x union (str, #f) x list (str) -> in-port
      (define http://getpost-impure-port
	(lambda (get? url post-data strings)
          (let*-values (((proxy) (assoc (url-scheme url) (current-proxy-servers)))
			((server->client client->server)
			 (make-ports url proxy)))
            (let ((access-string
                   (url->string
		    (if proxy
			url
			(make-url #f #f #f
				  (url-path url) (url-params url)
				  (url-query url) (url-fragment url))))))
              (for-each (lambda (s)
                          (display s client->server)
                          (display "\r\n" client->server))
                        (cons (format "~a ~a HTTP/1.0" (if get? "GET" "POST") access-string)
                              (cons (format "Host: ~a" (url-host url))
				    (if post-data
					(cons
					 (format "Content-Length: ~a" (string-length post-data))
					 strings)
					strings)))))
            (display "\r\n" client->server)
	    (when post-data
	      (display post-data client->server)
	      (flush-output client->server)) ;; technically not needed for TCP ports
            (tcp-abandon-port client->server)
            server->client)))
      
      ;; file://get-pure-port : url -> in-port
      (define file://get-pure-port
        (lambda (url)
          (when (url-host url)
            (url-error "Don't know how to get files from remote hosts"))
          (open-input-file (url-path url))))

      (define (schemeless-url url)
	(url-error "Missing protocol (usually \"http:\") at the beginning of URL: ~a" url))

      ;; getpost-impure-port : bool x url x list (str) -> in-port
      (define getpost-impure-port
	(lambda (get? url post-data strings)
          (let ((scheme (url-scheme url)))
            (cond
	     ((not scheme)
	      (schemeless-url url))
	     ((string=? scheme "http")
	      (http://getpost-impure-port get? url post-data strings))
	     ((string=? scheme "file")
	      (url-error "There are no impure file: ports"))
	     (else
	      (url-error "Scheme ~a unsupported" scheme))))))
      
      ;; get-impure-port : url [x list (str)] -> in-port
      (define get-impure-port
        (case-lambda
         [(url) (get-impure-port url '())]
         [(url strings) (getpost-impure-port #t url #f strings)]))

      ;; post-impure-port : url [x list (str)] -> in-port
      (define post-impure-port
        (case-lambda
         [(url post-data) (post-impure-port url post-data '())]
         [(url post-data strings) (getpost-impure-port #f url post-data strings)]))

      ;; getpost-pure-port : bool x url x list (str) -> in-port
      (define getpost-pure-port
        (lambda (get? url post-data strings)
          (let ((scheme (url-scheme url)))
            (cond
	     ((not scheme)
	      (schemeless-url url))
	     ((string=? scheme "http")
	      (let ((port (http://getpost-impure-port get? url post-data strings)))
		(with-handlers ([void (lambda (exn)
					(close-input-port port)
					(raise exn))])
		  (purify-port port))
		port))
	     ((string=? scheme "file")
	      (file://get-pure-port url))
	     (else
	      (url-error "Scheme ~a unsupported" scheme))))))
      
      ;; get-pure-port : url [x list (str)] -> in-port
      (define get-pure-port
        (case-lambda
         [(url) (get-pure-port url '())]
         [(url strings) (getpost-pure-port #t url #f strings)]))

      ;; post-pure-port : url str [x list (str)] -> in-port
      (define post-pure-port
        (case-lambda
         [(url post-data) (post-pure-port url post-data '())]
         [(url post-data strings) (getpost-pure-port #f url post-data strings)]))

      ;; display-pure-port : in-port -> ()
      (define display-pure-port
        (lambda (server->client)
	  (copy-port server->client (current-output-port))
          (close-input-port server->client)))
      
      (define empty-url?
        (lambda (url)
          (and (not (url-scheme url)) (not (url-params url))
               (not (url-query url)) (not (url-fragment url))
               (andmap (lambda (c) (char=? c #\space))
                       (string->list (url-path url))))))
      
      ;; file://combine-url/relative : fs-path x s/t/r -> fs-path
      
      (define file://combine-url/relative 
        (let ((path-segment-regexp (regexp "([^/]*)/(.*)"))
              (translate-dir
               (lambda (s)
                 (cond
		  [(string=? s "") 'same] ;; handle double slashes
		  [(string=? s "..") 'up]
		  [(string=? s ".") 'same]
		  [else s]))))
          (lambda (index offset)
	    (let*-values ([(simple-index) (simplify-path index)]
			  [(base name dir?)
			   (split-path simple-index)])
	      (if (string=? "" offset)
		  (build-path base name)
		  (build-path
		   (if (or dir?
			   (directory-exists? simple-index))
		       simple-index
		       (if (eq? base 'relative)
			   'same
			   base))
		   (let loop ((str offset))
		     (let ((m (regexp-match path-segment-regexp str)))
		       (cond
			[(not m) str]
			[else
			 (if (string=? "" (caddr m))
			     (translate-dir (cadr m))
			     (build-path (translate-dir (cadr m))
					 (loop (caddr m))))])))))))))
      
      ;; combine-url/relative : url x str -> url
      (define combine-url/relative
        (lambda (base string)
          (let ((relative (string->url string)))
            (cond
	     ((empty-url? base)		; Step 1
	      relative)
	     ((empty-url? relative)	; Step 2a
	      base)
	     ((url-scheme relative)	; Step 2b
	      relative)
	     (else				; Step 2c
	      (set-url-scheme! relative (url-scheme base))
	      (cond
	       ;; This case is here because the above tests
	       ;; ensure the relative extension is not really
	       ;; an absolute path itself, so we need not
	       ;; examine its contents further.
	       ((and (url-scheme base)   ; Interloper step
		     (string=? (url-scheme base) "file"))
		(set-url-path! relative
			       (file://combine-url/relative 
				(url-path base)
				(url-path relative)))
		relative)
	       ((url-host relative)	; Step 3
		relative)
	       (else
		(set-url-host! relative (url-host base))
		(set-url-port! relative (url-port base)) ; Unspecified!
		(let ((rel-path (url-path relative)))
		  (cond
		   ((and rel-path	; Step 4
			 (not (string=? "" rel-path))
			 (char=? #\/ (string-ref rel-path 0)))
		    relative)
		   ((or (not rel-path)	; Step 5
			(string=? rel-path ""))
		    (set-url-path! relative (url-path base))
		    (or (url-params relative)
			(set-url-params! relative (url-params base)))
		    (or (url-query relative)
			(set-url-query! relative (url-query base)))
		    relative)
		   (else		; Step 6
		    (merge-and-normalize 
		     (url-path base) relative)))))))))))
      
      (define merge-and-normalize
        (lambda (base-path relative-url)
          (let ((rel-path (url-path relative-url)))
            (let ((base-list (string->list base-path))
                  (rel-list (string->list rel-path)))
              (let*
                  ((joined-list
                    (let loop ((base (reverse base-list)))
                      (if (null? base)
                          rel-list
                          (if (char=? #\/ (car base))
                              (append (reverse base) rel-list)
                              (loop (cdr base))))))
                   (grouped
                    (let loop ((joined joined-list) (current '()))
                      (if (null? joined)
                          (list (list->string (reverse current)))
                          (if (char=? #\/ (car joined))
                              (cons (list->string
                                     (reverse (cons #\/ current)))
                                    (loop (cdr joined) '()))
                              (loop (cdr joined)
                                    (cons (car joined) current))))))
                   (grouped
                    (let loop ((grouped grouped))
                      (if (null? grouped) '()
                          (if (string=? "./" (car grouped))
                              (loop (cdr grouped))
                              (cons (car grouped) (loop (cdr grouped)))))))
                   (grouped
                    (let loop ((grouped grouped))
                      (if (null? grouped) '()
                          (if (null? (cdr grouped))
                              (if (string=? "." (car grouped)) '()
                                  grouped)
                              (cons (car grouped) (loop (cdr grouped)))))))
                   (grouped
                    (let remove-loop ((grouped grouped))
                      (let walk-loop ((r-pre '()) (post grouped))
                        (if (null? post)
                            (reverse r-pre)
                            (let ((first (car post))
                                  (rest (cdr post)))
                              (if (null? rest)
                                  (walk-loop (cons first r-pre) rest)
                                  (let ((second (car rest)))
                                    (if (and (not (string=? first "../"))
                                             (string=? second "../"))
                                        (remove-loop
                                         (append (reverse r-pre) (cddr post)))
                                        (walk-loop (cons first r-pre) rest)))))))))
                   (grouped
                    (let loop ((grouped grouped))
                      (if (null? grouped) '()
                          (if (null? (cdr grouped)) grouped
                              (if (and (null? (cddr grouped))
                                       (not (string=? (car grouped) "../"))
                                       (string=? (cadr grouped) ".."))
                                  '()
                                  (cons (car grouped) (loop (cdr grouped)))))))))
                (set-url-path! relative-url
                               (apply string-append grouped))
                relative-url)))))
      
      ;; call/input-url : url x (url -> in-port) x (in-port -> T)
      ;;                  [x list (str)] -> T
      (define call/input-url
        (let ((handle-port (lambda (server->client handler)
                             (dynamic-wind (lambda () 'do-nothing)
				 (lambda () (handler server->client))
				 (lambda () (close-input-port server->client))))))
          (case-lambda
           ((url getter handler)
            (handle-port (getter url) handler))
           ((url getter handler params)
            (handle-port (getter url params) handler)))))
      
      ;; purify-port : in-port -> header-string
      (define purify-port
        (lambda (port)
	  (let ([m (regexp-match-peek-positions #rx"^HTTP/.*?((\r\n\r\n)|(\n\n)|(\r\r))" port)])
	    (if m
		(read-string (cdar m) port)
		""))))
      
      (define character-set-size 256)
      
      ;; netscape/string->url : str -> url
      (define netscape/string->url
        (lambda (string)
          (let ((url (string->url string)))
            (if (url-scheme url)
                url
                (if (string=? string "")
                    (url-error "Can't resolve empty string as URL")
                    (begin
                      (set-url-scheme! url
                                       (if (char=? (string-ref string 0) #\/)
                                           "file"
                                           "http"))
                      url))))))
      
      ;; string->url : str -> url
      ;; New implementation, mostly provided by Neil Van Dyke
      (define string->url
	(let ((rx (regexp (string-append
			   "^"
			   "[ \t\f\r\n]*"
			   "("                ; <1  front-opt
			   "([a-zA-Z]*:)?"    ; =2  scheme-colon-opt
			   "("                ; <3  slashslash-opt
			   "//"
			   "([^:/@;?#]*@)?"   ; =4  user-at-opt
			   "([^:/@;?#]+)?"    ; =5  host-opt
			   "(:[0-9]*)?"       ; =6  colon-port-opt
			   ")?"               ; >3  slashslash-opt
			   ")?"               ; >1  front-opt
			   "([^;?#]*)"        ; =7  path
			   "(;[^?#]*)?"       ; =8  semi-parms-opt
			   "(\\?[^#]*)?"      ; =9  question-query-opt
			   "(#.*)?"           ; =10 hash-fragment-opt
			   "[ \t\f\r\n]*"
			   "$"))))
	  (lambda (str)
	    (let ([m (regexp-match #rx"^[ \t\f\r\n]*file:(.*)$" str)])
	      ;; File scheme:
	      (if m
		  (let ([path+fragment (regexp-match #rx"^([^#]*)(#(.*))?$" (cadr m))])
		    (let ([path (cadr path+fragment)]
			  [fragment (caddr path+fragment)])
		      (if (or (relative-path? path)
			      (absolute-path? path))
			  (make-url "file"
				    #f	; host
				    #f	; port
				    path
				    #f	; params
				    #f	; query
				    fragment)
			  (url-error "scheme 'file' path ~s neither relative nor absolute" path))))
		  ;; Other scheme:
		  (let ((match (regexp-match-positions rx str)))
		    (if match
			(let* ((get-str (lambda (pos skip-left skip-right)
					  (let ((pair (list-ref match pos)))
					    (if pair
						(substring str
							   (+ (car pair) skip-left)
							   (- (cdr pair) skip-right))
						#f))))
			       (get-num (lambda (pos skip-left skip-right)
					  (let ((s (get-str pos skip-left skip-right)))
					    (if s (string->number s) #f))))
			       (host (get-str 5  0 0)))
			  (make-url/user (get-str 2  0 1) ; scheme
					 host
					 (get-num 6  1 0) ; port
					 (let ([path (get-str 7  0 0)])
					   ;; If path is "" and the input is an absolute URL 
					   ;; with a hostname, then the intended path is "/", 
					   ;; but the URL is missing a "/" at the end.
					   (if (and (string=? path "")
						    host)
					       "/"
					       path))
					 (get-str 8  1 0) ; params
					 (get-str 9  1 0) ; query
					 (get-str 10 1 0) ; fragment
					 (get-str 4  0 1) ; user
					 ))
			(url-error "Invalid URL string: ~e" str))))))))
	
      (define (decode-some-url-parts url)
	(let ([uri-decode/maybe
	       (lambda (f)
		 ;; If #f, and leave unmolested any % that in't followed by hex digit
		 (and f (uri-decode (regexp-replace* "%([^0-9a-fA-F])" f "%25\\1"))))])
	  (make-url/user (uri-decode/maybe (url-scheme url))
			 (uri-decode/maybe (url-host url))
			 (uri-decode/maybe (url-port url))
			 (uri-decode/maybe (url-path url))
			 (url-params url)
			 (url-query url)
			 (uri-decode/maybe (url-fragment url))
			 (if (url/user? url)
			     (uri-decode/maybe (url/user-user url))
			     #f))))

#|
      Old version. See PR 6152 for information on its replacement.

      <old version elided.  That's what CVS is for.>
      
      |#

)))
