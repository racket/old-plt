
(define mred:html@
  (unit/sig mred:html^
    (import [mred:debug : mred:debug^]
	    mzlib:string^)

    (mred:debug:printf 'invoke "mred:html@")

    (define NUM-CACHED 10)
    (define cached (make-vector 10 null))
    (define cached-name (make-vector 10 ""))
    (define cached-use (make-vector 10 0))

    ;; if the path is absolute, it just arbitrarily picks the first
    ;; filesystem root.
    (define unixpath->path
      (letrec* ([r (regexp "([^/]*)/(.*)")]
		[translate-dir
		 (lambda (s)
		   (cond
		    [(string=? s "") 'same] ;; handle double slashes
		    [(string=? s "..") 'up]
		    [(string=? s ".") 'same]
		    [else s]))]
		[build-relative-path
		 (lambda (s)
		   (let ([m (regexp-match r s)])
		     (cond
		      [(string=? s "") 'same]
		      [(not m) s]
		      [else
		       (build-path (translate-dir (cadr m))
				   (build-relative-path (caddr m)))])))])
	       (lambda (s)
		 (let ([root (car (filesystem-root-list))])
		   (cond
		    [(string=? s "") ""]
		    [(string=? s "/") root]
		    [(char=? #\/ (string-ref s 0))
		     (build-path root
				 (build-relative-path (substring s 1 (string-length s))))]
		    [else (build-relative-path s)])))))
    
    (define cache-image
      (lambda (filename)
	(if (null? filename)
	    (make-object wx:image-snip%)
	    (let loop ([n 0])
	      (cond
	       [(= n NUM-CACHED)
		;; Look for item to uncache
		(let ([m (let loop ([n 1][m (vector-ref cached-use 0)])
			   (if (= n NUM-CACHED)
			       m
			       (loop (add1 n) (vector-ref cached-use n))))])
		  (let loop ([n 0])
		    (if (= (vector-ref cached-use n) m)
			(let ([image  (make-object wx:image-snip%
						   filename
						   wx:const-bitmap-type-gif)])
			  (vector-set! cached n image)
			  (vector-set! cached-name n filename)
			  (vector-set! cached-use n 0)
			  image)
			(loop (add1 n)))))]
	       [(string=? filename (vector-ref cached-name n))
		(vector-set! cached-use n (add1 (vector-ref cached-use n)))
		(send (vector-ref cached n) copy)]
	       [else (loop (add1 n))])))))

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
		(values
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

    (define get-ports-for-url
      (lambda (url)
	(let-values
	  (((scheme path search fragment)
	     (parse-url url)))
	  (let-values
	    (((hostname port-number access-path)
	       (path->host/port/path path url)))
	    (let-values
	      (((input-port output-port)
		 (let ((port-number (or port-number default-port-number/http)))
		   (tcp-connect hostname port-number))))
	      (fprintf output-port "GET ~a HTTP/1.0~n" access-path)
	      (values input-port output-port))))))

    (define print-text-at-url
      (lambda (url)
	(let-values
	  (((server->client client->server)
	     (get-ports-for-url url)))
	  (newline client->server)
	  (close-output-port client->server)
	  (let* ((protocol (read server->client))
		  (code (read server->client)))
	    (let loop ()
	      (let ((r (read-line server->client)))
		(unless (string=? r "")
		  (loop))))
	    (let loop ()
	      (let ((c (read-char server->client)))
		(unless (eof-object? c)
		  (display c)
		  (loop))))
	    (close-input-port server->client)))))

    (define html-convert
      (lambda (p b)
	(letrec 
	    ([normal (send (send b get-style-list)
			   find-named-style
			   "Standard")]
	     [get-character (ivar b get-character)]
	     [set-position (ivar b set-position)]
	     [insert (ivar b insert)]
	     [delete (ivar b delete)]
	     [get-text (ivar b get-text)]
	     [set-clickback (ivar b set-clickback)]
	     [change-style (ivar b change-style)]
	     [last-position (ivar b last-position)]
	     [add-link (ivar b add-link)]
	     [make-link-style (ivar b make-link-style)]
	     [add-tag (ivar b add-tag)]
	     [get-filename (ivar b get-filename)]
	     [set-modified (ivar b set-modified)]

	     [get-char (lambda () 
			 (let ([v (read-char p)])
			   (if (eof-object? v)
			       #\null
			       v)))]

	     [base-path (let ([f (get-filename)])
			  (let-values ([(base name dir?) (split-path f)])
			    (if (string? base)
				base
				(current-directory))))]
	     
	     [whitespaces (string #\space #\tab #\newline #\return)]

	     [verbatim-tags '(blockquote listing xmp plaintext)]
	     [preformatted-tags '(pre)]
	     [comment-tags '(script)]
	     [atomic-tags '(p br hr li dd dt img html ! meta link)]
	     [enum-tags '(ul dl ol)]

	     [delta:fixed (make-object wx:style-delta% 
				       wx:const-change-family 
				       wx:const-modern)]
	     [delta:bold (make-object wx:style-delta% 
				      wx:const-change-bold)] 
	     [delta:underline (make-object wx:style-delta% 
					   wx:const-change-underline)] 
	     [delta:slant (make-object wx:style-delta% 
				       wx:const-change-style 
				       wx:const-slant)]
	     [delta:h1 (let ([d (make-object wx:style-delta%
					     wx:const-change-bold)])
			 (send d set-size-mult 2.0)
			 d)]
	     [delta:h2 (let ([d (make-object wx:style-delta%
					     wx:const-change-bold)])
			 (send d set-size-mult 1.5)
			 d)]
	     [delta:h3 (let ([d (make-object wx:style-delta%
					     wx:const-change-bold)])
			 (send d set-size-mult 1.2)
			 d)]

	     ;; Don't report error; don't raise an exception
	     [html-error
	      (if (eq? mred:debug:on? 'html)
		  (lambda args
		    (begin
		      (apply fprintf (current-error-port) args)
		      (newline (current-error-port))))
		  void)]

	     [i-buffer null]
	     [buffer-pos 0]
	     [buffer-insert
	      (lambda (char pos)
		(when (null? i-buffer)
		      (set! buffer-pos pos))
		(set! i-buffer (cons char i-buffer)))]
	     [flush-i-buffer
	      (lambda ()
		(when (pair? i-buffer)
		      (insert (list->string (reverse! i-buffer)) buffer-pos)
		      (set! i-buffer null)))]

	     [parse-image-source
	      (let ([re:quote-img (regexp "SRC=\"([^\"]*)\"")]
		    [re:img (regexp "SRC=([^ ]*)")])
		(lambda (s)
		  (let ([m (or (regexp-match re:quote-img s)
			       (regexp-match re:img s))])
		    (if m
			(let ([s (unixpath->path (cadr m))])
			  (if (relative-path? s)
			      (build-path base-path s)
			      s))
			null))))]
	     
	     [parse-href
	      (let ([re:quote-tagged (regexp "[hH][rR][eE][fF]=\"([^\"#]*)#([^\"]*)\"")]
		    [re:tagged (regexp "[hH][rR][eE][fF]=([^ #]*)#([^ ]*)")]
		    [re:quote-plain (regexp "[hH][rR][eE][fF]=\"([^\"]*)\"")]
		    [re:plain (regexp "[hH][rR][eE][fF]=([^ ]*)")]
		    [re:quote-name (regexp "[nN][aA][mM][eE]=\"([^\"]*)\"")]
		    [re:name (regexp "[nN][aA][mM][eE]=([^ ]*)")]
		    [href-error
		     (lambda (s)
		       (html-error "bad reference in ~s" s))])
		(lambda (s)
		  (let-values ([(file tag)
				(cond 
				 [(or (regexp-match re:quote-tagged s)
				      (regexp-match re:tagged s))
				  =>
				  (lambda (m)
				    (if (string=? (cadr m) "")
					(values #f (caddr m))
					(values (cadr m) (caddr m))))]
				 [(or (regexp-match re:quote-plain s)
				      (regexp-match re:plain s))
				  =>
				  (lambda (m)
				    (values (cadr m) #f))]
				 [else
				  (values #f #f)])])
			      (let ([file (if (and file (string=? file ""))
					      (begin
						(href-error s)
						#f)
					      file)]
				    [tag (if (and tag (string=? tag ""))
					     (begin
					       (href-error s)
					       "top")
					     tag)]
				    [label (let ([m (if file
							#f
							(or (regexp-match re:quote-name s)
							    (regexp-match re:name s)))])
					     (if m
						 (cadr m)
						 #f))])
				(when tag
				      (string-lowercase! tag))
				(when label
				      (string-lowercase! label))
				(values file tag label)))))]
	     
	     ;; Make sure newline strength before pos is count
	     [try-newline
	      (lambda (pos count)
		(cond
		 [(zero? count) 0]
		 [(zero? pos) 0]
		 [(eq? #\newline (get-character (sub1 pos))) 
		  (try-newline (sub1 pos) (sub1 count))]
		 [else
		  (insert #\newline pos)
		  (add1 (try-newline pos (sub1 count)))]))]

	     [find-string 
	      (lambda (str pos keep?)
		(let ([first (string-ref str 0)]
		      [len (string-length str)])
		  (let loop ([pos pos][c (get-char)])
		    (cond
		     [(char=? #\null c)
		      (flush-i-buffer)
		      -1]
		     [(char-ci=? c first)
		      (let loop2 ([p 1][chars (list c)])
			(if (= p len)
			    (begin
			      (flush-i-buffer)
			      pos)
			    (let ([c (get-char)])
			      (cond
			       [(char-ci=? c (string-ref str p))
				(loop2 (add1 p) (cons c chars))]
			       [else
				(loop
				 (if keep?
				     (let ([s (list->string (reverse! chars))])
				       (flush-i-buffer)
				       (insert s pos)
				       (+ pos (string-length s)))
				     pos)
				 c)]))))]
		     [else
		      (loop
		       (if keep?
			   (begin
			     (buffer-insert c pos)
			     (add1 pos))
			   pos)
		       (get-char))]))))]

	     ;; Find next "<", translating whitespace, &# along the way
	     [find-bracket
	      (lambda (start-pos dewhite? del-white?)
		(let find-bracket ([pos start-pos][del-white? del-white?])
		  (let ([ch (get-char)])
		    (cond
		     [(char=? #\null ch) 
		      (flush-i-buffer)
		      (when (> pos start-pos)
			    (change-style normal start-pos pos))
		      (values -1 #f)]
		     [(and (char-whitespace? ch) dewhite?)
		      (if del-white?
			  (find-bracket pos #t)
			  (begin
			    (buffer-insert #\space pos)
			    (find-bracket (add1 pos) #t)))]
		     [(char=? #\< ch) 
		      (flush-i-buffer)
		      (when (> pos start-pos)
			    (change-style normal start-pos pos))
		      (values pos del-white?)]
		     [(char=? #\& ch) 
		      (let ([ch (get-char)]
			    [result
			     (lambda (v)
			       (flush-i-buffer)
			       (insert v pos)
			       (find-bracket (+ pos
						(if (string? v)
						    (string-length v)
						    1))
					     (eqv? #\space v)))])
			(if (char=? #\# ch)
			    (let loop ([val 0])
			      (let ([ch (get-char)])
				(if (char-numeric? ch)
				    (loop (+ (* 10 val) (- (char->integer ch) 48)))
				    (result (case val
					      [160 #\space]
					      [169 "(c)"]
					      [else ""])))))
			    (let loop ([l (list ch)])
			      (let ([ch (get-char)])
				(if (or (char=? #\null ch) (char=? #\; ch))
				    (result
				     (case (string->symbol (list->string (reverse! l)))
				       [(nbsp) #\space]
				       [(gt) #\>]
				       [(lt) #\<]
				       [else ""]))
				    (loop (cons ch l)))))))]
		     [else 
		      (buffer-insert ch pos)
		      (find-bracket (add1 pos) #f)]))))]

	     ;; Read inside of <>; return content-of-string
	     [read-bracket
	      (lambda ()
		(let ([first (get-char)])
		  (if (char=? #\! first)
		    ;; comment - special parsing
		    (let loop ()
		      (let ([ch (get-char)])
			(cond
			 [(char=? #\null ch)
			  (html-error "end-of-file looking for closing angle-bracket")
			  "!"]
			 [(char=? #\> ch) "!"]
			 [else (loop)])))
		    ;; Not a comment - parse with attention to quotes
		    (let ([done (lambda (name)
				  (list->string (reverse! name)))])
		      (let loop ([ch first][name null][quotes null])
			(cond
			 [(char=? #\null ch)
			  (html-error "end-of-file looking for closing angle-bracket")
			  (done name)]
			 [(and (null? quotes) (char=? #\> ch)) 
			  (done name)]
			 [(char=? #\" ch)
			  (loop (get-char) (cons ch name) 
				(if (or (null? quotes) (not (char=? #\" (car quotes))))
				    (cons #\" quotes)
				    (cdr quotes)))]
			 [else
			  (loop (get-char) (cons ch name) quotes)]))))))]
	     
	     ;; Parse string from inside <> into 
	     ;; (values html-tag-symbol tag-args-str end-tag?)
	     [parse-command
	      (let ([re:start (regexp (format "^([^~a/][^~a/]*)(.*)" whitespaces whitespaces))]
		    [re:end (regexp (format "^/([^~a]*)(.*)" whitespaces))])
		(lambda (str)
		  (let* ([match-start (regexp-match re:start str)]
			 [match-end (regexp-match re:end str)]
			 [match (or match-start match-end)]
			 [tag (if match (cadr match) str)]
			 [args (if match (caddr match) "")])
		    (unless match
			    (html-error "parse error for <~a>" str))
		    (string-lowercase! tag)
		    (values (string->symbol tag) args match-end))))]
	     
	     ;; Given CMD, find </CMD>; remove </CMD> and return position
	     ;; Translate nested <CMD2> ... </CMD2>
	     ;; Returns (values end-pos del-white? found-extra-end extra-args)
	     [find-end
	      (lambda (tag pos dewhite? del-white? enum-depth)
		(let-values ([(pos del-white?) (find-bracket pos dewhite? del-white?)])
		  (if (= pos -1)
		      (begin
			(html-error "couldn't find </~a>" tag)
			(values (last-position) del-white? #f #f))
		      (let ([cmd (read-bracket)]
			    [found-end
			     (lambda (pos del-white? found-tag args)
			       (if (eq? tag found-tag)
				   (values pos del-white? #f #f)
				   (begin
				     (html-error "found </~a> looking for </~a>"
						 found-tag tag)
				     (values pos del-white? found-tag args))))])
			(let-values ([(found-tag args end?) (parse-command cmd)])
			  (if (not end?)
			      (let-values ([(pos del-white? found-tag args) 
					    (translate-command pos dewhite? del-white? enum-depth
							       found-tag args)])
				(if found-tag
				    (found-end pos del-white? found-tag args)
				    (find-end tag pos dewhite? del-white? enum-depth)))
			      (found-end pos del-white? found-tag args)))))))]

	     [translate-command
	      (lambda (pos dewhite? del-white? enum-depth tag args)
		(cond
		 [(memq tag atomic-tags)
		  (let* ([atomic-values (lambda (pos del-white?)
					  (values pos del-white? #f #f))]
			 [break (lambda (newlines)
				  (insert (make-string enum-depth #\tab) pos)
				  (atomic-values (+ pos enum-depth (try-newline pos newlines)) #t))])
		    (case tag
		      [(!) (atomic-values pos del-white?)]
		      [(br)
		       (break 1)]
		      [(p hr)
		       (break 2)]
		      [(li dd) (break 1)]
		      [(dt) (break 2)]
		      [(img)
		       (insert (cache-image (parse-image-source args)) pos)
		       (atomic-values (add1 pos) #f)]
		      [else 
		       (html-error "unimplemented (atomic) tag: ~a" tag)
		       (atomic-values pos del-white?)]))]
		 [(memq tag verbatim-tags)
		  (let* ([str (format "</~a>" tag)]
			 [end-pos (find-string str pos #t)])
		    (values
		     (if (= -1 end-pos)
			 (begin
			   (html-error "verbatim closing tag </~a> not found" tag)
			   (last-position))
			 (begin
			   (change-style delta:fixed pos end-pos)
			   end-pos))
		     #t #f #f))]
		 [(memq tag comment-tags)
		  (let* ([str (format "</~a>" tag)]
			 [end-pos (find-string str pos #f)])
		    (when (negative? end-pos)
			  (html-error "comment closing tag </~a> not found" tag))
		    (values pos del-white? #f #f))]
		 [else
		  (let ([enum-depth (+ enum-depth
				       (if (memq tag enum-tags)
					   1
					   0))]
			[dewhite? (and dewhite?
				       (not (memq tag preformatted-tags)))])
		    (let-values ([(end-pos del-white? extra-tag extra-args) 
				  (find-end tag pos dewhite? del-white? enum-depth)])
				(let* ([result (lambda (pos del-white?)
						 (values pos del-white? extra-tag extra-args))]
				       [normal (lambda () (result end-pos del-white?))]
				       [restart (lambda () (result pos del-white?))]
				       [heading (lambda (delta)
						  (insert (string #\newline #\newline) end-pos)
						  (change-style delta pos end-pos)
						  (result (+ end-pos 2 (try-newline pos 1)) #t))])
				  (case tag
				    [(head body center) (normal)]
				    [(title)
				     (delete pos end-pos)
				     (restart)]
				    [(dl ul)
				     (insert #\newline end-pos)
				     (result (add1 end-pos) #t)]
				    [(b strong)
				     (change-style delta:bold pos end-pos)
				     (normal)]
				    [(u)
				     (change-style delta:underline pos end-pos)
				     (normal)]
				    [(i em var dfn cite)
				     (change-style delta:slant pos end-pos)
				     (normal)]
				    [(tt code samp kbd)
				     (change-style delta:fixed pos end-pos)
				     (normal)]
				    [(pre)
				     (normal)]
				    [(h1) (heading delta:h1)]
				    [(h2) (heading delta:h2)]
				    [(h3) (heading delta:h3)]
				    [(a) (let-values ([(name tag label) (parse-href args)])
					   (printf "name ~s tag ~s label ~s~n" name tag label)
					   (if (or name tag)
					       (begin
						 (add-link pos end-pos 
							   (unixpath->path name)
							   (or tag "top")
							   #t)
						 (make-link-style pos end-pos))
					       (when label
						 (add-tag label pos)))
						     (normal))]
				    [else 
				     (html-error "unimplemented tag: ~s" tag)
				     (normal)]))))]))]

	     ;; Given pos for open bracket, find end and translate contents.
	     ;; Return (values position-for-continuing-search del-white?)
	     [translate
	      (lambda (pos dewhite? del-white? enum-depth)
		(let-values ([(cmd) (read-bracket)])
		  (let-values ([(tag args end?) (parse-command cmd)])
		    (if end? 
			(begin
			  (html-error "closing </~a> without opening" tag)
			  (values pos del-white?))
			(let-values ([(end-pos del-white? extra-tag extra-args) 
				      (translate-command pos dewhite? del-white? enum-depth tag args)])
			  (when extra-tag
			    (html-error "closing </~a> without opening" tag))
			  (values end-pos del-white?))))))])

	     (add-tag "top" 0)
	     (let loop ([pos 0][del-white? #t])
	       (let-values ([(pos del-white?) (find-bracket pos #t del-white?)])
		 (unless (= pos -1)
		   (call-with-values
		    (lambda () (translate pos #t del-white? 0))
		    loop))))
	     (set-position 0))))))
