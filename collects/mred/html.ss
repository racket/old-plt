
(define mred:html@
  (unit/sig mred:html^
    (import [mred:debug : mred:debug^]
	    [mred:url : mred:url^]
	    mzlib:file^
	    mzlib:string^)

    (mred:debug:printf 'invoke "mred:html@")

    (define NUM-CACHED 10)
    (define cached (make-vector 10 null))
    (define cached-name (make-vector 10 ""))
    (define cached-use (make-vector 10 0))

    (define get-image-from-url
      (lambda (url)
	(let ([tmp-filename (wx:get-temp-file-name "img")])
	  (call-with-output-file tmp-filename
	    (lambda (op)
	      (mred:url:call/input-url 
	       url
	       mred:url:get-pure-port
	       (lambda (ip)
		 (let loop ()
		   (let ([c (read-char ip)])
		     (unless (eof-object? c)
		       (write-char c op)
		       (loop)))))))
	    'replace)
	  (begin0 (make-object wx:image-snip% tmp-filename wx:const-bitmap-type-gif)
		  (delete-file tmp-filename)))))
			    
    (define cache-image
      (lambda (url)
	(if (null? url)
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
			(let ([image (get-image-from-url url)])
			  (vector-set! cached n image)
			  (vector-set! cached-name n url)
			  (vector-set! cached-use n 0)
			  image)
			(loop (add1 n)))))]
	       [(equal? url (vector-ref cached-name n))
		(vector-set! cached-use n (add1 (vector-ref cached-use n)))
		(send (vector-ref cached n) copy)]
	       [else
		(loop (add1 n))])))))

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

	     [base-path (mred:url:string->url (get-filename))]
	     
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
	      (let ([re:quote-img (regexp "[Ss][Rr][Cc]=\"([^\"]*)\"")]
		    [re:img (regexp "[Ss][Rr][Cc]=([^ ]*)")])
		(lambda (s)
		  (let ([m (or (regexp-match re:quote-img s)
			       (regexp-match re:img s))])
		    (if m
			(mred:url:combine-url/relative base-path (cadr m))
			null))))]
	     
	     [parse-href
	      (let ([re:quote-href (regexp "[hH][rR][eE][fF]=\"([^\"]*)\"")]
		    [re:href (regexp "[hH][rR][eE][fF]=([^ ]*)")]
		    [re:quote-name (regexp "[nN][aA][mM][eE]=\"([^\"]*)\"")]
		    [re:name (regexp "[nN][aA][mM][eE]=([^ ]*)")]
		    [href-error
		     (lambda (s)
		       (html-error "bad reference in ~s" s))])
		(lambda (s)
		  (let* ([url-string
			 (cond 
			   [(or (regexp-match re:quote-href s)
				(regexp-match re:href s))
			    => (lambda (m)
				 (let ([str (cadr m)])
				   (if (string=? str "")
				       (begin (href-error s)
					      #f)
				       str)))]
			   [else #f])]
			 [label (let ([m (if url-string
					     #f
					     (or (regexp-match re:quote-name s)
						 (regexp-match re:name s)))])
				  (if m
				      (cadr m)
				      #f))])
				(values url-string label))))]
	     
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
		       (let* ([url (parse-image-source args)]
			      [b (cache-image url)])
			 (insert b pos))
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
				    [(a) (let-values ([(url-string label) (parse-href args)])
					   (if url-string
					       (begin
						 (add-link pos end-pos 
							   url-string
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
