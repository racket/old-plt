
(define mred:html@
  (unit/sig mred:html^
    (import [mred:debug : mred:debug^]
	    mzlib:string^)

    (mred:debug:printf 'invoke "mred:html@")

    (define NUM-CACHED 5)
    (define cached (cons (cons "" null) null))
    (set-cdr! cached
	      (let loop ([n (sub1 NUM-CACHED)])
		(if (zero? n)
		    cached
		    (cons (cons "" null) (loop (sub1 n))))))

    (define cache-image
      (lambda (filename)
	(if (null? filename)
	    (make-object wx:image-snip%)
	    (let loop ([l (cdr cached)])
	      (cond
	       [(string=? filename (caar l))
		(send (cdar l) copy)]
	       [(eq? l cached)
		(let ([image  (make-object wx:image-snip%
					   filename
					   wx:const-bitmap-type-gif)])
		  (set-car! cached (cons filename image))
		  (set! cached (cdr cached))
		  image)]
	       [else (loop (cdr l))])))))

    (define html-convert
      (lambda (b)
	(letrec 
	    ([begin-edit-sequence (ivar b begin-edit-sequence)]
	     [end-edit-sequence (ivar b end-edit-sequence)]
	     [get-character (ivar b get-character)]
	     [find-string (ivar b find-string)]
	     [insert (ivar b insert)]
	     [delete (ivar b delete)]
	     [change-style (ivar b change-style)]
	     [last-position (ivar b last-position)]
	     [add-link (ivar b add-link)]
	     [make-link-style (ivar b make-link-style)]
	     [add-tag (ivar b add-tag)]
	     [get-filename (ivar b get-filename)]
	     [modified? (ivar b modified?)]
	     [set-modified (ivar b set-modified)]

	     [reverse-links (ivar b reverse-links)]

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

	     [parse-image-source
	      (let ([re:quote-img (regexp "SRC=\"([^\"]*)\"")]
		    [re:img (regexp "SRC=([^ ]*)")])
		(lambda (s)
		  (let ([m (or (regexp-match re:quote-img s)
			       (regexp-match re:img s))])
		    (if m
			(let ([s (cadr m)])
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
				    [label (let ([m (or (regexp-match re:quote-name s)
							(regexp-match re:name s))])
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

	     ;; Find next "<", translating whitespace, &# along the way
	     [find-bracket
	      (lambda (pos dewhite? del-white?)
		(let find-bracket ([pos pos][del-white? del-white?])
		  (let ([ch (get-character pos)])
		    (cond
		     [(char=? #\null ch) (values -1 #f)]
		     [(and (char-whitespace? ch) dewhite?)
		      (if del-white?
			  (begin
			    (delete (add1 pos))
			    (find-bracket pos #t))
			  (begin
			    (unless (char=? #\space ch)
				    (insert #\space pos (add1 pos)))
			    (find-bracket (add1 pos) #t)))]
		     [(char=? #\< ch) (values pos del-white?)]
		     [(char=? #\& ch) 
		      (let ([ch (get-character (add1 pos))]
			    [result
			     (lambda (v end-pos)
			       (insert v pos (add1 end-pos))
			       (find-bracket (+ pos
						(if (string? v)
						    (string-length v)
						    1))
					     (eqv? #\space v)))])
			(if (char=? #\# ch)
			    (let loop ([pos2 (+ 2 pos)][val 0])
			      (let ([ch (get-character pos2)])
				(if (char-numeric? ch)
				    (loop (add1 pos2) (+ (* 10 val) (- (char->integer ch) 48)))
				    (result (case val
					      [160 #\space]
					      [169 "(c)"]
					      [else ""])
					    pos2))))
			    (let loop ([pos2 (+ 2 pos)][l (list ch)])
			      (let ([ch (get-character pos2)])
				(if (or (char=? #\null ch) (char=? #\; ch))
				    (result
				     (case (string->symbol (list->string (reverse! l)))
				       [(nbsp) #\space]
				       [(gt) #\>]
				       [(lt) #\<]
				       [else ""])
				     pos2)
				    (loop (add1 pos2) (cons ch l)))))))]
		     [else (find-bracket (add1 pos) #f)]))))]

	     ;; Read inside of <>; return (values content-of-string end-position)
	     [read-bracket
	      (lambda (pos)
		(if (char=? (get-character (add1 pos)) #\!)
		    ;; comment - special parsing
		    (let ([end (find-string ">" 1 (+ pos 2))])
		      (if (negative? end)
			  (begin
			    (html-error "end-of-file looking for closing angle-bracket")
			    (values "!" (last-position)))
			  (values "!" (add1 end))))
		    ;; Not a comment - parse with attention to quotes
		    (let ([done (lambda (name pos)
				  (values (list->string (reverse! name)) pos))])
		      (let loop ([pos (add1 pos)][name null][quotes null])
			(let ([ch (get-character pos)])
			  (cond
			   [(char=? #\null ch)
			    (html-error "end-of-file looking for closing angle-bracket")
			    (done name pos)]
			   [(and (null? quotes) (char=? #\> ch)) 
			    (done name (add1 pos))]
			   [(char=? #\" ch)
			    (loop (add1 pos) (cons ch name) 
				  (if (or (null? quotes) (not (char=? #\" (car quotes))))
				      (cons #\" quotes)
				      (cdr quotes)))]
			   [else
			    (loop (add1 pos) (cons ch name) quotes)]))))))]

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
	     [find-end
	      (lambda (tag pos dewhite? del-white? enum-depth)
		(let-values ([(pos del-white?) (find-bracket pos dewhite? del-white?)])
			    (if (= pos -1)
				(begin
				  (html-error "couldn't find </~a>" tag)
				  (values (last-position) del-white?))
				(let-values ([(cmd next-pos) (read-bracket pos)]
					     [(found-tag args end?) (parse-command cmd)])
					    (cond
					     [(not end?)
					      (delete pos next-pos)
					      (let-values ([(pos del-white?) 
							    (translate-command pos dewhite? del-white? enum-depth
									       found-tag args)])
							  (find-end tag pos dewhite? del-white? enum-depth))]
					     [(eq? tag found-tag)
					      (delete pos next-pos)
					      (values pos del-white?)]
					     [else
					      (html-error "found </~a> looking for </~a>"
							  found-tag tag)
					      (values pos del-white?)])))))]

	     ;; Given pos for open bracket, find end and translate contents.
	     ;; Return (values position-for-continuing-search del-white?)
	     [translate
	      (lambda (pos dewhite? del-white? enum-depth)
		(let-values ([(cmd next-pos) (read-bracket pos)]
			     [(tag args end?) (parse-command cmd)])
			    (delete pos next-pos)
			    (if end? 
				(begin
				  (html-error "closing </~a> without opening" tag)
				  (values pos del-white?))
				(translate-command pos dewhite? del-white? enum-depth tag args))))]

	     [translate-command
	      (lambda (pos dewhite? del-white? enum-depth tag args)
		(cond
		 [(memq tag atomic-tags)
		  (let ([break (lambda (newlines)
				 (insert (make-string enum-depth #\tab) pos)
				 (values (+ pos enum-depth (try-newline pos newlines)) #t))])
		    (case tag
		      [(!) (values pos del-white?)]
		      [(br)
		       (break 1)]
		      [(p hr)
		       (break 2)]
		      [(li dd) (break 1)]
		      [(dt) (break 2)]
		      [(img)
		       (insert (cache-image (parse-image-source args)) pos)
		       (values (add1 pos) #f)]
		      [else 
		       (html-error "unimplemented (atomic) tag: ~a" tag)
		       (values pos del-white?)]))]
		 [(memq tag verbatim-tags)
		  (let* ([str (format "</~a>" tag)]
			 [end-pos (find-string str 1 pos -1 #t #f)])
		    (values
		     (if (= -1 end-pos)
			 (begin
			   (html-error "verbatim closing tag </~a> not found" tag)
			   (last-position))
			 (begin
			   (delete end-pos (+ end-pos (string-length str)))
			   (change-style delta:fixed pos end-pos)
			   end-pos))
		     #t))]
		 [(memq tag comment-tags)
		  (let* ([str (format "</~a>" tag)]
			 [end-pos (find-string str 1 pos -1 #t #f)]
			 [use-end
			  (if (negative? end-pos)
			      (begin
				(html-error "verbatim closing tag </~a> not found" tag)
				(last-position))
			      end-pos)])
		    (delete pos (+ use-end (string-length str)))
		    (values pos del-white?))]
		 [else
		  (let ([enum-depth (+ enum-depth
				       (if (memq tag enum-tags)
					   1
					   0))]
			[dewhite? (and dewhite?
				       (not (memq tag preformatted-tags)))])
		    (let-values ([(end-pos del-white?) 
				  (find-end tag pos dewhite? del-white? enum-depth)])
				(let* ([normal (lambda () (values end-pos del-white?))]
				       [restart (lambda () (values pos del-white?))]
				       [heading (lambda (delta)
						  (insert (string #\newline #\newline) end-pos)
						  (change-style delta pos end-pos)
						  (values (+ end-pos 2 (try-newline pos 1)) #t))])
				  (case tag
				    [(head body center) (normal)]
				    [(title) 
				     (delete pos end-pos)
				     (restart)]
				    [(dl ul)
				     (insert #\newline end-pos)
				     (values (add1 end-pos) #t)]
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
						     (if (or name tag)
							 (begin
							   (add-link pos end-pos name 
								     (or tag "top")
								     #t)
							   (make-link-style pos end-pos))
							 (when label
							       (add-tag label pos)))
						     (normal))]
				    [else 
				     (html-error "unimplemented tag: ~s" tag)
				     (normal)]))))]))])
	  (let ([m? (modified?)])
	    (dynamic-wind
	     (lambda ()
	       (begin-edit-sequence #f))
	     (lambda ()
	       (add-tag "top" 0)
	       (let loop ([pos 0][del-white? #t])
		 (let-values ([(pos del-white?) (find-bracket pos #t del-white?)])
			     (unless (= pos -1)
				     (call-with-values
				      (lambda () (translate pos #t del-white? 0))
				      loop)))))
	     (lambda ()
	       (reverse-links)
	       (end-edit-sequence)
	       (unless m? 
		       (set-modified #f))))))))))


