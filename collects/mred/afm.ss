(module afm mzscheme
  (require (lib "file.ss")
	   (lib "list.ss"))

  (provide afm-draw-text
	   afm-get-text-extent
	   afm-expand-name
	   afm-glyph-exists?)

  (define orig-err (current-error-port))
  (define (report-exn exn)
    (fprintf orig-err "PostScript/AFM error: ~a~n"
	     (if (exn? exn)
		 (exn-message exn)
		 exn)))

  (define bytes->number
    (case-lambda
     [(s) (bytes->number s 10)]
     [(s r) (string->number (bytes->string/latin-1 s) r)]))
  
  (define adobe-name-to-code-point #f)
  (define got-long-name-list? #f)

  (define afm-dirs (path-list-string->path-list 
		    (or (getenv "PLTAFMDIR") "")
		    (list (collection-path "afm"))))
  (define cmap-dirs (path-list-string->path-list 
		     (or (getenv "PLTCMAPDIR") "")
		     (list (collection-path "afm" "CMap"))))

  (define (find-path l f)
    (or (ormap (lambda (d)
		 (let ([f (build-path d f)])
		   (and (file-exists? f)
			f)))
	       l)
	(error 'find-path "file not found: ~e in dirs: ~e" f l)))

  (define (read-names! gl.txt long?)
    (let ([ht (make-hash-table 'equal)])
      (with-handlers ([not-break-exn? report-exn])
	(call-with-input-file* 
	 (find-path afm-dirs gl.txt)
	 (lambda (i)
	   (let loop ()
	     (let ([l (read-bytes-line i)])
	       (unless (eof-object? l)
		 (let ([m (regexp-match #rx#"^([a-zA-Z0-9_-]+);([0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])$"
					l)])
		   (when m
		     (hash-table-put! ht
				      (cadr m)
				      (bytes->number (caddr m) 16))))
		 (loop)))))))
      (set! adobe-name-to-code-point ht)
      (set! got-long-name-list? long?)))

  (define (find-unicode name)
    (unless adobe-name-to-code-point
      (read-names! "glyphshortlist.txt" #f))
    (hash-table-get adobe-name-to-code-point
		    name
		    (lambda ()
		      (if got-long-name-list?
			  #f
			  (begin
			    (read-names! "glyphlist.txt" #t)
			    (find-unicode name))))))

  (define make-achar cons)
  (define achar-enc car)
  (define achar-width cdr)
  (define-struct font (descent ascent achars is-cid? char-set-name))

  ;; read-cmap : path hash-table-or-false -> hash-table[int->int]
  (define (read-cmap file to-unicode)
    (let* ([ht (make-hash-table 'equal)]
	   [put! (if to-unicode
		     (lambda (c cns)
		       (hash-table-put! ht 
					c
					(hash-table-get to-unicode cns
							(lambda () #f))))
		     (lambda (c cns)
		       (hash-table-put! ht cns c)))])
      (with-handlers ([not-break-exn? report-exn])
	(call-with-input-file* 
	 file
	 (lambda (i)
	   (let loop ()
	     (let ([l (read-bytes-line i)])
	       (cond
		[(eof-object? l) (void)]
		[(regexp-match #rx#"begincidchar" l)
		 (let char-loop ()
		   (let ([l (read-bytes-line i)])
		     (cond
		      [(regexp-match #rx#"endcidchar" l)
		       (loop)]
		      [(regexp-match #rx#"^<([0-9A-Fa-f]*)> *([0-9]+)$" l)
		       => (lambda (m)
			    (put! (bytes->number (cadr m) 16)
				  (bytes->number (caddr m)))
			    (char-loop))]
		      [else
		       ;; parse error
		       (char-loop)])))]
		[(regexp-match #rx#"begincidrange" l)
		 (let range-loop ()
		   (let ([l (read-bytes-line i)])
		     (cond
		      [(regexp-match #rx#"endcidrange" l)
		       (loop)]
		      [(regexp-match #rx#"^<([0-9A-Fa-f]+)> <([0-9A-Fa-f]+)> *([0-9]+)$" l)
		       => (lambda (m)
			    (let* ([end (bytes->number (caddr m) 16)])
			      (let loop ([start (bytes->number (cadr m) 16)]
					 [v (bytes->number (cadddr m))])
				(put! start v)
				(unless (= start end)
				  (loop (add1 start) (add1 v)))))
			    (range-loop))]
		      [else
		       ;; parse error
		       (range-loop)])))]
		[else (loop)]))))))
      ht))

  (define cns->unicode-table #f)
  (define (read-cns->unicode!)
    (set! cns->unicode-table
	  (read-cmap
	   (find-path cmap-dirs "UniCNS-UTF32-H")
	   #f)))
  
  (define cmap-table (make-hash-table 'equal))
  (define (get-cmap name)
    (unless cns->unicode-table
      (read-cns->unicode!))
    (hash-table-get
     cmap-table
     name
     (lambda ()
       (let ([t (read-cmap
		 (find-path cmap-dirs (bytes->path name))
		 cns->unicode-table)])
	 (hash-table-put! cmap-table name t)
	 t))))

  (define (parse-afm file)
    (let ([descender #f]
	  [bbox-down #f]
	  [cap-height #f]
	  [achars (make-hash-table 'equal)]
	  [char-set #f]
	  [char-set-name #f]
	  [is-cid? #f])
      (call-with-input-file*
       file
       (lambda (i)
	 (let loop ()
	   (let ([n (read i)])
	     (unless (eof-object? n)
	       (case n
		 [(descender) (set! descender (read i))]
		 [(fontbbox) (let ([l (read i)]
				   [t (read i)]
				   [r (read i)]
				   [b (read i)])
			       (set! bbox-down b))]
		 [(capheight) (set! cap-height (read i))]
		 [(characterset) (let ([m (regexp-match #rx#"[a-zA-Z_0-9-]+" (read-bytes-line i))])
				   (when m
				     (set! char-set-name (car m))
				     (set! char-set (get-cmap char-set-name))))]
		 [(iscidfont) (set! is-cid?
				    (regexp-match #rx#"true" (read-bytes-line i)))]
		 [(startcharmetrics)
		  (let ([cnt (read i)])
		    (let loop ()
		      (let ([n (read i)])
			(when (or (eq? n 'c)
				  (eq? n 'ch))
			  (let ([v (read i)]
				[rest (read-bytes-line i)])
			    (let ([nm (regexp-match #rx#"; *N +([a-zA-Z0-9]+) *;" rest)]
				  [wm (regexp-match #rx#"; *W.?X +([0-9]+) *;" rest)])
			      (when (or (and (eq? n 'c)
					     (integer? v))
					(and (eq? n 'ch)
					     (symbol? v)
					     (regexp-match #rx#"^<[0-9a-fA-F]>$" (symbol->string v))))
				(let ([name (or (and nm 
						     (if is-cid?
							 (bytes->number (cadr nm))
							 (cadr nm)))
						0)])
				  (hash-table-put!
				   achars
				   (if (and char-set is-cid?)
				       (hash-table-get char-set name (lambda () 0))
				       (find-unicode name))
				   (make-achar
				    (let ([v (if (eq? n 'c)
						 v
						 (let ([s (symbol->string v)])
						   (string->number (substring s 1 (sub1 (string-length s))) 16)))])
				      (if (= v -1)
					  name
					  v))
				    (or (and wm (bytes->number (cadr wm))) 500)))))))
			  (loop)))))]
		 [else (read-bytes-line i)])
	       (loop))))))
      (make-font (- (or descender bbox-down 0))
		 (or cap-height 1000)
		 achars
		 (and char-set #t)
		 char-set-name)))

  (define fonts (make-hash-table 'equal))

  (define (get-font name)
    (hash-table-get fonts name
		    (lambda ()
		      (hash-table-put! fonts
				       name
				       (with-handlers ([void (lambda (exn)
							       (report-exn exn)
							       #f)])
					 (parse-afm 
					  (find-path afm-dirs
						     (format "~a.afm" name)))))
		      (get-font name))))

  (define font-list #f)
  (define (get-all-fonts)
    (or font-list
	(begin
	  (set! font-list
		(apply append (map (lambda (dir)
				     (map (lambda (s)
					    (path->string (path-replace-suffix s #"")))
					  (filter (lambda (s)
						    (regexp-match #rx"[.][aA][fF][mM]$" (path->string s)))
						  (directory-list dir))))
				   afm-dirs)))
	  font-list)))
  
  (define (afm-get-text-extent font-name size string)
    (let* ([font (or (get-font font-name)
		     (make-font 0 1000 #hash() #f #f))]
	   [scale (/ size 1000.0)]
	   [descent (* scale (font-descent font))])
      (values (* scale
		 (apply +
			(map (lambda (c)
			       (let ([achar (hash-table-get (font-achars font)
							    (char->integer c)
							    (lambda ()
							      (make-achar 0 500)))])
				 (achar-width achar)))
			     (string->list string))))
	      (+ size descent)
	      descent
	      (* scale (- 1000 (font-ascent font))))))

  (define (afm-draw-text font-name size string out)
    (let* ([l (string->list string)]
	   [font (or (get-font font-name)
		     (make-font 0 0 #hash() #f #f))]
	   [show-simples (lambda (simples)
			   (unless (null? simples)
			     (if (font-is-cid? font)
				 (fprintf out "<~a> show\n"
					  (apply
					   string-append
					   (map (lambda (s)
						  (let ([s (format "000~x" s)])
						    (substring s (- (string-length s) 4))))
						(reverse simples))))
				 (fprintf out "(~a) show\n"
					  (list->bytes (reverse simples))))))])
      (let loop ([l l][simples null])
	(cond
	 [(null? l)
	  (show-simples simples)]
	 [(hash-table-get (font-achars font) (char->integer (car l))
			  (lambda () #f))
	  => (lambda (achar)
	       (if (integer? (achar-enc achar))
		   (loop (cdr l) 
			 (cons (achar-enc achar) simples))
		   ;; Not simple... use glyphshow
		   (begin
		     (show-simples simples)
		     (fprintf out "/~a glyphshow\n" (achar-enc achar))
		     (loop (cdr l) null))))]
	 [else
	  ;; No mapping for the character.
	  (show-simples simples)
	  ;; Draw a box, eventually...
	  (loop (cdr l) null)]))))

  (define (afm-expand-name font-name)
    (let ([f (get-font font-name)])
      (if (and f (font-char-set-name f))
	  (string-append font-name "-" (bytes->string/latin-1 (font-char-set-name f)))
	  font-name)))

  (define (afm-glyph-exists?* font-name char-val)
    (let ([f (get-font font-name)])
      (and f (hash-table-get (font-achars f) char-val (lambda () #f)))))

  (define (afm-glyph-exists? font-name char-val)
    (or (afm-glyph-exists?* font-name char-val)
	(ormap (lambda (fn)
		 (afm-glyph-exists?* fn char-val))
	       (get-all-fonts)))))

