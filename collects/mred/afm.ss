(module afm mzscheme
  (require (lib "file.ss"))

  (provide afm-draw-text
	   afm-get-text-extent)

  (define (report-exn exn)
    (fprintf (current-error-port) "PostScript/AFM error: ~a~n"
	     (if (exn? exn)
		 (exn-message exn)
		 exn)))
  
  (define adobe-name-to-code-point #f)

  (define (read-names!)
    (let ([ht (make-hash-table 'equal)])
      (with-handlers ([not-break-exn? report-exn])
	(call-with-input-file* 
	 (build-path (collection-path "afm") "glyphlist.txt")
	 (lambda (i)
	   (let loop ()
	     (let ([l (read-line i)])
	       (unless (eof-object? l)
		 (let ([m (regexp-match #rx"^([a-zA-Z]+);([0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])$"
					l)])
		   (when m
		     (hash-table-put! ht
				      (cadr m)
				      (string->number (caddr m) 16))))
		 (loop)))))))
      (set! adobe-name-to-code-point ht)))
  
  (define (find-unicode name)
    (unless adobe-name-to-code-point
      (read-names!))
    (hash-table-get adobe-name-to-code-point
		    name
		    (lambda () #f)))

  (define-struct achar (enc name code-point width))
  (define-struct font (descent ascent achars))

  (define (parse-afm file)
    (let ([descender #f]
	  [bbox-down #f]
	  [cap-height #f]
	  [achars null])
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
	       [(startcharmetrics)
		(let ([cnt (read i)])
		  (let loop ()
		    (let ([n (read i)])
		      (when (or (eq? n 'c)
				(eq? n 'ch))
			(let ([v (read i)]
			      [rest (read-line i)])
			  (let ([nm (regexp-match #rx"; *N +([a-zA-Z]+) *;" rest)]
				[wm (regexp-match #rx"; *WX +([0-9]+) *;" rest)])
			    (when (or (and (eq? n 'c)
					   (integer? v))
				      (and (eq? n 'ch)
					   (symbol? v)
					   (regexp-match #rx"^<[0-9a-fA-F]>$" (symbol->string v))))
			      (set! achars 
				    (cons
				     (make-achar
				      (if (eq? n 'c)
					  v
					  (let ([s (symbol->string v)])
					    (string->number (substring s 1 (sub1 (string-length s))) 16)))
				      (or (and nm (cadr nm)) 0)
				      (or (and nm (find-unicode (cadr nm))) -1)
				      (or (and wm (string->number (cadr wm))) 500))
				     achars)))))
			(loop)))))]
	       [else (read-line i)])
	     (loop))))))
    (make-font (- (or descender bbox-down 0))
	       (or cap-height 1000)
	       (let ([ht (make-hash-table 'equal)])
		 (for-each
		  (lambda (c)
		    (hash-table-put! ht (achar-code-point c) c))
		  achars)
		 ht))))

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
					  (build-path (collection-path "afm")
						      (format "~a.afm" name)))))
		      (get-font name))))
  
  (define (afm-get-text-extent font-name size string)
    (let* ([font (or (get-font font-name)
		     (make-font 0 1000 #hash()))]
	   [scale (/ size 1000.0)]
	   [descent (* scale (font-descent font))])
      (values (* scale
		 (apply +
			(map (lambda (c)
			       (let ([achar (hash-table-get (font-achars font)
							    (char->integer c)
							    (lambda ()
							      (make-achar 0 "none" 0 500)))])
				 (achar-width achar)))
			     (string->list string))))
	      (+ size descent)
	      descent
	      (* scale (- 1000 (font-ascent font))))))

  (define (afm-draw-text font-name size string out)
    (unless (string=? "" string)
      (let ([l (string->list string)]
	    [font (or (get-font font-name)
		      (make-font 0 0 #hash()))]
	    [show-simples (lambda (simples)
			    (unless (null? simples)
			      (fprintf out "(~a) show\n"
				       (list->bytes (reverse simples)))))])
	(let loop ([l l][simples null])
	  (cond
	   [(null? l)
	    (show-simples simples)]
	   [(hash-table-get (font-achars font) (char->integer (car l))
			    (lambda () #f))
	    => (lambda (achar)
		 (if (<= 1 (achar-enc achar) 255)
		     (loop (cdr l) 
			   (cons (achar-enc achar) simples))
		     ;; Not simple... use glyphshow
		     (begin
		       (show-simples simples)
		       (fprintf out "/~a glyphshow\n" (achar-name achar))
		       (loop (cdr l) null))))]
	   [else
	    ;; No mapping for the character.
	    (show-simples simples)
	    ;; Draw a box, eventually...
	    (loop (cdr l) null)]))))))
