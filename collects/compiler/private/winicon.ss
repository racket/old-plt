
(module winicon mzscheme
  (provide install-icon)

  (define (byte->integer p)
    (char->integer (read-char p)))
  (define (word->integer p)
    (integer-byte-string->integer (read-string 2 p) #f #f))
  (define (dword->integer p)
    (integer-byte-string->integer (read-string 4 p) #f #f))

  (define (flag v)
    (positive? (bitwise-and #x80000000 v)))
  (define (value v)
    (bitwise-and #x7FFFFFFF v))

  (define (find-rsrc-start p re:rsrc)
    ;; p is expected to be a file port
    (file-position p 60)
    (let ([pos (word->integer p)])
      ;; pos points to IMAGE_NT_HEADERS
      (file-position p pos)
      (unless (= #x4550 (dword->integer p))
	      (error "bad signature"))
      (word->integer p) ; skip machine
      (let ([num-sections (word->integer p)]
	    [_ (begin (dword->integer p)
		      (dword->integer p)
		      (dword->integer p))]
	    [size (word->integer p)])
	(let ([pos (+ pos
		      4       ; Signature : DWORD
		      20      ; FileHeader: IMAGE_FILE_HEADER
		      size)]) ; "optional" header
	  (let sloop ([section 0][section-pos pos])
	    (unless (= section num-sections)
		    (file-position p section-pos)
		    ;; p points to an IMAGE_SECTION_HEADER
		    (let ([name (read-string 8 p)])
		      (if (not (string=? ".rsrc\0\0\0" name))
			  (sloop (add1 section) (+ section-pos 40))
			  (let ([_ (dword->integer p)]
				[rsrc-virtual-addr (dword->integer p)]
				[rsrc-len (dword->integer p)]
				[rsrc-pos (dword->integer p)])
			    (let loop ([dir-pos 0][path ""])
			      (file-position p (+ rsrc-pos dir-pos 12))
			      (let ([num-named (word->integer p)]
				    [num-ided (word->integer p)])
				(let iloop ([i 0])
				  (if (= i (+ num-ided num-named))
				      #f
				      (let ([name-delta (dword->integer p)]
					    [data-delta (dword->integer p)]
					    [next (file-position p)])
					(or (let ([name (if (flag name-delta)
							    (begin
							      (file-position p (+ rsrc-pos (value name-delta)))
							      (let* ([len (word->integer p)])
								;; len is in unicode chars...
								(let ([unistr (read-string (* 2 len) p)])
								  ;; Assume it fits into ASCII...
								  (regexp-replace* "\0" unistr ""))))
							    (value name-delta))])
					      ;;(printf "Name: ~a~a = ~a~n" path name (+ rsrc-pos (value data-delta)))
					      (let ([full-name (format "~a~a" path name)])
						(if (flag data-delta)
						    (loop (value data-delta) (string-append full-name "."))
						    ;; Found the icon?
						    (and (regexp-match re:rsrc full-name)
							 ;; Yes, so read IMAGE_RESOURCE_DATA_ENTRY
							 (begin
							   (file-position p (+ rsrc-pos (value data-delta)))
							   (cons
							    (+ (dword->integer p)           ; offset (an RVA)
							       (- rsrc-pos
								  rsrc-virtual-addr))
							    (dword->integer p)))))))        ; size
					    (begin
					      (file-position p next)
					      (iloop (add1 i))))))))))))))))))

  (define-struct icon (desc data) (make-inspector))
  (print-struct #t)

  (define (get-icons file res?)
    (let ([p (if (input-port? file)
		 file
		 (open-input-file file))])
      (dynamic-wind
       void
       (lambda ()
	 (unless (= 0 (word->integer p))
		 (error 'get-icons "~a doesn't start with 0" file))
	 (unless (= 1 (word->integer p))
		 (error "type isn't 1"))
	 (let ([cnt (word->integer p)])
	   (let ([icons (let loop ([i 0])
			  (if (= i cnt)
			      null
			      (cons
			       (make-icon
				(list (byte->integer p)   ; w
				      (byte->integer p)   ; h
				      (byte->integer p)   ; colors
				      (byte->integer p)   ; 0
				      (word->integer p)   ; planes
				      (word->integer p))  ; bitcount
				(list (dword->integer p)  ; bytes
				      ((if res?           ; where or icon id
					   word->integer 
					   dword->integer)
				       p)))
			       (loop (add1 i)))))])
	     ;; (printf "~a~n" icons)
	     (for-each (lambda (icon)
			 (set-icon-data!
			  icon
			  (let ([size (car (icon-data icon))]
				[where (cadr (icon-data icon))])
			    (let ([icon-pos (if res?
						;; last number is icon id:
						(car (find-rsrc-start p (format "^3[.]~a[.]" where)))
						;; last number is file position:
						where)])
			      (file-position p icon-pos)
			      (cons icon-pos
				    (read-string size p))))))
		       icons)
	     icons)))
       (lambda ()
	 (when (string? file)
	       (close-input-port p))))))

  (define (install-icon exe-file ico-file)
    (let ([ico-icons (get-icons ico-file #f)]
	  [exe-icons (let ([p (open-input-file exe-file)])
		       (dynamic-wind
			void
			(lambda ()
			  (let ([pos+size (find-rsrc-start p "^14[.]")])
			    (file-position p (car pos+size))
			    (get-icons p #t)))
			(lambda () (close-input-port p))))])
      (let ([p (open-output-file exe-file 'update)])
	(dynamic-wind
	 void
	 (lambda ()
	   (for-each (lambda (exe-icon)
		       (for-each (lambda (ico-icon)
				   (let ([le (icon-desc exe-icon)]
					 [li (icon-desc ico-icon)])
				     (when (and (= (car li) (car le))
						(= (cadr li) (cadr le))
						(= (caddr li) (caddr le))
						(= (string-length (cdr (icon-data exe-icon)))
						   (string-length (cdr (icon-data ico-icon)))))
					   (file-position p (car (icon-data exe-icon)))
					   (display (cdr (icon-data ico-icon)) p))))
				 ico-icons))
		     exe-icons))
	 (lambda () (close-output-port p)))))))

