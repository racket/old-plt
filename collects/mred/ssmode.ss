;; originally by Dan Grossman
;; 6/30/95

; Scheme mode for MrEd.

(define mred:scheme-mode@
  (unit/sig mred:scheme-mode^
    (import [mred:debug : mred:debug^]
	    [mred:preferences : mred:preferences^]
	    [mred:application : mred:application^]
	    [mred : mred:container^]
	    [mred:mode : mred:mode^]
	    [mred:match-cache : mred:match-cache^]
	    [mred:paren : mred:paren^] 
	    [mred:scheme-paren : mred:scheme-paren^]
	    [mred:icon : mred:icon^]
	    [mred:handler : mred:handler^]
	    [mred:keymap : mred:keymap^]
	    [mzlib:string : mzlib:string^])
	    
    (mred:debug:printf 'invoke "mred:scheme-mode@")

    (define newline-string (string #\newline))

    (define scheme-mode-tabify-on-return? #t)
    (define scheme-mode-match-round-to-square? #t)

    (define scheme-media-wordbreak-map
      (make-object wx:media-wordbreak-map%))

    (define scheme-init-wordbreak-map
      (lambda (map)
	(let ([v (send map get-map (char->integer #\-))])
	  (unless (zero? (bitwise-ior v wx:const-break-for-line))
	    (send map set-map 
		  (char->integer #\-)
		  (- v wx:const-break-for-line))))))
    (scheme-init-wordbreak-map scheme-media-wordbreak-map)

    (mred:preferences:set-preference-default 'mred:highlight-parens #t)

    (mred:preferences:add-preference-panel
     "Indenting"
     (lambda (p)
       (letrec* ([all-keywords (hash-table-map (mred:preferences:get-preference 'mred:tabify) list)]
		 [pick-out (lambda (wanted in out)
			     (cond
			       [(null? in) out]
			       [else (if (eq? wanted (cadr (car in))) 
					 (pick-out wanted (cdr in) (cons (symbol->string (car (car in))) out))
					 (pick-out wanted (cdr in) out))]))]
		 [begin-keywords (pick-out 'begin all-keywords null)]
		 [define-keywords (pick-out 'define all-keywords null)]
		 [lambda-keywords (pick-out 'lambda all-keywords null)]
		 [add-callback
		  (lambda (keyword-type keyword-symbol list-box)
		    (lambda (button command)
		      (let ([new-one (wx:get-text-from-user 
				      (string-append keyword-type " Keyword")
				      (string-append "Enter new " keyword-type "-like keyword:"))])
			(unless (null? new-one)
			  (let ([parsed (with-handlers ((exn:read? (lambda (x) #f)))
					  (read (open-input-string new-one)))])
			    (cond
			      [(and (symbol? parsed)
				    (hash-table-get (mred:preferences:get-preference 'mred:tabify)
							      parsed
							      (lambda () #f)))
			       (wx:message-box (format "\"~a\" is already a specially indented keyword" parsed))]
			      [(symbol? parsed)
			       (hash-table-put! (mred:preferences:get-preference 'mred:tabify)
						parsed keyword-symbol)
			       (send list-box append (symbol->string parsed))]
			      [else (wx:message-box (format "expected a symbol, found: ~a" new-one) "Error")]))))))]
		 [delete-callback
		  (lambda (list-box)
		    (lambda (button command)
		      (let* ([selections (box null)]
			     [_ (send list-box get-selections selections)]
			     [symbols (map (lambda (x) (string->symbol (send list-box get-string x))) (unbox selections))])
			(for-each (lambda (x) (send list-box delete x)) (reverse (unbox selections)))
			(let ([ht (mred:preferences:get-preference 'mred:tabify)])
			  (for-each (lambda (x) (hash-table-remove! ht x)) symbols)))))])
	 (mred:horizontal-panel p #t #t
	   (list 
	    (vertical-panel #t #t (let ([msg (message "Begin-like Keywords")]
					[box (list-box null "" wx:const-multiple -1 -1 -1 -1 begin-keywords)])
				    (list msg box
					  (button (add-callback "Begin" 'begin box) "Add")
					  (button (delete-callback box) "Delete"))))
	    (vertical-panel #t #t (let ([msg (message "Define-like Keywords")]
					[box (list-box null "" wx:const-multiple -1 -1 -1 -1 define-keywords)])
				    (list msg box
					  (button (add-callback "Define" 'define box) "Add")
					  (button (delete-callback box) "Delete"))))
	    (vertical-panel #t #t (let ([msg (message "Lambda-like Keywords")]
					[box (list-box null "" wx:const-multiple -1 -1 -1 -1 lambda-keywords)])
				    (list msg box
					  (button (add-callback "Lambda" 'lambda box) "Add")
					  (button (delete-callback box) "Delete")))))))))

    (define make-scheme-mode% 
      (lambda (super%)
	(class-asi super%
	  (inherit keymap)
	  (rename [super-on-char on-char]
		  [super-install install])
	  (public
	    [indents (void)])
	  (sequence
	    (let ([hash-table (make-hash-table)])
	      (for-each (lambda (x) (hash-table-put! hash-table x 'define))
			'(define defmacro define-macro
			   define-signature define-syntax define-schema))
	      (for-each (lambda (x) (hash-table-put! hash-table x 'begin))
			'(cond begin begin0 delay
			       public private
			       inherit inherit-from
			       rename rename-from
			       share share-from
			       sequence))
	      (for-each (lambda (x) (hash-table-put! hash-table x 'lambda))
			'(lambda let let* letrec letrec* recur let-values
			   mred:vertical-panel mred:horizontal-panel mred:panel
			   let/cc let/ec letcc catch
			   let-syntax letrec-syntax syntax-case
			   let-struct let-macro
			   case when unless match
			   let-enumerate
			   class class* class-asi class-asi* define-some
			   do opt-lambda
			   local
			   unit unit/sig compound-unit/sig 
			   with-handlers
			   call-with-input-file with-input-from-file
			   with-input-from-port call-with-output-file
			   with-output-to-file with-output-to-port))
	      (mred:preferences:set-preference-default 'mred:tabify hash-table)
	      (mred:preferences:set-preference-un/marshall
		'mred:tabify 
	       (lambda (t) (hash-table-map t list))
	       (lambda (l) (let ([h (make-hash-table)])
			     (for-each (lambda (x) (apply hash-table-put! h x))
				       l)
			     h)))
	      (set! indents (mred:preferences:get-preference-box 'mred:tabify))))
	  (public
	    [name "Scheme"]
	    [backward-cache (make-object mred:match-cache:match-cache%)]
	    [forward-cache (make-object mred:match-cache:match-cache%)])
	  
	  (private
	    [just-once #f]
	    [suspend-highlight? #f])
	  (public
	    [on-focus
	     (lambda (edit on?)
	       (highlight-parens edit (not on?)))]
	    [on-change-style
	     (lambda (edit start len)
	       (send edit begin-edit-sequence)
	       #t)]
	    [after-change-style
	     (lambda (edit start len)
	       (send edit end-edit-sequence)
	       (highlight-parens edit))]
	    [on-edit-sequence
	     (lambda (edit)
	       (set! suspend-highlight? #t))]
	    [after-edit-sequence
	     (lambda (edit)
	       (set! suspend-highlight? #f)
	       (when just-once
		 (highlight-parens edit)))]
	    [on-insert
	     (lambda (edit start size)
	       (send edit begin-edit-sequence)
	       #t)]
	    [after-insert
	     (lambda (edit start size)
	       (send backward-cache invalidate start)
	       (send forward-cache forward-invalidate start size)
	       (send edit end-edit-sequence)
	       (highlight-parens edit)
	       #t)]
	    [on-delete
	     (lambda (edit start size)
	       (send backward-cache invalidate start)
	       (send forward-cache forward-invalidate (+ start size) (- size))
	       (send edit begin-edit-sequence)
	       #t)]
	    [after-delete
	     (lambda (edit start size)
	       (send edit end-edit-sequence)
	       (highlight-parens edit)
	       #t)]
	    [on-set-size-constraint
	     (lambda (edit)
	       (send edit begin-edit-sequence)
	       #t)]
	    [after-set-size-constraint
	     (lambda (edit)
	       (send edit end-edit-sequence)
	       (highlight-parens edit))]
	    [after-set-position 
	     (lambda (edit)
	       (highlight-parens edit))]
	    
	    [highlight-parens?-box (mred:preferences:get-preference-box 'mred:highlight-parens)]
	    
	    [highlight-parens
	     (let ([clear-old-location (lambda () (void))]
		   [color (apply make-object wx:colour% 
				 (map (lambda (x) (* (/ x 65535) 255))
				      (begin (list 65535 21437 18932)
					     (list 48896 48896 48896))))])
	       (opt-lambda (edit [just-clear? #f])
		 (if (or (not (unbox highlight-parens?-box))
			 suspend-highlight?)
		     (set! just-once #t)
		     (begin 
		       (set! just-once #f)
		       (dynamic-wind
			(lambda () (send edit begin-edit-sequence))
			(lambda ()
			  (clear-old-location)
			  (set! clear-old-location (lambda () (void)))
			  (unless just-clear?
			    (let ([here (send edit get-start-position)]
				  [there (send edit get-end-position)]
				  [is-paren?
				   (lambda (f char)
				     (ormap (lambda (x) (char=? char (string-ref (f x) 0)))
					    mred:scheme-paren:scheme-paren-pairs))])
			      (when (= here there)
				(let/ec k
				  (let-values ([(left right)
						(cond
						  [(is-paren? cdr (send edit get-character (sub1 here))) 
						   (let ([end-pos (mred:scheme-paren:scheme-backward-match
								   edit here (get-limit edit here)
								   backward-cache)])
						     (if end-pos
							 (values end-pos here)
							 (k (void))))]
						  [(is-paren? car (send edit get-character here))
						   (let ([end-pos (mred:scheme-paren:scheme-forward-match
								   edit here (send edit last-position)
								   forward-cache)])
						     (if end-pos
							 (values here end-pos)
							 (k (void))))]
						  [else (k (void))])])
				    (set! clear-old-location
					    (send edit highlight-range left right 
						  color
						  mred:icon:paren-highlight-bitmap))))))))
			(lambda () (send edit end-edit-sequence)))))))]
	    
	    [get-limit
	     (lambda (edit pos)
	       0)]
	    
	    [balance-parens
	     ;paren balancing keymap callback
	     (lambda (edit key)
	       (let* ([code (send key get-key-code)]
		      [here (send edit get-start-position)]
		      [limit (get-limit edit here)])
		 (send edit insert (integer->char code))
		 (let ([pos (mred:scheme-paren:scheme-backward-match 
			     edit (add1 here) limit backward-cache)])
		   (if (and pos
			    (or (not (= 34 code))
				(> 5 
				   (abs (- (send edit position-paragraph 
						 here)
					   (send edit position-paragraph 
						 pos))))))
		       (unless (= (add1 here) pos)
			 (send edit flash-on pos (add1 pos)))
		       (when (and (= 41 code) (match-round-to-square?))
			 (send* edit 
				(begin-edit-sequence)
				(insert #\] here (add1 here)));ensure right selection
			 (let ([round-pos (mred:scheme-paren:scheme-backward-match
					   edit (add1 here)
					   limit backward-cache)])
			   (if round-pos
			       (send* edit (end-edit-sequence)
				      (flash-on round-pos
						(add1 round-pos)))
			       (begin
				 (send* edit
					(insert #\) here (add1 here)) 
					(end-edit-sequence))
				 (wx:bell))))))))
	       #t)]
	    [tabify-on-return?
	     (lambda ()
	       scheme-mode-tabify-on-return?)]
	    [match-round-to-square?
	     (lambda ()
	       scheme-mode-match-round-to-square?)]
	    [tabify    
	     (opt-lambda (edit [pos (send edit get-start-position)])
	       (let* ([last-pos (send edit last-position)]
		      [para (send edit position-paragraph pos)]
		      [okay (> para 0)]
		      [end (if okay (send edit paragraph-start-position para) 0)]
		      [limit (get-limit edit pos)]
		      [contains 
		       (if okay
			   (begin
			     (send backward-cache invalidate end)
			     (mred:scheme-paren:scheme-backward-containing-sexp 
			      edit end limit backward-cache))
			   #f)]
		      [contain-para (if contains
					(send edit position-paragraph contains))]
		      [last 
		       (if contains
			   (mred:scheme-paren:scheme-backward-match 
			    edit end limit backward-cache) 
			   #f)]
		      [last-para (if last 
				     (send edit position-paragraph last))])
		 (letrec	
		     ([find-offset
		       (lambda (edit pos)
			 (let loop ([p pos][o 0])
			   (let ([c (send edit get-character p)])
			     (cond
			       [(char=? c #\tab)
				(loop (add1 p) (+ o (- 8 (modulo o 8))))]
			       [(char=? c #\newline)
				(cons o p)]
			       [(char-whitespace? c)
				(loop (add1 p) (add1 o))]
			       [else
				(cons o p)]))))]
		      [visual-offset
		       (lambda (edit pos)
			 (let loop ([p (sub1 pos)])
			   (let ([c (send edit get-character p)])
			     (cond
			       [(= p -1) 0]
			       [(char=? c #\null) 0]
			       [(char=? c #\tab)
				(let ([o (loop (sub1 p))])
				  (+ o (- 8 (modulo o 8))))]
			       [(char=? c #\newline) 0]
			       [else (add1 (loop (sub1 p)))]))))]
		      [do-indent
		       (lambda (amt)
			 (let* ([pos-start end]
				[curr-offset (find-offset edit pos-start)])
			   (if (not (= amt (car curr-offset)))
			       (begin
				 (send edit delete pos-start (cdr curr-offset))
				 (send edit insert
				       (string-append
					(make-string (quotient amt tab-size) #\tab)
					(make-string (remainder amt tab-size)
						     #\space))
				       pos-start)))))]
		      [id-walker
		       (lambda (str-list)
			 (if (null? str-list) 0
			     (let ([current (car str-list)])
			       (if (or (char-alphabetic? current)
				       (char-numeric? current))
				   (add1 (id-walker (cdr str-list)))
				   (case current
				     ((#\+ #\- #\. #\* #\/ #\< #\= #\> #\! #\? #\:
				       #\$ #\% #\_ #\& #\^ #\~)
				      (add1 (id-walker (cdr str-list))))
				     (else 0))))))]
		      [get-proc
		       (lambda ()
			 (let* ([text (send edit get-text contains
					    (send edit paragraph-end-position 
						  contain-para))])
			   (substring text 0 (id-walker (string->list text)))))]
		      [procedure-indent
		       (lambda ()
			 (let* ([proc-name (get-proc)])
			   (case (hash-table-get (unbox indents)
						 (string->symbol proc-name)
						 (lambda () 'other))
			     [(define) 1]
			     [(begin) 1]
			     [(lambda) 3]
			     [else 0])))]
		      [special-check
		       (lambda ()
			 (let* ([proc-name (get-proc)]
				[which (hash-table-get (unbox indents)
						       (string->symbol proc-name)
						       (lambda () 'other))])
			   (or (eq? which 'define)
			       (eq? which 'lambda))))]
		      [indent-first-arg
		       (lambda (start)
			 (car (find-offset edit start)))])
		   (if (and okay
			    (not (char=? (send edit get-character (sub1 end))
					 #\newline)))
		       (send edit insert (string #\newline) 
			     (send edit paragraph-start-position para)))
		   (cond   
		     [(let ([real-start (cdr (find-offset edit end))]) 
			(if
			 (and(<= (+ 3 real-start) (send edit last-position))
			     (string=? ";;;"
				       (send edit get-text real-start (+ 3 real-start))))
			 real-start
			 #f))
		      => (lambda (x) (send edit set-position x))]
		     [(= para 0)(do-indent 0)]
		     [(or (not contains)(= contains -1))
		      (do-indent 0)]
		     [(= contains last)
		      (do-indent (+ (visual-offset edit contains)
				    (procedure-indent)))]
		     [(special-check)
		      (do-indent (add1 (visual-offset edit contains)))]
		     [(= contain-para last-para)
		      (let ([name-length 
			     (id-walker (string->list 
					 (send edit get-text contains
					       (send edit paragraph-end-position
						     contain-para))))])
			(do-indent (+ (visual-offset edit contains)
				      name-length
				      (indent-first-arg (+ contains 
							   name-length)))))]
		     [else
		      (do-indent (indent-first-arg 
				  (send edit paragraph-start-position
					last-para)))]))))]
	    [tabify-selection
	     (opt-lambda (edit 
			  [start-pos (send edit get-start-position)]
			  [end-pos (send edit get-end-position)])
	       (let ([first-para (send edit position-paragraph start-pos)]
		     [end-para (send edit position-paragraph end-pos)])
		 (dynamic-wind
		  (lambda () 
		    (if (< first-para end-para)
			(wx:begin-busy-cursor))
		    (send edit begin-edit-sequence))
		  (lambda ()
		    (let loop ([para first-para])
		      (when (<= para end-para)
			(tabify edit (send edit paragraph-start-position para))
			(loop (add1 para))))
		    (if (and (>= (send edit position-paragraph start-pos) end-para)
			     (<= (mred:paren:skip-whitespace 
				  edit (send edit get-start-position) -1)
				 (send edit paragraph-start-position first-para)))
			(send edit set-position 
			      (let loop ([new-pos (send edit get-start-position)])
				(if (let ([next (send edit get-character new-pos)])
				      (and (char-whitespace? next)
					   (not (char=? next #\newline))))
				    (loop (add1 new-pos))
				    new-pos)))))
		  (lambda ()
		    (send edit end-edit-sequence)
		    (if (< first-para end-para)
			(wx:end-busy-cursor))))))]
	    [tabify-all
	     (lambda (edit)
	       (tabify-selection edit 0 (send edit last-position)))]
	    [insert-return
	     (lambda (edit)
	       (if (tabify-on-return?)
		   (begin 
		     (send edit begin-edit-sequence)
		     (send edit insert newline-string)
		     (tabify edit (send edit get-start-position))
		     (send edit set-position 
			   (let loop ([new-pos (send edit get-start-position)])
			     (if (let ([next (send edit get-character new-pos)])
				   (and (char-whitespace? next)
					(not (char=? next #\newline))))
				 (loop (add1 new-pos))
				 new-pos)))
		     (send edit end-edit-sequence))
		   (send edit insert newline-string)))]
	    [comment-out-selection
	     (opt-lambda (edit 
			  [start-pos (send edit get-start-position)]
			  [end-pos (send edit get-end-position)])
	       (send edit begin-edit-sequence)
	       (let* ([first-para (send edit position-paragraph start-pos)]
		      [last-para (send edit position-paragraph end-pos)])
		 (let para-loop ([curr-para first-para])
		   (if (<= curr-para last-para)
		       (let ([first-on-para (send edit paragraph-start-position 
						  curr-para)])
			 (if (not
			      (char=? #\; (send edit get-character first-on-para)))
			     (send edit insert ";" first-on-para))
			 (para-loop (add1 curr-para))))))
	       (send edit end-edit-sequence)
	       #t)]
	    [uncomment-selection
	     (opt-lambda (edit
			  [start-pos (send edit get-start-position)]
			  [end-pos (send edit get-end-position)])
	       (send edit begin-edit-sequence)
	       (let* ([first-para (send edit position-paragraph start-pos)]
		      [last-para (send edit position-paragraph end-pos)])
		 (let para-loop ([curr-para first-para])
		   (if (<= curr-para last-para)
		       (let ([first-on-para 
			      (mred:paren:skip-whitespace 
			       edit 
			       (send edit paragraph-start-position curr-para)
			       1)])
			 (send edit delete first-on-para
			       (+ first-on-para
				  (let char-loop ([n 0])
				    (if (char=? #\; 
						(send edit get-character 
						      (+  n first-on-para)))
					(char-loop (add1 n))
					n))))
			 (para-loop (add1 curr-para))))))
	       (send edit end-edit-sequence)
	       #t)]
	    [get-forward-sexp
	     (lambda (edit start-pos)
	       (mred:scheme-paren:scheme-forward-match 
		edit start-pos
		(send edit last-position)
		forward-cache))]
	    [remove-sexp
	     (lambda (edit start-pos)
	       (let ([end-pos (get-forward-sexp edit start-pos)])
		 (if end-pos 
		     (send edit kill 0 start-pos end-pos)
		     (wx:bell))
		 #t))]
	    [forward-sexp
	     (opt-lambda (edit start-pos (move? #t))
	       (let ([end-pos (get-forward-sexp edit start-pos)])
		 (if end-pos 
		     (if move?
			 (send edit set-position end-pos)
			 (send edit flash-on end-pos (add1 end-pos)))
		     (wx:bell)) 
		 #t))]
	    [select-forward-sexp
	     (lambda (edit start-pos)
	       (let ([end-pos (get-forward-sexp edit start-pos)])
		 (if end-pos 
		     (send edit set-position start-pos end-pos)
		     (wx:bell)) 
		 #t))]
	    [get-backward-sexp
	     (lambda (edit start-pos)
	       (let ([end-pos 
		      (mred:scheme-paren:scheme-backward-match 
		       edit start-pos 
		       (get-limit edit start-pos) 
		       backward-cache)]
		     [min-pos
		      (mred:scheme-paren:scheme-backward-containing-sexp 
		       edit start-pos
		       (get-limit edit start-pos) 
		       backward-cache)])
		 (if (and end-pos 
			  (or (not min-pos)
			      (>= end-pos min-pos)))
		     end-pos
		     #f)))]
	    [backward-sexp
	     (lambda (edit start-pos move?)
	       (let ([end-pos (get-backward-sexp edit start-pos)])
		 (if end-pos
		     (if move? 
			 (send edit set-position end-pos) 
			 (send edit flash-on end-pos (add1 end-pos)))
		     (wx:bell))
		 #t))] 
	    [up-sexp
	     (lambda (edit start-pos)
	       (let ([exp-pos
		      (mred:scheme-paren:scheme-backward-containing-sexp 
		       edit start-pos
		       (get-limit edit start-pos) 
		       backward-cache)])
		 (if (and exp-pos (> exp-pos 0))
		     (send edit set-position (sub1 exp-pos))
		     (wx:bell))
		 #t))]
	    [down-sexp
	     (lambda (edit start-pos)
	       (let ([last (send edit last-position)])
		 (let loop ([pos start-pos])
		   (let ([next-pos (mred:scheme-paren:scheme-forward-match 
				    edit pos last
				    forward-cache)])
		     (if (and next-pos (> next-pos pos))
			 (let ([back-pos
				(mred:scheme-paren:scheme-backward-containing-sexp 
				 edit (sub1 next-pos) pos backward-cache)])
			   (if (and back-pos
				    (> back-pos pos))
			       (send edit set-position back-pos)
			       (loop next-pos)))
			 (wx:bell)))))
	       #t)]
	    
	    [remove-parens-forward
	     (lambda (edit start-pos)
	       (let* ([pos (mred:paren:skip-whitespace edit start-pos 1)]
		      [first-char (send edit get-character pos)]
		      [paren? (or (char=? first-char #\( )
				  (char=? first-char #\[ ))]
		      [closer (if paren? 
				  (mred:scheme-paren:scheme-forward-match 
				   edit pos (send edit last-position)
				   forward-cache))])
		 (if (and paren? closer)
		     (send* edit (begin-edit-sequence)
			    (delete pos (add1 pos))
			    (delete (-  closer 2) (- closer 1))
			    (end-edit-sequence))
		     (wx:bell))
		 #t))]
	    [standard-style-delta
	     (let ([delta (make-object wx:style-delta% wx:const-change-normal)])
	       (send delta set-delta wx:const-change-family wx:const-modern)
	       (when (eq? wx:platform 'macintosh)
		     (wx:bell)
		     (send delta set-delta wx:const-change-size 9))
	       delta)]
	    [file-format wx:const-media-ff-text]
	    [install
	     (lambda (edit)
	       (send edit set-wordbreak-map scheme-media-wordbreak-map)
	       (send edit set-tabs '() 8 #f)
	       (send edit set-autowrap-bitmap mred:icon:autowrap-bitmap)
	       (set! tab-size 8)
	       (super-install edit))]
	    [evaluate-region
	     (lambda (edit start end)
	       (mred:application:eval-string (send edit get-text start end))
	       #t)]
	    
	    [transpose-sexp
	     (lambda (edit pos)
	       (let ([start-1 (get-backward-sexp edit pos)])
		 (if (not start-1)
		     (wx:bell)
		     (let ([end-1 (get-forward-sexp edit start-1)])
		       (if (not end-1)
			   (wx:bell)
			   (let ([end-2 (get-forward-sexp edit end-1)])
			     (if (not end-2)
				 (wx:bell)
				 (let ([start-2 (get-backward-sexp edit end-2)])
				   (if (or (not start-2)
					   (< start-2 end-1))
				       (wx:bell)
				       (let ([text-1 
					      (send edit get-text start-1 end-1)]
					     [text-2 
					      (send edit get-text start-2 end-2)])
					 (send* edit
						(begin-edit-sequence)
						(insert text-1 start-2 end-2)
						(insert text-2 start-1 end-1)
						(set-position end-2)
						(end-edit-sequence))))))))))))]
	    tab-size
	    [make-keymap
	     (lambda ()
	       global-scheme-mode-keymap)]))))

    (define scheme-mode% (make-scheme-mode% mred:mode:mode%))

    (define setup-global-scheme-mode-keymap
      (lambda (keymap)
	(let ([get-mode
	       (lambda (edit)
		 (ivar edit mode))])
	  
	  (mred:keymap:set-keymap-error-handler keymap)
	  (mred:keymap:set-keymap-implied-shifts keymap)

	  (send keymap add-key-function "tabify-at-caret"
		(lambda (edit event)
		  (send (get-mode edit) tabify-selection edit)))
	  
	  (send keymap add-key-function "do-return"
		(lambda (edit event)
		  (send (get-mode edit) insert-return edit)))

	  (send keymap add-key-function "balance-parens"
		(lambda (edit event)
		  (send (get-mode edit) balance-parens edit event)))

	  (send keymap add-key-function "comment-out"
		(lambda (edit event)
		  (send (get-mode edit) comment-out-selection edit)))
	  
	  (send keymap add-key-function "uncomment"
		(lambda (edit event)
		  (send (get-mode edit) uncomment-selection edit)))

	  (send keymap add-key-function "remove-sexp"
		(lambda (edit event)
		  (send (get-mode edit) remove-sexp 
			edit 
			(send edit get-start-position))))

	  (send keymap add-key-function "forward-sexp"
		(lambda (edit event)
		  (send (get-mode edit) forward-sexp 
			edit 
			(send edit get-start-position))))

	  (send keymap add-key-function "backward-sexp"
		(lambda (edit event)
		  (send (get-mode edit) backward-sexp 
			edit 
			(send edit get-start-position) #t)))

	  (send keymap add-key-function "up-sexp"
		(lambda (edit event)
		  (send (get-mode edit) up-sexp 
			edit 
			(send edit get-start-position))))

	  (send keymap add-key-function "down-sexp"
		(lambda (edit event)
		  (send (get-mode edit) down-sexp 
			edit 
			(send edit get-start-position))))

	  (send keymap add-key-function "flash-backward-sexp"
		(lambda (edit event)
		  (send (get-mode edit) backward-sexp 
			edit 
			(send edit get-start-position) #f)))
	  
	  (send keymap add-key-function "flash-forward-sexp"
		(lambda (edit event)
		  (send (get-mode edit) forward-sexp 
			edit 
			(send edit get-start-position) #f)))

	  (send keymap add-key-function "remove-parens-forward"
		(lambda (edit event)
		  (send (get-mode edit) remove-parens-forward 
			edit 
			(send edit get-start-position))))

	  (send keymap add-key-function "select-forward-sexp"
		(lambda (edit event)
		  (send (get-mode edit) select-forward-sexp 
			edit 
			(send edit get-start-position))))

	  (send keymap add-key-function "transpose-sexp"
		(lambda (edit event)
		  (send (get-mode edit) transpose-sexp 
			edit 
			(send edit get-start-position))))

	  (send keymap add-key-function "evaluate-buffer"
		(lambda (edit event)
		  (send (get-mode edit) evaluate-region 
			edit 0 (send edit last-position))))

	  (send keymap add-key-function "evaluate-region"
		(lambda (edit event)
		  (send (get-mode edit) evaluate-region 
			edit 
			(send edit get-start-position)
			(send edit get-end-position))))

	  (send keymap map-function "TAB" "tabify-at-caret")
	  (send keymap map-function "return" "do-return")
	  (send keymap map-function "c:c;c:r" "evaluate-region")
	  (send keymap map-function ")" "balance-parens")
	  (send keymap map-function "]" "balance-parens")
	  (send keymap map-function "}" "balance-parens")
	  (send keymap map-function "\"" "balance-parens")
	  (let ([map-meta
		 (lambda (key func)
		   (mred:keymap:send-map-function-meta keymap key func))])
	    (map-meta "c:semicolon" "comment-out")
	    (map-meta "c:=" "uncomment")
	    (map-meta "c:k" "remove-sexp")
	    (map-meta "c:f" "forward-sexp")
	    (map-meta "c:b" "backward-sexp")
	    (map-meta "c:u" "up-sexp")
	    (map-meta "c:d" "down-sexp")
	    (map-meta "c:p" "flash-backward-sexp")
	    (map-meta "c:n" "flash-forward-sexp")
	    (map-meta "c:space" "select-forward-sexp")
	    (map-meta "c:t" "transpose-sexp")
	    (map-meta "c:l" "evaluate-buffer"))
	  (send keymap map-function "c:c;c:b" "remove-parens-forward"))))

    (define global-scheme-mode-keymap (make-object wx:keymap%))
    (setup-global-scheme-mode-keymap global-scheme-mode-keymap)

    (mred:handler:insert-mode-handler "Scheme" '("ss" "scm" "sch" "mredrc")
				       (lambda args
					 (make-object scheme-mode%)))

    (define make-scheme-interaction-mode%
      (lambda (super%)
	(class-asi super%
		   (inherit keymap)
		   (public
		    [name "Scheme Interaction"]
		    [get-limit
		     (lambda (edit pos)
		       (send edit find-prompt pos))]
		    [make-keymap
		     (lambda ()
		       global-scheme-interaction-mode-keymap)]))))

    (define scheme-interaction-mode%
      (make-scheme-interaction-mode% scheme-mode%))

    (define setup-global-scheme-interaction-mode-keymap
      (lambda (keymap)
	(mred:keymap:set-keymap-error-handler keymap)
	(mred:keymap:set-keymap-implied-shifts keymap)
	
	(send keymap chain-to-keymap global-scheme-mode-keymap #f)
	
	(send keymap add-key-function "put-previous-sexp"
	      (lambda (edit event) 
		(send edit copy-prev-previous-expr)))
	(send keymap add-key-function "put-next-sexp"
	      (lambda (edit event) 
		(send edit copy-next-previous-expr)))
	
	(mred:keymap:send-map-function-meta keymap "p" "put-previous-sexp")
	(mred:keymap:send-map-function-meta keymap "n" "put-next-sexp")))

    (define global-scheme-interaction-mode-keymap (make-object wx:keymap%))
    (setup-global-scheme-interaction-mode-keymap global-scheme-interaction-mode-keymap)
    
    (mred:handler:insert-mode-handler "Scheme Interpreter" ()
				       (lambda args
					 (make-object scheme-interaction-mode%)))))

