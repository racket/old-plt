;;; Kurtis McCathern
;;; 10-10-95
;;; C++ Mode for Mr.Ed.

(define mred:c++-mode%
  (class mred:mode% ()
   (inherit keymap)
   (public
    [number-of-open-braces 0])
   (private
    [paren-cache (make-object mred:match-cache%)])
   (public
    [after-insert
     (lambda (edit start size)
       (send paren-cache invalidate start)
       #t)]
    [on-delete
     (lambda (edit start size)
       (send paren-cache invalidate start)
       #t)]
    [get-limit
     (lambda (edit)
       0)]
    [on-char
     ;paren balancing
     (lambda (edit key)
       (let ([limit (get-limit edit)])
	 (if (not (send keymap handle-key-event edit key))
	     (begin
	       ((ivar super on-char) edit key)
	       (let ([code (send key get-key-code)])
		 ; close paren is ASCII 41
		 ; close square bracket is ASCII 93
		 ; close brace in ASCII 125
		 (if (or (= 41 code)
			 (= 93 code)
			 (= 125 code)
			 (= 123 code))
		     (let ([here (send edit get-start-position)])
		       (send edit insert (integer->char code))
		       (let ([pos (mred:c++-backward-match 
				   edit (add1 here) limit paren-cache)])
			 (if (= 123 code)
			     (set! number-of-open-braces
				   (add1 number-of-open-braces)))
			 (if (= 125 code)
			     (set! number-of-open-braces
				   (sub1 number-of-open-braces)))
			 (if pos
			     (if (not (= (add1 here) pos))
				 (begin
				   (tabify edit here)
				   (send edit flash-on pos (add1 pos))))
			     (wx:bell)))
		       #t)
		     #f)))	
	     #t)))]
    [tabify-on-return  #t]
    [set-tab-on-return
     (lambda (val)
       (set! tabify-on-return val))]
    [tabify
     (lambda (edit pos)
       (let* ([last-pos (send edit last-position)]
	      [line (send edit position-line pos)]
	      [okay (> line 0)]
	      [end (if okay (send edit line-start-position line) 0)]
	      [limit (get-limit edit)]
	      [contains (if okay
			    (begin
			      (send paren-cache invalidate end)
			      (mred:c++-backward-containing-exp
			       edit end limit paren-cache))
			    #f)]
	      [contain-line (if contains (send edit position-line
					       contains))]
	      [last (if contains 
			(mred:c++-backward-match edit end limit
						 paren-cache)
			#f)]
	      [last-line (if last(send edit position-line last))])
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
		 (let*([pos-start end]
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
	      [procedure-indent
	       (lambda ()
		 (if (char=? (send edit get-character pos) #\})
		     -1
		     (let ([endpl (send edit get-character
					(- (send edit
						 line-start-position
						 line) 2))])
		       (if (and (not (char=? endpl #\;))
				(not (char=? endpl #\{)))
			   4
			   2))))])
	   (if (and okay
		    (not (char=? (send edit get-character (sub1 end))
				 #\newline)))
	       (send edit insert (string #\newline) 
		     (send edit line-start-position line)))
	   (cond   
	    [(let ([real-start (cdr (find-offset edit end))]) 
	       (if
		(and (<= (+ 3 real-start) (send edit last-position))
		     (string=? ";;;"
			       (send edit get-text real-start
				     (+ 3 real-start))))
		real-start
		#f))
	     (lambda (x) (send edit set-position x))]
	    [(= line 0)(do-indent 0)]
	    [(or (not contains)(= contains -1))
	     (do-indent 0)]
	    [else
	     (do-indent (+ (visual-offset edit contains)
			   (procedure-indent)))]))))]
    [tabify-selection
     (lambda (edit start-pos end-pos)
       (let ([first-line (send edit position-line start-pos)]
	     [end-line (send edit position-line end-pos)])
	 (dynamic-wind
	  (lambda () 
	    (if (< first-line end-line)
		(wx:begin-busy-cursor))
	    (send edit begin-edit-sequence))
	  (lambda ()
	    (do ([line first-line (add1 line)])
		((> line end-line))
	      (tabify edit (send edit line-start-position line)))
	    (if (and (>= (send edit position-line start-pos) end-line)
		     (<= (mred:skip-whitespace 
			  edit (send edit get-start-position) -1)
			 (send edit line-start-position first-line)))
		(send edit set-position 
		      (let loop ([new-pos (send edit get-start-position)])
			(if (let ([next (send edit get-character new-pos)])
			      (and (char-whitespace? next)
				   (not (char=? next #\newline))))
			    (loop (add1 new-pos))
			    new-pos)))))
	  (lambda ()
	    (send edit end-edit-sequence)(wx:end-busy-cursor)))))]
    [tabify-all
     (lambda (edit)
       (tabify-selection edit 0 (send edit last-position)))]
    [standard-style-delta
     (send (make-object wx:style-delta%
			wx:const-change-normal)
	   set-delta
	   wx:const-change-family
	   wx:const-modern)]
    [file-format wx:const-media-ff-text]
    [install
     (lambda (edit)
       (send edit set-tabs '() 8 #f)
       (set! tab-size 8)
       ((ivar super install) edit))]
    tab-size)
   (sequence
     (super-init)
     (send keymap add-key-function "tabify-at-caret"
	   (lambda (edit event)
	     (tabify-selection edit (send edit get-start-position)
			       (send edit get-end-position))))
     (send keymap add-key-function "do-return"
	   (lambda (edit event)
	     (if tabify-on-return
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
		 (send edit insert newline-string))))
     (send keymap map-function "TAB" "tabify-at-caret")
     (send keymap map-function "return" "do-return"))))

'(mred:insert-mode-handler "C/C++" '("cc" "c" "h" "cpp")
			   (lambda args
			     (make-object mred:c++-mode%)))
