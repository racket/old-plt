
(module keyfunc mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "mred-sig.ss" "mred"))

  (provide keyfunc@)
  (define keyfunc@
    (unit/sig (install-text-functions)
      (import mred^)
      
      (define install-text-functions
	;; Define some useful keyboard functions
	(let* ([ring-bell
		(lambda (edit event)
		  (bell))]
	       
	       [toggle-anchor
		(lambda (edit event)
		  (send edit set-anchor
			(not (send edit get-anchor))))]
	       [center-view-on-line
		(lambda (edit event)
		  (let ([new-mid-line (send edit position-line
					    (send edit get-start-position))]
			[bt (box 0)]
			[bb (box 0)])
		    (send edit get-visible-line-range bt bb)
		    (let* ([half (sub1 (quotient (- (unbox bb) (unbox bt)) 2))]
			   [top-pos (send edit line-start-position 
					  (max (- new-mid-line half) 0))]
			   [bottom-pos (send edit line-start-position 
					     (min (+ new-mid-line half)
						  (send edit position-line 
							(send edit last-position))))])
		      (send edit scroll-to-position 
			    top-pos
			    #f
			    bottom-pos)))
		  #t)]
	       [collapse-variable-space
		(lambda (leave-one? edit event)
		  (letrec ([end-pos (send edit last-position)]
			   [find-nonwhite
			    (lambda (pos d)
			      (let ([c (send edit get-character pos)])
				(cond
				 [(char=? #\newline c) pos]
				 [(or (and (< pos 0) (= d -1))
				      (and (> pos end-pos) (= d 1)))
				  (if (= d -1)
				      -1
				      end-pos)]
				 [(char-whitespace? c) 
				  (find-nonwhite (+ pos d) d)]
				 [else pos])))])
		    (unless (zero? end-pos)
		      (let ([sel-start (send edit get-start-position)]
			    [sel-end (send edit get-end-position)])
			(when (= sel-start sel-end)
			  (let ([start (+ (find-nonwhite (- sel-start 1) -1)
					  (if leave-one? 2 1))]
				[end (find-nonwhite sel-start 1)])
			    (if (< start end)
				(begin
				  (send edit begin-edit-sequence)
				  (send edit delete start end)
				  (if (and leave-one?
					   (not (char=? #\space
							(send edit get-character
							      (sub1 start)))))
				      (send edit insert " " (sub1 start) start))
				  (send edit set-position start)
				  (send edit end-edit-sequence))
				(when leave-one?
				  (let ([at-start
					 (send edit get-character sel-start)]
					[after-start
					 (send edit get-character 
					       (sub1 sel-start))])
				    (cond
				     [(char-whitespace? at-start)
				      (if (not (char=? at-start #\space))
					  (send edit insert " " sel-start 
						(add1 sel-start)))
				      (send edit set-position (add1 sel-start))]
				     [(char-whitespace? after-start)
				      (if (not (char=? after-start #\space))
					  (send edit insert " " (sub1 sel-start)
						sel-start))]
				     [else
				      (send edit insert " ")]))))))))))]
	       
	       [collapse-space
		(lambda (edit event)
		  (collapse-variable-space #t edit event))]
	       
	       [remove-space
		(lambda (edit event)
		  (collapse-variable-space #f edit event))]
	       
	       [collapse-newline
		(lambda (edit event)
		  (letrec ([find-nonwhite
			    (lambda (pos d offset)
			      (call/ec
			       (lambda (escape)
				 (let ([max (if (> offset 0)
						(send edit last-position)
						-1)])
				   (let loop ([pos pos])
				     (if (= pos max)
					 (escape pos)
					 (let ([c (send edit get-character 
							(+ pos offset))])
					   (cond
					    [(char=? #\newline c)
					     (loop (+ pos d))
					     (escape pos)]
					    [(char-whitespace? c) 
					     (loop (+ pos d))]
					    [else pos]))))))))])
		    (let ([sel-start (send edit get-start-position)]
			  [sel-end (send edit get-end-position)])
		      (if (= sel-start sel-end)
			  (let* ([pos-line
				  (send edit position-line sel-start #f)]
				 [pos-line-start
				  (send edit line-start-position pos-line)]
				 [pos-line-end
				  (send edit line-end-position pos-line)]
				 
				 [whiteline?
				  (let loop ([pos pos-line-start])
				    (if (>= pos pos-line-end)
					#t
					(and (char-whitespace?
					      (send edit get-character pos))
					     (loop (add1 pos)))))]
				 
				 [start (find-nonwhite pos-line-start -1 -1)]
				 [end (find-nonwhite pos-line-end 1 0)]
				 
				 [start-line 
				  (send edit position-line start #f)]
				 [start-line-start
				  (send edit line-start-position start-line)]
				 [end-line
				  (send edit position-line end #f)]
				 [end-line-start
				  (send edit line-start-position (add1 end-line))])
			    (cond
			     [(and whiteline?
				   (= start-line pos-line)
				   (= end-line pos-line))
					; Special case: just delete this line
			      (send edit delete pos-line-start (add1 pos-line-end))]
			     [(and whiteline? (< start-line pos-line))
					; Can delete before & after
			      (send edit begin-edit-sequence)
			      (send edit delete (add1 pos-line-end) end-line-start)
			      (send edit delete start-line-start pos-line-start)
			      (send edit end-edit-sequence)]
			     [else
					; Only delete after
			      (send edit delete (add1 pos-line-end) 
				    end-line-start)]))))))]
	       
	       [open-line
		(lambda (edit event)
		  (let ([sel-start (send edit get-start-position)]
			[sel-end (send edit get-end-position)])
		    (if (= sel-start sel-end)
			(send edit insert #\newline)
			(send edit set-position sel-start))))]
	       
	       [transpose-chars
		(lambda (edit event)
		  (let ([sel-start (send edit get-start-position)]
			[sel-end (send edit get-end-position)])
		    (when (= sel-start sel-end)
		      (let ([sel-start
			     (if (= sel-start
				    (send edit line-end-position
					  (send edit position-line sel-start)))
				 (sub1 sel-start)
				 sel-start)])
			(let ([s (send edit get-text
				       sel-start (add1 sel-start))])
			  (send edit begin-edit-sequence)
			  (send edit delete sel-start (add1 sel-start))
			  (send edit insert s (- sel-start 1))
			  (send edit set-position (add1 sel-start))
			  (send edit end-edit-sequence))))))]
	       
	       [transpose-words
		(lambda (edit event)
		  (let ([sel-start (send edit get-start-position)]
			[sel-end (send edit get-end-position)])
		    (when (= sel-start sel-end)
		      (let ([word-1-start (box sel-start)])
			(send edit find-wordbreak word-1-start #f 'caret)
			(let ([word-1-end (box (unbox word-1-start))])
			  (send edit find-wordbreak #f word-1-end 'caret)
			  (let ([word-2-end (box (unbox word-1-end))])
			    (send edit find-wordbreak #f word-2-end 'caret)
			    (let ([word-2-start (box (unbox word-2-end))])
			      (send edit find-wordbreak word-2-start #f 'caret)
			      (let ([text-1 (send edit get-text
						  (unbox word-1-start)
						  (unbox word-1-end))]
				    [text-2 (send edit get-text
						  (unbox word-2-start)
						  (unbox word-2-end))])
				(send edit begin-edit-sequence)
				(send edit insert text-1 
				      (unbox word-2-start)
				      (unbox word-2-end))
				(send edit insert text-2 
				      (unbox word-1-start)
				      (unbox word-1-end))
				(send edit set-position (unbox word-2-end))
				(send edit end-edit-sequence)))))))))]
	       
	       [capitalize-it
		(lambda (edit char-case1 char-case2)
		  (let ([sel-start (send edit get-start-position)]
			[sel-end (send edit get-end-position)]
			[real-end (send edit last-position)])
		    (when (= sel-start sel-end)
		      (let ([word-end (let ([b (box sel-start)])
					(send edit find-wordbreak #f b 'caret)
					(min real-end (unbox b)))])
			(send edit begin-edit-sequence)
			(let loop ([pos sel-start]
				   [char-case char-case1])
			  (when (< pos word-end)
			    (let ([c (send edit get-character pos)])
			      (cond
			       [(char-alphabetic? c)
				(send edit insert 
				      (list->string
				       (list (char-case c)))
				      pos (add1 pos))
				(loop (add1 pos) char-case2)]
			       [else 
				(loop (add1 pos) char-case)]))))
			(send edit end-edit-sequence)
			(send edit set-position word-end)))))]
	       
	       [capitalize-word
		(lambda (edit event)
		  (capitalize-it edit char-upcase char-downcase))]
	       [upcase-word
		(lambda (edit event)
		  (capitalize-it edit char-upcase char-upcase))]
	       [downcase-word
		(lambda (edit event)
		  (capitalize-it edit char-downcase char-downcase))]
	       
	       [kill-word
		(lambda (edit event)
		  (let ([sel-start (send edit get-start-position)]
			[sel-end (send edit get-end-position)])
		    (let ([end-box (box sel-end)])
		      (send edit find-wordbreak #f end-box 'caret)
		      (send edit kill 0 sel-start (unbox end-box)))))]
	       
	       [backward-kill-word
		(lambda (edit event)
		  (let ([sel-start (send edit get-start-position)]
			[sel-end (send edit get-end-position)])
		    (let ([start-box (box sel-start)])
		      (send edit find-wordbreak start-box #f 'caret)
		      (send edit kill 0 (unbox start-box) sel-end))))]
	       
	       [region-click
		(lambda (edit event f)
		  (when (send event button-down?)
		    (let ([x-box (box (send event get-x))]
			  [y-box (box (send event get-y))]
			  [eol-box (box #f)])
		      (send edit global-to-local x-box y-box)
		      (let ([click-pos (send edit find-position 
					     (unbox x-box)
					     (unbox y-box)
					     eol-box)]
			    [start-pos (send edit get-start-position)]
			    [end-pos (send edit get-end-position)])
			(let ([eol (unbox eol-box)])
			  (if (< start-pos click-pos)
			      (f click-pos eol start-pos click-pos)
			      (f click-pos eol click-pos end-pos)))))))]
	       [copy-click-region
		(lambda (edit event)
		  (region-click edit event
				(lambda (click eol start end)
				  (send edit flash-on start end)
				  (send edit copy #f 0 start end))))]
	       [cut-click-region
		(lambda (edit event)
		  (region-click edit event
				(lambda (click eol start end)
				  (send edit cut #f 0 start end))))]
	       [paste-click-region
		(lambda (edit event)
		  (region-click edit event
				(lambda (click eol start end)
				  (send edit set-position click)
				  (send edit paste 0 click))))]
	       
	       [mouse-copy-clipboard
		(lambda (edit event)
		  (send edit copy #f (send event get-time-stamp)))]
	       
	       [mouse-paste-clipboard
		(lambda (edit event)
		  (send edit paste (send event get-time-stamp)))]
	       
	       [mouse-cut-clipboard
		(lambda (edit event)
		  (send edit cut #f (send event get-time-stamp)))]
	       
	       [select-click-word
		(lambda (edit event)
		  (region-click edit event
				(lambda (click eol start end)
				  (let ([start-box (box click)]
					[end-box (box click)])
				    (send edit find-wordbreak 
					  start-box
					  end-box
					  'selection)
				    (send edit set-position
					  (unbox start-box)
					  (unbox end-box))))))]
	       [select-click-line
		(lambda (edit event)
		  (region-click edit event
				(lambda (click eol start end)
				  (let* ([line (send edit position-line 
						     click eol)]
					 [start (send edit line-start-position
						      line #f)]
					 [end (send edit line-end-position
						    line #f)])
				    (send edit set-position start end)))))]
	       
	       [goto-line
		(lambda (edit event)
		  (let ([num-str (get-text-from-user
				  "Goto Line"
				  "Goto Line:")])
		    (if (string? num-str)
			(let ([line-num (string->number num-str)])
			  (if line-num
			      (let ([pos (send edit line-start-position 
					       (sub1 line-num))])
				(send edit set-position pos))))))
		  #t)]
	       [goto-position
		(lambda (edit event)
		  (let ([num-str (get-text-from-user 
				  "Goto Position" 
				  "Goto Position:")])
		    (if (string? num-str)
			(let ([pos (string->number num-str)])
			  (if pos
			      (send edit set-position (sub1 pos))))))
		  #t)]
	       [repeater
		(lambda (n edit)
		  (let* ([km (send edit get-keymap)]
			 [done
			  (lambda ()
			    (send km set-break-sequence-callback void)
			    (send km remove-grab-key-function))])
		    (send km set-grab-key-function
			  (lambda (name local-km edit event)
			    (if name
				(begin
				  (done)
				  (dynamic-wind
				      (lambda ()
					(send edit begin-edit-sequence))
				      (lambda ()
					(let loop ([n n])
					  (unless (zero? n)
					    (send local-km call-function name edit event)
					    (loop (sub1 n)))))
				      (lambda ()
					(send edit end-edit-sequence))))
				(let ([k (send event get-key-code)])
				  (if (and (char? k)
					   (char<=? #\0 k #\9))
				      (set! n (+ (* n 10) (- (char->integer k) (char->integer #\0))))
				      (begin
					(done)
					(dynamic-wind
					    (lambda ()
					      (send edit begin-edit-sequence))
					    (lambda ()
					      (let loop ([n n])
						(unless (zero? n)
						  (send edit on-char event)
						  (loop (sub1 n)))))
					    (lambda ()
					      (send edit end-edit-sequence)))))))			       
			    #t))
		    (send km set-break-sequence-callback done)
		    #t))]
	       [make-make-repeater
		(lambda (n)
		  (lambda (edit event)
		    (repeater n edit)))]
	       [current-macro '()] 
	       [building-macro #f] [build-macro-km #f] [build-protect? #f]
	       [do-macro
		(lambda (edit event)
					; If c:x;e during record, copy the old macro
		  (when building-macro
		    (set! building-macro (append (reverse current-macro) 
						 (cdr building-macro))))
		  (let ([bm building-macro]
			[km (send edit get-keymap)])
		    (dynamic-wind
			(lambda ()
			  (set! building-macro #f)
			  (send edit begin-edit-sequence))
			(lambda ()
			  (let/ec escape
			    (for-each
			     (lambda (f)
			       (let ([name (car f)]
				     [event (cdr f)])
				 (if name
				     (unless (send km call-function name edit event #t)
				       (escape #t))
				     (send edit on-char event))))
			     current-macro)))
			(lambda ()
			  (send edit end-edit-sequence)
			  (set! building-macro bm))))
		  #t)]
	       [start-macro
		(lambda (edit event)
		  (if building-macro
		      (send build-macro-km break-sequence)
		      (letrec ([km (send edit get-keymap)]
			       [done
				(lambda ()
				  (if build-protect?
				      (send km set-break-sequence-callback done)
				      (begin
					(set! building-macro #f)
					(send km set-break-sequence-callback void)
					(send km remove-grab-key-function))))])
			(set! building-macro '())
			(set! build-macro-km km)
			(send km set-grab-key-function
			      (lambda (name local-km edit event)
				(dynamic-wind
				    (lambda ()
				      (set! build-protect? #t))
				    (lambda ()
				      (if name
					  (send local-km call-function name edit event)
					  (send edit on-default-char event)))
				    (lambda ()
				      (set! build-protect? #f)))
				(when building-macro
				  (set! building-macro 
					(cons (cons name event)
					      building-macro)))
				#t))
			(send km set-break-sequence-callback done)))
		  #t)]
	       [end-macro
		(lambda (edit event)
		  (when building-macro
		    (set! current-macro (reverse building-macro))
		    (set! build-protect? #f)		    
		    (send build-macro-km break-sequence))
		  #t)]
	       [toggle-overwrite
		(lambda (edit event)
		  (send edit set-overwrite-mode
			(not (send edit get-overwrite-mode))))])

	  (lambda (kmap)
	    (let* ([add (lambda (name func)
			  (send kmap add-function name func))])

	      (add-text-keymap-functions kmap)
	      
	      (add "toggle-overwrite" toggle-overwrite)
	      
	      (add "ring-bell" ring-bell)
	      
	      (add "toggle-anchor" toggle-anchor)
	      (add "center-view-on-line" center-view-on-line)
	      (add "collapse-space" collapse-space)
	      (add "remove-space" remove-space)
	      (add "collapse-newline" collapse-newline)
	      (add "open-line" open-line)
	      (add "transpose-chars" transpose-chars)
	      (add "transpose-words" transpose-words)
	      (add "capitalize-word" capitalize-word)
	      (add "upcase-word" upcase-word)
	      (add "downcase-word" downcase-word)
	      (add "kill-word" kill-word)
	      (add "backward-kill-word" backward-kill-word)
	      
	      (let loop ([n 9])
		(unless (negative? n)
		  (let ([s (number->string n)])
		    (add (string-append "command-repeat-" s)
			 (make-make-repeater n))
		    (loop (sub1 n)))))
	      
	      (add "do-saved-macro" do-macro)
	      (add "start-macro-record" start-macro)
	      (add "end-macro-record" end-macro)
	      
	      (add "mouse-copy-clipboard" mouse-copy-clipboard)
	      (add "mouse-cut-clipboard" mouse-cut-clipboard)
	      (add "mouse-paste-clipboard" mouse-paste-clipboard)
	      (add "mouse-copy-click-region" copy-click-region)
	      (add "mouse-cut-click-region" cut-click-region)
	      (add "mouse-paste-click-region" paste-click-region)
	      (add "mouse-select-click-word" select-click-word)
	      (add "mouse-select-click-line" select-click-line)
	      
	      (add "goto-line" goto-line)
	      (add "goto-position" goto-position))))))))

