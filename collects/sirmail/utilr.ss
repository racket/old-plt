
(module utilr mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "mred-sig.ss" "mred")
	   (lib "qp-sig.ss" "net")
	   (lib "base64-sig.ss" "net")
	   (lib "etc.ss"))

  (require "sirmails.ss")

  (provide util@)
  (define util@
    (unit/sig sirmail:utils^
      (import mred^
	      net:base64^
	      net:qp^)

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Utilities                                              ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define crlf (string #\return #\linefeed))

      (define (split s re)
	(let loop ([offset 0][accum null])
	  (let ([m (regexp-match-positions re s offset)])
	    (if m
		(let ([start (caar m)]
		      [end (cdar m)])
		  (loop end (cons (substring s offset start) accum)))
		(reverse!
		 (cons (substring s offset (string-length s))
		       accum))))))

      (define (splice l sep)
	(if (null? l)
	    ""
	    (let ([p (open-output-string)])
	      (let loop ([l l])
		(display (car l) p)
		(unless (null? (cdr l))
		  (display sep p)
		  (loop (cdr l))))
	      (get-output-string p))))
      
      (define (split-crlf s)
	(split s (regexp crlf)))

      (define (split-lf s)
	(split s (regexp (string #\linefeed))))

      (define (crlf->lf s)
	(splice (split-crlf s) (string #\linefeed)))

      (define (lf->crlf s)
	(splice (split-lf s) crlf))

      (define (enumerate n)
	(let loop ([n n][a null])
	  (if (zero? n)
	      a
	      (loop (sub1 n) (cons n a)))))

      (define (find i l)
	(let loop ([l l][pos 0])
	  (if (null? l)
	      #f
	      (if (eq? (car l) i)
		  pos
		  (loop (cdr l) (add1 pos))))))

      (define (string->regexp s)
	(regexp
	 (list->string
	  (apply
	   append
	   (map
	    (lambda (c)
	      (cond 
	       [(memq c '(#\$ #\| #\\ #\[ #\] #\. #\* #\? #\+ #\( #\) #\^))
		(list #\\ c)]
	       [(char-alphabetic? c)
		(list #\[ (char-upcase c) (char-downcase c) #\])]
	       [else (list c)]))
	    (string->list s))))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (break-really-hard? set-d!)
	(let* ([d (make-object dialog% "Danger")]
	       [p (make-object vertical-pane% d)]
	       [bp (make-object horizontal-pane% d)]
	       [result #f])
	  (send bp stretchable-width #f)
	  (send bp stretchable-height #f)
	  (make-object message% "Breaking now is dangerous." p)
	  (make-object message% "It requires killing the window." p)
	  (make-object message% "" p)
	  (make-object message% "Are you sure you want to kill?" p)
	  (make-object button% "&Kill" bp (lambda (b e)
					    (set! result #t)
					    (send d show #f)))
	  (make-object button% "Cancel" bp (lambda (b e) (send d show #f)))
	  (set-d! d)
	  (send d show #t)
	  result))

      ;; as-background: (bool window<%> braek-thunk ->)
      ;;                (break-ok-thunk break-not-ok-thunk -> X) 
      ;;                (-> Y) -> X
      ;; Runs a task in the background.
      ;; The `enable' function is called with #f as the first argument
      ;;  to start the background task. The `break-thunk' can be
      ;;  called to interrupt the task.
      ;; The `go' function performs the task (in a thread created by
      ;;   as-background); it receives thunks that it can call to
      ;;   indicate when breaks are "safe". If the user tries to break
      ;;   at a non-safe point, the user is warned; if the user
      ;;   proceeds, things are killed and `exit' is called. If
      ;;   the user breaks at a safe point, a break signal is sent
      ;;   to the thread for the background task.
      ;; The `pre-kill' thunk is called before things are killed
      ;;   for a non-"safe" break.
      (define (as-background enable go pre-kill)
	(let* ([v #f]
	       [exn #f]
	       [break-ok? #f]
	       [breaking-dialog #f]
	       [adjust-break (make-semaphore 1)]
	       [change-break-ok (lambda (ok?)
				  (lambda ()
				    (semaphore-wait adjust-break)
				    (set! break-ok? ok?)
				    (let ([waiting? (and ok? breaking-dialog)])
				      (when waiting?
					(send breaking-dialog show #f)
					(set! breaking-dialog #f))
				      (semaphore-post adjust-break)
				      (when waiting?
					(break-thread (current-thread))))))]
	       [s (make-semaphore 0)]
	       [t (thread (lambda ()
			    (with-handlers ([void (lambda (x) 
						    (set! exn x))])
			      (set! v (call-with-values 
					  (lambda () (go (change-break-ok #f)
							 (change-break-ok #t)))
					list))
			      ((change-break-ok #f)))
			    (when breaking-dialog
			      (send breaking-dialog show #f))
			    (semaphore-post s)))])
	  (let ([v (enable #f #f
			   (lambda ()
			     (semaphore-wait adjust-break)
			     (if break-ok?
				 (break-thread t)
				 (let ([v (break-really-hard? (lambda (d) 
								(set! breaking-dialog d)
								(semaphore-post adjust-break)))])
				   (semaphore-wait adjust-break)
				   (set! breaking-dialog #f)
				   (semaphore-post adjust-break)
				   (when v
				     (pre-kill)
				     (kill-thread t)
				     (exit))))))])
	    (yield s)
	    (enable #t v void))
	  (if exn
	      (raise exn)
	      (apply values v))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (make-fixed-width c e wrap? wrap-bm)
	(let ([s (send (send e get-style-list)
		       find-named-style "Standard")])
	  (send s set-delta
		(make-object style-delta% 'change-family 'modern))
	  (send e set-tabs null 8 #f)
	  (let ([font (send s get-font)]
		[dc (send c get-dc)]
		[wbox (box 0)]
		[hbox (box 0)])
	    (send e get-view-size wbox hbox)
	    (let-values ([(w h) (send c get-size)]
			 [(1w 1h d a) (send dc get-text-extent "X" font)])
	      (let ([80chars (+ (* 1w 80)
				2 ; +2 for caret
				(if wrap-bm 
				    (send wrap-bm get-width) 
				    0))])
		(when wrap? 
		  (when wrap-bm 
		    (send e set-autowrap-bitmap wrap-bm))
		  (send e set-max-width 80chars))
		(send c min-width 
		      (inexact->exact (round (+ 80chars (- w (unbox wbox)))))))))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define confirm-box 
	(opt-lambda (title message [parent #f] [style null])
	  (if (= 1 (message-box/custom
		    title
		    message
		    "&Yes"
		    "&No"
		    #f
		    parent
		    (append (if (memq 'app style) null '(caution)) 
			    '(default=1))
		    2))
	      'yes
	      'no)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Decoding `from' names                                  ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define re:iso (regexp "^(.*)=[?][iI][sS][oO]-8859-1[?]([qQbB])[?](.*?)[?]=(.*)$"))
      (define (parse-iso-8859-1 s)
	(and s
	     (let ([m (regexp-match re:iso s)])
	       (if m
		   (let ([s ((if (member (caddr m) '("q" "Q"))
				 ;; quoted-printable; strip newline:
				 (lambda (s)
				   (let ([s (qp-decode s)])
				     (substring s 0 (sub1 (string-length s)))))
				 ;; base64:
				 base64-decode)
			     (cadddr m))])
		     (parse-iso-8859-1
		      (string-append
		       (cadr m)
		       s
		       (cadddr (cdr m)))))
		   s)))))))

