
(module readr mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
           (lib "etc.ss")
	   (lib "mred-sig.ss" "mred"))

  (require (lib "string.ss")
           (lib "list.ss"))

  (require "sirmails.ss")

  (require (lib "imap-sig.ss" "net")
	   (lib "smtp-sig.ss" "net")
	   (lib "head-sig.ss" "net")
	   (lib "base64-sig.ss" "net"))

  (require (lib "hierlist-sig.ss" "hierlist"))

  (require (lib "sendurl.ss" "net"))

  (provide read@)
  (define read@
    (unit/sig sirmail:read^
      (import sirmail:options^
	      sirmail:environment^
	      sirmail:utils^
	      sirmail:send^
	      mred^
	      net:imap^
	      net:smtp^
	      net:head^
	      net:base64^
	      hierlist^
	      (install-text-functions)
	      (install-emacs-bindings))
      
      (define main-frame #f)
      
      (define (show-error x)
	(message-box "Error" 
		     (if (exn? x)
			 (exn-message x)
			 (format "Strange exception: ~s" x))
		     main-frame))
      
      (initial-exception-handler
       (lambda (x)
	 (show-error x)
	 ((error-escape-handler))))
      (current-exception-handler
       (initial-exception-handler))
      
      ; Make my bindings global for file dialog, etc.
      (let ([km (make-object keymap%)])
	(install-text-functions km)
	(install-emacs-bindings km)
	(let ([f (current-text-keymap-initializer)])
	  (current-text-keymap-initializer
	   (lambda (k)
	     (send k chain-to-keymap km #f)
	     (f k)))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Mailbox List                                           ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define mailboxes
	(with-handlers ([void (lambda (x) '(("Inbox" "inbox")))])
	  (with-input-from-file (build-path LOCAL* "mailboxes")
	    read)))

      (unless (assoc mailbox-name mailboxes)
	(error 'sirmail "No local mapping for mailbox: ~a" mailbox-name))
      
      (define mailbox-dir (build-path LOCAL* (cadr (assoc mailbox-name mailboxes))))
      
      (unless (directory-exists? mailbox-dir)
	(error 'sirmail "Mailbox directory missing: ~a" mailbox-dir))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Message data structure                                 ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; We use a lists so they can be easily read and written
      
      (define mailbox (with-handlers ([void (lambda (x) null)])
			(with-input-from-file
			    (build-path mailbox-dir "mailbox")
			  read)))
      
      (define message-uid car)
      (define message-position cadr)
      (define message-downloaded? caddr)
      (define message-from cadddr)
      (define message-subject (lambda (m) (list-ref m 4)))
      (define message-flags (lambda (m) (list-ref m 5)))
      (define message-size (lambda (m) (let ([l (list-tail m 6)])
                                         ; For backward compatibility:
					 (if (pair? l)
					     (car l)
					     #f))))
      (define set-message-position! (lambda (m v) (set-car! (cdr m) v)))
      (define set-message-downloaded?! (lambda (m v) (set-car! (cddr m) v)))
      (define set-message-flags! (lambda (m v) (set-car! (list-tail m 5) v)))

      (define (message-marked? m) (memq 'marked (message-flags m)))
      
      (define (write-mailbox)
	(status "Saving mailbox information...")
	(with-output-to-file (build-path mailbox-dir "mailbox")
	  (lambda ()
	    (write mailbox))
	  'truncate))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Connection                                             ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define-values (connect disconnect force-disconnect)
	(let ([connection #f]
	      [message-count 0]
	      [next-uid 0])
	  (values
	   (letrec ([connect
		     (case-lambda
                       [() (connect 'reuse)]
                       [(mode)
                        (if connection
                            
                            ;; Already Connected
                            (cond
                              [(eq? mode 'reselect)
                               (let-values ([(count new) (imap-reselect connection mailbox-name)]
                                            [(uid-l) (imap-status connection mailbox-name '(uidnext))])
                                 (set! message-count count)
                                 (set! next-uid (car uid-l))
                                 (values connection count new next-uid))]
                              [(eq? mode 'next-uid)
                               (let-values ([(uid-l) (imap-status connection mailbox-name '(uidnext))])
                                 (set! next-uid (car uid-l))
                                 (values connection message-count 0 next-uid))]
                              [else
                               (values connection message-count 0 next-uid)])
                            
                            ;; New connection
                            (begin
                              (unless (get-PASSWORD)
                                (let ([p (get-text-from-user "Password" 
                                                             (format "Password for ~a:" USERNAME))])
                                  (unless p (error 'connect "connection cancelled"))
                                  (set-PASSWORD p)))
                              (let*-values ([(imap count new) (imap-connect IMAP-SERVER USERNAME (get-PASSWORD) mailbox-name)]
                                            [(uid-l) (imap-status imap mailbox-name '(uidnext))])
                                (status "(Connected, ~a messages)" count)
                                (set! connection imap)
                                (set! message-count count)
                                (set! next-uid (car uid-l))
                                (send disconnected-msg show #f)
                                (values imap count new next-uid))))])])
	     connect)
	   (lambda ()
	     (when connection
	       (status "Disconnecting...")
	       (as-background 
		enable-main-frame
		(lambda (break-bad break-ok) 
		  (with-handlers ([void no-status-handler])
		    (imap-disconnect connection)))
		void)
	       (status "")
	       (set! connection #f)))
	   (lambda ()
	     (with-handlers ([void void])
	       (disconnect))
	     (imap-force-disconnect connection)
	     (set! connection #f)))))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Mailbox actions                                        ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define initialized? #f)
      (define new-messages? #f)
      (define current-next-uid 0)
      
      (define (initialized next-uid)
	(set! initialized? #t)
	(set! new-messages? #f)
	(set! current-next-uid next-uid)
	(send new-mail-msg show #f))
      
      (define (update-local)
	(status "Updating ~a from ~a..." mailbox-name IMAP-SERVER)
	(let-values ([(imap count new next-uid) (connect 'reselect)])
	  (start-biff)
	  (status "Getting message ids...")
	  (let* ([positions (enumerate count)]
		 [data (imap-get-messages imap 
					  (enumerate count)
					  '(uid))]
		 [uids (map car data)]
		 [curr-uids (map car mailbox)]
		 [deleted (remove* uids curr-uids)]
		 [position-uids (map cons uids positions)]
		 [new (remove* curr-uids position-uids
			       (lambda (a b) (equal? a (car b))))])
	    (status "~a deleted, ~a locally new" (length deleted) (length new))
	    
	    (unless (null? new)
	      (status "Getting new headers..."))
	    (let* ([new-data (imap-get-messages imap 
						(map cdr new)
						'(header size))]
		   [new-headers (map car new-data)]
		   [new-sizes (map cadr new-data)]
		   [new-uid/size-map (map cons (map car new) new-sizes)])
	      (if (and (null? deleted) (null? new))
		  (begin
		    (initialized next-uid)
		    (status "No new messages")
		    #f)
		  (begin
		    (unless (null? deleted)
		      (status "Deleting local messages...")
		      (for-each
		       (lambda (uid)
			 (with-handlers ([void void])
			   (let ([path (build-path mailbox-dir (format "~a" uid))])
			     (delete-file path)
			     (let ([body (string-append path "body")])
			       (when (file-exists? body)
				 (delete-file body))))))
		       deleted))
		    
		    (unless (null? new-headers)
		      (status "Saving new headers...")
		      (for-each
		       (lambda (position-uid header)
			 (with-output-to-file
			     (build-path mailbox-dir (format "~a" (car position-uid)))
			   (lambda ()
			     (display header))
			   'truncate))
		       new new-headers))
		    
		    (set! mailbox (map
				   (lambda (uid pos)
				     (let ([old (assoc uid mailbox)])
				       `(,uid ,pos 
                                         ,(if old
                                              (message-downloaded? old)
                                              #f)
                                         ,(if old
                                              (message-from old)
                                              (extract-field "From" (get-header uid)))
                                         ,(if old
                                              (message-subject old)
                                              (extract-field "Subject" (get-header uid)))
                                         ,(if old
                                              (message-flags old)
                                              null)
                                         ,(if old
                                              (message-size old)
                                              (let ([new (assoc uid new-uid/size-map)])
                                                (if new
							 (cdr new)
							 0))))))
				   uids positions))
		    (write-mailbox)
		    (initialized next-uid)
		    (display-message-count (length mailbox))
		    (let ([len (length new-headers)])
		      (status "Got ~a new message~a" 
			      len
			      (if (= 1 len) "" "s")))
		    #t))))))
      
      (define (check-for-new)
	(status "Checking ~a at ~a..." mailbox-name IMAP-SERVER)
	(unless new-messages?
	  (let-values ([(imap count new next-uid) (connect 'next-uid)])
	    (unless (= next-uid current-next-uid)
	      (set! new-messages? #t))))
	(if new-messages?
	    (begin
	      (send new-mail-msg show #t)
	      (status "New mail")
	      #t)
	    (begin
	      (status "No new mail")
	      #f))
	new-messages?)
      
      (define (get-header uid)
	(let ([file (build-path mailbox-dir (format "~a" uid))])
	  (with-input-from-file file
	    (lambda ()
	      (read-string (file-size file))))))
      
      (define (get-body uid)
	(let ([v (assoc uid mailbox)]
	      [file (build-path mailbox-dir (format "~abody" uid))])
	  (when (not v)
	    (error 'internal-error "unknown message: ~a" uid))
	  (unless (message-downloaded? v)
	    (status "Getting message ~a..." uid)
	    (let ([size (message-size v)])
	      (when (and size (> size WARN-DOWNLOAD-SIZE))
		(unless (eq? 'yes
			     (message-box "Large Message"
					  (format "The message is ~s bytes.~nReally download?" size)
					  main-frame
					  '(yes-no)))
		  (status "")
		  (error "Download aborted"))))
	    (let*-values ([(imap count new next-uid) (connect)])
	      (let ([body (caar (imap-get-messages 
				 imap 
				 (list (message-position v))
				 '(body)))])
		(status "Saving message ~a..." uid)
		(with-output-to-file file
		  (lambda () (display body))
		  'truncate)
                
		(set-message-downloaded?! v #t)
		(write-mailbox))))
	  (begin0
            (with-input-from-file file
              (lambda ()
                (read-string (file-size file))))
            (status ""))))
      
      (define (check-positions imap msgs)
	(status "Checking message mapping...")
	(let ([ids (imap-get-messages imap (map message-position msgs) '(uid))])
	  (unless (equal? (map car ids) (map message-uid msgs))
	    (error 'position-check "server's position->id mapping doesn't match local copy. server: ~s local: ~s" (map car ids) (map message-uid msgs)))))
      
      (define (remove-delete-flags imap)
	(status "Removing old delete flags...")
	(imap-store imap '- (map message-position mailbox) (list (symbol->imap-flag 'deleted))))
      
      (define (purge-marked)
	(let* ([marked (filter message-marked? mailbox)])
	  (unless (null? marked)
	    (let-values ([(imap count new next-uid) (connect)])
	      (check-positions imap marked)
	      (remove-delete-flags imap)
	      (status "Deleting marked messages...")
	      (imap-store imap '+ (map message-position marked)
			  (list (symbol->imap-flag 'deleted)))
	      (imap-expunge imap)
	      (set! mailbox
		    (filter
		     (lambda (m) (not (message-marked? m)))
		     mailbox))
	      (let loop ([l mailbox][p 1])
		(unless (null? l)
		  (set-message-position! (car l) p)
		  (loop (cdr l) (add1 p))))
	      (write-mailbox)
	      (let* ([problems null]
		     [try-delete
		      (lambda (f)
			(with-handlers ([void 
					 (lambda (x)
					   (set! problems (cons x problems)))])
			  (delete-file f)))])
		(for-each
		 (lambda (m)
		   (let ([uid (message-uid m)])
		     (try-delete (build-path mailbox-dir (format "~a" uid)))
		     (when (message-downloaded? m)
		       (try-delete (build-path mailbox-dir (format "~abody" uid))))))
		 marked)
		(unless (null? problems)
		  (message-box "Warning"
			       (apply
				string-append
				"There we problems deleting some local files:"
				(map
				 (lambda (x)
				   (string-append
				    (string #\newline)
				    (if (exn? x)
					(exn-message x)
					"<unknown exn>")))))
			       main-frame))
		(display-message-count (length mailbox))
		(status "Messages deleted"))))))
      
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Mail Reader GUI                                        ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define FRAME-WIDTH 560)
      (define FRAME-HEIGHT 600)
      (let-values ([(display-width display-height) (get-display-size)])
	(set! FRAME-HEIGHT (min display-height FRAME-HEIGHT))
	(set! FRAME-WIDTH (min display-width FRAME-WIDTH)))
      
      
      (define FROM-WIDTH 150)
      (define SUBJECT-WIDTH 300)
      (define (update-frame-width w)
	(set! FRAME-WIDTH w)
	(let* ([goofy-margin 15]
	       [calc-w (- FRAME-WIDTH goofy-margin)])
	  (set! FROM-WIDTH (quotient calc-w 3))
	  (set! SUBJECT-WIDTH (- calc-w FROM-WIDTH)))
	
	(when (object? header-list)
	  (let ([e (send header-list get-editor)])
	    (send e begin-edit-sequence)
	    (for-each (lambda (item)
			(let* ([e (send item get-editor)]
			       [embedded-editors
				(let loop ([s (send e find-first-snip)]
					   [l null])
				  (cond
				   [(not s) (reverse! l)]
				   [(is-a? s editor-snip%) (loop (send s next)
								 (cons s l))]
				   [else (loop (send s next) l)]))]
			       [from-snip (car embedded-editors)]
			       [subject-snip (cadr embedded-editors)])
			  (send from-snip set-min-width FROM-WIDTH)
			  (send from-snip set-max-width FROM-WIDTH)
			  (send subject-snip set-min-width SUBJECT-WIDTH)
			  (send subject-snip set-max-width SUBJECT-WIDTH)))
		      (send header-list get-items))
	    (send e end-edit-sequence))))
      
      (update-frame-width FRAME-WIDTH)
      
      (define show-full-headers? #f)
      (define quote-in-reply? #t)
      
      (define unselected-delta (make-object style-delta% 'change-normal-color))
      (define selected-delta (make-object style-delta%))
      (send selected-delta set-delta-foreground "BLUE")
      
      (define unread-delta (make-object style-delta% 'change-bold))
      (define read-delta (make-object style-delta% 'change-weight 'normal))
      
      (define marked-delta (make-object style-delta% 'change-italic))
      (define unmarked-delta (make-object style-delta% 'change-style 'normal))
      
      (define (apply-style i delta)
	(define (get-header-editors i)
	  (let ([e (send i get-editor)])
	    (let loop ([s (send e find-first-snip)]
		       [l null])
	      (cond
                [(not s) (reverse! l)]
                [(is-a? s editor-snip%) (loop (send s next)
                                              (cons (send s get-editor)
                                                    l))]
                [else (loop (send s next) l)]))))
	(for-each
	 (lambda (e)
	   (send e change-style delta
		 0 (send e last-position)))
	 (get-header-editors i)))
      
      (define current-selected #f)
      
      (define (set-current-selected i)
	(unless (eq? current-selected i)
	  (let ([e (send header-list get-editor)])
	    (send e begin-edit-sequence)
	    (when current-selected
	      (apply-style current-selected unselected-delta))
	    (set! current-selected i)
	    (when i
	      (apply-style i selected-delta)
              ; In case we downloaded it just now:
	      (apply-style i read-delta))
	    (send e end-edit-sequence))))
      
      (define (logout)
	(with-handlers ([void
			 (lambda (x)
			   (show-error x)
			   (when (eq? 'yes
				      (message-box
				       "Error"
				       "There was an error disconnecting. Exit anyway?"
				       main-frame
				       '(yes-no)))
			     (exit-sirmail)
			     (send main-frame show #f)))])
	  (disconnect)
	  (when biff (send biff stop))
	  (exit-sirmail)
	  (send main-frame show #f)))
      
      (define global-keymap (make-object keymap%))
      (send global-keymap add-function "new-mailer"
	    (lambda (w e) (start-new-mailer #f "" "" "" "" "" null)))
      (send global-keymap add-function "disconnect"
	    (lambda (w e)
	      (disconnect)
	      (send disconnected-msg show #t)))
      (send global-keymap add-function "get-new-mail"
	    (lambda (w e) (get-new-mail)))
      (send global-keymap add-function "prev-msg"
	    (lambda (w e) (send header-list select-prev)))
      (send global-keymap add-function "next-msg"
	    (lambda (w e) (send header-list select-next)))
      (send global-keymap add-function "mark-msg"
	    (lambda (w e) (send header-list mark-message)))
      (send global-keymap add-function "unmark-msg"
	    (lambda (w e) (send header-list unmark-message)))
      (send global-keymap add-function "hit-msg"
	    (lambda (w e) (send header-list hit)))
      (send global-keymap add-function "scroll-down"
	    (lambda (w e) 
	      (if (send header-list selected-hit?)
		  (let*-values ([(e) (send message get-editor)]
				[(x y) (send e editor-location-to-dc-location 0 0)])
		    (send e move-position 'down #f 'page)
		    (let*-values ([(x2 y2) (send e editor-location-to-dc-location 0 0)])
		      (when (= y y2)
			(let ([current (send header-list get-selected)])
			  (send header-list select-next)
			  (unless (eq? current (send header-list get-selected))
			    (send header-list hit))))))
		  (send header-list hit))))
      (send global-keymap add-function "scroll-up"
	    (lambda (w e) 
	      (when (send header-list selected-hit?)
		(let ([e (send message get-editor)])
		  (send e move-position 'up #f 'page)))))
      (send global-keymap add-function "purge"
	    (lambda (w e) 
	      (purge-marked/update-headers)))
      (send global-keymap add-function "gc"
	    (lambda (w e) (dump-memory-stats) (collect-garbage)))
      
      (send global-keymap map-function ":m" "new-mailer")
      (send global-keymap map-function ":g" "get-new-mail")
      (send global-keymap map-function ":i" "disconnect")
      (send global-keymap map-function ":n" "next-msg")
      (send global-keymap map-function ":p" "prev-msg")
      (send global-keymap map-function ":return" "hit-msg")
      (send global-keymap map-function ":d" "mark-msg")
      (send global-keymap map-function ":u" "unmark-msg")
      (send global-keymap map-function ":space" "scroll-down")
      (send global-keymap map-function ":b" "scroll-up")
      (send global-keymap map-function "#" "purge")
      (send global-keymap map-function "!" "gc")
      
      (define icon (make-object bitmap% (build-path (collection-path "sirmail")
						    "postmark.bmp")))
      (define icon-mask (make-object bitmap% (build-path (collection-path "sirmail")
							 "postmark-mask.xbm")))
      (unless (and (send icon ok?)
		   (send icon-mask ok?))
	(set! icon #f))
      
      (define sm-frame%
	(class100 frame% args
          (rename [super-on-subwindow-char on-subwindow-char])
          (inherit get-menu-bar set-icon)
          (override
            [on-size
             (lambda (w h)
               (update-frame-width w))]
            [can-close? (lambda () (send (get-menu-bar) is-enabled?))]
            [on-close (lambda () (logout))]
            [on-subwindow-char
             (lambda (w e)
               (or (and
                    (send (send main-frame get-menu-bar) is-enabled?)
                    (or (send global-keymap handle-key-event w e)
                        (and (eq? #\tab (send e get-key-code))
                             (member w (list header-list message))
                             (send (if (eq? w message)
                                       header-list
                                       message)
                                   focus))))
                   (super-on-subwindow-char w e)))])
          (sequence
            (apply super-init args)
            (when icon
              (set-icon icon icon-mask 'both)))))
      
      (define header-list%
	(class100 hierarchical-list% args
          (inherit get-items show-focus)
          (private-field
           [selected #f])
          (public
            [mark (lambda (marked?) 
                    (when selected
                      (let* ([uid (send selected user-data)]
                             [m (assoc uid mailbox)]
                             [flags (message-flags m)])
                        (unless (eq? (not marked?) 
                                     (not (memq 'marked flags)))
                          (set-message-flags! m (if marked?
                                                    (cons 'marked flags)
                                                    (remq 'marked flags)))
                          (write-mailbox)
                          (apply-style selected 
                                       (if marked? 
                                           marked-delta
                                           unmarked-delta))
                          (status "~aarked" 
                                  (if marked? "M" "Unm"))))))]
            [hit (lambda () (when selected
                              (on-double-select selected)))]
            [mark-message (lambda () (mark #t))]
            [unmark-message (lambda () (mark #f))]
            [selected-hit? (lambda () (eq? selected current-selected))])
          (override
            [on-select
             (lambda (i) (set! selected i))]
            [on-double-select
             (lambda (i)
               (let ([e (send message get-editor)]
                     [uid (send i user-data)])
		 (dynamic-wind
		     (lambda ()
		       (send e lock #f)
		       (send e begin-edit-sequence))
		     void
		     (lambda ()
		       (send e erase)
		       (set-current-selected #f)
		       (let* ([h (get-header uid)]
			      [small-h (if show-full-headers?
					   h
					   (let loop ([l (reverse MESSAGE-FIELDS-TO-SHOW)]
						      [small-h empty-header])
					     (if (null? l)
						 small-h
						 (let ([v (extract-field (car l) h)])
						   (if v
						       (loop (cdr l) (insert-field
								      (car l)
								      v
								      small-h))
						       (loop (cdr l) small-h))))))])
			 (send e insert (crlf->lf small-h)
			       0 'same #f))
		       (send e insert 
			     (crlf->lf (as-background 
					enable-main-frame
					(lambda (break-bad break-ok) 
					  (with-handlers ([exn:break? (lambda (x) "<interrupted>")])
					    (get-body uid)))
					void))
			     (send e last-position) 'same #f)
		       (when SHOW-URLS (hilite-urls e))
		       ;;(handle-formatting e) ; too slow
		       (send e set-position 0)
		       (set-current-selected i))
		     (lambda ()
		       (send e end-edit-sequence)
		       (send e lock #t)))))])
          (sequence
            (apply super-init args)
            (show-focus #t))))
      
      (define (header-changing-action downloads? go)
	(let ([old-mailbox mailbox])
	  (go)
	  (let ([items (send header-list get-items)]
		[selected (send header-list get-selected)]
		[need-del-selection? #f]
		[set-selection? #f])
	    (send (send header-list get-editor) begin-edit-sequence)
	    (for-each
	     (lambda (i)
	       (let ([a (assoc (send i user-data) mailbox)])
		 (if a
		     (begin ; Message still here
		       (when (and downloads? (message-downloaded? a))
			 (apply-style i read-delta))
		       (when need-del-selection?
			 (set! need-del-selection? #f)
			 (send i select #t)))
		     (begin ; Message gone
		       (when (eq? i selected)
			 (set! need-del-selection? #t))
		       (when (eq? i current-selected)
			 (let ([e (send message get-editor)])
			   (send e lock #f)
			   (send e erase)
			   (send e lock #t))
			 (set-current-selected #f))
		       (send header-list delete-item i)))))
	     items)
	    (for-each
	     (lambda (m)
	       (unless (assoc (message-uid m) old-mailbox)
		 (let ([i (add-message m)])
		   (unless set-selection?
		     (set! set-selection? #t)
		     (send i select #t)
		     (send i scroll-to)))))
	     mailbox)
	    (send (send header-list get-editor) end-edit-sequence))))
      
      (define (get-new-mail)
	(with-handlers ([void
			 (lambda (x)
			   (status "")
			   (if (send disconnected-msg is-shown?)
			       (raise x)
			       (begin
				 (show-error x)
				 (when (exn:i/o? x)
				   (when (eq? 'yes
					      (message-box
					       "Error"
					       (format "There was an communication error.~nClose the connection?")
					       main-frame
					       '(yes-no)))
				     (send disconnected-msg show #t)
				     (set! initialized? #f)
				     (force-disconnect))))))])
	  (header-changing-action
	   #f
	   (lambda ()
	     (as-background
	      enable-main-frame
	      (lambda (break-bad break-ok) 
		(when (or (not initialized?)
			  (check-for-new))
		  (update-local)))
	      void)))))
      
      (define (purge-marked/update-headers)
	(header-changing-action 
	 #f
	 (lambda ()
	   (as-background 
	    enable-main-frame
	    (lambda (break-bad break-ok) 
	      (with-handlers ([void no-status-handler])
		(purge-marked)))
	    void))))
      
      (define vertical-line-snipclass
	(make-object
            (class100 snip-class% ()
              (override
                [read (lambda (s) (make-object vertical-line-snip%))])
              (sequence
                (super-init)))))
      (send vertical-line-snipclass set-version 1)
      (send vertical-line-snipclass set-classname "sirmail:vertical-line%")
      (send (get-the-snip-class-list) add vertical-line-snipclass)
      (define body-pen (send the-pen-list find-or-create-pen "forest green" 0 'solid))
      (define body-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))
      (define vertical-line-snip%
	(class100 snip% ()
          (inherit set-snipclass get-style get-admin)
          (private-field
           [width 15]
           [height 10])
          (override
            [get-extent
             (lambda (dc x y w-box h-box descent-box space-box lspace-box rspace-box)
               (for-each (lambda (box) (when box (set-box! box 0)))
                         (list w-box h-box lspace-box rspace-box))
               (let ([old-font (send dc get-font)])
                 (send dc set-font (send (get-style) get-font))
                 (let-values ([(w h descent ascent)
                               (send dc get-text-extent "yxX")])
                   (when w-box
                     (set-box! w-box width))
                   
                   ;; add one here because I know the descent for the entire
                   ;; line is going to be one more than the descent of the font.
                   (when descent-box
                     (set-box! descent-box (+ descent 1)))
                   
                   (when space-box
                     (set-box! space-box ascent))
                   (let ([text (and (get-admin)
                                    (send (get-admin) get-editor))])
                     
                     ;; add 2 here because I know lines are two pixels taller
                     ;; than the font. How do I know? I just know.
                     (set! height (+ h 2))
                     (when h-box
                       (set-box! h-box (+ h 2)))
                     
                     (send dc set-font old-font)))))]
            [draw
             (lambda (dc x y left top right bottom dx dy draw-caret)
               (let ([orig-pen (send dc get-pen)]
                     [orig-brush (send dc get-brush)])
                 (send dc set-pen body-pen)
                 (send dc set-brush body-brush)
                 
                 (send dc draw-line 
                       (+ x (quotient width 2))
                       y
                       (+ x (quotient width 2))
                       (+ y (- height 1)))
                 
                 (send dc set-pen orig-pen)
                 (send dc set-brush orig-brush)))]
            [write
             (lambda (s)
               (void))]
            [copy
             (lambda ()
               (let ([s (make-object vertical-line-snip%)])
                 (send s set-style (get-style))
                 s))])
          (sequence
            (super-init)
            (set-snipclass vertical-line-snipclass))))
      
      (define common-style-list #f)
      (define (single-style t)
	;; Commented out for now:
	'(if common-style-list
	     (send t set-style-list common-style-list)
	     (set! common-style-list (send t get-style-list)))
	t)
      
      (define (make-field w)
	(let ([m (instantiate editor-snip% ()
                   (editor (single-style (let ([e (make-object text% 0.0)])
                                           (send e set-keymap #f)
                                           (send e set-max-undo-history 0)
                                           e)))
                   (with-border? #f )
                   (top-margin 1)
                   (top-inset 1)
                   (bottom-margin 1)
                   (bottom-inset 1)
                   (min-width w)
                   (max-width w))])
	  (send m set-flags (remove 'handles-events (send m get-flags)))
	  m))
      
      (define re:one-line (regexp (format "^[^~a~a]*" #\newline #\return)))
      
      (define (add-message m)
	(let* ([i (send header-list new-item)]
	       [e (send i get-editor)]
	       [from (make-field FROM-WIDTH)]
	       [sep (make-object vertical-line-snip%)]
	       [subject (make-field SUBJECT-WIDTH)]
	       [one-line (lambda (s)
			   (let ([m (regexp-match re:one-line s)])
			     (if m (car m) s)))])
	  (send e begin-edit-sequence)
          (send e set-line-spacing 0)
	  (send i user-data (message-uid m))
	  (send e insert from)
	  (send e insert sep)
	  (send e insert subject)
	  (send (send from get-editor) insert 
		(one-line (or (message-from m) "<unknown>")))
	  (send (send subject get-editor) insert 
		(one-line (or (message-subject m) "<No subject>")))
	  (unless (message-downloaded? m)
	    (apply-style i unread-delta))
	  (when (memq 'marked (message-flags m))
	    (apply-style i marked-delta))      
	  (send e end-edit-sequence)
	  i))
      
      (define (status . args)
	(let ([s (apply format args)])
	  (send f set-status-text s)))
      
      (define can-poll? #t)
      
      (define (enable-main-frame on? refocus break-proc)
	(let ([w (send main-frame get-focus-window)])
	  (set! can-poll? on?)
	  (send header-list enable on?)
	  (send message enable on?)
	  (send (send main-frame get-menu-bar) enable on?)
	  (set! cancel-button-todo break-proc)
	  (send cancel-button enable (not on?))
	  (when (and on? refocus)
	    (send refocus focus))
	  w))
      
      (define f (make-object sm-frame% mailbox-name #f FRAME-WIDTH FRAME-HEIGHT))
      (set! main-frame f)
      (define button-panel (make-object horizontal-panel% f))
      (define header-list (make-object header-list% f))
      (send (send header-list get-editor) set-line-spacing 0)
      (define message (make-object editor-canvas% f))
      (let ([e (make-object text%)])
	((current-text-keymap-initializer) (send e get-keymap))
	(send e set-max-undo-history 0)
	(send message set-editor e)
	(make-fixed-width message e #f #f)
	(let ([b (make-object bitmap% (build-path (collection-path "icons") "return.xbm") 'xbm)])
	  (when (send b ok?)
	    (send e set-autowrap-bitmap b)))
	(send e lock #t))
      
      (define mb (make-object menu-bar% f))
      (define file-menu (make-object menu% "&File" mb))
      (define edit-menu (make-object menu% "&Edit" mb))
      (define msg-menu (make-object menu% "&Message" mb))
      (make-object menu-item% "&Get New Mail" file-menu
        (lambda (i e) (get-new-mail))
        #\G)
      (make-object menu-item% "&Download All" file-menu
        (lambda (i e) (download-all))
        #\L)
      
      (make-object menu-item% "&New Message" file-menu
		   (lambda (i e) (start-new-mailer #f "" "" "" "" "" null))
		   #\M)
      (make-object menu-item% "&Resume Message..." file-menu
        (lambda (i e) 
          (let ([file (get-file "Select message to reume"
                                main-frame)])
            (when file
              (start-new-mailer file "" "" "" "" "" null)))))
      (instantiate menu-item% () 
        (label "Send Queued Messages")
        (parent file-menu)
        (demand-callback
         (lambda (menu-item) 
           (send menu-item enable (not (= 0 (length (directory-list queue-directory)))))))
        (callback
         (lambda (i e)
           (send-queued-messages))))
      
      (make-object separator-menu-item% file-menu)
      (make-object menu-item% "&Save Message As..." file-menu
        (lambda (i e)
          (let ([f (put-file "Save message to"
                             main-frame)])
            (when f
              (send (send message get-editor) save-file f 'text)))))
      (make-object menu-item% "&Print" file-menu
        (lambda (i e) (send (send message get-editor) print))
        #\P)
      (make-object separator-menu-item% file-menu)
      (define mailboxes-menu (make-object menu% "&Open Mailbox" file-menu))
      (make-object menu-item% "&Add Mailbox..." file-menu
        (lambda (i e) (add-mailbox)))
      (make-object separator-menu-item% file-menu)
      (make-object menu-item% "D&isconnect" file-menu
        (lambda (i e) 
          (disconnect)
          (send disconnected-msg show #t))
        #\I)
      (make-object menu-item% "&Quit" file-menu
        (lambda (i e) (logout))
        #\Q)
      
      (make-object menu-item% "&Reply" msg-menu
		   (lambda (i e) (do-reply #f quote-in-reply?))
		   #\R)
      (make-object menu-item% "&Follow Up" msg-menu
        (lambda (i e) (do-reply #t quote-in-reply?))
        #\F)
      (make-object menu-item% "F&orward" msg-menu
        (lambda (i e) (do-forward))
        #\Z)
      (send (make-object checkable-menu-item% "&Quote Original" msg-menu
              (lambda (item e)
                (set! quote-in-reply? (send item is-checked?))))
	    check #t)
      (make-object separator-menu-item% msg-menu)
      (make-object menu-item% "&Mark Selected" msg-menu
		   (lambda (i e)
		     (send header-list mark-message))
		   #\D)
      (make-object menu-item% "&Unmark Selected" msg-menu
        (lambda (i e)
          (send header-list unmark-message))
        #\U)
      (define (mark-all mark?)
	(let* ([marked-uids (map message-uid (filter (if mark?
							 (lambda (x) (not (message-marked? x)))
							 message-marked?)
						     mailbox))]
	       [items (send header-list get-items)]
	       [selected (send header-list get-selected)])
	  (for-each
	   (lambda (i)
	     (when (member (send i user-data) marked-uids)
	       (send i select #t)
	       (if mark?
		   (send header-list mark-message)
		   (send header-list unmark-message))))
	   items)
	  (if selected
	      (send selected select #t)
	      (send (send header-list get-selected) select #f))))
      
      (make-object menu-item% "Mark All" msg-menu
        (lambda (i e) (mark-all #t)))
      (make-object menu-item% "Unmark All" msg-menu
        (lambda (i e) (mark-all #f)))
      
      (make-object separator-menu-item% msg-menu)
      (make-object menu-item% "&Delete Marked" msg-menu
        (lambda (i e)
          (when (eq? 'yes
                     (message-box
                      "Delete Marked?"
                      "Really delete the marked messages?"
                      main-frame
                      '(yes-no)))
            (purge-marked/update-headers))))
      (define move-menu (make-object menu% "&Copy Marked To" msg-menu))
      
      (when ROOT-MAILBOX-FOR-LIST
	(let ([mailbox-menu (make-object menu% "M&ailboxes" mb)])
	  (make-object menu-item%
            "&Show Folders Window"
            mailbox-menu
            (lambda x
              (open-folders-window)))
	  (make-object (class100 menu-item% args
			 (inherit enable set-label)
			 (override
			   [on-demand
			    (lambda ()
			      (let ([folder (get-active-folder)])
				(enable folder)
				(when folder
				  (set-label (format "&Copy Selected to ~a" folder)))))])
			 (sequence (apply super-init args)))
            "&Copy Selected to Folders Window Selection" 
            mailbox-menu
            (lambda x
              (let ([mbox (get-active-folder)])
                (if mbox
                    (copy-marked-to mbox)
                    (bell)))))))
      
      (when AUTO-FILE-TABLE
	(make-object separator-menu-item% msg-menu)
	(make-object menu-item% "Auto File" msg-menu
          (lambda (i e)
            (auto-file))))
      
      (make-object separator-menu-item% msg-menu)
      (define sort-menu (make-object menu% "&Sort" msg-menu))
      (make-object checkable-menu-item% "&Wrap Lines" msg-menu
        (lambda (item e)
          (send (send message get-editor) auto-wrap
                (send item is-checked?))))
      (make-object checkable-menu-item% "&View Full Header" msg-menu
        (lambda (i e)
          (set! show-full-headers? (send i is-checked?))
          (let ([i (send header-list get-selected)])
            (when current-selected
              (send header-list on-double-select current-selected)))))
      
      (make-object menu-item% "by Sender" sort-menu (lambda (i e) (sort-by-sender)))
      (make-object menu-item% "by Subject" sort-menu (lambda (i e) (sort-by-subject)))
      (make-object menu-item% "by Date" sort-menu (lambda (i e) (sort-by-date)))
      (make-object menu-item% "by Order Received" sort-menu (lambda (i e) (sort-by-order-received)))
      
      (define (sort-by-date) (sort-by (list (list "date" date-cmp))))
      (define (sort-by-sender) (sort-by (list (list "from" string-cmp))))
      (define (sort-by-subject) (sort-by (list (list "subject" subject-cmp))))
      (define (sort-by-order-received) (sort-by null))
      
      (define re:date
	(regexp
	 "([0-9]*)[ 	]+([A-Za-z]+)[ 	]+([0-9]+)[ 	]+([0-9][0-9]):([0-9][0-9]):([0-9][0-9])"))
      
      ;; using the tz seems to require a date->seconds -- too expensive.
      (define (date-cmp aid bid a b)
	(define (month->number mon)
	  (string-lowercase! mon)
	  (case (string->symbol mon)
	    [(jan) 1]
	    [(feb) 2]
	    [(mar) 3]
	    [(apr) 4]
	    [(may) 5]
	    [(jun) 6]
	    [(jul) 7]
	    [(aug) 8]
	    [(sep) 9]
	    [(oct) 10]
	    [(nov) 11]
	    [(dec) 12]))
        
	(define (pairwise-cmp l1 l2)
	  (cond
            [(and (null? l1) (null? l2)) 'same]
            [(or (null? l1) (null? l2)) (error 'pairwise-cmp "internal error; date lists mismatched")]
            [(= (car l1) (car l2)) (pairwise-cmp (cdr l1) (cdr l2))]
            [else (< (car l1) (car l2))]))
        
	(define (get-date a)
	  (let* ([m (regexp-match re:date a)])
	    (if m
		(let* ([datel (cdr m)]
		       [day (string->number (first datel))]
		       [month (month->number (second datel))]
		       [year (string->number (third datel))]
		       [hours (string->number (fourth datel))]
		       [minutes (string->number (fifth datel))]
		       [seconds (string->number (sixth datel))])
		  (list year month day
			hours minutes seconds))
		(list 0 0 0
		      0 0 0))))
        
	(pairwise-cmp
	 (get-date a)
	 (get-date b)))
      
      (define re:re (regexp "^[rR][eE]: *(.*)"))
      (define (subject-cmp aid bid a b)
	(let ([simplify (lambda (s)
			  (if s
			      (let ([m (regexp-match re:re s)])
				(if m
				    (cadr m)
				    s))
			      ""))])
	  (string-cmp aid bid (simplify a) (simplify b))))
      
      (define (string-cmp aid bid a b)
	(if (string-ci=? a b)
	    'same
	    (string-ci<? a b)))
      
      (define (sort-by fields)
	(let* ([ht (make-hash-table)]
	       [get-header/cached 
		(lambda (uid first-field)
		  (hash-table-get
		   ht
		   uid
		   (lambda ()
		     (let* ([h (get-header uid)]
			    [p (cons h
				     (and first-field
					  (extract-field 
					   first-field 
					   h)))])
		       (hash-table-put! ht uid p)
		       p))))])
	  (as-background
	   enable-main-frame
	   (lambda (break-bad break-ok)
	     (status "Sorting...")
	     (send header-list sort
		   (lambda (a b)
		     (let ([aid (send a user-data)]
			   [bid (send b user-data)])
		       (cond
                         ;; fastest: order recived:
                         [(null? fields) (< aid bid)]
                         ;; faster: sender
                         [(and (= (length fields) 1)
                               (string=? (caar fields) "from"))
                          (let ([ma (assq aid mailbox)]
                                [mb (assq bid mailbox)])
                            (let ([c ((cadar fields) 
                                      aid bid
                                      (message-from ma)
                                      (message-from mb))])
                              (if (eq? c 'same)
                                  (< aid bid)
                                  c)))]
                         ;; slow: general case
                         [else
                          (let ([ah+f (get-header/cached aid (and (pair? fields)
                                                                  (caar fields)))]
                                [bh+f (get-header/cached bid (and (pair? fields)
                                                                  (caar fields)))])
                            (let loop ([fields fields][first? #t])
                              (if (null? fields)
                                  (< aid bid)
                                  (let ([c ((cadar fields)
                                            aid bid
                                            (if first?
                                                (cdr ah+f)
                                                (extract-field (caar fields) (car ah+f)))
                                            (if first?
                                                (cdr bh+f)
                                                (extract-field (caar fields) (car bh+f))))])
                                    (if (eq? c 'same)
                                        (loop (cdr fields) #f)
                                        c)))))]))))
	     (status ""))
	   void)))
      
      (append-editor-operation-menu-items edit-menu #t)
      ; Certain menu items are never useful since the window is read-only:
      (for-each
       (lambda (i)
	 (when (and (is-a? i labelled-menu-item<%>)
		    (member (send i get-plain-label) '("Undo" "Redo" "Clear")))
	   (send i enable #f)))
       (send edit-menu get-items))

      
      (define no-status-handler (lambda (x) (status "") (raise x)))
      
      (define (add-mailbox)
	(let ([t (get-text-from-user "New Mailbox" "New mailbox name:" main-frame)])
	  (when t
	    
	    (when (let-values ([(imap count new next-uid) (connect)])
		    (if (as-background
			 enable-main-frame
			 (lambda (break-bad break-ok)
			   (status "Checking for mailbox...")
			   (begin0
                             (with-handlers ([void no-status-handler])
                               (imap-mailbox-exists? imap t))
                             (status "")))
			 void)
			#t
			(and (eq? 'yes
				  (message-box "New Mailbox?"
					       (format "The mailbox ~a does not currently exists.~nCreate it?" t)
					       main-frame
					       '(yes-no)))
			     
			     (as-background
			      enable-main-frame
			      (lambda (break-bad break-ok) 
				(status "Creating mailbox...")
				(with-handlers ([void no-status-handler])
				  (imap-create-mailbox imap t))
				(status "")
				#t)
			      void))))
	      
	      (let ([dir (regexp-replace* "/" t ".")])
		(with-handlers ([void void]) (make-directory (build-path LOCAL* dir)))
		(set! mailboxes
		      (append mailboxes
			      (list (list t dir)))))
	      (with-output-to-file (build-path LOCAL* "mailboxes")
		(lambda () (write mailboxes))
		'truncate)
	      (reset-mailboxes-menus)
	      (status "Added mailbox")))))
      
      (define (reset-mailboxes-menus)
	(for-each
	 (lambda (i) (send i delete))
	 (append (send mailboxes-menu get-items)
		 (send move-menu get-items)))
        
	(for-each
	 (lambda (mb)
	   (unless (string=? (car mb) mailbox-name)
	     (make-object menu-item% (car mb) move-menu
               (lambda (i e)
                 (copy-marked-to (car mb))))))
	 mailboxes)
        
	(for-each
	 (lambda (mb)
	   (unless (string=? (car mb) mailbox-name)
	     (make-object menu-item% (car mb) mailboxes-menu
               (lambda (i e)
                 (let ([mailbox-name (car mb)]
                       [mailbox-options (cddr mb)])
                   (open-mailbox mailbox-name mailbox-options))))))
	 mailboxes))
      
      (reset-mailboxes-menus)
      
      ;; send-queued-messsages : -> void
      ;; sends the files queued in `queue-directory'
      (define (send-queued-messages)
        (for-each send-queued-message (directory-list queue-directory)))
      
      ;; send-queued-message : string -> void
      ;; sends the email message in `filename' by opening a window and sending it a message
      (define (send-queued-message filename)
        (start-new-window
	 (lambda ()
           (let ([full-filename (build-path queue-directory filename)])
             (send (new-mailer full-filename "" "" "" "" "" null)
                   send-message)
             (delete-file full-filename)))))
      
      (define (copy-marked-to dest-mailbox-name)
	(let* ([marked (filter message-marked? mailbox)])
	  (unless (null? marked)
	    (let-values ([(imap count new next-uid) (connect)])
	      (check-positions imap marked)
	      (status (format "Copying marked to ~a..." dest-mailbox-name))
	      (as-background
	       enable-main-frame
	       (lambda (break-bad break-ok)
		 (status (format "Copying marked to ~a..." dest-mailbox-name))
		 (with-handlers ([void no-status-handler])
		   (imap-copy imap (map message-position marked) dest-mailbox-name)))
	       void)
	      (status (format "Copied to ~a" dest-mailbox-name))))))
      
      (define (auto-file)
	(as-background
	 enable-main-frame
	 (lambda (break-bad break-ok)
	   (break-ok)
	   (map
	    (lambda (auto)
	      (let* ([dest-mailbox-name (car auto)]
		     [fields (map car (cadr auto))]
		     [val-rxs (map string->regexp (map cadr (cadr auto)))])
		(with-handlers ([void no-status-handler])
		  (break-ok)
		  (status (format "Finding ~a messages..." dest-mailbox-name))
		  (let ([file-msgs 
			 (filter
			  (lambda (m)
			    (and (not (message-marked? m))
				 (let ([h (get-header (message-uid m))])
				   (ormap (lambda (field val-rx)
					    (let ([v (extract-field field h)])
					      (and v (regexp-match val-rx v))))
					  fields val-rxs))))
			  mailbox)])
		    (unless (null? file-msgs)
		      (status (format "Filing to ~a..." dest-mailbox-name))
		      (break-bad)
		      (let-values ([(imap count new next-uid) (connect)])
			(status (format "Filing to ~a..." dest-mailbox-name))
                        ; Copy messages for filing:
			(imap-copy imap (map message-position file-msgs) dest-mailbox-name)
                        ; Mark them (let the user delete)
			(for-each (lambda (m)
				    (set-message-flags! m (cons 'marked (message-flags m)))
				    (let ([i (let ([items (send header-list get-items)]
						   [uid (message-uid m)])
					       (ormap (lambda (i) (and (eq? (send i user-data) uid)
								       i))
						      items))])
				      (apply-style i marked-delta)))
				  file-msgs)
			(write-mailbox)))))))
	    AUTO-FILE-TABLE))
	 void)
	(status "Auto file done"))
      
      (send header-list min-height 150)
      (send header-list stretchable-height #f)
      (send message min-height 200)
      
      (send button-panel stretchable-height #f)
      (define disable-button-panel (make-object horizontal-panel% button-panel))
      (define mailbox-message (make-object message% (format "~a: XXXXX" mailbox-name) disable-button-panel))
      (define (display-message-count n)
	(send mailbox-message set-label (format "~a: ~a" mailbox-name n)))
      (display-message-count (length mailbox))
      (define-values (new-mail-msg disconnected-msg)
	(let ([font (send disable-button-panel get-label-font)])
	  (send disable-button-panel set-label-font 
		(make-object font% 
                  (send font get-point-size)
                  'system 'normal 'bold))
	  (let ([m (make-object message% "  New Mail!" disable-button-panel)]
		[d (make-object message% "Disconnected" disable-button-panel)])
	    (send disable-button-panel set-label-font font)
	    (send m show #f)
	    (values m d))))
      (define cancel-button
	(make-object button% "Stop" button-panel
		     (lambda (b e) (cancel-button-todo))))
      (define cancel-button-todo void)
      (send cancel-button enable #f)
      
      (define (download-all)
	(get-new-mail)
	(header-changing-action
	 #t
	 (lambda ()
	   (as-background
	    enable-main-frame
	    (lambda (break-bad break-ok)
	      (with-handlers ([exn:break?
			       (lambda (x) "<interrupted>")])
		(break-ok)
		(for-each (lambda (message)
			    (with-handlers ([exn:break?
					     (lambda (x)
					       (void))])
			      (let ([uid (message-uid message)])
				(break-bad)
				(get-body uid)
				(break-ok))))
			  mailbox)))
	    void))))
      
      (send header-list focus)
      
      (for-each add-message mailbox)
      
      (send f create-status-line)
      
      (send f show #t)
      
      (when SORT
	(case SORT
	  [(date) (sort-by-date)]
	  [(subject) (sort-by-subject)]
	  [(from) (sort-by-sender)]
	  [(id) (sort-by-order-received)]
	  [else
	   (message-box
	    "SirMail"
	    (format "invalid setting for SORT preference: ~s" SORT)
	    main-frame)]))
      
      (unless (null? mailbox)
	(let ([last (car (last-pair (send header-list get-items)))])
	  (send last select #t)
	  (queue-callback (lambda () (send last scroll-to)))))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Biff                                                   ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define biff%
	(class100 timer% ()
	  (inherit stop)
	  (override
	    [notify (lambda ()
		      (when can-poll?
			(unless (or (send disconnected-msg is-shown?)
				    new-messages?)
			  (with-handlers ([void
					   (lambda (x)
					     (stop)
					     (send disconnected-msg show #t)
					     (set! initialized? #f)
					     (force-disconnect)
					     (status "Error connecting: ~s"
						     (if (exn? x)
							 (exn-message x)
							 x)))])
			    (when (as-background
				   enable-main-frame
				   (lambda (break-bad break-ok) 
				     (check-for-new))
				   void)
			      (bell))))))])
	  (sequence
	    (super-init))))
      
      (define biff
	(if BIFF-DELAY
	    (make-object biff%)
	    #f))
      
      (define (start-biff)
	(when biff
	  (send biff start (* 1000 BIFF-DELAY))))
      
      (start-biff)
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Mail Sending                                           ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define my-address 
	(with-handlers ([void (lambda (x) "<bad address>")])
	  (car (extract-addresses MAIL-FROM 'address))))
      
      (define my-username-@
	(let ([m (regexp-match "^([^@]*)@" my-address)])
	  (if m
	      (cadr m)
	      (string-append my-address "@"))))
      
      (define (not-me? name-addr-full)
	(let ([addr (cadr name-addr-full)])
	  (cond
            [(string=? addr my-address)
             #f]
            [(and SELF-ADDRESSES (member addr SELF-ADDRESSES)) #f]
            [(and (> (string-length addr) (string-length  my-username-@))
                  (string=? my-username-@ (substring addr 0 (string-length  my-username-@))))
             (eq? (message-box
                   "Identity?"
                   (format "Are you ~a?" (caddr name-addr-full))
                   main-frame
                   '(yes-no))
                  'no)]
            [else #t])))
      
      (define (do-reply follow-up? quote-msg?)
	(define selected (send header-list get-selected))
	(unless selected
	  (bell))
	(when selected
	  (let* ([uid (send selected user-data)]
		 [h (get-header uid)]
		 [body (get-body uid)])
	    (start-new-mailer
	     #f
	     (or (extract-field "Reply-To" h) 
		 (extract-field "From" h)
		 "")
	     (if follow-up?
		 (let ([to (extract-field "To" h)]
		       [cc (extract-field "CC" h)])
		   (if (or to cc)
		       (let ([to (map
				  caddr
				  (filter
				   not-me?
				   (append
				    (if to
					(extract-addresses to 'all)
					null)
				    (if cc
					(extract-addresses cc 'all)
					null))))])
			 (if (null? to)
			     ""
			     (assemble-address-field to)))
		       ""))
		 "")
	     (let ([s (extract-field "Subject" h)])
	       (cond
                 [(not s) ""]
                 [(regexp-match "^[Rr][Ee][(]([0-9]+)[)]:(.*)$" s)
                  ;; Other mailer is counting replies. We'll count, too.
                  => (lambda (m)
                       (format "~a(~a):~a"
                               (substring s 0 2)
                               (add1 (string->number (cadr m)))
                               (caddr m)))]
                 [(regexp-match "^[Rr][Ee]:" s) s]
                 [else (string-append "Re: " s)]))
	     (let ([id (extract-field "Message-Id" h)]
		   [refs (extract-field "References" h)])
	       (format "~a~a"
		       (if id
			   (format "In-Reply-To: ~a~a" id crlf)
			   "")
		       (if (or refs id)
			   (format "References: ~a~a"
				   (cond
                                     [(and refs id)
                                      (format "~a~a~a~a~a"
                                              refs crlf #\tab #\tab id)]
                                     [else (or refs id)])
				   crlf)
			   "")))
	     (if quote-in-reply?
		 (string-append
		  (format "Quoting ~a:~a" 
			  (let ([from (extract-field "From" h)])
			    (let ([name (with-handlers ([void (lambda (x) #f)])
					  (car (extract-addresses from 'name)))])
			      (or name "<unknown>")))
			  crlf)
		  "> "
		  (let* ([s (splice (split body crlf)
				    (string-append crlf "> "))]
			 [len (string-length s)])
		    (if (and (>= len 2)
			     (string=? "> " (substring s (- len 2) len)))
			(substring s 0 (- len 2))
			s)))
		 "")
	     null))))

      (define (do-forward)
	(define selected (send header-list get-selected))
	(unless selected
	  (bell))
	(when selected
	  (let* ([uid (send selected user-data)]
		 [h (get-header uid)]
		 [body (get-body uid)])
	    (start-new-mailer
	     #f "" ""
	     (let ([s (extract-field "Subject" h)])
	       (if (and s (not (regexp-match "^[Ff][Ww][Dd]:" s)))
		   (string-append "Fwd: " s)
		   (or s "Fwd")))
	     "" ""
	     (list
	      (make-enclosure
	       "Forwarded Message"
	       (insert-field
		"Content-Type" "message/rfc822"
		(insert-field 
		 "Content-Transfer-Encoding" "7bit"
		 (insert-field
		  "Content-Disposition" "inline"
		  empty-header)))
	       (string-append h body)))))))
      
      (define (start-new-mailer file to cc subject other-headers body enclosures)
	(start-new-window
	 (lambda ()
	   (new-mailer file to cc subject other-headers body enclosures))))
      
      (define (start-new-mailer/send-message file to cc subject other-headers body enclosures)
	(start-new-window
	 (lambda ()
           (send (new-mailer file to cc subject other-headers body enclosures)
                 send-message))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                                                                  ;;
      ;;                        misc formatting                           ;;
      ;;                                                                  ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; handle-formatting : text -> void
      (define (handle-formatting e)
        (let loop ([line (send e last-line)])
          (unless (zero? line)
            (for-each
             (lambda (regexp/action)
               (handle-single-line/formatting
                (car regexp/action) 
                (cadr regexp/action)
                e
                line))
             regexps/actions)
            (loop (- line 1)))))
      
      (define (handle-single-line/formatting regexp action e line)
        (let ([start (send e line-start-position line)]
              [end (send e line-end-position line)])
          (let loop ([string (send e get-text start end #f)]
                     [line-offset 0])
            (cond
              [(regexp-match-positions regexp string)
               =>
               (lambda (m)
                 (let ([before (cadr m)]
                       [during (caddr m)]
                       [after (cadddr m)])
                   (action e
                           (+ line-offset start (car during))
                           (+ line-offset start (cdr during)))
                   (loop (substring string (car before) (cdr before))
                         (+ (car before) line-offset))
                   (loop (substring string (car after) (cdr after))
                         (+ (car after) line-offset))))]
              [else (void)]))))
      
      ;; emoticon-path (may not exist)
      (define emoticon-path 
        (build-path (this-expression-source-directory) "emoticon"))
      
      ;; emoticon : string string -> (listof (list regexp (text number number -> void)))
      (define (emoticon img . icons)
        (let ([snip (make-object image-snip% (build-path emoticon-path img))])
          (map
           (lambda (icon)
             (list (regexp (string-append "(.*)(" (quote-regexp-chars icon) ")(.*)"))
                   (lambda (e start end)
                     (send e insert (send snip copy) start end))))
           icons)))
      
      (define (quote-regexp-chars str)
        (apply
         string
         (let loop ([chars (string->list str)])
           (cond
             [(null? chars) null]
             [else (let ([char (car chars)])
                     (if (memq char regexp-special-chars)
                         (list* #\\ char (loop (cdr chars)))
                         (cons char (loop (cdr chars)))))]))))
      
      (define regexp-special-chars (string->list "()*+?[].^\\|"))
      
      ;; all regexps must have three parenthesized sub-expressions
      ;; the first is unmatched text before the regexp, the second
      ;; is the matched tetx and the third is unmatched text after the regexp.
      (define regexps/actions
        (list*
         (list (regexp "(.*)( \\*([^\\*]*)\\* )(.*)")
               (lambda (e start end) (send e change-style bold-style-delta start end)))
         (list (regexp "(.*) _([^_]*)_ (.*)")
               (lambda (e start end) (send e change-style italic-style-delta start end)))
         (if (directory-exists? emoticon-path)
             (append
              (emoticon "bigsmile.gif" ":D" ":-D")
              (emoticon "cry.gif" ":')" ":'-)")
              (emoticon "happy.gif" ":)" ":-)" ":>" ":->" "<-:" "<:" "(-:" "(:")
              (emoticon "kiss.gif" "*:" ":*")
              (emoticon "sad.gif" ":(" ":-(" ":<" ":-<" ">-:" ">:" "):" ")-:")
              (emoticon "tongue.gif" ":P" ":-P")
              (emoticon "wink.gif" ";)" ";-)" ";>" ";->"))
             null)))

    
      (define bold-style-delta (make-object style-delta% 'change-bold))
      (define italic-style-delta (make-object style-delta% 'change-italic))
    
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                                                                  ;;
      ;;                       highlighting urls                          ;;
      ;;                                                                  ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; hilite-urls : text -> void
      ;; highligts all of the urls (strings beginning with `http:', `https:' or `ftp:')
      ;; in the buffer to be able to click on them.
      (define (hilite-urls e)
        (define (hilite-urls/prefix prefix)
          (let loop ([pos 0])
            (when (< pos (send e last-position))
              (let ([start-pos (send e find-string prefix 'forward pos 'eof #t #f)])
                (when start-pos
                  (let ([eou-pos (let loop ([eou-pos start-pos])
                                   (cond
                                     [(= eou-pos (send e last-position)) eou-pos]
                                     [(char-whitespace? (send e get-character eou-pos))
				      ;; Back up past . and >:
				      (let loop ([eou-pos eou-pos])
					(if (memq (send e get-character (sub1 eou-pos))
						  '(#\. #\>))
					    (loop (sub1 eou-pos))
					    eou-pos))]
                                     [else (loop (+ eou-pos 1))]))])
                    (send e change-style url-delta start-pos eou-pos)
                    (send e set-clickback start-pos eou-pos 
                          (lambda (e start-pos eou-pos)
                            (send-url (send e get-text start-pos eou-pos))))
                    (loop eou-pos)))))))
        (hilite-urls/prefix "http:")
        (hilite-urls/prefix "https:")
        (hilite-urls/prefix "ftp:"))

      ;; url-delta :  style-delta
      ;; this is used to higlight urls in the editor window
      (define url-delta (make-object style-delta% 'change-underline #t))
      (send url-delta set-delta-foreground "blue"))))

