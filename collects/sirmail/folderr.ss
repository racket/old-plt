

(module folderr mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   (lib "mred-sig.ss" "mred"))

  (require (lib "string.ss")
	   (lib "list.ss")
	   (lib "etc.ss"))

  (require "sirmails.ss")

  (require (lib "imap-sig.ss" "net"))

  (require (lib "hierlist-sig.ss" "hierlist"))

  (provide folder@)
  (define folder@
    (unit/sig ()
      (import sirmail:environment^
	      (shutdown-folders-window)
	      sirmail:options^
	      mred^
	      net:imap^
	      hierlist^)

      (define (imap-open-connection)
	(imap-connect IMAP-SERVER
		      USERNAME
		      (or (get-PASSWORD) (get-text-from-user "Enter Password"))
		      mailbox-name))

      (define imap-mailbox-name-mixin
	(lambda (list%)
	  (class100 list% args
		 (private-field
		   [full-mailbox-name 'unknown-full-mailbox-name])
		 (public
		   [set-full-mailbox-name
		    (lambda (fm)
		      (set! full-mailbox-name fm))]
		   [get-full-mailbox-name
		    (lambda ()
		      full-mailbox-name)])
		 (sequence
		   (apply super-init args)))))

      (define imap-mailbox-list-mixin
	(lambda (list%)
	  (class100 list% args
		 (private-field
		   [mailbox-name 'unknown-mailbox-name])
		 (public
		   [get-mailbox-name
		    (lambda ()
		      mailbox-name)]
		   [set-mailbox-name
		    (lambda (m)
		      (set! mailbox-name m))])

		 (inherit new-list new-item delete-item get-items)
		 (public
		   [refresh-children
		    (lambda ()
		      (with-handlers ([(lambda (x) #t)
				       (lambda (x)
					 (message-box "Error getting IMAP directory"
						      (if (exn? x)
							  (exn-message x)
							  (format "uncaught exception: ~e" x))))])
			(for-each (lambda (i) (delete-item i)) (get-items))
			(let-values ([(imap msg-count recent-count)
				      (imap-open-connection)])
			  (let ([mailbox-name-length (string-length mailbox-name)]
				[get-child-mailbox-name
				 (lambda (item)
				   (format "~a" (second item)))]
				[child-mailboxes
				 (imap-list-child-mailboxes imap mailbox-name)])
			    (for-each (lambda (item)
					(let* ([child-mailbox-name (get-child-mailbox-name item)]
					       [child-mailbox-flags (first item)]
					       [flat-mailbox?
						(member 'noinferiors
							(map imap-flag->symbol child-mailbox-flags))]
					       [sub-list (if flat-mailbox?
							     (new-item imap-mailbox-name-mixin)
							     (new-list imap-mailbox-mixin))]
					       [text (send sub-list get-editor)])

					  (send sub-list set-full-mailbox-name child-mailbox-name)
					  (unless flat-mailbox?
					    (send sub-list set-mailbox-name child-mailbox-name))
					  (let* ([child-name-length (string-length child-mailbox-name)]
						 [strip-prefix?
						  (and (> child-name-length
							  mailbox-name-length)
						       (string=?
							(substring child-mailbox-name
								   0 mailbox-name-length)
							mailbox-name))])
					    (send text insert 
						  (if strip-prefix?
						      (substring child-mailbox-name 
								 ;; strip separator (so, add1)
								 (add1 mailbox-name-length)
								 child-name-length)
						      child-mailbox-name)))))
				      (quicksort
				       child-mailboxes
				       (lambda (x y)
					 (string<=? (get-child-mailbox-name x)
						    (get-child-mailbox-name y)))))
			    (imap-disconnect imap)))))])
		 (sequence
		   (apply super-init args)))))

      (define imap-mailbox-mixin
	(compose 
	 imap-mailbox-list-mixin
	 imap-mailbox-name-mixin))

      (define imap-top-list% 
	(class100 (imap-mailbox-list-mixin hierarchical-list%) (frame)
	       (private-field
		 [selected-mailbox #f])
	       (public
		 [get-selected-mailbox
		  (lambda ()
		    selected-mailbox)])
	       (rename [super-on-select on-select])
	       (override
		 [on-select
		  (lambda (i)
		    (set! selected-mailbox (and i (send i get-full-mailbox-name)))
		    (send open-button enable i)
		    (send selected-message set-label
			  (if i
			      (send i get-full-mailbox-name)
			      ""))
		    (super-on-select i))]
		 [on-item-opened
		  (lambda (i)
		    (send i refresh-children))])
	       (sequence (super-init frame))))

      (define folders-frame%
	(class100 frame% (name)
	       (override
		 [on-close
		  (lambda ()
		    (shutdown-folders-window))])
	       (public
		 [get-mailbox-name
		  (lambda ()
		    (send top-list get-selected-mailbox))])
	       (sequence (super-init name))))

      (define icon (make-object bitmap% (build-path (collection-path "sirmail")
						    "postmark.bmp")))
      (define icon-mask (make-object bitmap% (build-path (collection-path "sirmail")
							 "postmark-mask.xbm")))
      (define frame (make-object folders-frame% "Folders"))
      (define top-panel (make-object horizontal-panel% frame))
      (send top-panel stretchable-height #f)
      
      (define re:setup-mailboxes "^([^/]*)/(.*)$")
      (define (setup-mailboxes-file mailbox-name)
	(define mailboxes-file (build-path LOCAL* "mailboxes"))
	(define mailboxes
	  (with-handlers ([void (lambda (x) '(("Inbox" "inbox")))])
	    (with-input-from-file mailboxes-file
	      read)))

	(define mailbox-loc (assoc mailbox-name mailboxes))

	(unless mailbox-loc

	  (let ([fns (let loop ([str mailbox-name])
		       (cond
			[(regexp-match re:setup-mailboxes str)
			 =>
			 (lambda (m)
			   (cons (cadr m)
				 (loop (caddr m))))]
			[else
			 (if (string=? str "")
			     null
			     (list str))]))])

	    (unless (null? fns)
	      (let ([mailbox-dir
		     (let loop ([fns (if (string=? (car fns) "")
					 (cdr fns)
					 fns)]
				[local-dir 'same]
				[fs-dir LOCAL*])
		       (cond
			[(null? fns) local-dir]
			[else (let ([new-fs-dir (build-path fs-dir (car fns))])
				(unless (directory-exists? new-fs-dir)
				  (make-directory new-fs-dir))
				(loop (cdr fns)
				      (build-path local-dir (car fns))
				      new-fs-dir))]))])

		(with-output-to-file (build-path LOCAL* "mailboxes")
		  (lambda () (write
			      (append mailboxes
				      (list (list mailbox-name mailbox-dir)))))
		  'truncate))))))


      (define open-button
	(make-object button% "Open Maibox"
		     top-panel
		     (lambda xxx
		       (let ([mail-box (send top-list get-selected-mailbox)])
			 (when mail-box
			   (setup-mailboxes-file mail-box)
			   (open-mailbox mail-box))))))
      (send open-button enable #f)
      
      (define selected-message (make-object message% "" top-panel))
      (send selected-message stretchable-width #t)
      (define top-list (make-object imap-top-list% frame))

      (when (and (send icon ok?) (send icon-mask ok?))
	(send frame set-icon icon icon-mask 'both))

      (send frame show #t)
      (send frame min-width 350)
      (send frame min-height 450)
      (send top-list set-mailbox-name ROOT-MAILBOX-FOR-LIST)
      (send top-list refresh-children)
      frame)))
