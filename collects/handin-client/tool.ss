(module tool mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "unitsig.ss")
	   (lib "tool.ss" "drscheme")
	   (lib "etc.ss")
	   (lib "framework.ss" "framework")
	   "client.ss"
	   "info.ss")

  (provide tool@)

  (define server (#%info-lookup 'server))
  (define port-no (#%info-lookup 'port-no))
  (define handin-name (#%info-lookup 'name))
  (define this-collection (#%info-lookup 'collection))

  (preferences:set-default 'submit:username "" string?)
  (define (remembered-user)
    (preferences:get 'submit:username))
  (define (remember-user user)
    (preferences:set 'submit:username user))

  (define (connect)
    (handin-connect server
		    port-no
		    (build-path
		     (collection-path this-collection)
		     "server-cert.pem")))

  (define handin-frame% 
    (class dialog%
      (inherit show is-shown?)
      (super-new [label "Handin"])

      (init-field content)

      (define status (new message%
			  [label (format "Making secure connection to ~a..." server)]
			  [parent this]
			  [stretchable-width #t]))
      (define username (new text-field%
			    [label "Username:"]
			    [init-value (remembered-user)]
			    [parent this]
			    [callback (lambda (t e) (activate-ok))]
			    [stretchable-width #t]))
      (define passwd (new text-field%
			  [label "Password:"]
			  [parent this]
			  [callback (lambda (t e) (activate-ok))]
			  [style '(single password)]
			  [stretchable-width #t]))
      (define assignment (new choice%
			      [label "Assignment:"]
			      [choices null]
			      [parent this]
			      [callback void]
			      [stretchable-width #t]))
      
      (define button-panel (new horizontal-pane%
				[parent this]
				[stretchable-height #f]))
      (make-object vertical-pane% button-panel) ; spacer
      (define ok (new button%
		      [label "Handin"]
		      [parent button-panel]
		      [callback (lambda (b e)
				  (disable-interface)
				  (send status set-label "Handing in...")
				  (parameterize ([current-custodian
						  comm-cust])
				    (thread
				     (lambda ()
				       (with-handlers ([void
							(lambda (exn)
							  (report-error
							   "Handin failed."
							   exn))])
					 (remember-user (send username get-value))
					 (submit-assignment
					  connection
					  (send username get-value)
					  (send passwd get-value)
					  (send assignment
						get-string
						(send assignment get-selection))
					  content
					  (lambda () 
					    (semaphore-wait commit-lock)
					    (send status set-label "Comitting...")
					    (set! committing? #t)
					    (semaphore-post commit-lock)))
					 (queue-callback
					  (lambda ()
					    (when abort-commit-dialog
					      (send abort-commit-dialog show #f))
					    (send status set-label "Handin sucessful.")
					    (set! committing? #f)
					    (send cancel set-label "Close"))))))))]
		      [style '(border)]))

      (define ok-can-enable? #f)
      (define (activate-ok)
	(send ok enable (and ok-can-enable?
			     (not (string=? "" (send username get-value)))
			     (not (string=? "" (send passwd get-value))))))

      (define cancel  (new button%
			   [label "Cancel"]
			   [parent button-panel]
			   [callback (lambda (b e)
				       (let ([go? (begin
						    (semaphore-wait commit-lock)
						    (if committing?
							(begin
							  (semaphore-post commit-lock)
							  (send abort-commit-dialog show #t)
							  continue-abort?)
							#t))])
					 (when go?
					   (custodian-shutdown-all comm-cust)
					   (show #f))))]))

      (define continue-abort? #f)
      (define abort-commit-dialog
	(let ([d (make-object dialog% "Commit in Progress")])
	  (make-object message% "The commit action is in progress." d)
	  (make-object message% "Cancelling now may or may not work." d)
	  (make-object message% "Cancel anyway?" d)
	  (let ([b (new horizontal-panel%
			[parent d]
			[stretchable-height #f]
			[alignment '(center center)])])
	    (make-object button% "Continue Commit" d (lambda (b e) (send d show #f)))
	    (make-object button% "Try to Cancel" d (lambda (b e)
						     (set! continue-abort? #t)
						     (send d show #f))))))

      (define (disable-interface)
	(send ok enable #f)
	(send username enable #f)
	(send passwd enable #f)
	(send assignment enable #f))

      (define (report-error tag exn)
	(queue-callback
	 (lambda ()
	   (custodian-shutdown-all comm-cust)
	   (send status set-label tag)
	   (disable-interface)
	   (when (is-shown?)
	     (message-box
	      "Server Error"
	      (if (exn? exn)
		  (let ([s (exn-message exn)])
		    (if (string? s)
			s
			(format "~e" s))))
	      this)))))

      (define go-sema (make-semaphore))
      (define commit-lock (make-semaphore 1))
      (define committing? #f)

      (define connection #f)

      (define comm-cust (make-custodian))
      (define comm-thread
	(parameterize ([current-custodian comm-cust])
	  (thread (lambda ()
		    (let/ec escape
		      (with-handlers ([void
				       (lambda (exn)
					 (report-error 
					  "Connection failed."
					  exn)
					 (escape))])
			(semaphore-wait go-sema)
			(let-values ([(h l) (connect)])
			  (when (null? l)
			    (error 'handin "there are no active assignments"))
			  (set! connection h)
			  (for-each (lambda (assign)
				      (send assignment append assign))
				    l)
			  (send assignment enable #t)
			  (set! ok-can-enable? #t)
			  (activate-ok)
			  (send status set-label (format "Connected securely for ~a." handin-name)))))))))
      
      (send ok enable #f)
      (send assignment enable #f)

      (semaphore-post go-sema)
      (show #t)))

  (define (manage-handin-account)
    (new
     (class dialog%
       (inherit show is-shown?)
       (super-new [label "Handin Account"]
		  [alignment '(left center)])

       (define status (new message%
			   [label (format "Manage ~a account at ~a." handin-name server)]
			   [parent this]
			   [stretchable-width #t]))

       (define (mk-txt label parent)
	 (new text-field%
	      [label label]
	      [parent parent]
	      [callback (lambda (t e) (activate-ok))]
	      [stretchable-width #t]))

       (define username (mk-txt "Username:" this))
       (send username set-value (remembered-user))
       (define new-user? (new check-box%
			      [label "New User"]
			      [parent this]
			      [callback (lambda (t e) (activate-ok))]))

       (define (mk-passwd label parent)
	 (new text-field%
	      [label label]
	      [parent parent]
	      [callback (lambda (t e) (activate-ok))]
	      [style '(single password)]
	      [stretchable-width #t]))

       (define old-user-box (new group-box-panel%
				 [label "Change Password"]
				 [parent this]))
       (define old-passwd (mk-passwd "Old:" old-user-box))
       (define new-passwd (mk-passwd "New:" old-user-box))
       (define confirm-passwd (mk-passwd "New again:" old-user-box))
       
       (define new-user-box (new group-box-panel%
				 [label "New Information"]
				 [parent this]))
       (define full-name (mk-txt "Full Name:" new-user-box))
       (define student-id (mk-txt "ID:" new-user-box))
       (define add-passwd (mk-passwd "Password:" new-user-box))
       
       (define (activate-ok)
	 (let ([new? (send new-user? get-value)]
	       [non-empty? (lambda (t)
			     (not (string=? "" (send t get-value))))])
	   (send new-user-box enable new?)
	   (send old-user-box enable (not new?))
	   (send ok enable
		 (and (non-empty? username)
		      (if new?
			  (and (non-empty? full-name)
			       (non-empty? student-id)
			       (non-empty? add-passwd))
			  (and (non-empty? old-passwd)
			       (non-empty? new-passwd)
			       (non-empty? confirm-passwd)))))))
       
       (define (report-error tag exn)
	 (queue-callback
	  (lambda ()
	    (custodian-shutdown-all comm-cust)
	    (send status set-label tag)
	    (when (is-shown?)
	      (message-box
	       "Server Error"
	       (if (exn? exn)
		   (let ([s (exn-message exn)])
		     (if (string? s)
			 s
			 (format "~e" s))))
	       this)
	      (set! comm-cust (make-custodian))
	      (send username enable #t)
	      (send new-user? enable #t)
	      (activate-ok)))))

       (define comm-cust (make-custodian))

       (define button-panel (new horizontal-pane%
				 [parent this]
				 [stretchable-height #f]))
       (make-object vertical-pane% button-panel) ; spacer
       (define ok (new button%
		       [label "Send"]
		       [parent button-panel]
		       [callback (lambda (b e)
				   (let ([new? (send new-user? get-value)])
				     (if (and (not new?)
					      (not (string=? (send new-passwd get-value)
							     (send confirm-passwd get-value))))
					 (message-box "Password Error"
						      "The \"New\" and \"New again\" passwords are not the same.")
					 (begin
					   (send ok enable #f)
					   (send username enable #f)
					   (send new-user? enable #f)
					   (send new-user-box enable #f)
					   (send old-user-box enable #f)
					   (parameterize ([current-custodian comm-cust])
					     (thread
					      (lambda ()
						(with-handlers ([void (lambda (exn)
									(report-error
									 "Update failed."
									 exn))])
						  (remember-user (send username get-value))
						  (send status set-label "Making secure connection...")
						  (let-values ([(h l) (connect)])						    
						    (send status set-label "Updating server...")
						    (if new?
							(submit-addition
							 h
							 (send username get-value)
							 (send full-name get-value)
							 (send student-id get-value)
							 (send add-passwd get-value))
							(submit-password-change
							 h
							 (send username get-value)
							 (send old-passwd get-value)
							 (send new-passwd get-value))))
						  (send status set-label "Success.")
						  (send cancel set-label "Close")))))))))]
		       [style '(border)]))
       (define cancel  (new button%
			    [label "Cancel"]
			    [parent button-panel]
			    [callback (lambda (b e)
					(custodian-shutdown-all comm-cust)
					(show #f))]))

       (activate-ok)
       (show #t))))

  (define (scale-by-half file)
    (let* ([bm (make-object bitmap% file)]
	   [w (send bm get-width)]
	   [h (send bm get-height)]
	   [bm2 (make-object bitmap% (quotient w 2) (quotient h 2))]
	   [mdc (make-object bitmap-dc% bm2)])
      (send mdc set-scale 0.5 0.5)
      (send mdc draw-bitmap bm 0 0)
      (send mdc set-bitmap #f)
      bm2))

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define phase1 void)
      (define phase2 void)
      
      (define tool-button-label
	(drscheme:unit:make-bitmap
	 "Handin"
	 (scale-by-half
	  (build-path (collection-path this-collection) "icon.png"))))

      (define (make-new-unit-frame% super%)
	(class super%
	  (inherit get-button-panel
		   get-definitions-text
		   get-interactions-text)
	  (super-instantiate ())

	  (rename [super-file-menu:between-open-and-revert
                   file-menu:between-open-and-revert])
          (define/override (file-menu:between-open-and-revert file-menu)
            (new menu-item%
		 (label (format "Manage ~a..." handin-name))
		 (parent file-menu)
		 (callback (lambda (m e) (manage-handin-account))))
            (super-file-menu:between-open-and-revert file-menu))	  
	  (define button
	    (new button% 
		 [label (tool-button-label this)]
		 [parent (get-button-panel)]
		 [callback (lambda (button evt)
			     (let ([content (let* ([base (make-object editor-stream-out-string-base%)]
						   [stream (make-object editor-stream-out% base)])
					      (write-editor-version stream base)
					      (write-editor-global-header stream)
					      (send (get-definitions-text) write-to-file stream)
					      (send (get-interactions-text) write-to-file stream)
					      (write-editor-global-footer stream)
					      (send base get-string))])
			       (new handin-frame% [parent this] [content content])))]
		 [style '(deleted)]))
	  (send (get-button-panel) change-children
		(lambda (l) (cons button l)))))
      
      (drscheme:get/extend:extend-unit-frame make-new-unit-frame% #f))))
