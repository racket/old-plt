(module pref mzscheme

  (require (lib "class.ss")
	   (lib "framework.ss" "framework")
	   (lib "mred.ss" "mred")
	   (lib "list.ss"))

  (provide get-pref put-pref
	   show-pref-dialog
	   add-preferences-menu-items)

  (define (string-or-false? x) (or (not x) (string? x)))
  (define (ip-string? x) (and (string? x)
			      (positive? (string-length x))))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Preference Definitions                                 ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (preferences:set-default 'sirmail:mail-from "SirMail User <sirmail@plt-scheme.org>" string?)
  (preferences:set-default 'sirmail:username "username" string?)
  (preferences:set-default 'sirmail:password #f string-or-false?)
  (preferences:set-default 'sirmail:default-to-domain "plt-scheme.org" ip-string?)

  (preferences:set-default 'sirmail:imap-server "imap.plt-scheme.org" ip-string?)
  (preferences:set-default 'sirmail:smtp-server "sendmail.plt-scheme.org" ip-string?)

  (preferences:set-default 'sirmail:local-directory 
			   (build-path (find-system-path 'home-dir)
				       "SirMail")
			   (lambda (x)
			     (and (string? x)
				  (absolute-path? x))))
  (preferences:set-default 'sirmail:sent-directory 
			   (build-path (find-system-path 'home-dir)
				       "SentMail")
			   (lambda (x)
			     (or (not x)
				 (and (string? x)
				      (absolute-path? x)))))
  (preferences:set-default 'sirmail:root-mailbox-folder #f string-or-false?)

  (preferences:set-default 'sirmail:initial-sort 'id
			   (lambda (x) (memq x '(id date subject from))))
  (preferences:set-default 'sirmail:biff-delay
			   60
			   (lambda (x)
			     (or (not x)
				 (and (number? x)
				      (exact? x)
				      (integer? x)
				      (positive? x)))))
  (preferences:set-default 'sirmail:warn-download-size 32000
			   (lambda (x) (or (not x) (and (number? x) (real? x)))))
  (preferences:set-default 'sirmail:external-composer 'xemacs
			   (lambda (x) (memq x '(xemacs gnu-emacs))))
  (preferences:set-default 'sirmail:use-extenal-composer? #f boolean?)
  (preferences:set-default 'sirmail:show-urls? #t boolean?)
  (preferences:set-default 'sirmail:show-gc-icon #f boolean?)
  (preferences:set-default 'sirmail:wrap-lines #f boolean?)

  (preferences:set-default 'sirmail:aliases-file (build-path (find-system-path 'home-dir) ".sirmail.aliases")
			   (lambda (x) (or (not x)
					   (and (string? x) (absolute-path? x)))))
  (preferences:set-default 'sirmail:auto-file-table-file (build-path (find-system-path 'home-dir) ".sirmail.auto-file")
			   (lambda (x) (or (not x)
					   (and (string? x) (absolute-path? x)))))

  (preferences:set-default 'sirmail:self-addresses null
			   (lambda (x) (and (list? x) (andmap string? x))))
  (preferences:set-default 'sirmail:fields-to-show '("From" "To" "CC" "Subject" "Date" "X-Mailer")
			   (lambda (x) (and (list? x) (andmap string? x))))


  (let ([fw 560]
	[fh 600])
    (let-values ([(display-width display-height) (get-display-size)])
      (preferences:set-default 'sirmail:frame-width
			       (min display-height fh)
			       (lambda (x) (and (number? x) (<= 0 x 32768))))
      (preferences:set-default 'sirmail:frame-height 
			       (min display-width fw)
			       (lambda (x) (and (number? x) (<= 0 x 32768))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Preference Manager                                     ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define prefs-eventspace (make-eventspace))

  (define (in-preferences-eventspace thunk)
    (let ([val #f]
	  [s (make-semaphore)])
      (parameterize ([current-eventspace prefs-eventspace])
	(queue-callback
	 (lambda ()
	   (with-handlers ([void (lambda (x)
				   ;; Assume all raised values are exns
				   (set! val x))])
	     (set! val (thunk)))
	   (semaphore-post s))))
      (semaphore-wait s)
      (if (exn? val)
	  (raise val)
	  val)))

  (define (get-pref id)
    (in-preferences-eventspace (lambda ()
				 (preferences:get id))))

  (define (put-pref id val)
    (in-preferences-eventspace (lambda ()
				 (preferences:set id val))))

  (define (add-preferences-menu-items edit-menu)
    (make-object separator-menu-item% edit-menu)
    (make-object menu-item% "Preferences" edit-menu
		 (lambda (x y) (in-preferences-eventspace preferences:show-dialog))))
  
  (define (show-pref-dialog)
    (in-preferences-eventspace 
     (lambda ()
       (preferences:show-dialog)
       (yield 'wait))))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Preference Dialog                                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-text-field label panel width-num pref optional?)
    (define p0 (and optional?
		    (instantiate horizontal-panel% (panel) [stretchable-height #f])))
    (define e (and optional?
		   (make-object check-box% label p0
				(lambda (c e)
				  (let ([on? (send c get-value)])
				    (send t enable on?)
				    (preferences:set
				     pref
				     (and on?
					  (send t get-value))))))))

    (define t (make-object (class text-field% 
			     (inherit get-value)
			     (define/override (on-focus on?)
			       (unless on?
				 (preferences:set pref (get-value))))
			     (super-make-object
			      (if optional? #f label) (or p0 panel)
			      void
			      (make-string width-num #\space)))))
    (send t set-value (or (preferences:get pref) ""))
    (when e
      (send t enable (send e get-value)))
    (preferences:add-callback pref (lambda (name val)
				     (when e
				       (send e set-value val)
				       (send t enable val))
				     (when val
				       (send t set-value val)))))
	     
  (define make-file/directory-button
    (lambda (dir? button-label parent pref enabler)
      (define p0 (and enabler
		      (instantiate horizontal-panel% (parent) [stretchable-height #f])))
      (define e (and enabler
		     (make-object check-box% enabler p0
				  (lambda (c e)
				    (let ([on? (send c get-value)])
				      (send p enable on?)
				      (preferences:set
				       pref
				       (and on?
					    (send field get-value))))))))
      (define p (instantiate horizontal-panel% ((or p0 parent))
			     [stretchable-height #f]))
      (define (set-it v)
	(if (and (absolute-path? v)
		 (or (and dir? (directory-exists? v))
		     (and (not dir?) (file-exists? v))))
	    (preferences:set pref v)
	    (message-box (if dir? "Directory" "File")
			 (format "The path ~s ~a"
				 v
				 (if (absolute-path? v)
				     (if dir? 
					 (if (file-exists? v)
					     "is not a directory"
					     "does not exist")
					 (if (directory-exists? v)
					     "is not a file"
					     "does not exist"))
				     "is ill-formed"))
			 '(ok stop))))
      (define field (make-object text-field% button-label p
				 ;; For now, just counteract edits:
				 (lambda (t e)
				   (send field set-value (preferences:get pref)))
				 (or (preferences:get pref)
				     (current-directory))))
      (when e
	(send e set-value (preferences:get pref))
	(send p enable (send e get-value)))
      (preferences:add-callback pref (lambda (name val)
				       (when e
					 (send e set-value val)
					 (send p enable val))
				       (when val
					 (send field set-value val))))
      (make-object button% "Set..." p (lambda (b e)
					(let ([v ((if dir? get-directory get-file)
						  (or enabler button-label))])
					  (when v
					    (set-it v)))))
      p))

  (define (make-text-list label parent pref)
    (let ([p (make-object vertical-panel% parent)])
      (define l (make-object list-box% label (or (preferences:get pref) null) p
			     (lambda (l e)
			       (send delete enable (pair? (send l get-selections))))
			     '(multiple)))
      (define hp (instantiate horizontal-panel% (p) 
			      [stretchable-height #f]
			      [alignment '(center center)]))
      (define add (make-object button% "Add" hp (lambda (b e)
						  (let ([v (get-text-from-user (format "Add to ~a" label) ""
									       (send parent get-top-level-window))])
						    (when v
						      (send l append v)
						      (set-prefs))))))
      (define delete (make-object button% "Delete" hp (lambda (b e)
							(let ([d (send l get-selections)])
							  (for-each (lambda (i)
								      (send l delete i))
								    (quicksort d >))
							  (set-prefs)))))
      (define (set-prefs)
	(send delete enable (pair? (send l get-selections)))
	(preferences:set
	 pref
	 (let ([n (send l get-number)])
	   (let loop ([i 0])
	     (if (= i n)
		 null
		 (cons (send l get-string i)
		       (loop (add1 i))))))))
      (send delete enable #f)
      (preferences:add-callback pref (lambda (name val)
				       (send l clear)
				       (for-each (lambda (i)
						   (send l append i))
						 val)
				       (send delete enable (pair? (send l get-selections)))))))

  (define (make-addresses-preferences-panel parent)
    (let ([p (instantiate vertical-panel% (parent))])
      
      (make-text-field "Mail From" p 20 'sirmail:mail-from #f)
      (make-text-field "SMTP Server" p 20 'sirmail:smtp-server #f)

      (make-file/directory-button #t #f p
				  'sirmail:local-directory
				  "Save Sent Files")


      (make-text-field "Default To Domain" p 20 'sirmail:default-to-domain #f)
      (make-file/directory-button #f #f p
				  'sirmail:aliases-file
				  "Aliases File")

      (make-text-list "Self Addresses" p 'sirmail:self-addresses)

      p))

  (define (make-mbox-preferences-panel parent)
    (let ([p (instantiate vertical-panel% (parent))])
      
      (make-text-field "Username" p 10 'sirmail:username #f)
      (make-text-field "IMAP Server" p 20 'sirmail:imap-server #f)

	
      (make-file/directory-button #t "Local Directory" p
				  'sirmail:local-directory
				  #f)

      (make-text-field "Folder List Root" p 20 'sirmail:root-mailbox-folder #t)
		       

      (make-file/directory-button #f #f p
				  'sirmail:auto-file-table-file
				  "Auto-File Table File")

      (make-text-list "Shown Header Fields" p 'sirmail:fields-to-show)

      p))


  (in-preferences-eventspace
   (lambda ()
     (preferences:add-panel "Reading" make-mbox-preferences-panel)
     (preferences:add-panel "Sending" make-addresses-preferences-panel)
     (preferences:add-editor-checkbox-panel))))

