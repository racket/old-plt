#| 

This file gets REQUIREd by optionr.ss.

To debug: edit out the setting of current-exception-handler in readr.ss.

TO DO:

- correct preferences:set-default's tester
  (currently assumes won't be invalid)
- handle all (optional) fields
- missing fields have value "" -- won't always be appropriate
  [this assumption is coded into lookup-pref/prefs]
- hide password entry (on screen and in file)

|#

(module pref mzscheme

  (provide create-preferences 
	   lookup-pref/prefs
	   lookup-pref/prefs/fail-false
	   sirmail-login-pref)

  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "framework.ss" "framework")
           (lib "framework-sig.ss" "framework")
           (lib "plt-installer.ss" "setup")
           (lib "plt-installer-sig.ss" "setup")
           (lib "mred-sig.ss" "mred")
           (lib "mred.ss" "mred"))

  (define sirmail-login-pref 'sirmail:login)

  (define (make-text-field label panel updater width-num)
    (make-object text-field% label panel 
		 (lambda (x y) (updater))
		 (make-string width-num #\space)))

  (define make-directory-button
    (lambda (button-label parent val-box updater)
      (make-object button% button-label parent
		   (lambda (the-button event)
		     (let ([f (get-directory
			       (string-append "Select " button-label)
			       #f
			       (let ([v (unbox val-box)])
				 (and v 
				      (string? v)
				      (or (relative-path? v) (absolute-path? v))
				      (directory-exists? v)
				      v)))])
		       (when f
			 (set-box! val-box f)
			 (updater)))))))

  (define (make-check-box label parent updater on?)
    (let ([c (make-object check-box% label parent updater)])
      (send c set-value on?)
      c))

  (define (lookup-pref/prefs/fail-false tag prefs)
    (cadr (or (assoc tag prefs) '('dummy-tag #f))))

  (define (lookup-pref/prefs tag prefs)
    (cadr (or (assoc tag prefs) '('dummy-tag ""))))

  (define (make-mbox-preferences-panel parent)
    (letrec ([p (instantiate vertical-panel% (parent)
                  [stretchable-width #f]
                  [stretchable-height #t]
                  [alignment '(left top)])]

	     ;; update-prefs records the new preferences
	     [update-prefs
	      (lambda ()
		(preferences:set sirmail-login-pref
				 `((mail-from ,(send mail-from get-value))
				   (username ,(send username get-value))
				   (default-to-domain ,(send default-to-domain get-value))
				   (imap-server ,(send imap-server get-value))
				   (local-dir ,(unbox local-dir-box))
				   (save-sent? ,(send save-sent?-check get-value))
				   (save-sent-dir ,(unbox save-sent-dir-box))
				   )))]

	     ;; initialize-gui sets up the GUI initially;
	     ;; update-gui does the job from there on
	     [initialize-gui
	      (lambda (prefs)
		(define (lookup tag)
		  (lookup-pref/prefs tag prefs))
		(send mail-from set-value (lookup 'mail-from))
		(send username set-value (lookup 'username))
		(send imap-server set-value (lookup 'imap-server))
		;; nothing to set for local-dir
		(send save-sent?-check refresh)
		(send save-sent-dir enable (send save-sent?-check get-value))
		(send default-to-domain set-value (lookup 'default-to-domain))
		)]

	     ;; update-gui does the screen updating
             [update-gui
              (lambda (prefs)
		(send mail-from refresh)
		(send username refresh)
		(send imap-server refresh)
		(send local-dir refresh)
		(send save-sent?-check refresh)
		(send save-sent-dir refresh)
		(send default-to-domain refresh)
		)]

	     [mail-from (make-text-field "Mail From" p update-prefs 20)]
	     [username (make-text-field "Username" p update-prefs 10)]
	     [imap-server (make-text-field "IMAP Server" p update-prefs 20)]

	     [local-dir-box (box (or
				  (lookup-pref/prefs/fail-false
				   'local-dir
				   (preferences:get sirmail-login-pref))
				  (build-path (find-system-path 'home-dir)
					      "SirMail")))]
	     [local-dir
	      (make-directory-button "Local Directory" p
				     local-dir-box update-prefs)]

	     [save-sent?-check
	      (make-check-box "Save Sent Files?" p
			      (lambda (the-box event)
				(send save-sent-dir enable 
				      (send the-box get-value))
				(when (send the-box get-value)
				      (send save-sent-dir refresh))
				(update-prefs))
			      (lookup-pref/prefs/fail-false 
			       'save-sent?
			       (preferences:get sirmail-login-pref)))]
	     [save-sent-dir-box (box (or 
				      (lookup-pref/prefs/fail-false
				       'save-sent-dir
				       (preferences:get sirmail-login-pref))
				      (build-path (unbox local-dir-box)
						  "Sent")))]
	     [save-sent-dir
	      (make-directory-button "Save Sent Directory" p
				     save-sent-dir-box update-prefs)]

	     ;; other fields go here

	     [default-to-domain (make-text-field "Default To  Domain" p update-prefs 20)]
	     )

      (preferences:add-callback sirmail-login-pref
                                (lambda (name val)
                                  (update-gui val)))
      (initialize-gui (preferences:get sirmail-login-pref))
      p))

  (define (create-preferences edit-menu)
    (make-object menu-item% "Preferences" edit-menu
		 (lambda (x y) (preferences:show-dialog)))
    (preferences:add-panel "Mailbox" make-mbox-preferences-panel)
    (preferences:add-editor-checkbox-panel))

)
