#| TO DO:

- correct preferences:set-default's tester
  (currently assumes won't be invalid)
- handle all (optional) fields
- missing fields have value "" -- won't always be appropriate
  [this assumption is coded into lookup-pref/prefs]
- hide password entry (on screen and in file)

|#

(module pref mzscheme

  (provide create-preferences lookup-pref/prefs sirmail-login-pref)

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
				   (imap-server ,(send imap-server get-value)))))]

	     ;; update-gui does the screen updating
             [update-gui
              (lambda (prefs)
		(define (lookup tag)
		  (lookup-pref/prefs tag prefs))
		(send mail-from set-value (lookup 'mail-from))
		(send username set-value (lookup 'username))
		(send default-to-domain set-value (lookup 'default-to-domain))
		(send imap-server set-value (lookup 'imap-server)))]

	     [mail-from (make-text-field "Mail From" p update-prefs 20)]
	     [username (make-text-field "Username" p update-prefs 10)]
	     [default-to-domain (make-text-field "Default To  Domain" p update-prefs 20)]
	     [imap-server (make-text-field "IMAP Server" p update-prefs 20)]
	     ;; other fields go here
	     )

      (preferences:add-callback sirmail-login-pref
                                (lambda (name val)
                                  (update-gui val)))
      (update-gui (preferences:get sirmail-login-pref))
      p))

  (define (create-preferences edit-menu)
    (make-object menu-item% "Preferences" edit-menu
		 (lambda (x y) (preferences:show-dialog)))
    (preferences:add-editor-checkbox-panel)
    (preferences:add-panel "Mailbox" make-mbox-preferences-panel))

)
