
(module optionr mzscheme
  (require (lib "unitsig.ss"))

  (require (lib "imap-sig.ss" "net")
           (lib "mred-sig.ss" "mred")
	   (lib "framework.ss" "framework"))

  (require "sirmails.ss")

  (require "pref.ss")

  (provide option@)
  (define option@
    (unit/sig sirmail:options^
      (import sirmail:environment^
	      net:imap^
              mred^)

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Preferences                                            ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define prefs (with-input-from-file
			(build-path (find-system-path 'pref-dir)
				    ".sirmail")
		      read))

      (define (get-pref key)
	(let ([s (assoc key mailbox-options)])
	  (if s
	      (cadr s)
	      (let ([s (assoc key prefs)])
		(if s
		    (cadr s)
		    (error 'sirmail "can't find preference: ~a" key))))))

      (define (get-optional-pref key)
	(with-handlers ([void (lambda (x) #f)])
	  (get-pref key)))

      (define (pref-prefs) (preferences:get sirmail-login-pref))
      (define (MAIL-FROM) (lookup-pref/prefs 'mail-from (pref-prefs)))
      (define (USERNAME) (lookup-pref/prefs 'username (pref-prefs)))
      (define (DEFAULT-DOMAIN) (lookup-pref/prefs 'default-to-domain (pref-prefs)))
      (define (IMAP-SERVER) (lookup-pref/prefs 'imap-server (pref-prefs)))
      (define (LOCAL-DIR)
	(or (lookup-pref/prefs/fail-false 'local-dir (pref-prefs))
	    (build-path (find-system-path 'home-dir) "SirMail")))
      (define (SAVE-SENT)
	(if (lookup-pref/prefs/fail-false 'save-sent? (pref-prefs))
	    (lookup-pref/prefs 'save-sent-dir (pref-prefs))
	    #f))

      (define PASSWORD (get-optional-pref 'password))
      (define (get-PASSWORD) PASSWORD)
      (define (set-PASSWORD p) (set! PASSWORD p))

      (define SMTP-SERVERS (let ([many (get-optional-pref 'smtp-servers)])
			     (if (and (list? many)
				      (pair? many))
				 many
				 (list (get-pref 'smtp-server)))))
      (define ALIASES (let ([f (get-optional-pref 'aliases-file)]
			    [aliases (or (get-optional-pref 'aliases) null)])
			(if f
			    (append (with-input-from-file f read) aliases)
			    aliases)))
      (define SELF-ADDRESSES (get-optional-pref 'self-addresses))
      (define AUTO-FILE-TABLE (get-optional-pref 'auto-file))

      (define BIFF-DELAY (get-optional-pref 'biff-delay-seconds))

      (define SORT (get-optional-pref 'sort-by))

      (define MESSAGE-FIELDS-TO-SHOW (or (get-optional-pref 'message-fields)
					 '("From" "To" "CC" "Subject" "Date" "X-Mailer")))

      (define ROOT-MAILBOX-FOR-LIST (get-optional-pref 'root-mailbox-folder))

      (define USE-EXTERNAL-COMPOSER? (get-optional-pref 'use-external-composer?))

      (let ([n (get-optional-pref 'imap-server-port)])
	(when n
	  (imap-port-number n)))

      (define WARN-DOWNLOAD-SIZE 32000)
      
      (define SHOW-URLS 
        (let ([pref-value (get-optional-pref 'show-urls)])
          (case pref-value
            [(#f) #t]
            [(on) #t]
            [(off) #f]
            [else 
             (message-box 
              "SirMail"
              (format "WARNING: uknown value for show-urls pref: ~e, defaulting to on" pref-value))
             #t]))))))
