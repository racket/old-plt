; SirMail: Simple Imap Reader for Mail

; There's also a mail composer in here that could be moved into a
; separate component; the only trouble is splitting up the
; configuration information.

(module sirmail mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "mred-sig.ss" "mred")
	   (lib "mred.ss" "mred"))
  
  (require "sirmails.ss"
	   "keyfunc.ss"
	   "keymap.ss"
	   "sirmailr.ss")

  (require (lib "imap-sig.ss" "net")
	   (lib "smtp-sig.ss" "net")
	   (lib "head-sig.ss" "net")
	   (lib "base64-sig.ss" "net")
	   (lib "imap.ss" "net")
	   (lib "smtp.ss" "net")
	   (lib "head.ss" "net")
	   (lib "base64.ss" "net"))

  (require (lib "hierlist-sig.ss" "hierlist")
	   (lib "hierlist.ss" "hierlist"))

  (define-values/invoke-unit/sig (install-text-functions) 
    keyfunc@
    #f
    mred^)
  (define-values/invoke-unit/sig (install-emacs-bindings)
    keymap@
    #f
    mred^)

  (define inbox-name "Inbox")
  (define default-mailbox-options null)
  
  (define prim-exit (exit-handler))
  (define exit-count 0)
  (define exit-sema (make-semaphore 1))
  (define (exit-sirmail)
    ;; Lock is because a separate process might be calling exit
    (semaphore-wait exit-sema)
    (set! exit-count (sub1 exit-count))
    (when (zero? exit-count)
      (prim-exit 0))
    (semaphore-post exit-sema))

  (define (start-new-window thunk)
    (parameterize ([current-eventspace (make-eventspace)])
      (set! exit-count (add1 exit-count))
      (queue-callback
       (lambda ()
	 (exit-handler (lambda (x) (exit-sirmail)))
	 (let ([eeh (error-escape-handler)])
	   (error-escape-handler
	    (lambda () 
	      (unless (pair? (get-top-level-windows))
		;; Didn't start up...
		(exit-sirmail))
	      (eeh))))
	 (thunk)
	 (yield (make-semaphore))
	 (exit-sirmail)))))

  (define open-mailbox
    (case-lambda
     [(mailbox-name) (open-mailbox mailbox-name default-mailbox-options)]
     [(mailbox-name mailbox-options)
      (start-new-window
       (lambda ()
	 (invoke-unit/sig
	  sirmail@
	  sirmail:environment^
	  mred^
	  net:imap^
	  net:smtp^
	  net:head^
	  net:base64^
	  hierlist^
	  (install-text-functions)
	  (install-emacs-bindings))))]))

  (define folders-window #f)

  (require "optionr.ss"
	  "folderr.ss")

  (define (open-folders-window)
    (if folders-window
	(send folders-window show #t)
	(let ([shutdown-folders-window
	       (lambda ()
		 (set! folders-window #f)
		 (exit-sirmail))]
	      [mailbox-name "INBOX"]
	      [mailbox-options default-mailbox-options])
	  (start-new-window
	   (lambda ()
	     (set! folders-window
		   (invoke-unit/sig
		    (compound-unit/sig
		     (import [env : sirmail:environment^]
			     [s : (shutdown-folders-window)]
			     [mred : mred^]
			     [imap : net:imap^]
			     [hierlist : hierlist^])
		     (link [options : sirmail:options^
				    (option@
				     env
				     imap
                                     mred)]
			   [folder : ()
				   (folder@
				    env
				    s
				    options
				    mred imap
				    hierlist)])
		     (export))
		    sirmail:environment^
		    (shutdown-folders-window)
		    mred^
		    net:imap^
		    hierlist^)))))))

  (define (get-active-folder)
    (and folders-window
	 (send folders-window get-mailbox-name)))
  
  (open-mailbox inbox-name)

  ;; afaik, this only stops the startup repl from appearing.
  ;; There doesn't seem to be any other way to do that, short
  ;; of command-line flags.
  (yield (make-semaphore)))
