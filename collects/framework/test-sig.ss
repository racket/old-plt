(module test-sig mzscheme
  (require (lib "unitsig.ss"))
  (provide framework:keys^
	   framework:test^)

  (define-signature framework:keys^
    (get-shifted-key-list))

  (define-signature framework:test^
    (run-interval
     number-pending-actions
     reraise-error
     run-one

     current-get-eventspaces
     
     close-top-level-window
     
     ;; (frame-has? f p) =
     ;;    f is a frame and it has a child (in it or a subpanel) that responds #t to p

     button-push
     #|
     ((union (lambda (str)
	       (and (string? str)
		    (frame-has? (get-top-level-focus-window)
				(lambda (x)
				  (and (is-a? x button%)
				       (string=? (send x get-label) str)
				       (send x is-enabled?)
				       (send x is-shown?))))))
	     (lambda (btn)
	       (and (is-a? btn button%)
		    (frame-has? (get-top-level-focus-window)
				(lambda (x)
				  (and (eq? x btn)
				       (send x is-enabled?)
				       (send x is-shown?)))))))
      ->
      void)
     |#

     set-radio-box!
     set-check-box!
     set-choice!
     keystroke  
     menu-select
     mouse-click
     new-window)))