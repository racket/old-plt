;;
;; $Id: testable.ss,v 1.7 1998/04/29 13:46:46 robby Exp $
;;
;; Testable classes go between last wx: class and first mred: class.
;; Keep track of currently active frame and focused window.
;;
;; (mred:test:get-active-frame)   => active frame or dialog-box.
;; (mred:test:get-focused-window) => window with keyboard focus.
;;
;; on-activate applies to wx:frame and wx:dialog-box.
;; on-set-focus applies to wx:window.
;;

(unit/sig mred:testable-window^
  (import [wx : wx^])

  (define inactive-frame-value   #f)
  (define unfocused-window-value #f)
  
  (define current-active-frame   inactive-frame-value)
  (define current-focused-window unfocused-window-value)

  (define test:current-get-eventspaces (make-parameter (lambda () (list (wx:current-eventspace)))))

  (define test:get-active-frame
    (lambda ()
      (or current-active-frame
	  (let/ec k
	    (for-each (lambda (evtspace)
			(for-each
			 (lambda (window)
			   (when (send window get-active?)
			     (k window)))
			 (parameterize ([wx:current-eventspace evtspace])
			   (wx:get-frame-list))))
		      ((test:current-get-eventspaces)))
	    #f))))

  (define test:get-focused-window (lambda () (send (test:get-active-frame) get-focused-window)))
  
  (define add-get-focus
    (lambda (%)
      (class-asi %
	(rename [super-on-set-focus   on-set-focus]
		[super-on-kill-focus  on-kill-focus]
		[super-set-cursor set-cursor])
	(private
	  [get-frame
	   (lambda ()
	     (let loop ([window this])
	       (cond
		[(null? window) (error 'get-frame "no frame!")]
		[(or (is-a? window wx:frame%)
		     (is-a? window wx:dialog-box%)) window]
		[else (loop (send window get-parent))])))])
	(public
	  [on-set-focus
	    (lambda ()
	      (send (get-frame) set-focused-window this)
	      (super-on-set-focus))]
	  [on-kill-focus
	    (lambda ()
	      (send (get-frame) set-focused-window #f)
	      (super-on-kill-focus))]))))
  
  (define add-get-active
    (lambda (%)
      (class-asi %
        (rename [super-on-activate  on-activate])
	(private
	  [focused-window #f]
	  [is-active? #f])
        (public 
	  [get-focused-window
	   (lambda ()
	     focused-window)]
	  [set-focused-window
	   (lambda (window)
	     (set! focused-window window))]

	  [get-active?
	   (lambda ()
	     is-active?)]
          [on-activate
            (lambda (active?)
	      (set! is-active? active?)
              (set! current-active-frame
                (if active? this inactive-frame-value))
              (super-on-activate active?))]))))
  
  (define testable-canvas%       (add-get-focus wx:canvas%))
  (define testable-panel%        (add-get-focus wx:panel%))
  (define testable-media-canvas% (add-get-focus wx:media-canvas%))
  (define testable-text-window%  (add-get-focus wx:text-window%))
  
  (define testable-button%       (add-get-focus wx:button%))
  (define testable-check-box%    (add-get-focus wx:check-box%))
  (define testable-choice%       (add-get-focus wx:choice%))
  (define testable-gauge%        (add-get-focus wx:gauge%))
  (define testable-list-box%     (add-get-focus wx:list-box%))
  (define testable-message%      (add-get-focus wx:message%))
  (define testable-radio-box%    (add-get-focus wx:radio-box%))
  (define testable-slider%       (add-get-focus wx:slider%))
  (define testable-text%         (add-get-focus wx:text%))
  (define testable-multi-text%   (add-get-focus wx:multi-text%))
  
  (define testable-frame%        (add-get-focus (add-get-active wx:frame%)))
  (define testable-dialog-box%   (add-get-focus (add-get-active wx:dialog-box%))))
