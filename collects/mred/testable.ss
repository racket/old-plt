;;
;; $Id$
;;
;; Testable classes go between last wx: class and first mred: class.
;; Keep track of currently active frame and focused window.
;;
;; (mred:test:get-active-frame    => active frame or dialog-box.
;; (mred:test:get-focused-window) => window with keyboard focus.
;;
;; on-activate applies to wx:frame and wx:dialog-box.
;; on-set-focus applies to wx:window.
;;

(unit/sig mred:testable-window^
  (import [wx : mred:wx^])

  (define inactive-frame-value   #f)
  (define unfocused-window-value #f)
  
  (define current-active-frame   inactive-frame-value)
  (define current-focused-window unfocused-window-value)

  (define test:get-active-frame   (lambda () current-active-frame))
  (define test:get-focused-window (lambda () current-focused-window))
  
  (define add-get-focus
    (lambda (%)
      (class-asi %
	(rename [super-on-set-focus   on-set-focus]
		[super-on-kill-focus  on-kill-focus])
	(public
	  [on-set-focus
	    (lambda ()
	      (set! current-focused-window this)
	      (super-on-set-focus))]
	  [on-kill-focus
	    (lambda ()
	      (set! current-focused-window unfocused-window-value)
	      (super-on-kill-focus))]))))
  
  (define add-get-active
    (lambda (%)
      (class-asi %
        (rename [super-on-activate  on-activate])
        (public 
          [on-activate
            (lambda (active?)
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
  (define testable-dialog-box%   (add-get-focus (add-get-active wx:dialog-box%)))
  
  )
