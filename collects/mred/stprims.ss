;;
;; $Id: stprims.ss,v 1.2 1997/07/06 20:21:26 krentel Exp krentel $
;;
;; Primitives for faking user input.
;; Buttons, Keystrokes, Menus, Mice.
;;

(unit/sig mred:test:primitives^
  
  (import 
    [wx : mred:wx^]
    [mred : mred:testable-window^]
    [mred:test : mred:test:struct^]
    [mred:test : mred:test:globals^])

  (define arg-error error)
  (define run-error error)
  
  ;;
  ;; Do the action now in separate thread.
  ;; SWITCH THIS OVER TO RUN-SINGLE OR RUN-MULTIPLE.
  ;;
  
  (define do-now
    (lambda (thunk-maker)
      (lambda args
	(let* ([thunk  (mred:test:event-thunk (apply thunk-maker args))]
	       [done   (make-semaphore 0)]
	       [ans    (void)])
	  (thread
	   (lambda ()
	     (set! ans (thunk))
	     (semaphore-post done)))
	  (semaphore-wait done)
	  ans))))
  
  ;;
  ;; BUTTONS are pushed by
  ;; (send <button> command <wx:command-event>)
  ;; button : must explicitly supply the button.
  ;;
  ;; NEED TO CHECK: BUTTON IS SHOWN AND IN ACTIVE-FRAME.
  ;;
  
  (define button-push
    (let ([tag  'button-push])
      (lambda (button)
	(cond
	  [(not (is-a? button wx:button%))
	   (arg-error tag "button is not a wx:button")]
	  [else
	   (mred:test:make-event
	    (lambda ()
	      (let ([event  (make-object wx:command-event% 
					 wx:const-event-type-button-command)])
		(send button command event)
		(void))))]))))
  
  (define button-push-now (do-now button-push))
  
  ;;
  ;; MENU ITEMS are selected with
  ;; (send <wx:frame> command <menu-item-id>)
  ;; menu, item: strings
  ;;
  
  (define menu-select
    (let ([tag  'menu-select])
      (lambda (menu item)
	(cond
	  [(not (string? menu))
	   (arg-error tag "invalid menu")]
	  [(not (string? item))
	   (arg-error tag "invalid menu item")]
	  [else
	   (mred:test:make-event
	    (lambda ()
	      (let*
		  ([frame     (mred:test:top-frame)]
		   [menu-bar  (mred:test:frame->menu-bar frame)]
		   [item-id   (mred:test:menu-bar->item-id menu-bar menu item)])
		(send frame command item-id)
		(void))))]))))
  
  (define menu-select-now (do-now menu-select))
  
  ;;
  ;; KEYSTROKES are sent to the currently-focused window.
  ;; (send <window> on-char <wx:key-event>)
  ;; key: char or integer
  ;; MAYBE WANT TO CHECK WINDOW IS IN ACTIVE FRAME.
  ;; DOESN'T HANDLE MENU CHECKBOXES YET.
  ;;
  
  (define keystroke
    (let ([tag  'keystroke])
      (lambda (key)
	(cond
	  [(not (or (char? key) (integer? key)))
	   (arg-error tag "key not char or integer")]
	  [else
	   (mred:test:make-event
	    (lambda ()
	      (let
		  ([window   (mred:test:get-focused-window)]
		   [key-num  (if (integer? key) key (char->integer key))]
		   [event    (make-object wx:key-event% 
					  wx:const-event-type-char)])
		(send event set-key-code key-num)
		(cond
		  [(not (ivar-in-class? 'on-char (object-class window)))
		   (run-error tag "focused window does not have on-char")]
		  [(not (send window is-shown?))
		   (run-error tag "focused window is not shown")]
		  [else
		   (send window on-char event)
		   (void)]))))]))))
  
  (define keystroke-now (do-now keystroke))
  
  ;;
  ;; SIMPLE MOUSE ACTIONS.  NEED TO EXPAND THIS LATER.
  ;; Left-click mouse in current canvas.
  ;; Send 3 wx:mouse-events to the canvas: motion, down, up.
  ;;
  
  (define mouse-click
    (let ([tag  'mouse-click])
      (lambda (x y)
	(cond
	  [(not (and (real? x) (real? y)))
	   (arg-error tag "x, y must be reals.")]
	  [else
	   (mred:test:make-event
	    (lambda ()
	      (let
		  ([canvas  (mred:test:get-focused-window)]
		   [motion  (make-object wx:mouse-event% wx:const-event-type-motion)]
		   [down    (make-object wx:mouse-event% wx:const-event-type-left-down)]
		   [up      (make-object wx:mouse-event% wx:const-event-type-left-up)])
		(send motion set-x x)  (send motion set-y y)
		(send down   set-x x)  (send down   set-y y)
		(send up     set-x x)  (send up     set-y y)
		(cond
		  [(not (is-a? canvas wx:canvas%))
		   (run-error tag "focused window is not canvas")]
		  [(not (send canvas is-shown?))
		   (run-error tag "canvas is not shown")]
		  [else
		   (send canvas on-event motion)
		   (send canvas on-event down)
		   (send canvas on-event up)
		   (void)]))))]))))

  (define mouse-click-now (do-now mouse-click))
  
  )
