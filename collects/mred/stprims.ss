;;
;; $Id: stprims.ss,v 1.4 1997/07/22 18:44:19 krentel Exp krentel $
;;
;; Primitives for faking user input.
;; Buttons, Keystrokes, Menus, Mice.
;;

(unit/sig mred:test:primitives^
  
  (import 
    [wx : mred:wx^]
    [mred : mred:testable-window^]
    [mred:test : mred:test:struct^]
    [mred:test : mred:test:globals^]
    [mred:test : mred:test:run^])

  (define arg-error error)
  (define run-error error)
  
  ;;
  ;; Do one action now in single/multiple thread.
  ;;
  
  (define do-now
    (lambda (thunk-maker)
      (lambda args
	(mred:test:run-single (list (apply thunk-maker args))))))
  
  (define do-now*
    (lambda (thunk-maker)
      (lambda args
	(mred:test:run-multiple (list (apply thunk-maker args))))))
  
  ;;
  ;; Return list of window's ancestors from root down to window
  ;; (including window).  Used for pre-on-char and pre-on-event.
  ;; get-parent returns () for no parent.
  ;;

  (define ancestor-list
    (lambda (window)
      (let loop ([w window] [l null])
	(if (null? w)
	    l
	    (loop (send w get-parent) (cons w l))))))
  
  
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
  
  (define button-push-now  (do-now  button-push))
  (define button-push-now* (do-now* button-push))
  
  
  ;;
  ;; KEYSTROKES 
  ;;
  ;; Give ancestors (from root down) option of handling key event
  ;; with pre-on-char.  If none want it, then send to focused window
  ;; with (send <window> on-char <wx:key-event>).
  ;; key: char or integer
  ;;
  ;; MAYBE WANT TO CHECK WINDOW IS IN ACTIVE FRAME.
  ;; NEED TO ADD MODIFIER KEYS: CONTROL, META, ALT, SHIFT.
  ;;
  
  (define keystroke
    (let ([tag  'mred:test:keystroke])
      (lambda (key)
	(cond
	  [(not (or (char? key) (integer? key)))
	   (arg-error tag "key not char or integer")]
	  [else
	   (mred:test:make-event
	    (lambda ()
	      (let
		  ([window  (mred:test:get-focused-window)]
		   [event   (make-key-event key)])
		(cond
		  [(not window)
		   (run-error tag "no focused window")]
		  [(not (ivar-in-class? 'on-char (object-class window)))
		   (run-error tag "focused window does not have on-char")]
		  [(not (send window is-shown?))
		   (run-error tag "focused window is not shown")]
		  [else
		   (send-key-event window event)
		   (void)]))))]))))

  (define make-key-event
    (lambda (key)
      (let ([event  (make-object wx:key-event% wx:const-event-type-char)]
	    [num    (if (integer? key) key (char->integer key))])
	(send event set-key-code num)
	event)))
  
  (define send-key-event
    (lambda (window event)
      (let loop ([l  (ancestor-list window)])
	(cond
	  [(null? l)  (send window on-char event)]
	  [(send (car l) pre-on-char window event)  #f]
	  [else  (loop (cdr l))]))))
 
  (define keystroke-now  (do-now  keystroke))
  (define keystroke-now* (do-now* keystroke))
  
  
  ;;
  ;; MENU ITEMS are selected with
  ;; (send <wx:frame> command <menu-item-id>)
  ;; menu, item: strings
  ;; DOESN'T HANDLE MENU CHECKBOXES YET.
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
  
  (define menu-select-now  (do-now  menu-select))
  (define menu-select-now* (do-now* menu-select)) 
  
  
  ;;
  ;; SIMPLE MOUSE EVENTS.
  ;;
  ;; Simple left-click mouse in current canvas.
  ;; Sends 3 wx:mouse-events to canvas: motion, down, up.
  ;;
  ;; Give ancestors (from root down) option of handling mouse event
  ;; with pre-on-event.  If none want it, then send to focused window
  ;; with on-event.
  ;;
  ;; NEED TO EXPAND: DRAGGING, DOUBLE-CLICK, MOVING TO OTHER CANVASES,
  ;; MODIFIER KEYS (SHIFT, META, CONTROL, ALT).
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
		   (send-mouse-event canvas motion)
		   (send-mouse-event canvas down)
		   (send-mouse-event canvas up)
		   (void)]))))]))))
  
  (define send-mouse-event
    (lambda (window event)
      (let loop ([l  (ancestor-list window)])
	(cond
	  [(null? l)  (send window on-event event)]
	  [(send (car l) pre-on-event window event)  #f]
	  [else  (loop (cdr l))]))))

  (define mouse-click-now  (do-now  mouse-click))
  (define mouse-click-now* (do-now* mouse-click))
  
  )
