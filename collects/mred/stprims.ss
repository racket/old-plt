;;
;; $Id: stprims.ss,v 1.12 1997/08/19 21:59:26 krentel Exp krentel $
;;
;; Primitives for faking user input.
;; Buttons, Keystrokes, Menus, Mice.
;;

(unit/sig mred:test:primitives^
  
  (import 
    [wx        : mred:wx^]
    [mred      : mred:testable-window^]
    [mred      : mred:keymap^]
    [mred:test : mred:test:run^])

  (define arg-error error)  ;; naive error handling (for now).
  (define run-error error)
  
  (define time-stamp current-milliseconds)
  
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
  ;; Returns #t if window is in active-frame, else #f.
  ;; get-parent returns () for no parent.
  ;;
  
  (define in-active-frame?
    (lambda (window)
      (let ([frame  (mred:test:get-active-frame)])
	(let loop ([window  window])
	  (cond [(null? window)      #f]
		[(eq? window frame)  #t]
		[else  (loop (send window get-parent))])))))
  
  ;;
  ;; Verify modifier list.
  ;; l, valid : lists of symbols.
  ;; returns first item in l *not* in valid, or else #f.
  ;;
  
  (define verify-list
    (lambda (l  valid)
      (cond [(null? l)  #f]
	    [(member (car l) valid)  (verify-list (cdr l) valid)]
	    [else  (car l)])))
    
    
  ;;
  ;; BUTTONS are pushed by
  ;; (send <button> command <wx:command-event>)
  ;; button : must explicitly supply the button.
  ;;
  ;; Button must be shown and in active frame.
  ;;
  
  (define button-tag 'mred:test:button-push)
  
  (define button-push
    (lambda (button)
      (cond
	[(not (is-a? button wx:button%))
	 (arg-error button-tag "expected wx:button, given: ~s" button)]
	[else
	 (mred:test:run-one
	  (lambda ()
	    (cond
	      [(not (send button is-shown?))
	       (run-error button-tag "button is not shown")]
	      [(not (in-active-frame? button))
	       (run-error button-tag "button is not in active frame")]
	      [else
	       (let ([event  (make-button-event button)])
		 (send button command event)
		 (void))])))])))
  
  (define make-button-event
    (lambda (button)
      (let* ([type   wx:const-event-type-button-command]
	     [event  (make-object wx:command-event% type)])
	(send event set-event-object button)
	event)))

  
  ;;
  ;; KEYSTROKES 
  ;;
  ;; Give ancestors (from root down) option of handling key event
  ;; with pre-on-char.  If none want it, then send to focused window
  ;; with (send <window> on-char <wx:key-event>).
  ;;
  ;; key: char or integer.
  ;; optional modifiers: 'alt, 'control, 'meta, 'shift, 
  ;;   'noalt, 'nocontrol, 'nometa, 'noshift.
  ;;
  ;; Keystrokes apply to window with keyboard focus (testable classes).
  ;; Window must be shown, in active frame, and either the window has
  ;; on-char, or else some ancestor must grab key with pre-on-char.
  ;;
  
  (define key-tag 'mred:test:keystroke)
  (define legal-keystroke-modifiers
    (list 'alt 'control 'meta 'shift 'noalt 'nocontrol 'nometa 'noshift))
  
  (define keystroke
    (lambda (key . modifier-list)
      (cond
	[(not (or (char? key) (integer? key)))
	 (arg-error key-tag "expects char or integer, given: ~s" key)]
	[(verify-list  modifier-list  legal-keystroke-modifiers)
	 => (lambda (mod) (arg-error key-tag "unknown key modifier: ~s" mod))]
	[else
	 (mred:test:run-one
	  (lambda ()
	    (let ([window  (mred:test:get-focused-window)])
	      (cond
		[(not window)
		 (run-error key-tag "no focused window")]
		[(not (send window is-shown?))
		 (run-error key-tag "focused window is not shown")]
		[(not (in-active-frame? window))
		 (run-error key-tag "focused window is not in active frame")]
		[else
		 (let ([event  (make-key-event key window modifier-list)])
		   (send-key-event window event)
		   (void))]))))])))
  
  ;; delay test for on-char until all ancestors decline pre-on-char.

  (define send-key-event
    (lambda (window event)
      (let loop ([l  (ancestor-list window)])
	(cond [(null? l) 
	       (if (ivar-in-class? 'on-char (object-class window))
		   (send window on-char event)
		   (run-error key-tag "focused window does not have on-char"))]
	      [(send (car l) pre-on-char window event)  #f]
	      [else  (loop (cdr l))]))))
  
  ;; Make full wx:key-event% object.
  ;; Shift is determined implicitly from key-code.
  ;; Alt, Meta, Control come from modifier-list.
  ;; get-alt-down, etc are #f unless explicitly set to #t.
  ;; WILL WANT TO ADD SET-POSITION WHEN THAT GETS IMPLEMENTED.
  
  (define make-key-event
    (lambda (key window modifier-list)
      (let ([event  (make-object wx:key-event% wx:const-event-type-char)]
	    [int    (if (integer? key) key (char->integer key))])
	(send event set-key-code int)
	(send event set-event-object window)
	(send event set-time-stamp (time-stamp))
	(set-key-modifiers event int modifier-list)
	event)))
  
  (define set-key-modifiers
    (lambda (event int modifier-list)
      (when (shifted? int) (send event set-shift-down #t))
      (let loop ([l  modifier-list])
	(unless (null? l)
	  (let ([mod  (car l)])
	    (cond
	      [(eq? mod 'alt)        (send event set-alt-down     #t)]
	      [(eq? mod 'control)    (send event set-control-down #t)]
	      [(eq? mod 'meta)       (send event set-meta-down    #t)]
	      [(eq? mod 'shift)      (send event set-shift-down   #t)]
	      [(eq? mod 'noalt)      (send event set-alt-down     #f)]
	      [(eq? mod 'nocontrol)  (send event set-control-down #f)]
	      [(eq? mod 'nometa)     (send event set-meta-down    #f)]
	      [(eq? mod 'noshift)    (send event set-shift-down   #f)]
	      [else  (run-error key-tag "unknown key modifier: ~s" mod)])
	    (loop (cdr l)))))))
  
  ;; A-Z and mred:shifted-key-list are implicitly shifted.
  ;; mred:shifted-key-list is list of strings of length 1.
  ;; vector-ref is faster than member.
  
  (define shifted?
    (let* 
	([ascii-size  256]
	 [keys     (make-vector ascii-size #f)]
	 [letters  (list  "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" 
			  "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")]
	 [set-shifted
	  (lambda (str) 
	    (vector-set! keys (char->integer (string-ref str 0)) #t))])
      (for-each set-shifted letters)
      (for-each set-shifted mred:shifted-key-list)
      (lambda (int) (and (< 0 int ascii-size) (vector-ref keys int)))))
    
  
  ;;
  ;; MENU ITEMS 
  ;;
  ;; Select menu item with: 
  ;;   (send <wx:frame> command <menu-item-id>)
  ;; menu, item: strings
  ;;
  ;; DOESN'T HANDLE MENU CHECKBOXES YET.
  ;;
  
  (define menu-tag 'mred:test:menu-select)
  
  (define menu-select
    (lambda (menu item)
      (cond
	[(not (string? menu))
	 (arg-error menu-tag "expects string, given: ~s" menu)]
	[(not (string? item))
	 (arg-error menu-tag "expects string, given: ~s" item)]
	[else
	 (mred:test:run-one
	  (lambda ()
	    (let* ([frame    (mred:test:get-active-frame)]
		   [item-id  (menu-item-id frame menu item)])
	      (send frame command item-id))))])))

  ;; get-active-frame => #f for no active frame.
  ;; get-menu-bar     => () for no menu bar.
  ;; find-menu-item   => -1 for no such item.
  ;; dialog boxes don't have menu-bars or get-menu-bar method.

  (define menu-item-id
    (lambda (frame menu item)
      (cond
	[(not frame)
	 (run-error menu-tag "no active frame")]
	[(not (ivar-in-class? 'get-menu-bar (object-class frame)))
	 (run-error menu-tag "active frame does not have menu bar")]
	[else
	 (let ([menu-bar  (send frame get-menu-bar)])
	   (if (null? menu-bar)
	       (run-error menu-tag "active frame does not have menu bar")
	       (let ([item-id  (send menu-bar find-menu-item menu item)])
		 (if (= item-id -1)
		     (run-error menu-tag "menu ~s does not contain item ~s" menu item)
		     item-id))))])))
  
  
  ;;
  ;; SIMPLE MOUSE EVENTS
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
    (let ([tag  'mred:test:mouse-click])
      (lambda (x y)
	(cond
	  [(not (and (real? x) (real? y)))
	   (arg-error tag "x, y must be reals.")]
	  [else
	   (mred:test:run-one
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
  
  ;; NEED TO MOVE THE CHECK FOR 'ON-EVENT TO HERE.
  
  (define send-mouse-event
    (lambda (window event)
      (let loop ([l  (ancestor-list window)])
	(cond
	  [(null? l)  (send window on-event event)]
	  [(send (car l) pre-on-event window event)  #f]
	  [else  (loop (cdr l))]))))
  
  
  ;;
  ;; Move mouse to new window.
  ;; Implement with three events:
  ;; leave old window, show top-level frame, enter new window, set-focus.
  ;;
  ;; NEED TO CLEAN UP ACTIONS FOR MOVING TO NEW FRAME.
  ;;
  
  (define new-window
    (let ([tag  'mred:test:new-window])
      (lambda (new-window)
	(cond
	  [(not (is-a? new-window wx:window%))
	   (arg-error tag "new-window is not a wx:window")]
	  [else
	   (mred:test:run-one
	    (lambda ()
	      (let
		  ([old-window  (mred:test:get-focused-window)]
		   [leave   (make-object wx:mouse-event% wx:const-event-type-leave-window)]
		   [enter   (make-object wx:mouse-event% wx:const-event-type-enter-window)]
		   [root    (car (ancestor-list new-window))])
		(send leave  set-x 0)   (send leave  set-y 0)
		(send enter  set-x 0)   (send enter  set-y 0)
		
		;; SOME KLUDGES HERE TO WORK AROUND WX:TEXT% PROBLEMS.
		
		(when (and old-window (ivar-in-class? 'on-event (object-class old-window)))
		  (send-mouse-event old-window leave))
		(send root show #t)
		(when (ivar-in-class? 'on-event (object-class new-window))
		  (send-mouse-event new-window enter))
		(send new-window set-focus)
		(void))))]))))
  
  )
