;;
;; $Id: stprims.ss,v 1.13 1997/08/29 22:11:09 krentel Exp krentel $
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
  
  (define verify-item 
    (lambda (item valid)
      (verify-list (list item) valid)))
    
    
  ;;
  ;; BUTTONS are pushed by
  ;; (send <button> command <wx:command-event>)
  ;; button : must either explicitly supply the button
  ;;          or supply a string naming the button.
  ;; Button must be shown and in active frame.
  ;;
  
  (define button-tag 'mred:test:button-push)
  
  (define (find-button b-desc)
    (cond
      [(string? b-desc)
       (let* ([active-frame (mred:test:get-active-frame)]
	      [_ (unless active-frame
		   (run-error button-tag
			      "could not find button: ~a, no active frame" 
			      b-desc))]
	      [found
	       (let loop ([panel (send active-frame get-top-panel)])
		 (ormap (lambda (child)
			  (cond
			    [(and (is-a? child wx:button%)
				  (equal? (send child get-label) b-desc))
			     child]
			    [(is-a? child wx:panel%) (loop child)]
			    [else #f]))
			(ivar panel children)))])
	 (if found
	     found
	     (run-error button-tag "no button named ~a in active frame"
			b-desc)))]
      [(is-a? b-desc wx:button%) b-desc]
      [else (run-error button-tag "expected either a button or string as input, received: ~a"
		       b-desc)]))

  (define button-push
    (lambda (button-input)
      (mred:test:run-one
       (lambda ()
	 (let ([button (find-button button-input)])
	   (cond
	     [(not (send button is-shown?))
	      (run-error button-tag "button is not shown")]
	     [(not (in-active-frame? button))
	      (run-error button-tag "button is not in active frame")]
	     [else
	      (let ([event  (make-button-event button)])
		(send button command event)
		(void))]))))))
  
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
  
  (define mouse-tag 'mred:test:mouse-action)
  (define legal-mouse-buttons (list 'left 'middle 'right))
  (define legal-mouse-modifiers
    (list 'alt 'control 'meta 'shift 'noalt 'nocontrol 'nometa 'noshift))
  
  (define mouse-click
    (lambda (button x y . modifier-list)
      (cond 
	[(verify-item button legal-mouse-buttons)
	 => (lambda (button)
	      (run-error mouse-tag "unknown mouse button: ~s" button))]
	[(not (real? x))
	 (run-error mouse-tag "expected real, given: ~s" x)]
	[(not (real? y))
	 (run-error mouse-tag "expected real, given: ~s" y)]
	[(verify-list modifier-list legal-mouse-modifiers)
	 => (lambda (mod) 
	      (run-error mouse-tag "unknown mouse modifier: ~s" mod))]
	[else
	 (mred:test:run-one
	  (lambda ()
	    (let ([window  (mred:test:get-focused-window)])
	      (cond 
		[(not window)
		 (run-error mouse-tag "no focused window")]
		[(not (send window is-shown?))
		 (run-error mouse-tag "focused window is not shown")]
		[(not (in-active-frame? window))
		 (run-error mouse-tag "focused window is not in active frame")]
		[else
		 (let ([motion  (make-mouse-event 'motion window x y modifier-list)]
		       [down    (make-mouse-event (list button 'down) 
						  window x y modifier-list)]
		       [up      (make-mouse-event (list button 'up)
						  window x y modifier-list)])
		   (send-mouse-event window motion)
		   (send-mouse-event window down)
		   (send-mouse-event window up)
		   (void))]))))])))
	 
    
  ;; NEED TO MOVE THE CHECK FOR 'ON-EVENT TO HERE.
  
  (define send-mouse-event
    (lambda (window event)
      (let loop ([l  (ancestor-list window)])
	(cond
	  [(null? l)
	   (if (ivar-in-class? 'on-event (object-class window))
	       (send window on-event event)
	       (run-error mouse-tag "focused window does not have on-event"))]
	  [(send (car l) pre-on-event window event)  #f]
	  [else  (loop (cdr l))]))))
  
  ;;
  ;; Make mouse event.
  ;;
  
  (define make-mouse-event
    (lambda (type window x y modifier-list)
      (let ([event  (make-object wx:mouse-event% (mouse-type-const type))])
	(send event set-event-object window)
	(when (and (pair? type) (not (eq? (cadr type) 'up)))
	  (set-mouse-modifiers event (list (car type))))
	(set-mouse-modifiers event modifier-list)
	(send event set-x x)
	(send event set-y y)
	(send event set-time-stamp (time-stamp))
	event)))
  
  (define set-mouse-modifiers
    (lambda (event modifier-list)
      (unless (null? modifier-list)
	(let ([mod  (car modifier-list)])
	  (cond
	    [(eq? mod 'alt)        (send event set-alt-down     #t)]
	    [(eq? mod 'control)    (send event set-control-down #t)]
	    [(eq? mod 'meta)       (send event set-meta-down    #t)]
	    [(eq? mod 'shift)      (send event set-shift-down   #t)]
	    [(eq? mod 'left)       (send event set-left-down    #t)]
	    [(eq? mod 'middle)     (send event set-middle-down  #t)]
	    [(eq? mod 'right)      (send event set-right-down   #t)]
	    [(eq? mod 'noalt)      (send event set-alt-down     #f)]
	    [(eq? mod 'nocontrol)  (send event set-control-down #f)]
	    [(eq? mod 'nometa)     (send event set-meta-down    #f)]
	    [(eq? mod 'noshift)    (send event set-shift-down   #f)]
	    [else  (run-error mouse-tag "unknown mouse modifier: ~s" mod)]))
	(set-mouse-modifiers event (cdr modifier-list)))))
      
  (define mouse-type-const
    (lambda (type)
      (cond
	[(symbol? type)
	 (cond
	   [(eq? type 'motion)  wx:const-event-type-motion]
	   [(eq? type 'enter)   wx:const-event-type-enter-window]
	   [(eq? type 'leave)   wx:const-event-type-leave-window]
	   [else  (bad-mouse-type type)])]
	[(and (pair? type) (pair? (cdr type)))
	 (let ([button (car type)] [action (cadr type)])
	   (cond
	     [(eq? button 'left)
	      (cond 
		[(eq? action 'down)    wx:const-event-type-left-down]
		[(eq? action 'up)      wx:const-event-type-left-up]
		[(eq? action 'dclick)  wx:const-event-type-left-dclick]
		[else  (bad-mouse-type type)])]
	     [(eq? button 'middle)
	      (cond
		[(eq? action 'down)    wx:const-event-type-middle-down]
		[(eq? action 'up)      wx:const-event-type-middle-up]
		[(eq? action 'dclick)  wx:const-event-type-middle-dclick]
		[else  (bad-mouse-type type)])]
	     [(eq? button 'right)
	      (cond
		[(eq? action 'down)    wx:const-event-type-right-down]
		[(eq? action 'up)      wx:const-event-type-right-up]
		[(eq? action 'dclick)  wx:const-event-type-right-dclick]
		[else  (bad-mouse-type type)])]
	     [else  (bad-mouse-type type)]))]
	[else  (bad-mouse-type type)])))
  
  (define bad-mouse-type
    (lambda (type)
      (run-error mouse-tag "unknown mouse event type: ~s" type)))

  
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
