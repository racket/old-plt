;;
;; $Id: test-prims.ss,v 1.3 1998/11/19 21:24:36 robby Exp $
;;
;; Primitives for faking user input.
;; Buttons, Keystrokes, Menus, Mice.
;;

;; originally by Mark Krentel
;; modified by Paul Steckler, Robby Findler

(unit/sig framework:test:primitives^
  
  (import [mred : mred-interfaces^]
	  [keys : framework:keys^]
	  [test : framework:test:run^])
  
  (define current-eventspaces
    (make-parameter (lambda () (list (mred:current-eventspace)))))

  (define (get-active-frame)
    (ormap (lambda (eventspace)
	     (parameterize ([mred:current-eventspace eventspace])
	       (mred:get-top-level-focus-window)))
	   ((current-eventspaces))))

  (define (get-focused-window)
    (let ([f (get-active-frame)])
      (and f
	   (send f get-focus-window))))

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
      (let ([frame  (get-active-frame)])
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
    
;;;
;;; find-object obj-class b-desc 
;;; returns an object belonging to obj-class, where b-desc
;;; is either an object, or a string
;;;

  (define object-tag 'test:find-object)

  (define (find-object obj-class b-desc)
    (lambda ()
      (cond
	[(string? b-desc)
	 (let* ([active-frame (get-active-frame)]
		[_ (unless active-frame
		     (run-error object-tag
				"could not find object: ~a, no active frame" 
				b-desc))]
		[found
		 (let loop ([panel active-frame])
		   (ormap (lambda (child)
			    (cond
			      [(and (is-a? child obj-class)
				    (equal? (send child get-label) b-desc))
			       child]
			      [(is-a? child mred:area-container<%>) (loop child)]
			      [else #f]))
			  (send panel get-children)))])
	   (if found
	       found
	       (run-error object-tag 
			  "no object of class ~a named ~s in active frame"
			  obj-class
			  b-desc)))]
	[(is-a? b-desc obj-class) b-desc]
	[else (run-error 
	       object-tag
	       "expected either a string or an object of class ~a as input, received: ~a"
	       obj-class b-desc)])))

;;; CONTROL functions, to be specialized for individual controls 

  (define control-action
    (lambda (error-tag event-sym find-ctrl update-control)
      (test:run-one
       (lambda ()
	 (let ([event (make-object mred:control-event% event-sym)]
	       [ctrl (find-ctrl)])
	   (cond
	     [(not (send ctrl is-shown?))
	      (run-error error-tag "control ~s is not shown" ctrl)]
	     [(not (in-active-frame? ctrl))
	      (run-error error-tag "control ~s is not in active frame" ctrl)]
	     [else
	      (update-control ctrl)
	      (send ctrl command event)
	      (void)]))))))	     

  ;;
  ;; BUTTON
  ;;

  (define (button-push button)
    (control-action
     'test:button-push
     'button
     (find-object mred:button% button)
     void))

;; 
;; CHECK-BOX 
;;

  (define (set-check-box! in-cb state) 
    (control-action
     'test:set-check-box!
     'check-box 
     (find-object mred:check-box% in-cb)
     (lambda (cb) (send cb set-value state))))

;;; CHOICE 

; set-choice! : ((instance in-choice%) (union string number) -> void)
  (define (set-choice! in-choice str)
    (control-action
     'test:set-choice!
     'choice
     (find-object mred:choice% in-choice)
     (lambda (choice)
       (cond
	 [(number? str) (send choice set-selection str)]
	 [(string? str) (send choice set-string-selection str)]
	 [else (error 'test:set-choice!
		      "expected a string or a number as second arg, got: ~e (other arg: ~e)"
		      str in-choice)]))))
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
  ;; Window must be shown, in active frame, and either the window has
  ;; on-char, or else some ancestor must grab key with pre-on-char.
  ;;
  
  (define key-tag 'test:keystroke)
  (define legal-keystroke-modifiers
    (list 'alt 'control 'meta 'shift 'noalt 'nocontrol 'nometa 'noshift))

  (define valid-key-symbols
    (list 'start 'cancel 'clear 'shift 'control 'menu 'pause 'capital
	  'prior 'next 'end 'home 'left 'up 'right 'down 'select 'print
	  'execute 'snapshot 'insert 'help 'numpad0 'numpad1 'numpad2
	  'numpad3 'numpad4 'numpad5 'numpad6 'numpad7 'numpad8 'numpad9
	  'multiply 'add 'separator 'subtract 'decimal 'divide 'f1 'f2 'f3
	  'f4 'f5 'f6 'f7 'f8 'f9 'f10 'f11 'f12 'f13 'f14 'f15 'f16 'f17
	  'f18 'f19 'f20 'f21 'f22 'f23 'f24 'numlock 'scroll))

  (define (keystroke key . modifier-list)
    (cond
      [(not (or (char? key) (memq key valid-key-symbols)))
       (arg-error key-tag "expects char or valid key symbol, given: ~s" key)]
      [(verify-list  modifier-list  legal-keystroke-modifiers)
       => (lambda (mod) (arg-error key-tag "unknown key modifier: ~s" mod))]
      [else
       (test:run-one
	(lambda ()
	  (let ([window (get-focused-window)])
	    (cond
	      [(not window)
	       (run-error key-tag "no focused window")]
	      [(not (send window is-shown?))
	       (run-error key-tag "focused window is not shown")]
	      [(not (in-active-frame? window))
	       (run-error key-tag "focused window is not in active frame")]
	      [else
	       (let ([event (make-key-event key window modifier-list)])
		 (send-key-event window event)
		 (void))]))))]))
  
  ;; delay test for on-char until all ancestors decline pre-on-char.

  (define (send-key-event window event)
    (let loop ([l (ancestor-list window)])
      (cond [(null? l) 
	     (if (ivar-in-class? 'on-char (object-class window))
		 (send window on-char event)
		 (run-error key-tag "focused window does not have on-char"))]
	    [(send (car l) pre-on-char window event) #f]
	    [else (loop (cdr l))])))
  
  ;; Make full mred:key-event% object.
  ;; Shift is determined implicitly from key-code.
  ;; Alt, Meta, Control come from modifier-list.
  ;; get-alt-down, etc are #f unless explicitly set to #t.
  ;; WILL WANT TO ADD SET-POSITION WHEN THAT GETS IMPLEMENTED.
  
  (define make-key-event
    (lambda (key window modifier-list)
      (let ([event (make-object mred:key-event%)])
	(send event set-key-code key)
	(send event set-event-object window)
	(send event set-time-stamp (time-stamp))
	(set-key-modifiers event key modifier-list)
	event)))
  
  (define set-key-modifiers
    (lambda (event key modifier-list)
      (when (shifted? key) (send event set-shift-down #t))
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
  
  ;; A-Z and keymap:get-shifted-key-list are implicitly shifted.
  ;; keymap:get-shifted-key-list is list of strings of length 1.
  ;; vector-ref is faster than member.
  
  (define shifted?
    (let* ([letters  (list  #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M 
			    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)]
	   [all #f])
      (lambda (key)
	(unless all
	  (set! all (append letters (map (lambda (s) (string-ref s 0)) (keys:get-shifted-key-list)))))
	(memq key all))))
    
  ;;
  ;; MENU ITEMS 
  ;;
  ;; Select menu item with: 
  ;;   (send <frame> command <menu-item-id>)
  ;; menu, item: strings
  ;;
  ;; DOESN'T HANDLE MENU CHECKBOXES YET.
  ;;
  
  (define menu-tag 'test:menu-select)
  
  (define menu-select
    (lambda (menu item)
      (cond
	[(not (string? menu))
	 (arg-error menu-tag "expects string, given: ~s" menu)]
	[(not (string? item))
	 (arg-error menu-tag "expects string, given: ~s" item)]
	[else
	 (test:run-one
	  (lambda ()
	    (let* ([frame    (get-active-frame)]
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
  ;; Sends 3 mouse-events to canvas: motion, down, up.
  ;;
  ;; Give ancestors (from root down) option of handling mouse event
  ;; with pre-on-event.  If none want it, then send to focused window
  ;; with on-event.
  ;;
  ;; NEED TO EXPAND: DRAGGING, DOUBLE-CLICK, MOVING TO OTHER CANVASES,
  ;; MODIFIER KEYS (SHIFT, META, CONTROL, ALT).
  ;; 
  
  (define mouse-tag 'test:mouse-action)
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
	 (test:run-one
	  (lambda ()
	    (let ([window  (get-focused-window)])
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
      (let ([event (make-object mred:mouse-event% (mouse-type-const type))])
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
	   [(eq? type 'motion)  'motion]
	   [(eq? type 'enter)   'enter]
	   [(eq? type 'leave)   'leave]
	   [else  (bad-mouse-type type)])]
	[(and (pair? type) (pair? (cdr type)))
	 (let ([button (car type)] [action (cadr type)])
	   (cond
	     [(eq? button 'left)
	      (cond 
		[(eq? action 'down)    'left-down]
		[(eq? action 'up)      'left-up]
		[(eq? action 'dclick)  'left-dclick]
		[else  (bad-mouse-type type)])]
	     [(eq? button 'middle)
	      (cond
		[(eq? action 'down)    'middle-down]
		[(eq? action 'up)      'middle-up]
		[(eq? action 'dclick)  'middle-dclick]
		[else  (bad-mouse-type type)])]
	     [(eq? button 'right)
	      (cond
		[(eq? action 'down)    'right-down]
		[(eq? action 'up)      'right-up]
		[(eq? action 'dclick)  'right-dclick]
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
    (let ([tag  'test:new-window])
      (lambda (new-window)
	(cond
	  [(not (is-a? new-window mred:top-level-window<%>))
	   (arg-error tag "new-window is not a mred:top-level-window<%>")]
	  [else
	   (test:run-one
	    (lambda ()
	      (let
		  ([old-window  (get-focused-window)]
		   [leave   (make-object mred:mouse-event% 'leave)]
		   [enter   (make-object mred:mouse-event% 'enter)]
		   [root    (car (ancestor-list new-window))])
		(send leave  set-x 0)   (send leave  set-y 0)
		(send enter  set-x 0)   (send enter  set-y 0)
		
		;; SOME KLUDGES HERE TO WORK AROUND TEXT% PROBLEMS.
		
		(when (and old-window (ivar-in-class? 'on-event (object-class old-window)))
		  (send-mouse-event old-window leave))
		(send root show #t)
		(when (ivar-in-class? 'on-event (object-class new-window))
		  (send-mouse-event new-window enter))
		(send new-window set-focus)
		(void))))])))))
