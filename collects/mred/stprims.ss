;;
;; $Id: stprims.ss,v 1.1 1997/07/02 21:23:24 krentel Exp krentel $
;;
;; Primitives for faking user input.
;;

(unit/sig mred:test:primitives^
  
  (import 
    [wx : mred:wx^]
    [mred:test : mred:test:struct^]
    [mred:test : mred:test:globals^])

  (define arg-error error)
  (define run-error error)
  
  ;;
  ;; Do the action now in separate thread.
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
  ;; Menu items are selected with
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
  ;; Keystrokes are faked by
  ;; (send <canvas> on-char <wx:key-event>)
  ;; key: char or integer
  ;;
  
  (define keystroke
    (let ([tag  'keystroke])
      (opt-lambda (key [canvas #f])
	(cond
	  [(not (or (char? key) (integer? key)))
	   (arg-error tag "key not char or integer")]
	  [(and canvas (not (is-a? canvas wx:canvas%)))
	   (arg-error tag "canvas is not a wx:canvas")]
	  [else
	   (mred:test:make-event
	    (lambda ()
	      (let*
		  ([the-canvas
		    (or canvas
			(mred:test:frame->active-canvas (mred:test:top-frame)))]
		   [event
		    (make-object wx:key-event% wx:const-event-type-char)]
		   [key-num  (if (integer? key) key (char->integer key))])
		(send event set-key-code key-num)
		(send the-canvas on-char event)
		(void))))]))))
  
  (define keystroke-now (do-now keystroke))
         
  )
