;;
;; $Id$
;;
;; Primitives for faking user input.
;;

(unit/sig mred:test:primitives^
  
  (import 
    [wx : mred:wx^]
    [mred:test : mred:test:global^]
    [mred:test : mred:test:active-frame^])

  (define arg-error error)
  (define run-error error)
  
  (define no-frame? not)
  (define no-menu-bar? null?)
  (define no-item? (lambda (x) (= x -1)))
    
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
  ;;
  
  (define menu-select
    (let ([tag  'menu-select])
      (lambda (menu item)
	(cond
	  [(not (string? menu))
	   (arg-error tag "invalid menu")]
	  [(not (string? item))
	   (arg-error tag "invalid item")]
	  [else
	   (mred:test:make-event
	    (lambda ()
	      (let ([frame  (mred:test:get-active-frame)])
		(if (no-frame? frame)
		    (run-error tag "no active frame")
		    (let ([menu-bar  (send frame get-menu-bar)])
		      (if (no-menu-bar? menu-bar)
			  (run-error tag "no menu bar")
			  (let ([item-id  (send menu-bar find-menu-item menu item)])
			    (if (no-item? item-id)
				(run-error tag "no matching menu item")
				(begin
				  (send frame command item-id)
				  (void))))))))))]))))
  
  (define menu-select-now (do-now menu-select))
  
  
  )
