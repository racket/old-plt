(module map mzscheme
  (require (lib "framework.ss" "framework")
	   (lib "mred.ss" "mred")
	   (lib "class.ss"))

  (provide show-map
	   set-map-position)

  (define map-window (make-object frame:basic% "Help Desk Map" 400))
  (define map-message (make-object message% "" map-window))
  (send map-message stretchable-width #t)

  (define map-position #f)

  (define (update-map-view)
    (send map-message set-label (or map-position "")))

  (define (show-map)
    (send map-window show #t))

  (define (set-map-position pos)
    (set! map-position pos)
    (update-map-view)))
