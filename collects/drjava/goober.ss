(unit/sig goober^
  (import mred^)

  (define goober-panel%
    (class horizontal-panel% (parent def-text)
      (sequence (super-init parent))
      (inherit stretchable-height change-children add-child)
      (private
	[hilight
	 (lambda (code)
	   (let* ([line (sub1 (arithmetic-shift code -10))]
		  [column (sub1 (bitwise-and code 1023))]
		  [location (send def-text paragraph-start-position line)]
		  [end (send def-text paragraph-end-position line)])
	     (send def-text set-position (+ location column) end)
	     (send (send def-text get-canvas) focus)))]
	[errors
	 (make-object goober-list% "Errors" this hilight)]
	[warnings
	 (make-object goober-list% "Warnings" this hilight)])
      (public
	[error
	 (lambda (code message)
	   (send errors add code message))]
	[warning
	 (lambda (code message)
	   (send warnings add code message))]
	[reset
	 (lambda ()
	   (hide)
	   (send errors reset)
	   (send warnings reset))]
	[hide
	 (lambda () 
	   (change-children (lambda _ null)))]
	[appear
	 (lambda ()
	   (send errors possibly-appear)
	   (send warnings possibly-appear))])
      (sequence
	(stretchable-height #f)
	(hide))))
  
  (define goober-list%
    (class choice% (label parent report-code)
      (inherit get-selection get-number clear append stretchable-width)
      
      ;; codes corresponds to the list of messages, but backwards.
      ;; append is given for the list of messages, but
      ;; cons is cheaper for the list of codes.
      (private [codes null])
      (public
	[reset (lambda () (clear) (set! codes null))]
	[add
	 (lambda (code message)
	   (set! codes (cons code codes))
	   (append message))]
	[possibly-appear
	 (lambda ()
	   (unless (zero? (get-number))
	     (send parent add-child this)))])
      (sequence
	(super-init label null parent
		    (lambda (choice event)
		      (report-code (list-ref codes (- (get-number) (get-selection) 1)))))
	(stretchable-width #t)))))