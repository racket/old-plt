(unit/sig countdown^
  (import mzlib:date^
	  mzlib:thread^
	  mzlib:function^
	  mzlib:pretty-print^
	  mred^)

  (define init-params
    (lambda ()
      (date-display-format 'american)))
  (init-params)

  (define separator-snipclass
    (make-object
     (class-asi snip-class%
       (override
	 [read (lambda (s) 
		 (let ([size-box (box 0)])
		   (send s get size-box)
		   (make-object separator-snip%)))]))))
  (send* separator-snipclass
    (set-version 1)
    (set-classname "sepatator-snip%"))
  (send (get-the-snip-class-list) add separator-snipclass)
  
  ;; the two numbers 1 and 2 which appear here are to line up this snip
  ;; with the embedded snips around it in the drscheme rep.
  ;; I have no idea where the extra pixels are going.
  (define separator-snip%
    (class snip% ()
      (inherit get-style set-snipclass set-flags get-flags get-admin)
      (private [width 500]
	       [height 1]
	       [white-around 2])
      (override
	[write (lambda (s) 
		 (send s put (char->integer #\r)))]
	[copy (lambda () 
		(let ([s (make-object (object-class this))])
		  (send s set-style (get-style))
		  s))]
	[get-extent
	 (lambda (dc x y w-box h-box descent-box space-box lspace-box rspace-box)
	   (for-each (lambda (box) (when box (set-box! box 0)))
		     (list descent-box space-box lspace-box rspace-box))
	   (let* ([admin (get-admin)]
		  [reporting-media (send admin get-editor)]
		  [reporting-admin (send reporting-media get-admin)]
		  [widthb (box 0)]
		  [space 2])
	     (send reporting-admin get-view #f #f widthb #f)
	     (set! width (- (unbox widthb) 
			    space
			    2)))
	   (set! height 1)
	   (when w-box
	     (set-box! w-box width))
	   (when h-box
	     (set-box! h-box (+ (* 2 white-around) height))))]
	[draw
	 (let* ([body-pen (send the-pen-list find-or-create-pen "BLUE" 0 'solid)]
		[body-brush (send the-brush-list find-or-create-brush "BLUE" 'solid)])
	   (lambda (dc x y left top right bottom dx dy drawCaret)
	     (let ([orig-pen (send dc get-pen)]
		   [orig-brush (send dc get-brush)])
	       (send dc set-pen body-pen)
	       (send dc set-brush body-brush)
	       
	       (send dc draw-rectangle (+ x 1)
		     (+ white-around y) width height)
	       
	       (send dc set-pen orig-pen)
	       (send dc set-brush orig-brush))))]
	[get-text
	 (opt-lambda (offset num [flattened? #f])
	   "1")])
      (sequence
	(super-init)
	(set-flags (cons 'hard-newline (get-flags)))
	(set-snipclass separator-snipclass))))
  
  (define seconds->delta
    (let ([seconds-per-minute 60]
	  [minutes-per-hour 60]
	  [hours-per-day 24]
	  [days-per-week 7]
	  [weeks-per-year 52]
	  [next (lambda (n n-per-m)
		  (values (modulo n n-per-m)
			  (quotient n n-per-m)))])
      (lambda (total-seconds)
	(let*-values ([(seconds total-minutes)
		       (next total-seconds seconds-per-minute)]
		      [(minutes total-hours)
		       (next total-minutes minutes-per-hour)]
		      [(hours total-days)
		       (next total-hours hours-per-day)]
		      [(days total-weeks)
		       (next total-days days-per-week)]
		      [(weeks total-years)
		       (next total-weeks weeks-per-year)])
	  (values seconds
		  minutes
		  hours
		  days
		  weeks
		  total-years)))))
  
  (define label-delta (make-object style-delta% 'change-bold))
  (define date-delta (make-object style-delta% 'change-italic))

  (define y-inset 0)
  (define x-inset 0)
  (define show-border? #f)

  (define make-hidden-edit%
    (lambda (super%)
      (class super% args
	(inherit hide-caret set-max-undo-history)
	(sequence
	  (apply super-init args)
	  (set-max-undo-history 0)
	  (hide-caret #t)))))

  (define inner-edit% (make-hidden-edit% text%))

  (define date%
    (class editor-snip% (date-edit main-edit i-second i-minute i-hour i-day i-month i-year)
      (private
	[TMP-date (seconds->date (current-seconds))]
	[second i-second]
	[minute (or i-minute (date-minute TMP-date))]
	[hour (or i-hour (date-hour TMP-date))]
	[day (or i-day (date-day TMP-date))]
	[month (or i-month (date-month TMP-date))]
	[year (or i-year (date-year TMP-date))]
	[seconds (find-seconds second minute hour day month year)])
      (private
	[edit (make-object inner-edit%)]
	[redisplay
	 (lambda (delta d-second d-minute d-hour d-day d-week d-year)
	   (let* ([hit-non-zero? #f]
		  [do (lambda (count id)
			(when (or hit-non-zero?
				  (not (= count 0)))
			  (set! hit-non-zero? #t)
			  (send edit insert (format "~a ~a~a " 
						    count id
						    (if (= count 1)
							""
							"s"))
				(send edit get-start-position)
				'same
				#f)))])
	     (send edit lock #f)
	     (send edit delete 0 (send edit last-position) #f)
	     (do d-year "year")
	     (do d-week "week")
	     (do d-day "day")
	     (do d-hour "hour")
	     (do d-minute "minute")
	     (do d-second "second")
	     (if hit-non-zero?
		 (let ([last (send edit last-position)])
		   (send edit insert (if (< delta 0) "until" "ago")
			 last last #f))
		 (send edit insert "NOW" (send edit get-start-position) 'same #f))
	     (send edit lock #t)))]
	[update-date-edit
	 (lambda ()
	   (send* date-edit
	     (lock #f)
	     (delete 0 (send date-edit last-position) #f)
	     (insert (date->string (seconds->date seconds) #t)
		     (send date-edit get-start-position) 'same #f)
	     (change-style date-delta 0 (send date-edit last-position))
	     (lock #t)))]
	[init-seconds
	 (lambda (current-secs)
	   (let* ([leap?
		   (lambda (year)
		     (or (= (modulo year 4) 0)
			 (= (modulo year 100) 0)))]
		  [days-in-month
		   (lambda (month)
		     (case month
		       [(0 2 4 6 7 9 11) 31]
		       [(1) (if (leap? year)
				29
				28)]
		       [(3 5 8 10) 30]))]
		  [date (seconds->date current-secs)]
		  [c-second second]
		  [c-minute minute]
		  [c-hour hour]
		  [c-day day]
		  [c-month month]
		  [c-year year]
		  [c-seconds seconds]
		  [increment-year
		   (lambda ()
		     (set! c-year (+ 1 c-year)))]
		  [increment-month
		   (lambda ()
		     (if (= c-month 12)
			 (begin (set! c-month 1)
				(increment-year))
			 (set! c-month (+ 1 c-month))))]
		  [increment-day
		   (lambda ()
		     (if (= c-day (days-in-month c-month))
			 (begin (set! c-day 1)
				(increment-month))
			 (set! c-day (+ 1 c-day))))]
		  [increment-hour
		   (lambda ()
		     (if (= c-hour 23)
			 (begin (set! c-hour 0)
				(increment-day))
			 (set! c-hour (+ 1 c-hour))))]
		  [increment-minute
		   (lambda ()
		     (if (= c-minute 59)
			 (begin (set! c-minute 0)
				(increment-hour))
			 (set! c-minute (+ 1 c-minute))))])
	     (when (<= c-seconds current-secs)
	       (if i-minute
		   (if i-hour
		       (if i-day
			   (if i-month
			       (if i-year
				   (void)
				   (increment-year))
			       (increment-month))
			   (increment-day))
		       (increment-hour))
		   (increment-minute)))
	     (set! second c-second)
	     (set! minute c-minute)
	     (set! hour c-hour)
	     (set! day c-day)
	     (set! month c-month)
	     (set! year c-year)
	     (set! seconds (find-seconds c-second c-minute c-hour c-day c-month c-year))))])
      (public
	[get-seconds
	 (lambda ()
	   seconds)])
      (private
	[last-time-before-current? #f])
      (public
	[update
	 (lambda ()
	   (let ([current (current-seconds)])
	     (when (and (not i-year)
			(< seconds current))
	       (let loop ()
		 (when (< seconds current)
		   (init-seconds current)
		   (loop)))
	       (update-date-edit)
	       (send main-edit sort-lines))
	     (when (and last-time-before-current?
			(< seconds current))
	       (set! last-time-before-current? #f)
	       (send main-edit sort-lines))
	     (let ([delta (- current seconds)])
	       (let-values ([(d-seconds d-minutes d-hours d-days d-weeks d-years)
			     (seconds->delta (abs delta))])
		 (redisplay delta d-seconds d-minutes d-hours d-days d-weeks d-years)))))])
      (sequence (super-init edit show-border? x-inset y-inset x-inset y-inset)
		(let ([current (current-seconds)])
		  (init-seconds current)
		  (set! last-time-before-current? (< current seconds)))
		(update-date-edit))))
  
  (define paren-snip%
    (class-asi editor-snip%
      (private
	[what 'not-paren])
      (public
	[set-open (lambda () (set! what 'open))]
	[set-close (lambda () (set! what 'close))]
	[get-paren (lambda () what)])))

  (define main-canvas%
    (class-asi editor-canvas%
      (inherit get-editor)
      (public
	[update-snip-size
	 (let ([width (box 0)]
	       [height (box 0)]
	       [leftm (box 0)]
	       [rightm (box 0)]
	       [topm (box 0)]
	       [bottomm (box 0)]
	       [left-edge-box (box 0)]
	       [ignored-box (box 0)])
	   (lambda (s)
	     (when (is-a? s editor-snip%)
	       (let* ([edit (get-editor)]
		      [snip-media (send s get-editor)]
		      [admin (send edit get-admin)])
		 (send admin get-view #f #f width height)
		 (send s get-margin leftm topm rightm bottomm)


		 ;; when the width is to be maximized and there is a
		 ;; newline just behind the snip, we know that the left
		 ;; edge is zero. Special case for efficiency in the 
		 ;; console printer
		 (let ([fallback
			(lambda ()
			  (send edit get-snip-position-and-location
				s #f left-edge-box ignored-box))])
		   (cond
		    [(let ([prev (send s previous)])
		       (and prev
			    (member 'hard-newline (send prev get-flags))))
		     (set-box! left-edge-box 0)]
		    [else (fallback)]))


		 
		 (let ([snip-width
			(max 0
			     (- (unbox width)
				(unbox left-edge-box)
				(unbox leftm)
				(unbox rightm)
				
				;; this two is the space that 
				;; the caret needs at the right of
				;; a buffer.
				2))])
		   (send* s 
			  (set-min-width snip-width)
			  (set-max-width snip-width))
		   (when snip-media
		     (send snip-media set-max-width 0)))))))])
      (override
       [on-size
	(lambda (_1 _2)
	  (let ([edit (get-editor)])
	    (when edit
	      (let loop ([s (send edit find-snip 0 'before)])
		(when s
		  (update-snip-size s)
		  (loop (send s next)))))))])))


  (define main-edit%
    (class (make-hidden-edit% text%) args
      (inherit begin-edit-sequence end-edit-sequence
	       insert get-admin delete get-canvas
	       last-position lock hide-caret
	       get-start-position)
      (private
	[bx (box 0)]
	[by (box 0)]
	[bw (box 0)]
	[bh (box 0)])
      (private
	[lines null]
	[counters null]
	[sema (make-semaphore 1)]
	[update-counters
	 (lambda ()
	   (with-semaphore 
	    sema
	    (lambda ()
	      (begin-edit-sequence)
	      (lock #f)
	      (for-each (lambda (x) (send x update)) counters)
	      (lock #t)
	      (end-edit-sequence))))])
      (public
	[clear-events
	 (lambda ()
	   (with-semaphore 
	    sema
	    (lambda ()
	      (begin-edit-sequence)
	      (lock #f)
	      (delete 0 (last-position) #f)
	      (set! counters null)
	      (set! lines null)
	      (lock #t)	   
	      (end-edit-sequence))))]
	[shutdown
	 (lambda ()
	   (kill-thread thread-id))]
	[sort-lines
	 (lambda ()
	   (let ([get-seconds
		  (lambda (line)
		    (let* ([media (send line get-editor)]
			   [last (send media last-position)]
			   [first (send media find-snip last 'before)]
			   [seconds (send first get-seconds)])
		      seconds))]
		 [admin (get-admin)])
	     (lock #f)
	     (delete 0 (last-position) #f)
	     (let ([current (current-seconds)]
		   [step (string #\tab)])
	       (let loop ([lines (quicksort lines
					    (lambda (x y)
					      (<= (get-seconds x)
						  (get-seconds y))))]
			  [first? #t]
			  [crossed-line? #f]
			  [this-inset ""])
		 (cond
		   [(null? lines) (void)]
		   [else (let* ([outer (car lines)]
				[this-above-line? (< (get-seconds outer) current)]
				[insert-separator?
				 (and (not this-above-line?)
				      (not crossed-line?)
				      (not first?))]
				[next-inset (case (send outer get-paren)
					      [(not-paren) this-inset]
					      [(open) (string-append this-inset step)]
					      [(close) (substring this-inset
								   0
								   (- (string-length this-inset)
								      (string-length step)))])]
				[inset (if (eq? (send outer get-paren) 'close)
					   next-inset
					   this-inset)])
			   (when insert-separator?
			     (insert (string #\newline) (get-start-position) 'same #f)
			     (insert (make-object separator-snip%) (get-start-position) 'same #f))
			   (when (and (not first?) 
				      (not insert-separator?))
			     (insert (string #\newline) (get-start-position) 'same #f))
			   (insert inset (get-start-position) 'same #f)
			   (insert outer (get-start-position) 'same #f)
			   (loop (cdr lines)
				 #f
				 (or crossed-line? insert-separator?)
				 next-inset))])))
	     (lock #t)))]
	
	[new-counter
	 (let ([first-time? #t])
	   (lambda (name second minute hour day month year)
	     (lock #f)
	     (let* ([date-edit (make-object inner-edit%)]
		    [make-snip
		     (lambda (edit)
		       (make-object editor-snip% edit show-border? x-inset y-inset x-inset y-inset))]
		    [date-snip (make-snip date-edit)]
		    [display (make-object date% date-edit this second minute hour day month year)]
		    [label-edit (make-object inner-edit%)]
		    [label (make-snip label-edit)]
		    [main (make-object inner-edit%)]
		    [outer (make-object paren-snip% main #t 4 4 4 4)])
	       (set! counters (cons display counters))
	       (set! lines (cons outer lines))
	       (if first-time?
		   (set! first-time? #f)
		   (insert (string #\newline) (get-start-position) 'same #f))
	       (insert outer (get-start-position) 'same #f)
	       (cond
		[(string? name)
		 (send label-edit insert name 0 0 #f)
		 (send label-edit change-style label-delta 0 (send label-edit last-position))]
		[(procedure? name)
		 (name label-edit)]
		[else (error 'remember "expected procedure or string as first argument, got: ~a~n")])
	       (send* main
		 (insert label (send main get-start-position) 'same #f)
		 (insert (string #\newline) (send main get-start-position) 'same #f)
		 (insert date-snip (send main get-start-position) 'same #f)
		 (insert (string #\newline) (send main get-start-position) 'same #f)
		 (insert display (send main get-start-position) 'same #f))
	       (for-each (lambda (e) (send e lock #t))
			 (list date-edit label-edit main))
	       (send (get-canvas) update-snip-size outer)
	       (lock #t)
	       outer)))]
	[sync
	 (lambda ()
	   (sort-lines)
	   (update-counters))])
      (sequence
	(apply super-init args)
	(lock #f))
      (private
	[thread-id
	 (thread
	  (lambda ()
	    (init-params)
	    (let loop ()
	      (update-counters)
	      (sleep 5)
	      (loop))))]))))

