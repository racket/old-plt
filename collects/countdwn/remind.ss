(unit/sig countdown^
  (import mzlib:date^
	  mzlib:thread^
	  mzlib:function^
	  mzlib:pretty-print^
	  [mred : mred^])

  (date-display-format 'american)

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
  
  (define label-delta (make-object wx:style-delta% wx:const-change-bold))
  (define date-delta (make-object wx:style-delta% wx:const-change-italic))

  (define date%
    (class wx:media-snip% (date-edit main-edit i-second i-minute i-hour i-day i-month i-year)
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
	[edit (make-object wx:media-edit%)]
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
				-1
				#f)))])
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
		 (send edit insert "NOW" (send edit get-start-position) -1 #f))))]
	[update-date-edit
	 (lambda ()
	   (send* date-edit
	     (delete 0 (send date-edit last-position))
	     (insert (date->string (seconds->date seconds) #t) (send date-edit get-start-position) -1 #f)
	     (change-style date-delta 0 (send date-edit last-position))))]
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
	     '(printf "finding seconds for (original) ~a~n" (list i-second i-minute i-hour i-day i-month i-year))
	     (set! seconds (find-seconds c-second c-minute c-hour c-day c-month c-year))))])
      (public
	[get-seconds
	 (lambda ()
	   seconds)]
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
	     (let ([delta (- current seconds)])
	       (let-values ([(d-seconds d-minutes d-hours d-days d-weeks d-years)
			     (seconds->delta (abs delta))])
		 (redisplay delta d-seconds d-minutes d-hours d-days d-weeks d-years)))))])
      (sequence (super-init edit #f)
		(init-seconds (current-seconds))
		(update-date-edit))))
  
  (define main-edit%
    (class mred:media-edit% args
      (inherit begin-edit-sequence end-edit-sequence
	       insert get-admin delete
	       last-position
	       get-start-position)
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
	      (for-each (lambda (x) (send x update)) counters)
	      (end-edit-sequence))))])
      (public
	[shutdown
	 (lambda ()
	   (kill-thread thread-id))]
	[sort-lines
	 (lambda ()
	   (let ([get-seconds
		  (lambda (line)
		    (let* ([media (send line get-media)]
			   [last (send media last-position)]
			   [first (send media find-snip last wx:const-snip-before)]
			   [seconds (send first get-seconds)])
		      seconds))])
	     (for-each (lambda (outer)
			 (let ([admin (send outer get-admin)])
			   (unless (null? admin)
			     (send admin release-snip outer))))
		       lines)
	     (delete 0 (last-position) #f)
	     (for-each (lambda (outer)
			 (insert outer (get-start-position) -1 #f)
			 (insert (string #\newline) (get-start-position) -1 #f))
		       (quicksort lines
				  (lambda (x y) (<= (get-seconds x)
						    (get-seconds y)))))))]
	
	[new-counter
	 (lambda (name second minute hour day month year)
	   (let* ([date-edit (make-object wx:media-edit%)]
		  [date-snip (make-object wx:media-snip% date-edit #f)]
		  [display (make-object date% date-edit this second minute hour day month year)]
		  [label-edit (make-object wx:media-edit%)]
		  [label (make-object wx:media-snip% label-edit #f)]
		  [main (make-object wx:media-edit%)]
		  [outer (make-object wx:media-snip% main #t)])
	     (set! counters (cons display counters))
	     (set! lines (cons outer lines))
	     (insert outer (get-start-position) -1 #f)
	     (insert (string #\newline) (get-start-position) -1 #f)
	     (send* label-edit
	       (insert name 0 0 #f)
	       (change-style label-delta 0 (string-length name)))
	     (send* main
	       (insert label (send main get-start-position) -1 #f)
	       (insert (string #\newline) (send main get-start-position) -1 #f)
	       (insert date-snip (send main get-start-position) -1 #f)
	       (insert (string #\newline) (send main get-start-position) -1 #f)
	       (insert display (send main get-start-position) -1 #f))))]
	[sync
	 (lambda ()
	   (sort-lines)
	   (update-counters))])
      (sequence
	(apply super-init args))
      (private
	[thread-id
	 (thread
	  (rec f
	       (lambda ()
		 (update-counters)
		 (sleep 2)
		 (f))))]))))
