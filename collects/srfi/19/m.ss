
;; Dummy module, very similar to mzscheme only that it does not export time, and date related procedures.
;; It is used *only* by time.ss.  If you know of a better way, please let me know.

(module m mzscheme
  (provide (all-from-except mzscheme
			    ;; time:
			    time
			    ;; date:
			    struct:date
			    date
			    make-date date?
			    date-second date-minute date-hour date-day date-month date-year date-week-day date-year-day date-dst? date-time-zone-offset
			    set-date-second! set-date-minute! set-date-hour! set-date-day! set-date-month! set-date-year! set-date-week-day!
			    set-date-year-day! set-date-dst?! set-date-time-zone-offset!
			    )
	   tm:local-tz-offset
	   )

  ;; relies on the fact that we named our time zone accessor
  ;; differently from MzScheme's....
  ;; This should be written to be OS specific.
  (define (tm:local-tz-offset)
    (date-time-zone-offset (seconds->date (current-seconds))))
  )