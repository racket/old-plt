(define (wait-for-drscheme-frame)
  (let loop ([warned? #f])
    (let ([active (mred:test:get-active-frame)])
      (if (and active
	       (is-a? active drscheme:export:unit:frame%))
	  active
	  (begin
	    (sleep 1/2)
	    (unless warned?
	      (printf "select drscheme frame~n"))
	    (loop #t))))))

(define (clear-definitions frame)
  (mred:test:new-window (ivar frame definitions-canvas))
  (mred:test:menu-select "Edit" "Select All")
  (mred:test:menu-select "Edit" (if (eq? wx:platform 'macintosh)
				    "Clear"
				    "Delete")))

(define (type-in-definitions frame str)
  (let ([len (string-length str)])
    (mred:test:new-window (ivar frame definitions-canvas))
    (let loop ([i 0])
      (unless (>= i len)
	(let ([c (string-ref str i)])
	  (mred:test:keystroke
	   (if (char=? c #\newline)
	       #\return
	       c)))
	(loop (+ i 1))))))

(define wait
  (case-lambda 
   [(test desc-string) (wait test desc-string 5)]
   [(test desc-string time)
    (let ([int 1/2])
      (let loop ([sofar 0])
	(cond
	  [(> sofar time) (error 'wait desc-string)]
	  [(test) (void)]
	  [else (sleep int)
		(loop (+ sofar int))])))]))
(define (wait-pending)
  (wait (lambda () (= 0 (mred:test:number-pending-actions)))
	"pending actions didn't terminate")
  (mred:test:reraise-error))
