(unit/sig before^
  (import countdown^
	  mred^)

  (define quit-semaphore (make-semaphore 0))

  (define main-frame%
    (class frame% (label parent in-width in-height in-x in-y)
      (rename [super-on-close on-close]
	      [super-on-size on-size]
	      [super-on-move on-move])
      (private [x in-x]
	       [y in-y]
	       [width in-width]
	       [height in-height]
	       [write-out
		(lambda ()
		  (let ([str (format "~s" (list x y width height))])
		    ;(display str) (newline)
		    (write-resource "mred" "countdwn"
				    (format "~s" (list x y width height))
				    (find-graphical-system-path 'setup-file))))])
      (override
       [on-move
	(lambda (in-x in-y)
	  (set! x in-x)
	  (set! y in-y)
	  (write-out)
	  (super-on-move in-x in-y))]
       [on-size
	(lambda (w h)
	  (set! width w)
	  (set! height h)
	  (write-out)
	  (super-on-size w h))]
       [on-close
	(lambda ()
	  (and (super-on-close)
	       (semaphore-post quit-semaphore)))])
      (sequence
	(super-init label parent in-width in-height in-x in-y))))

  (define min-width/height 200)

  (define-values (frame-x frame-y frame-width frame-height)
    (let/ec k
      (let* ([b (box "")]
	     [result (get-resource "mred" "countdwn" b (find-graphical-system-path 'setup-file))]
	     [jump-out
	      (lambda ()
		(k 10 10 min-width/height min-width/height))])
	(unless result
	  (jump-out))
	(let ([sizes (read (open-input-string (unbox b)))])
	  (unless (and (list? sizes)
		       (= 4 (length sizes))
		       (number? (car sizes))
		       (number? (cadr sizes)))
	    (jump-out))
	  (apply values sizes)))))

  (define frame (make-object main-frame% "Countdown" #f frame-width frame-height frame-x frame-y))
  (define panel (make-object vertical-panel% frame))
  (define canvas (make-object main-canvas% panel))
  (define edit (make-object main-edit%))

  (send frame min-width min-width/height)
  (send frame min-height min-width/height)

  (send canvas set-editor edit))
