(unit/sig before^
  (import countdown^
	  mred^)

  (define quit-semaphore (make-semaphore 0))

  (define main-frame%
    (class-asi frame%
      (rename [super-on-close on-close]
	      [super-on-size on-size]
	      [super-move move])
      (override
       [move
	(lambda x
	  (printf "move: ~a~n" x)
	  (apply super-move x))]
       [on-size
	(lambda (w h)
	  (write-resource "mred" "countdwn"
			  (format "~s" (list w h))
			  (find-graphical-system-path 'setup-file))
	  (super-on-size w h))]
       [on-close
	(lambda ()
	  (and (super-on-close)
	       (semaphore-post quit-semaphore)))])))

  (define-values (frame-width frame-height)
    (let/ec k
      (let* ([b (box "")]
	     [result (get-resource "mred" "countdwn" b (find-graphical-system-path 'setup-file))]
	     [jump-out
	      (lambda ()
		(k 200 200))])
	(unless result
	  (jump-out))
	(let ([sizes (read (open-input-string (unbox b)))])
	  (unless (and (list? sizes)
		       (= 2 (length sizes))
		       (number? (car sizes))
		       (number? (cadr sizes)))
	    (jump-out))
	  (apply values sizes)))))

  (define frame (make-object main-frame% "Countdown" #f frame-width frame-height))
  (define panel (make-object vertical-panel% frame))
  (define canvas (make-object main-canvas% panel))
  (define edit (make-object main-edit%))

  (send frame min-width 200)
  (send frame min-height 200)

  (send canvas set-edit edit))
