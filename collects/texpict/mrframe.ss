
(require-library "mrpict.ss" "texpict")

(define (show-pict p)
  (define s (make-semaphore))

  (define scale 1)

  (define f (make-object (class frame% ()
			   (override
			     [on-close
			      (lambda () (semaphore-post s))])
			   (sequence
			     (super-init "Drawing" #f 450 300)))))
  
  (define c (make-object (class canvas% ()
			   (override
			     [on-paint
			      (lambda ()
				(let ([dc (send this get-dc)])
				  (send dc set-scale scale scale)
				  (draw-pict p dc 0 0)))])
			   (sequence (super-init f)))))

  (define mb (make-object menu-bar% f))

  (define file-menu (make-object menu% "File" mb))

  (define scale-menu (make-object menu% "Scale" mb))
  (define (add-scale s)
    (make-object checkable-menu-item% (format "~a" s)
		 scale-menu (lambda (i e)
			      (for-each
			       (lambda (i) (send i check #f))
			       (send scale-menu get-items))
			      (send i check #t)
			      (let ([dc (send c get-dc)])
				(set! scale s)
				(send dc clear)
				(send c on-paint)))))
  (add-scale 1/4)
  (add-scale 1/2)
  (send (add-scale 1) check #t)
  (add-scale 2)
  (add-scale 4)
  (add-scale 8)
  (add-scale 16)

  (make-object menu-item% "Quit" file-menu
	       (lambda (i e)
		 (when (send f can-close?)
		   (send f show #f)
		   (send f on-close)))
	       #\Q)

  (send f show #t)
  (yield s))

;; Bitmap DC sizes the same as the screen:
(dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1)))
