
(require-library "mrpict.ss" "texpict")

(define (show-pict p)
  (define s (make-semaphore))

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
				(draw-pict p (send this get-dc)
					   0 0))])
			   (sequence (super-init f)))))

  (define mb (make-object menu-bar% f))

  (define scale-menu (make-object menu% "Scale" mb))
  (define (add-scale s)
    (make-object checkable-menu-item% (format "~a~n" s)
		 scale-menu (lambda (i e)
			      (for-each
			       (lambda (i) (send i check #f))
			       (send scale-menu get-items))
			      (send i check #t)
			      (let ([dc (send c get-dc)])
				(send dc set-scale s s)
				(send dc clear)
				(send c on-paint)))))
  (add-scale 1/4)
  (add-scale 1/2)
  (send (add-scale 1) check #t)
  (add-scale 2)
  (add-scale 4)
  (add-scale 8)
  (add-scale 16)

  (send f show #t)
  (yield s))

;; Bitmap DC sizes the same as the screen:
(dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1)))
