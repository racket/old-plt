
(require-library "mrpict.ss" "texpict")

;; Create PS up front
(define ps-dc (make-object post-script-dc%))
(send ps-dc start-doc "Pict output")
(send ps-dc start-page)

(dc-for-text-size ps-dc)

(define (show-pict p)
  (draw-pict ps-dc
	     p
	     0 0)
  (send ps-dc end-page)
  (send ps-dc end-doc))

  
  
  
