(module simple-gui mzscheme

  (define board "")

  (define (get-board s width height)
    (set! board s)
    (display-board board))

  (define (get-robots l)
    (let ((b (copy-string (board))))
      (for-each
       (lambda (robot)
	 (string-set! b (+ (* width (sub1 (caddr robot)))
			   (sub1 (cadr robot)))
		      (car robot)))
       l)
      (display-board b)))
      
  

  (define f (instantiate frame% ("Simple Gui" #f 200 200)))
  (define c (instantiate editor-canvas% (f)))
  (define t (instantiate text% ()))
  (send c set-editor t)
  (send f show #t)

  (define (display-board b)
    (send t delete 'start 'back)
    (sent t insert b 1))


  )
