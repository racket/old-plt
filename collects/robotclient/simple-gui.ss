(module simple-gui mzscheme

  (require (lib "mred.ss" "mred")
           (lib "class.ss"))


  (define gui%
    (class object%
      
      (init-field board width height)

      (define f (instantiate frame% ("Simple Gui" #f 200 200)))
      (define c (instantiate editor-canvas% (f)))
      (define t (instantiate text% ()))
      (send c set-editor t)
      (send f show #t)

      (define (display-board b)
        (send t select-all)
        (send t delete)
        (send t insert b 1))
      
      (display-board board)
      
      
      (define/public (set-robots l)
        (let ((b (string-copy board)))
          (for-each
           (lambda (robot)
             (string-set! b (+ (* width (sub1 (caddr robot)))
                               (sub1 (cadr robot)))
                          (string-ref (number->string (car robot) 16) 0)))
           l)
          (display-board b)))
      (super-instantiate ())))

  )
