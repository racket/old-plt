(require (lib "plot.ss" "plplot"))

(require (lib "syntax.ss" "plplot")
         (lib "class.ss")
         (lib "renderer-helpers.ss" "plplot"))
; (number -> number) mumbo-jumbo -> 2d-renderer
(define dashed-line
  (r-lambda fun 2dview (x-min x-max) ((samples 100) (segments 20) (color 'red) (width 1))    
    (let* ((dash-size (/ (- x-max x-min) segments))
           (x-lists (build-list (/ segments 2) 
                                (lambda (index)                                    
                                  (x-values 
                                   (/ samples segments) 
                                   (+ x-min (* 2 index dash-size))
                                   (+ x-min (* (add1 ( * 2 index)) dash-size)))))))
      (send* 2dview 
        (set-line-color color)
        (set-line-width width))
      (for-each (lambda (dash)
                  (send 2dview plot-line 
                        (map (lambda (x) (vector x (fun x))) dash))) 
                x-lists))))
                     ;                                  
;(define dashed-line-renderer
 ; (class* (renderer<%>)
 ;   (init-fields) ...
 ;   
 ;   (render
;     )))
    
(plot (dashed-line (lambda (x) x) '((color red))))