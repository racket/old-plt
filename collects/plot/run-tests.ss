;(putenv "PLPLOT_LIB" (build-path (car (current-library-collection-paths)) "plplot"))
(require (lib "plot.ss" "plplot"))
;(require (lib "plot-tests.scm" "plplot"))
;(require (lib "test.ss"    "schemeunit"))
;(require (lib "text-ui.ss" "schemeunit"))

;(test/text-ui
 ;(make-test-suite
  ; "plot-tests"
  ; plot-tests))

(newline)
(display "graphical demos")


(plot 
 '((x-max 30) (y-max 30)) 
 (points (map (lambda (x) (make-posn x (random x))) (build-list 30 (lambda (x) (add1 x))))))

(plot '() 
      (line (lambda (x) x) '((color red))))
;
(plot '() 
      (line (lambda (x) (sin x))))
;      
(plot '((title "gradient field of F(x,y) = sin(x) * sin(y)"))
      (field (gradient (lambda (x y) (* (sin x) (cos y)))) '((samples 25))))
;
(define (trig x y) (* (sin x) (sin y)))
(plot '((x-min -1.5) (x-max 1.5) (y-min -1.5) (y-max 1.5) 
        (title "gradient field +shdade + contours of F(x,y) = sin(x) * sin(y)")     
      (shade trig))
      (contour trig)
      (field (gradient trig ) '((samples 25))))
;
;; plot : '(plot options) plot-items+ -> plot-snips
;
;;(compose plot1 plot2)
;
(define a (plot  (contour trig)))
(define b (plot  (shade trig)))
(mix b a)
;(instantiate 2dplot% (x-min 5) (x-max 10)
;  (instantiate contour% (samples 25) trig )
;  
;  )

;(plot (line (lambda (x) (sin x))))
;(instantiate 2dplot% 
;  (instantiate line% (...)))


;(mix (plot '() (contour trig)) (plot '() (line (lambda (x) (cos x)))))

;(plot3d '() (surface trig '((color green))))
;(plot (field (make-vec (lambda (x y) x) (lambda (x y) y))))

; (number->number)  -> (number number number number 2dview -> nothing)

(require (lib "syntax.ss" "plplot")
         (lib "class.ss")
         (lib "renderer-helpers.ss" "plplot"))
(define dashed-line
  (r-lambda fun ((samples 100) (segments 20) (color 'red) (width 1))    
    (lambda (x-min x-max y-min y-max 2dview)
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
                    (send 2dview plot-line (map (lambda (x) (make-posn x (fun x))) dash))) x-lists)))))
  
(plot (dashed-line (lambda (x) x) '((color red) (widht 2))))
                                              
           
     