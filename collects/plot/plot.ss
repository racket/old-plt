(module plot mzscheme
  (require
   (lib "list.ss")
   (lib "syntax.ss" "plot")
   (lib "math.ss" "plot")
   (lib "view.ss" "plot")
   (lib "renderers.ss" "plot")   
   (lib "fit.ss" "plot")
   (lib "class.ss"))
  
  ; plot : [asslist] plottable*
  (define-syntax plot  
    (syntax-rules ()
      [(_ renderer )
       (instantiate 2d-view% (renderer))]
      [(_ renderer (option value) ...)
       (instantiate 2d-view% (renderer) (option value) ...)]))
 
  ; mix : renderer renderer -> renderer
  ; creates a renderer that will renderer both of the inputs
  (define (mix renderer1 renderer2)
    (lambda (view)
      (send view reset-to-default)
      (renderer1 view)
      (send view reset-to-default)
      (renderer2 view)))
  
  ; mix* : renderer+ -> renderer
  ; combine any number of renderers
  (define (mix* r1 . the-rest)
    (if (empty? the-rest) 
        r1
        (mix r1 (apply mix* the-rest))))
    
  ; make-2d-renderer : (2d-view% -> void)
  ; provides a user with the ability to create their own renderers
  ; without providing the implimentation
  (define (make-2d-renderer x)
    x)
    
  ; plot3d : asslist 3dplottable ...
  ; plots a set of 3d plot items
  (define plot3d
    (lookup-lambda args plot-items ((x-min -5) (x-max 5) (y-min -5) (y-max 5) (z-min -5) (z-max 5) 
                                    (alt 30 ) (az 45)
                                    (z-label "Z-Axis") (x-label "X-Axes") (y-label "Y-Axes") 
                                    (title "a graph"))
      (let ((view3d (make-object 3d-view%)))
        (send* view3d
          (start-plot)
          (set-plot-environment -1 1 -1 1 0 -2)
          (world3d 1 1 1 x-min x-max y-min y-max z-min z-max alt az)
          (box3d 
           "bnstu" x-label 0 0
           "bnstu" y-label 0 0
           "bnstu" z-label 0 0)
          (set 'args args)
          (set 'plot-items plot-items))
        ; send the view the plot items and the args
        (with-handlers ((exn? (lambda (ex) (send view3d finish-plot) (raise ex))))
          (for-each (lambda (ploter) 
                      (send view3d reset-to-default)
                      (ploter x-min x-max y-min y-max z-min z-max view3d))
                    plot-items))
        (send view3d finish-plot)
        view3d)))
  
  (provide
   
   ; to make plots
   plot
   plot3d

   ; to combine/create renderers
   mix
   mix*
   make-2d-renderer
   
   ; 2d-renderers
   error-bars
   points
   line
   field
   contour
   shade
   
   ; 3d-rendereres
   surface
   
   ; from math-tools
   derivative
   gradient
   make-vec
   
   ; curve-fitter
   fit
   fit-lambda
   fit-result-function
   fit-result-final-params
   
   
   
   ))


