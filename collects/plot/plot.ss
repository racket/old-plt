(module plot mzscheme
  (require
   (lib "list.ss")
   (lib "etc.ss")
   (lib "math.ss" "plot")
   (lib "view.ss" "plot")
   (lib "renderer-helpers.ss" "plot")
   (lib "renderers.ss" "plot")   
   (lib "fit.ss" "plot")
   (lib "class.ss"))
  
  ; plot : plottable (option value)*
  (define-syntax plot  
    (syntax-rules ()
      [(_ ren )
       (instantiate 2d-view% () (renderer ren))]
      [(_ ren (option value) ...)
       (instantiate 2d-view% () (renderer ren) (option value) ...)]))
  
  (define-syntax plot3d
    (syntax-rules ()
      [(_ ren )
       (instantiate 3d-view% () (renderer ren))]
      [(_ ren (option value) ...)
       (instantiate 3d-view% () (renderer ren) (option value) ...)]))
 
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
  (define make-2d-renderer identity)
    

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
   fit-result-final-params))


