(module renderers mzscheme
  (require 
   (lib "math.ss" "plot")
   (lib "renderer-helpers.ss" "plot")
   (lib "class.ss"))
  
  ;line : (number -> number) [number] [symbol] [number] -> (2dplotview -> nothing)
  (define line 
    (r-lambda func 2dplotview (x-min x-max) ((samples 150) (color 'red) 
                                             (width 1) 
                                             (mode 'standard) 
                                             (mapping 'cartesian)
                                             (t-min -5) (t-max 5))
      
      (let*
          ((t-min (if (eq? mapping 'polar) t-min x-min))
           (t-max (if (eq? mapping 'polar) t-max x-max))  ; maybe let-values?
           (points
            (case mode
              ((standard) (map (lambda (x) (vector x (func x))) 
                               (x-values samples x-min x-max)))
              ((parametric) (map func (x-values samples t-min t-max))))))
        (send* 2dplotview 
          (set-line-color color) (set-line-width width)
          (plot-line 
           (case mapping
             ((cartesian) points)
             ((polar) (map 
                       (lambda (point)  ; convert to cartesian from theta, r
                         (vector 
                          (* (vector-y point) (cos (vector-x point)))
                          (* (vector-y point) (sin (vector-x point)))))))))))))
  
    ; error-bars : (listof (vector x y err)) [symbol] -> (2dplotview -> nothing)
  (define error-bars
    (r-lambda errs 2dplotview () ((color 'red))
      (let* ((y-list (map vector-y errs))
             (e-list (map vector-z errs))
             (y-mins (map (lambda (y e) (- y e)) y-list e-list ))
             (y-maxs (map (lambda (y e) (+ y e)) y-list e-list )))              
        (send 2dplotview set-line-color color)
        (send 2dplotview plot-y-errors (map vector (map vector-x errs) y-mins y-maxs)))))
  

  ; field : (vector -> vector) [number] [symbol] [number] [symbol] -> (2dplotview -> nothing)
  ; plots a vector field
  ; styles are 
  ; scaled -> vector field with scaled vectors
  ; normalized -> all vectors same size, indicates direction 
  ; real -> all vectors drawn to scale
  (define field
    (r-lambda vfun 2dplotview 
      (x-min x-max y-min y-max)
      ([samples 20] [color 'black] [width 1] [style 'scaled])
      (let* ((points (xy-list samples x-min x-max y-min y-max))
             (results (map vfun points))
             (new-results 
              (case style                  
                [(real) results]
                [(scaled) (scale-vectors results (sample-size samples x-min x-max) (sample-size samples y-min y-max))]
                [(normalized) (normalize-vectors results (sample-size samples x-min x-max) (sample-size samples y-min y-max))]
                [else (error (string-append "Unknown vector field style passed to field-renderer: " (symbol->string style)))])))
        (send* 2dplotview  
          (set-line-color color) (set-line-width width)
          (plot-vectors points new-results)))))

  ; contour : (nubmer number -> number) [number] [symbol] [number] [number u listof-number] ->  (2dplotview -> void)
  ; renders a contour plot given function and contour levels
  (define contour
    (r-lambda fun3d 2dplotview 
      (x-min x-max y-min y-max)
      ((samples 50) (color 'black) (width 1) (levels 10))
      (let* ((x-vals (x-values samples x-min x-max))
             (y-vals (x-values samples y-min y-max))
             (grid (zgrid fun3d x-vals y-vals samples))
             (z-max (apply max (map (lambda (row) (apply max row)) grid)))
             (z-min (apply min (map (lambda (row) (apply min row)) grid)))
             (c-levels (if (list? levels) levels (x-values levels z-min z-max))))
        (send* 2dplotview 
          (set-line-color color) (set-line-width width)
          (plot-contours grid x-vals y-vals c-levels)))))
           
  ; shade : (number number -> number) [number] [symbol] [number] [number / listof-number] ->  (2dplotview -> nothing)
  ; renders a shade plot given function and shade levels    
  (define shade
    (r-lambda fun3d 2dplotview (x-min x-max y-min y-max) ((samples 50) (levels 10))
      (let* ((x-vals (x-values samples x-min x-max))
             (y-vals (x-values samples y-min y-max))
             (grid (zgrid fun3d x-vals y-vals samples))
             (z-max (apply max (map (lambda (row) (apply max row)) grid)))
             (z-min (apply min (map (lambda (row) (apply min row)) grid)))
             (c-levels (x-values levels z-min z-max)))
        (send* 2dplotview 
          (plot-shades grid x-vals y-vals c-levels)))))
           
  ; points : (listof vector) [number] -> (2dplotview -> nothing)
  ; plots a set of points using a specific character
  (define points
    (r-lambda lop 2dplotview ((char 9))
      (send 2dplotview plot-points lop char)))
       
       
  ;; 3D PLOTTERS
  ; plot a surface
  (define surface 
    (r-lambda fun3d 3dplotview (x-min x-max y-min y-max) ((samples 50) (color 'black) (width '0))
      (let* ((x-vals (x-values samples x-min x-max))
             (y-vals (x-values samples y-min y-max))
             (grid (zgrid fun3d x-vals y-vals samples)))
          (send* 3dplotview 
            (set-line-color color) (set-line-width width) 
            (plot-surface x-vals y-vals grid)))))
  
  (provide (all-defined)))

  
