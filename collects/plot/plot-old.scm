(module plot-old mzscheme
  
  (require 
;   (lib "plplot-low-level.ss" "plplot")
   (lib "etc.ss")
   (lib "class.ss")
   (lib "file.ss")
   (lib "mred.ss" "mred")
   (lib "posn.ss" "lang")
   (lib "list.ss")
   (lib "math.ss"))
  
  ; for dymamic require
  (define-syntax my-dynamic-require
    (syntax-rules () 
      [(_ mpath id ...)
       (begin
         (define id (dynamic-require mpath (quote id))) ...
         )]))
  
  ; for plplot to work properly
  (putenv "PLPLOT_LIB" (build-path (car (current-library-collection-paths)) "plplot"))
;  (require (lib "plplot-low-level.ss" "plplot"))
  
  (my-dynamic-require 
   "plplot-low-level.ss"
   pl-set-device 
   pl-set-output-file 
   pl-init-plot                      
   pl-finish-plot 
   pl-set-plot-environment
   pl-set-labels
   pl-plot-line
   pl-plot-segment
   pl-set-background-color
   pl-select-colormap0-index
   pl-set-colormap0-index
   pl-set-line-width
   pl-write-text
   pl-2d-contour-plot
   pl-2d-shade-plot
   pl-plot-points
   pl-x-error-bars
   pl-y-error-bars
   pl-world-3d
   pl-plot3d
   pl-box3)
  
  ; posn-size : posn -> number
  ; computes distance from 0,0 to point
  (define (posn-length point)
    (sqrt (+ (sqr (posn-x point)) (sqr (posn-y point)))))
  
  ; KEYWORD/DEFAULT LAMBDA   
  
  ; this is used for the 'plot and 'plot3d functions
  ; maybe should be replaced by instantiate
  (define-syntax lookup-lambda
    (syntax-rules ()
      [(_ ((var default) ...) body)
       (lookup-lambda unused unused2 ((var default) ...) body)]
      [(_ args items ((var default) ...) body)
       (lambda [args . items]
         (let ((var (cond [(assq 'var args) => cadr]
                          [else default]))
               ...)
           body))]))
  
  ; this is used by all the renderers
  ; takes a data-source and an optional list of parameters
  (define-syntax r-lambda
    (syntax-rules ()
      [(_ ((var default) ...) body)
       (r-lambda unused ((var default) ...) body)]
      [(_ data ((var default) ...) body)
       (opt-lambda [data (args null)]
         (let ((var (cond [(assq 'var args) => cadr]
                          [else default]))
               ...)
           body))]))
    
  ; plot : [asslist] plottable*
  (define plot  
    (lambda args
      (if (procedure? (car args))
          (apply opt-plot '() args)
          (apply opt-plot (car args) (cdr args)))))
                   
  ; mix : plot% plot% -> plot%
  (define (mix plota plotb)
    (apply opt-plot (send plota get 'args) (append (send plota get 'plot-items) (send plotb get 'plot-items))))
  
  ; plot : asslist plottable*
  ; plots a set of plot-items
  (define opt-plot
    (lookup-lambda args plot-items ((x-min -5) (x-max 5) (y-min -5) (y-max 5) (x-label "x") (y-label "y") (title "a graph"))
      (let ((view (make-object 2d-view%)))
        (send* view 
          (start-plot)
          (set-plot-environment x-min x-max y-min y-max 0 1)
          (set-labels x-label y-label title)
          (set 'args args)
          (set 'plot-items plot-items))    
        
        ; send the view the plot items and the args
        (with-handlers ((exn? (lambda (ex) (send view finish-plot) (raise ex))))
          (for-each (lambda (ploter) 
                      (send view reset-to-default)
                      (ploter x-min x-max y-min y-max view))
                    plot-items))
        (send view finish-plot)
        view)))
   
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
     
  ; base class for a plot view
  ; 
  (define plot-view%
    (class* image-snip%()
      (public 
        start-plot
        finish-plot  
        set-line-color
        set-line-width
        get
        set
        set-plot-environment
        reset-to-default)
      
      ; get : 'symbol -> any
      ; get the value of a field
      (define (get field)
        (cond [(eqv? field 'plot-items) plot-items]
              [(eqv? field 'args) args]))
                     
      ; set : 'symbol any -> null
      ; changes a field
      (define (set field value)
        (cond [(eqv? field 'plot-items) (set! plot-items value)]
              [(eqv? field 'args) (set! args value)]))              
         
      (define file #f)      
      (inherit load-file)  
      
      (init-field (args '())
                  (plot-items '()))
      
      ; set the initial environment 
      (define (set-plot-environment x-min x-max y-min y-max just other)        
        (pl-set-plot-environment x-min x-max y-min y-max just other))
      
      ; changes the *initial* colormap to match the colors
      ; this should probably be done dynamically
      (define (init-colors)
        (pl-set-colormap0-index 0 255 255 255) ; 0 to white
        (pl-set-colormap0-index 1 0 0 0) ; 1 to black
        (pl-set-colormap0-index 15 255 0 0)) ; 15 to red
      
      ; these are the colors to whitch the plot will be initialzed
      (define colors '((white 0) (black 1) (yellow 2) (green 3) (aqua 4) (pink 5) (wheat 6) (grey 7) 
                       (brown 8) (blue 9) (violet 10) (cyan 11) (turquoise 12) (magenta 13) (salmon 14) (red 15)))
      
      ; set-line-width : number -> nothing
      (define (set-line-width width) (pl-set-line-width width))
      
      ; reset-to-default : void
      ; resets some of the state to default
      (define (reset-to-default) 
        (init-colors)
        (set-line-color 'black)
        (set-line-width 0))
                                        
      ;set-line-color : symbol -> nothing
      (define (set-line-color color)
        (let ((index (cond [(assq color colors ) => cadr]
                           [else (error (string-append "color \"" color "\" not found"))])))
          (pl-select-colormap0-index index)))
      
      ; start the plot
      ; does housekeeping/setup for plplot
      (define (start-plot)        
        (set! file (make-temporary-file))        
        (init-colors)
        (pl-set-device "png") 
        (pl-set-output-file file) 
        (pl-init-plot))
      
      ; finish the plot.. loads the file
      (define (finish-plot)       
        (pl-finish-plot)
        (load-file file))       
      
      (super-instantiate ())))
        
  ;; a 2d plot view
  (define 2d-view% 
    (class* plot-view% ()
      (public 
        set-labels        
        plot-vector
        plot-vectors      
        plot-points
        plot-line
        plot-contours
        plot-shades)
      
      ; set-labels : string string string -> nothing
      ; sets the x, y and title lables
      (define (set-labels x-label y-label title)
        (pl-set-labels x-label y-label title))

      ; plot-contours: listoflistof number, listof-number, listof-number, listof-number
      (define (plot-contours z x-vals y-vals levels)
        (pl-2d-contour-plot z x-vals y-vals levels))
      
      ; plot-shades: listoflistof number, listof-number, listof-number, listof-number
      (define (plot-shades z x-vals y-vals levels)
        (pl-2d-shade-plot z x-vals y-vals levels))
  
      ; plot-line : listof-posn
      ; plots a line with the given points
      (define (plot-line points)        
        (pl-plot-line (length points) (map posn-x points) (map posn-y points)))
      
      ; plot-points : listof-posn
      ; plots given points with a . symbol
      (define (plot-points points sym)
        (pl-plot-points (length points) (map posn-x points) (map posn-y points) sym))
      
      (define v-head-ratio 1/4) ; size of the vector head
      (define rot (* 5 pi 1/6))
      
      ; plot-vectors: listof listof posn, posn - > nothing
      (define (plot-vectors from delta)
        (for-each (lambda (f d) (send this plot-vector f d)) from delta))
      
      ; plot-vector : posn posn -> nothing
      (define (plot-vector from delta)
        (unless (= 0 (posn-length delta))
          (let* ((x (posn-x from)) (x2 (+ x (posn-x delta)))
                 (y (posn-y from)) (y2 (+ y (posn-y delta)))
                 (ang (atan  (posn-y delta) (posn-x delta)))
                 (len (posn-length delta))
                 (x3 (+ x2 (* len v-head-ratio (cos (+ ang rot))))) (x4 (+ x2 (* len v-head-ratio (cos (- ang rot)))))
                 (y3 (+ y2 (* len v-head-ratio (sin (+ ang rot))))) (y4 (+ y2 (* len v-head-ratio (sin (- ang rot))))))         
            (plot-line (list from (make-posn x2 y2) (make-posn x3 y3) (make-posn x4 y4) (make-posn x2 y2))))))

      (super-instantiate ())))
  
  ; 3d view
  ; for making meshes and stuff
  
  (define 3d-view%
    (class* plot-view% ()
      (public 
        box3d
        world3d
        plot3d)
      
      ; define the 3d world
      (define (world3d x y z xmin xmax ymin ymax zmin zmax alt az)
        (pl-world-3d x y z xmin xmax ymin ymax zmin zmax alt az))
      
      ; set up the axies box
      (define (box3d 
               xopts xlabel xticks nxsub
               yopts ylabel yticks nysub
               zopts zlabel zticks nzsub)
        (pl-box3 xopts xlabel xticks nxsub
                 yopts ylabel yticks nysub
                 zopts zlabel zticks nzsub))
      
      ; draw a 3d plot
      (define (plot3d x y z)
        (pl-plot3d x y z))
      
      
      (super-instantiate ())))
      

  ;; FUNCTIONAL SOURCES AND RENDERERS
  
  ; sample-size: number number number -> number
  (define (sample-size samples x-min x-max)
    (/ (- x-max x-min) (- samples 1)))
  
  ; x-values : number number number -> listof-number
  (define (x-values samples x-min x-max)
    (let ((ss (sample-size samples x-min x-max)))
      (build-list samples (lambda (x) (+ x-min (* x ss))))))

  
  ; scale-vectors : listof-posn number number -> listof-posn
  ; scales vectors, causing them to fit in their boxes
  (define (scale-vectors deltas x-sample-size y-sample-size)
    (let* ((x-max-value (apply max (map posn-x deltas)))
           (y-max-value (apply max (map posn-y deltas)))
           (x-div-const (/ x-max-value x-sample-size))
           (y-div-const (/ y-max-value y-sample-size)))
      (map (lambda (point) (make-posn (* (/ (posn-x point) x-div-const) 9/10) 
                                      (* (/ (posn-y point) y-div-const) 9/10))) deltas)))
  
  ; normalze-vector : posn number number -> posn
  (define (normalize-vector vec x-sample-size y-sample-size)
    (let* ((size (posn-length vec)))
      (make-posn (* (/ (posn-x vec) size) x-sample-size 9/10)
                 (* (/ (posn-y vec) size) y-sample-size 9/10))))
      
  ; normalize-vector : listof-posn number number -> listolf-posn
  (define (normalize-vectors deltas x-sample-size y-sample-size)
    (map (lambda (vec) (normalize-vector vec x-sample-size y-sample-size)) deltas))
  
  ; make-column : number listof-number -> listof-points
  (define (make-column x-val y-values)
    (map (lambda (y) (make-posn x-val y)) y-values))
 
  ; xy-list : number number number number number  -> listof-posn
  ; make a list of all the positions on the graph
  (define (xy-list samples x-min x-max y-min y-max)
    (let* ((x-vals (x-values samples x-min x-max))
           (y-vals (x-values samples y-min y-max)))      
      (foldr append '() (map (lambda (x) (make-column x y-vals)) x-vals))))
          
  ;  make-vec : (number number -> number) (number number -> number) -> (posn -> posn)
  (define (make-vec func1 func2)
    (lambda (point) (make-posn (func1 (posn-x point) (posn-y point)) (func2 (posn-x point) (posn-y point)))))
  
  ; derivative : (number -> number) [number] -> (number -> number)
  (define derivative
    (opt-lambda (func [h .00000001])
      (lambda (x) (/ (- (func (+ x h)) (func x)) h))))
  
  ; gradient : (number number -> number) [number] -> (posn -> posn)
  (define gradient
    (opt-lambda (func-3d [h .00000001]) 
      (lambda (point) (make-posn ((derivative (lambda (x) (func-3d x (posn-y point))) h) (posn-x point))
                                 ((derivative (lambda (y) (func-3d (posn-x point) y)) h) (posn-y point))))))
  

  ; zgrid : (number number -> number) listof-number listof-number -> listof-listof number
  (define (zgrid func x-vals y-vals samples)
    (map (lambda (x) (map (lambda (y) (func x y)) y-vals)) x-vals))
  
  ;line : (number -> number) [number] [symbol] [number] -> ( number number number number 2dplotview -> nothing)
  (define line 
    (r-lambda func ((samples 150) (color 'red) (width 1))
      (lambda (x-min x-max y-min y-max 2dplotview)
        (send* 2dplotview 
          (set-line-color color) (set-line-width width)
          (plot-line (map (lambda (x) (make-posn x (func x))) (x-values samples x-min x-max)))))))

  
  ; field : (posn -> posn) [number] [symbol] [number] [symbol] -> ( number number number number 2dplotview -> nothing)
  ; plots a vector field
  ; styles are 
  ; scaled -> vector field with scaled vectors
  ; normalized -> all vectors same size, indicates direction 
  ; real -> all vectors drawn to scale
  (define field
    (r-lambda vfun ([samples 20] [color 'black] [width 1] [style 'scaled])
      (lambda (x-min x-max y-min y-max 2dplotview) 
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
            (plot-vectors points new-results))))))
  
  ; contour : (posn -> posn) [number] [symbol] [number] [number u listof-number] ->  (number number number number 2dplotview -> nothing)
  ; renders a contour plot given function and contour levels
  (define contour
    (r-lambda fun3d ((samples 50) (color 'black) (width 1) (levels 10))
      (lambda (x-min x-max y-min y-max 2dplotview)
        (let* ((x-vals (x-values samples x-min x-max))
               (y-vals (x-values samples y-min y-max))
               (grid (zgrid fun3d x-vals y-vals samples))
               (z-max (apply max (map (lambda (row) (apply max row)) grid)))
               (z-min (apply min (map (lambda (row) (apply min row)) grid)))
               (c-levels (if (list? levels) levels (x-values levels z-min z-max))))
          (send* 2dplotview 
            (set-line-color color) (set-line-width width)
            (plot-contours grid x-vals y-vals c-levels))))))
  
  ; shade : (posn -> posn) [number] [symbol] [number] [number / listof-number] ->  (number number number number 2dplotview -> nothing)
  ; renders a shade plot given function and shade levels    
  (define shade
    (r-lambda fun3d ((samples 50) (levels 10))
      (lambda (x-min x-max y-min y-max 2dplotview)
        (let* ((x-vals (x-values samples x-min x-max))
               (y-vals (x-values samples y-min y-max))
               (grid (zgrid fun3d x-vals y-vals samples))
               (z-max (apply max (map (lambda (row) (apply max row)) grid)))
               (z-min (apply min (map (lambda (row) (apply min row)) grid)))
               (c-levels (x-values levels z-min z-max)))
          (send* 2dplotview 
            (plot-shades grid x-vals y-vals c-levels))))))
  
  ; points : listofposn [number]-> (number number number number 2dplotview -> nothing)
  ; plots a set of points using a specific character
  (define points
    (r-lambda lop ((char 9))
      (lambda (x-min x-max y-min y-max 2dplotview)
        (send 2dplotview plot-points lop char))))
              
  
  ;; 3D PLOTTERS
  ; plot a surface
  (define surface 
    (r-lambda fun3d ((samples 50) (color 'black) (width '0))
      (lambda (x-min x-max y-min y-max z-min z-max 3dplotview)
        (let* ((x-vals (x-values samples x-min x-max))
               (y-vals (x-values samples y-min y-max))
               (grid (zgrid fun3d x-vals y-vals samples)))
          (send* 3dplotview 
            (set-line-color color) (set-line-width width) 
            (plot3d x-vals y-vals grid))))))
            
  (provide
   (all-defined)
   (struct posn (x y))))