(module view mzscheme
  (require 
   (lib "math.ss" "plot")
   (lib "etc.ss")
   (lib "class.ss")
   (lib "file.ss")
   (lib "mred.ss" "mred")
   (lib "list.ss")
   (lib "math.ss")
   (lib "4.ss" "srfi")
   (lib "draw.ss" "htdp"))
  
  ; for dymamic require
  (define-syntax my-dynamic-require
    (syntax-rules () 
      [(_ mpath id ...)
       (begin
         (define id (dynamic-require mpath (quote id))) ...
         )]))
  
  ; macro for creating a field in a class with a getter and a setter
  (define-syntax (fields-with-accessors stx) 
    (define (join-identifier prefix ident)
      (datum->syntax-object 
       ident 
       (string->symbol (string-append (symbol->string prefix )(symbol->string (syntax-e ident)))) ))
    (syntax-case stx ()
      [(_ (field init) ... )
       (let ((accessors (map (lambda (id) (join-identifier 'get- id)) (syntax-e #'(field ...))))
             (setters (map (lambda (id) (join-identifier 'set- id)) (syntax-e #'(field ...)))))
         (with-syntax (((accessor ... ) accessors)
                       ((setter ...) setters))
           #'(fields-with-accessors-helper (accessor setter field init) ...)))]))
  
  ; for accessors
  (define-syntax fields-with-accessors-helper
    (syntax-rules ()
      [(_ (accessor setter field init) ...)
       (begin 
         (init-field (field init)) ...
         (define (accessor) field) ...
         (define (setter val) (set! field val)) ...) ]))
  
  ; for plplot to work properly, it needs to know where it's fnt files are
  (putenv "PLPLOT_LIB" (this-expression-source-directory))
  
  ; dynamic require is used because putenv does not change values in current module
  (my-dynamic-require 
   '(lib "plplot-low-level.ss" "plot")
   u8vec->scheme-string
   pl-setup-memory
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
  
  ; base class for a plot view
  ; 
  (define plot-view%
    (class* image-snip% ()
      (public 
        set-line-color
        set-line-width                       
        set-plot-environment
        reset-to-default
        get-x-min
        get-x-max
        get-y-min
        get-y-max)
      
      (init-field
       renderer)
      
      (fields-with-accessors
       (x-min -5)
       (x-max 5)
       (y-min -5)
       (y-max 5)
       (x-label "X axis")
       (y-label "Y axis")
       (title "")
       (device 'png))   
     
      (define bitmap #f)
      (define x-size 640)
      (define y-size 480)
     
      (inherit 
        set-bitmap
        load-file)  
      
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
      (define colors '((white 0) (black 1) (yellow 2) (green 3) 
                       (aqua 4) (pink 5) (wheat 6) (grey 7) 
                       (brown 8) (blue 9) (violet 10) (cyan 11)
                       (turquoise 12) (magenta 13) (salmon 14) (red 15)))
      
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
        (cond
          [(eq? device 'png)           
           (set! bitmap (make-temporary-file))        
           (init-colors)
           (pl-set-device "png") 
           (pl-set-output-file bitmap) 
           (pl-init-plot)]
          [(eq? device 'mem)           
           (init-colors)
           (set! bitmap (make-u8vector (* x-size y-size 4) 255))
           (pl-setup-memory x-size y-size bitmap)
           (pl-set-device "mem") 
           (pl-init-plot)]
          [else
           (error "Incorrect device specified")]))
      
      ; finish the plot.. loads the file
      (define (finish-plot)
        (cond
          [(eq? device 'png)
           (pl-finish-plot)
           (load-file bitmap)]
          [(eq? device 'mem)
           (pl-finish-plot)
           (set-bitmap (bits->bitmap-dc% bitmap))]
          [else
           (error "Incorrect device specified")]))
      
      (define (bits->bitmap-dc% bitmap)
        (let ((bmdc (instantiate bitmap-dc% () (bitmap (make-object bitmap% x-size y-size #f))))
              (result-string (u8vec->scheme-string bitmap)))
          (send bmdc set-argb-pixels 0 0  x-size y-size result-string)
          (begin0
            (send bmdc get-bitmap)
            (send bmdc set-bitmap #f))))
      
      (define (plot)
        (start-plot)
        (set-plot-environment x-min x-max y-min y-max 0 1)
        (with-handlers ((exn? (lambda (ex) (finish-plot) (raise ex))))
          (renderer this))
        (finish-plot)
        this)
      
      (super-instantiate ())
      (plot)))
        
  ;; a 2d plot view
  (define 2d-view% 
    (class* plot-view% ()
      (public 
        set-labels 
        plot-y-errors
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
  
      ; plot-line : (listof vector) -> void
      ; plots a line with the given points
      (define (plot-line points)        
        (pl-plot-line (length points) 
                      (map vector-x points)
                      (map vector-y points)))
      
      ; plot-points : (listof vector) -> void
      ; plots given points with a . symbol
      (define (plot-points points sym)
        (pl-plot-points (length points) 
                        (map vector-x points)
                        (map vector-y points)
                        sym))

      (define v-head-ratio 1/4) ; size of the vector head
      (define rot (* 5 pi 1/6))
      
      ; plot-vectors: (listof (list vector vector)) - > void
      (define (plot-vectors from delta)
        (for-each (lambda (f d) (send this plot-vector f d)) from delta))
      
      ; plot-vector : vector vector -> nothing
      (define (plot-vector from delta)
        (unless (= 0 (vector-magnitude delta))
          (let* ((x (vector-x from)) (x2 (+ x (vector-x delta)))
                 (y (vector-y from)) (y2 (+ y (vector-y delta)))
                 (ang (atan  (vector-y delta) (vector-x delta)))
                 (len (vector-magnitude delta))
                 (x3 (+ x2 (* len v-head-ratio (cos (+ ang rot))))) 
                 (x4 (+ x2 (* len v-head-ratio (cos (- ang rot)))))
                 (y3 (+ y2 (* len v-head-ratio (sin (+ ang rot))))) 
                 (y4 (+ y2 (* len v-head-ratio (sin (- ang rot))))))         
            (plot-line (list from 
                             (vector x2 y2) 
                             (vector x3 y3) 
                             (vector x4 y4) 
                             (vector x2 y2))))))
      
      ; plot-y-errors (listof (vector x y-min y-max)) ->nothing
      ; plots y error bars given a vector containing the x y and z (error magnitude) points
      (define (plot-y-errors errlist)
        (pl-y-error-bars (length errlist) (map vector-x errlist) (map vector-y errlist) (map vector-z errlist)))

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
 
  (provide
   2d-view%
   3d-view%))