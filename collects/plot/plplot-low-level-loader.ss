; loads the low level library using dynamic require
(module plplot-low-level-loader mzscheme
  
  (require (lib "etc.ss"))
  
  (define-syntax require-and-provide
    (syntax-rules () 
      [(_ mpath id ...)
       (begin
         (define id (dynamic-require mpath (quote id))) ...
         (provide id) ...
         )]))
  
  ; for plplot to work properly, it needs to know where it's fnt files are
  (putenv "PLPLOT_LIB" (this-expression-source-directory))
  
  ; dynamic require is used because putenv does not change values in current module
  (require-and-provide
   '(lib "plplot-low-level.ss" "plot")  
   pl-setup-page
   ;pl-setup-memory
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
   pl-mesh3d
   pl-mesh3dc
   pl-box3
   pl-poly3
   pl-line3
   pl-fill))
  
    