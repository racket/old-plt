(module plplot mzscheme

(require (lib "foreign.ss") (lib "etc.ss"))

(define libplplot
  (ffi-lib
   (build-path (this-expression-source-directory)
               "compiled" "native" (system-library-subpath) "libplplot")))

(define plplotlibdir (get-ffi-obj "plplotLibDir" libplplot _string))

;; set the lib dir to contain the fonts. 
(set-ffi-obj! "plplotLibDir" libplplot _string
              (path->string (this-expression-source-directory)))

(define _plflt _double*)
(define _plint _int)

(define-syntax define*
  (syntax-rules ()
    [(_ (name . args) body ...)
     (begin (provide name) (define (name . args) body ...))]
    [(_ name expr)
     (begin (provide name) (define name expr))]))

(define* pl-setup-page
  (get-ffi-obj "c_plspage" libplplot
               (_fun (xp : _plflt = 0.)
                     (yp : _plflt = 0.)
                     (xleng : _plflt)
                     (yleng : _plflt)
                     (xoff : _plint = 0)
                     (yoff : _plint = 0)
                     -> _void)))

(define* pl-set-device
  (get-ffi-obj "c_plsdev" libplplot (_fun _string -> _void)))

(define* pl-set-output-file
  (get-ffi-obj "c_plsfnam" libplplot (_fun _string -> _void)))

(define* pl-init-plot
  (get-ffi-obj "c_plinit" libplplot (_fun -> _void)))

(define* pl-finish-plot
  (get-ffi-obj "c_plend" libplplot (_fun -> _void)))

(define* pl-set-plot-environment
  (get-ffi-obj "c_plenv" libplplot
    (_fun _plflt _plflt _plflt _plflt _plint _plint -> _void)))

(define* pl-set-labels
  (get-ffi-obj "c_pllab" libplplot
    (_fun _string _string _string -> _void)))

(define* pl-plot-line
  (get-ffi-obj "c_plline" libplplot
    (_fun _plint (x : (_list i _plflt)) (y : (_list i _plflt)) -> _void)))

(define* pl-plot-segment
  (get-ffi-obj "c_pljoin" libplplot
    (_fun _plflt _plflt _plflt _plflt -> _void)))

(define* pl-set-background-color
  (get-ffi-obj "c_plscolbg" libplplot
    (_fun _plint _plint _plint -> _void)))


(define* pl-select-colormap0-index
  (get-ffi-obj "c_plcol0" libplplot
    (_fun _plint -> _void)))

(define* pl-set-colormap0-index
  (get-ffi-obj "c_plscol0" libplplot
    (_fun _plint _plint _plint _plint -> _void)))

(define* pl-set-line-width
  (get-ffi-obj "c_plwid" libplplot
    (_fun _plint -> _void)))

(define* pl-write-text
  (get-ffi-obj "c_plptex" libplplot
               (_fun _plflt _plflt _plflt _plflt _plflt _string -> _void)))

;;(define* pl-2d-countour-plot ...)
;;(define* pl-2d-shade-plot ...)

(define* pl-x-error-bars
  (get-ffi-obj "c_plerrx" libplplot
    (_fun _plint (_list i _plflt)
          (_list i _plflt)
          (_list i _plflt) -> _void)))

(define* pl-y-error-bars
  (get-ffi-obj "c_plerry" libplplot
    (_fun _plint (_list i _plflt)
          (_list i _plflt)
          (_list i _plflt) -> _void)))

(define* pl-plot-points
  (get-ffi-obj "c_plpoin" libplplot
    (_fun _plint (x : (_list i _plflt)) (y : (_list i _plflt)) _plint
          -> _void)))

(define* pl-2d-shade-plot (lambda (x) x))
(define* pl-plot-pionts (lambda (x) x))
(define* pl-2d-contour-plot (lambda (x) x))
(define* pl-world-3d (lambda (x) x))
(define* pl-plot3d (lambda (x) x))
(define* pl-mesh3d  (lambda (x) x))
(define* pl-mesh3dc (lambda (x) x))
(define* pl-box3    (lambda (x) x))
(define* pl-poly3   (lambda (x) x))
(define* pl-line3   (lambda (x) x))
(define* pl-fill    (lambda (x) x))
)

#|

NOT WORKING

;; 3D functions

(define* pl-world-3d
  (get-ffi-obj "c_plw3d" libplplot
    (_fun _plflt _plflt _plflt _plflt _plflt _plflt _plflt _plflt _plflt _plflt _plflt -> _void)))

(define* pl-plot3d
  (get-ffi-obj "c_plot3d" libplplot
    (_fun (x : (_list i _plflt))
          (y : (_list i _plflt))
          (z : (_list i (_list i _plflt)))
          (nx : _plint)
          (ny : _plint)
          (opt : _plint)
          (size : _plint)
          -> _void)))

(define* pl-mesh3d
  (get-ffi-obj "c_plmesh" libplplot
    (_fun ...)))

(define* pl-mesh3dc ...)

(define* pl-box3 ...)

(define* pl-poly3 ...)

(define* pl-line3 ...)

(define* pl-fill ...)

|#


#|

TEST CODE, created X11 plot window

(require libplplot)

(pl-sdev "xwin")

(pl-init-plot)

(pl-set-plot-environment 0.0 5.0 0.0 5.0 0 1)

(pl-plot-line 4 (list 1.0 2.0 3.0 4.0 5.0) (list 1.0 2.0 3.0 4.0 5.0))

(pl-finish-plot)

|#
