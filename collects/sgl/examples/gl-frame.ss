;; By Brendan Burns

(module gl-frame 
  (lib "plt-pretty-big.ss" "lang")
  (require 
   (lib "gl.ss" "sgl")
   (lib "gl-vectors.ss" "sgl"))
  
  (provide
   set-gl-draw-fn
   set-gl-init-fn
   init-textures
   image->gl-vector
   gl-load-texture
   get-texture
   add-key-mapping
   clear-key-mappings
   gl-run)
  
  (define gl-draw void)
  (define gl-init 
    (lambda ()
      (glShadeModel GL_SMOOTH)
      (glClearColor 0.0 0.0 0.0 0.5)
      (glClearDepth 1)
      (glEnable GL_DEPTH_TEST)
      (glDepthFunc GL_LEQUAL)
      (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)))
  
  (define set-gl-draw-fn
    (lambda (fn)
      (set! gl-draw fn)))
  
  (define set-gl-init-fn
    (lambda (fn)
      (set! gl-init fn)))
  
  (define glcontext '())
  (define glcanvas '())
  (define frame '())
  
  (define gl-thunk
    (lambda ()
      (if (send glcontext ok?)
          (begin
            (send glcanvas with-gl-context gl-draw)
            (send glcanvas swap-gl-buffers)
            (set! gl-thunk 
                  (lambda ()
                    (send glcanvas with-gl-context gl-draw)
                    (send glcanvas swap-gl-buffers))))
          (begin 
            (display "Error: OpenGL context failed to initialize")
            (newline)
            (exit)))))
    
 ; (define (gluPerspective fovy aspect znear zfar)
 ;   (let ((f (/ 1 (tan (/ (* fovy (/ pi 180)) 2))))
 ;         (g (- znear zfar)))
 ;     (glMultMatrixd
 ;      (vector->gl-double-vector 
 ;      (vector
 ;        (/ f aspect) 0 0 0
 ;        0 f 0 0
 ;        0 0 (/ (+ znear zfar) g) -1
 ;        0 0 (/ (* 2 znear zfar) g) 0)))))
    
    
   ;; A function that recorrects for a new aspect ratio when the window is resized
(define (gl-resize width height)
  (glViewport 0 0 width height)
  
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluPerspective 45 (/ width height) 0.1 100)
  
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))
  
  (define (recursive-handle-key list code)
    (cond
      ((empty? list) void)
      ((equal? (caar list) code) ((car (cdr (car list)))))
      (#t (recursive-handle-key (rest list) code))))
  
  (define *key-mappings* '())
  
  (define (add-key-mapping key fn)
    (set! *key-mappings* (cons (list key fn) *key-mappings*)))
  
  (define (clear-key-mappings)
    (set! *key-mappings* '()))
  
  (define (gl-handlekey key)
    (recursive-handle-key *key-mappings* (send key get-key-code)))
  
  ;; Make a 640 ? 480 frame
(define make-frame 
  (lambda ()
    (instantiate frame% () 
      (label "OpenGL Window") 
      (width 640) 
      (height 480))))

(define glcanvas%
  (class canvas%
    (override on-paint on-size on-superwindow-show on-char)
    (define (on-paint) (gl-thunk))
    
    (define (on-size w h)
      (send this with-gl-context 
            (lambda () 
              (gl-resize w h))))
    
    (define (on-superwindow-show shown)
      (if shown
          (void)
          (set! gl-loop (lambda () #t))))
    
    (define (on-char key) (gl-handlekey key))
    (super-instantiate ())))
  
  (define gl-loop
    (lambda ()
      (if (send glcanvas is-shown?)
          (begin
            (yield)
            (gl-thunk)
            (gl-loop)))))
  
  (define gl-run
    (lambda ()
      
      (set! frame (make-frame))
      (set! glcanvas (instantiate glcanvas% (frame)))
      (set! glcontext (send (send glcanvas get-dc) get-gl-context))
      
      (send frame show #t)
      (letrec ((wait
                (lambda () (if (send glcontext ok?) (void) (wait)))))
        (wait))
      (send glcanvas with-gl-context gl-init)
      (gl-loop)))
  
  (define *textures* '())
  
  (define init-textures
    (lambda (count)
      (set! *textures* (glGenTextures count))))
  
  (define image->gl-vector
    (lambda (file)
      (let* (
             (bmp  (make-object bitmap% file 'unknown #f))
             (dc (instantiate bitmap-dc% (bmp)))
             (pixels (* (send bmp get-width) (send bmp get-height)))
             (vec (make-gl-ubyte-vector (* pixels 3)))
             (data (make-bytes (* pixels 4)))
             (i 0)
             )
        (send dc get-argb-pixels 0 0 (send bmp get-width) (send bmp get-height) data)
        (letrec
            ([loop
              (lambda ()
                (if (< i pixels)
                    (begin
                      (gl-vector-set! vec (* i  3) 
                                      (bytes-ref data (+ (* i 4) 1)))
                      (gl-vector-set! vec (+ (* i 3) 1) 
                                      (bytes-ref data (+ (* i 4) 2)))
                      (gl-vector-set! vec (+ (* i 3) 2) 
                                      (bytes-ref data (+ (* i 4) 3)))
                      (set! i (+ i 1))
                      (loop))))])
          (loop))
        (list (send bmp get-width) (send bmp get-height) vec))))
  
  (define gl-load-texture
    (lambda (image-vector width height min-filter mag-filter ix)
      (glBindTexture GL_TEXTURE_2D (gl-vector-ref *textures* ix))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER min-filter)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER mag-filter)
      (if (or (= min-filter GL_LINEAR_MIPMAP_NEAREST)
               (= mag-filter GL_LINEAR_MIPMAP_NEAREST))
          (gluBuild2DMipmaps GL_TEXTURE_2D 3 width height GL_RGB GL_UNSIGNED_BYTE image-vector)
          (glTexImage2D GL_TEXTURE_2D 0 3 width height 0 GL_RGB GL_UNSIGNED_BYTE image-vector)))
    )
  
  (define get-texture
    (lambda (ix)
      (gl-vector-ref *textures* ix)))
)



