;; sgl -- An OpenGL extension of MzScheme
;;
;; Copyright (C) 2003 Scott Owens <sowens@cs.utah.edu>
;;
;; This  library is  free  software; you  can  redistribute it  and/or
;; modify it under the terms  of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2.1 of
;; the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE. See  the GNU
;; Lesser General Public License for more details.
 
(module sgl mzscheme
  (require (lib "etc.ss")
           "gl-vectors.ss"
           "gl.ss")
  
  
  (define-syntax-set (multi-type multi-type-v make-enum-table make-inv-enum-table)
    
    (define (iota n)
      (cond
        ((= 0 n) null)
        (else (cons n (iota (sub1 n))))))
    
    (define (make-name base num type num?)
      (datum->syntax-object base (string->symbol (format "~a~a~a"
                                                         (syntax-e base)
                                                         (if num? num "")
                                                         type))
        base base))
    
    (define (get-possible-types ts)
      (combine-str
       (map (lambda (t)
              (case t
                ((i s b) "exact integer")
                ((ui us ub) "non-negative exact integer")
                ((f d) "real number")
                (else (error (format "error: ~a" t)))))
            ts)))
    
    (define (get-possible-types-v ts)
      (combine-str
       (map (lambda (t)
              (case t
                ((iv) "gl-int-vector")
                ((sv) "gl-short-vector")
                ((bv) "gl-byte-vector")
                ((uiv) "gl-uint-vector")
                ((usv) "gl-ushort-vector")
                ((ubv) "gl-ubyte-vector")
                ((dv) "gl-double-vector")
                ((fv) "gl-float-vector")
                (else (error (format "~a" t)))))
            ts)))
    
    (define (combine-str strs)
      (cond
        ((null? strs) "")
        ((null? (cdr strs))
         (string-append "or " (car strs)))
        (else
         (string-append (car strs) ", " (combine-str (cdr strs))))))
        
    (define (multi-type/proc stx)
      (syntax-case stx ()
        ((_ name gl-name num-args types num?)
         (let ((num-args (syntax-object->datum #'num-args))
               (types (syntax-object->datum #'types)))
           #`(define name
               (case-lambda
                 #,@(map (lambda (num-arg)
                           (let ((arg-list (generate-temporaries (iota num-arg))))
                             #`(#,arg-list
                                (cond
                                  #,@(map (lambda (type)
                                            #`(#,(cond
                                                   ((memq type '(i s b))
                                                    #`(and 
                                                       #,@(apply append 
                                                                 (map
                                                                  (lambda (arg)
                                                                    (list  
                                                                     #`(real? #,arg)
                                                                     #`(exact? #,arg)
                                                                     #`(integer? #,arg)))
                                                                  arg-list))))
                                                   ((memq type '(ui us ub))
                                                    #`(and 
                                                       #,@(apply append 
                                                                 (map
                                                                  (lambda (arg)
                                                                    (list  
                                                                     #`(real? #,arg)
                                                                     #`(exact? #,arg)
                                                                     #`(integer? #,arg)
                                                                     #`(>= #,arg 0)))
                                                                  arg-list))))
                                                   ((memq type '(d f))
                                                    #`(and
                                                       #,@(map (lambda (arg)
                                                                 #`(real? #,arg))
                                                               arg-list))))
                                                (#,(make-name #'gl-name num-arg type
                                                              (syntax-e #'num?))
                                                  #,@arg-list)))
                                          types)
                                  (else (raise-type-error 'name
                                                          #,(get-possible-types types)
                                                          (list #,@arg-list)))))))
                         num-args)))))))
  (define (multi-type-v/proc stx)
      (syntax-case stx ()
        ((_ name gl-name lengths types num?)
         (let ((lengths (syntax-object->datum #'lengths))
               (types (syntax-object->datum #'types)))
           #`(define (name x)
               (cond
                 #,@(map (lambda (type)
                           #`((#,(case type
                                   ((dv) #'gl-double-vector?)
                                   ((fv) #'gl-float-vector?)
                                   ((iv) #'gl-int-vector?)
                                   ((sv) #'gl-short-vector?)
                                   ((bv) #'gl-byte-vector?)
                                   ((uiv) #'gl-uint-vector?)
                                   ((usv) #'gl-ushort-vector?)
                                   ((ubv) #'gl-ubyte-vector?))
                                x)
                              (let ((len (#,(case type
                                              ((dv) #'gl-double-vector-length)
                                              ((fv) #'gl-float-vector-length)
                                              ((iv) #'gl-int-vector-length)
                                              ((sv) #'gl-short-vector-length)
                                              ((bv) #'gl-byte-vector-length)
                                              ((uiv) #'gl-uint-vector-length)
                                              ((usv) #'gl-ushort-vector-length)
                                              ((ubv) #'gl-ubyte-vector-length))
                                           x)))
                                (case len
                                  #,@(map (lambda (length)
                                            #`((#,length)
                                               (#,(make-name #'gl-name length type
                                                             (syntax-e #'num?))
                                                 x)))
                                          lengths)))))
                         types)
                 (else
                  (raise-type-error 'name #,(get-possible-types-v types) x))))))))
  
    (define gl-regex2 (regexp "(^glu\\-)|(^gl\\-)"))
    (define _-regex (regexp "_"))
    
    (define (string-downcase s)
      (list->string (map char-downcase (string->list s))))
    
    (define (translate-cname name)
      (string->symbol
       (string-downcase
        (regexp-replace* gl-regex2
                         (regexp-replace* _-regex
                                          (symbol->string name)
                                          "-")
                         ""))))
    
    (define (make-enum-table/proc stx)
      (syntax-case stx ()
        ((_ consts ...)
         (let* ((consts (syntax->list #'(consts ...)))
                (syms (map translate-cname (map syntax-e consts))))
           (quasisyntax/loc stx
             (let ((ht (make-hash-table)))
               (for-each (lambda (key value)
                           (hash-table-put! ht key value))
                         '#,syms (list #,@consts))
               (values
                (lambda (enum-sym)
                  (hash-table-get ht enum-sym (lambda () #f)))
                (format "symbol in ~a" '#,syms))))))))
    
    (define (make-inv-enum-table/proc stx)
      (syntax-case stx ()
        ((_ consts ...)
         (let* ((consts (syntax->list #'(consts ...)))
                (syms (map translate-cname (map syntax-e consts))))
           (quasisyntax/loc stx
             (let ((ht (make-hash-table)))
               (for-each (lambda (key value)
                           (hash-table-put! ht key value))
                         (list #,@consts) '#,syms)
               (lambda (enum-val)
                 (hash-table-get ht enum-val (lambda () #f)))))))))
    )
  
  (provide (rename gl-begin begin)
           
           (rename glEnd end)
           (rename glEdgeFlag edge-flag)
           (rename glEdgeFlagv edge-flag-v)
           (rename glDepthRange depth-range)
           (rename glViewport viewport)
           (rename glLoadIdentity load-identity)
           (rename glRotated rotate)
           (rename glTranslated translate)
           (rename glScaled scale)
           (rename glFrustum frustum)
           (rename glOrtho ortho)
           (rename glPushMatrix push-matrix)
           (rename glPopMatrix pop-matrix)
           (rename glPointSize point-size)
           (rename glLineWidth line-width)
           (rename glLineStipple line-stipple)
           (rename glPolygonStipple polygon-stipple)
           (rename glPolygonOffset polygon-offset)
           (rename glPixelZoom pixel-zoom)
           (rename glBitmap bitmap)
           (rename glDeleteTextures delete-textures)
           (rename glGenTextures gen-textures)
           (rename glAreTexturesResident are-textures-resident)
           (rename glPrioritizeTextures prioritize-textures)
           (rename glScissor scissor)
           (rename glIndexMask index-mask)
           (rename glColorMask color-mask)
           (rename glDepthMask depth-mask)
           (rename glStencilMask stencil-mask)
           (rename glClearColor clear-color)
           (rename glClearIndex clear-index)
           (rename glClearDepth clear-depth)
           (rename glClearStencil clear-stencil)
           (rename glClearAccum clear-accum)
           (rename glInitNames init-names)
           (rename glPopName pop-name)
           (rename glPushName push-name)
           (rename glLoadName load-name)
           (rename glSelectBuffer select-buffer)
           (rename glPassThrough pass-through)
           (rename glEndList end-list)
           (rename glCallList call-list)
           (rename glListBase list-base)
           (rename glIsList is-list)
           (rename glDeleteLists delete-lists)
           (rename glFlush flush)
           (rename glFinish finish)
           (rename glIsTexture is-texture)
           (rename glPopAttrib pop-attrib)
           (rename glPopClientAttrib pop-client-attrib)
           (rename glGenLists gen-lists)
           )

  (define get-error-table
    (make-inv-enum-table GL_NO_ERROR 
                         GL_INVALID_ENUM
                         GL_INVALID_VALUE
                         GL_INVALID_OPERATION
                         GL_STACK_OVERFLOW
                         GL_STACK_UNDERFLOW
                         GL_OUT_OF_MEMORY))
  (provide get-error)
  (define (get-error)
    (get-error-table (glGetError)))

  (define-values (begin-table-lookup begin-table-syms)
    (make-enum-table GL_LINES
                     GL_LINE_LOOP
                     GL_LINE_STRIP
                     GL_POINTS
                     GL_POLYGON
                     GL_QUADS
                     GL_QUAD_STRIP
                     GL_TRIANGLES
                     GL_TRIANGLE_FAN
                     GL_TRIANGLE_STRIP))
  (define (gl-begin enum)
    (let ((v (begin-table-lookup enum)))
      (if v
          (glBegin v)
          (raise-type-error 'begin begin-table-syms 0 enum))))
  
  (provide vertex vertex-v)
  (multi-type vertex glVertex (2 3 4) (i d) #t)
  (multi-type-v vertex-v glVertex (2 3 4) (dv iv fv sv) #t)

  (provide tex-coord tex-coord-v)
  (multi-type tex-coord glTexCoord (1 2 3 4) (i d) #t)
  (multi-type-v tex-coord-v glTexCoord (1 2 3 4) (dv iv fv sv) #t)

  ;;(provide multi-tex-coord multi-tex-coord-v)
  (define-values (multi-tex-coord-table-lookup multi-tex-coord-syms)
    (make-enum-table GL_TEXTURE0
                     GL_TEXTURE1
                     GL_TEXTURE2
                     GL_TEXTURE3
                     GL_TEXTURE4
                     GL_TEXTURE5
                     GL_TEXTURE6
                     GL_TEXTURE7
                     GL_TEXTURE8
                     GL_TEXTURE9
                     GL_TEXTURE10
                     GL_TEXTURE11
                     GL_TEXTURE12
                     GL_TEXTURE13
                     GL_TEXTURE14
                     GL_TEXTURE15
                     GL_TEXTURE16
                     GL_TEXTURE17
                     GL_TEXTURE18
                     GL_TEXTURE19
                     GL_TEXTURE20
                     GL_TEXTURE21
                     GL_TEXTURE22
                     GL_TEXTURE23
                     GL_TEXTURE24
                     GL_TEXTURE25
                     GL_TEXTURE26
                     GL_TEXTURE27
                     GL_TEXTURE28
                     GL_TEXTURE29
                     GL_TEXTURE30
                     GL_TEXTURE31))

  
  ;; UNIMPLEMENTED
  (define multi-tex-coord void)
  (define multi-tex-coord-v void)
  
  (provide normal normal-v)
  (multi-type normal glNormal (3) (i d) #t)
  (multi-type-v normal-v glNormal (3) (dv iv fv sv bv) #t)

  (provide color color-v)
  (multi-type color glColor (3 4) (i d) #t)
  (multi-type-v color-v glColor (3 4) (dv iv uiv fv ubv bv usv sv) #t)
  
  (provide index index-v)
  (multi-type index glIndex (1) (i d) #f)
  (multi-type-v index-v glIndex (1) (dv iv fv sv ubv) #f)

  (define-values (enable-client-state/disable-client-state-table-lookup
                  enable-client-state/disable-client-state-syms)
    (make-enum-table GL_EDGE_FLAG_ARRAY
                     GL_TEXTURE_COORD_ARRAY
                     GL_COLOR_ARRAY
                     GL_INDEX_ARRAY
                     GL_NORMAL_ARRAY
                     GL_VERTEX_ARRAY))
  (provide enable-client-state)
  (define (enable-client-state x)
    (let ((v (enable-client-state/disable-client-state-table-lookup x)))
      (if v
          (glEnableClientState v)
          (raise-type-error 'enable-client-state enable-client-state/disable-client-state-syms 0 x))))
  (provide disable-client-state)
  (define (disable-client-state x)
    (let ((v (enable-client-state/disable-client-state-table-lookup x)))
      (if v
          (glDisableClientState v)
          (raise-type-error 'disable-client-state enable-client-state/disable-client-state-syms 0 x))))

  (provide client-active-texture)
  (define (client-active-texture texture)
    (let ((v0 (multi-tex-coord-table-lookup texture)))
      (if v0
          (glClientActiveTexture v0)
          (raise-type-error 'client-active-texture multi-tex-coord-syms 0 texture))))
  
  (provide rect rect-v)
  (multi-type rect glRect (4) (d i f s) #f)
  (multi-type-v rect-v glRect (4) (dv iv fv sv) #f)
  
  (define-values (matrix-mode-table-lookup matrix-mode-syms)
    (make-enum-table GL_MODELVIEW GL_PROJECTION GL_TEXTURE))
  (provide matrix-mode)
  (define (matrix-mode x)
    (let ((v (matrix-mode-table-lookup x)))
      (if v
          (glMatrixMode v)
          (raise-type-error 'matrix-mode matrix-mode-syms 0 x))))
  (provide load-matrix)
  (define (load-matrix x)
    (cond
      ((gl-double-vector? x) (glLoadMatrixd x))
      ((gl-float-vector? x) (glLoadMatrixf x))
      (else (raise-type-error 'load-matrix "gl-double-vector, or gl-float-vector" x))))
  (provide mult-matrix)
  (define (mult-matrix x)
    (cond
      ((gl-double-vector? x) (glMultMatrixd x))
      ((gl-float-vector? x) (glMultMatrixf x))
      (else (raise-type-error 'mult-matrix "gl-double-vector or gl-float-vector" x))))
  
  (provide load-transpose-matrix mult-transpose-matrix)
  (define (load-transpose-matrix x)
    (cond
      ((gl-double-vector? x) (glLoadTransposeMatrixd x))
      ((gl-float-vector? x) (glLoadTransposeMatrixf x))
      (else (raise-type-error 'load-transpose-matrix "gl-double-vector or gl-float-vector" 0 x))))
  (define (mult-transpose-matrix x)
    (cond
      ((gl-double-vector? x) (glMultTransposeMatrixd x))
      ((gl-float-vector? x) (glMultTransposeMatrixf x))
      (else (raise-type-error 'mult-transpose-matrix "gl-double-vector or gl-float-vector" 0 x))))
  
  (provide active-texture)
  
  (define (active-texture texture)
    (let ((v (multi-tex-coord-table-lookup texture)))
      (if v
          (glActiveTexture v)
          (raise-syntax-error 'active-texture multi-tex-coord-syms v 0))))

  ;;(provide tex-gen tex-gen-v)
  (define-values (tex-gen-coord-table-lookup tex-gen-coord-syms)
    (make-enum-table GL_S GL_T GL_R GL_Q))
  (define-values (tex-gen-pname-table-lookup tex-gen-pname-syms)
    (make-enum-table GL_TEXTURE_GEN_MODE
                     GL_OBJECT_PLANE
                     GL_EYE_PLANE))
  ;; UNIMPLEMENTED
  (define tex-gen void)
  (define tex-gen-v void)
  

  (define-values (clip-plane-table-lookup clip-plane-syms)
    (make-enum-table GL_CLIP_PLANE0
                     GL_CLIP_PLANE1
                     GL_CLIP_PLANE2
                     GL_CLIP_PLANE3
                     GL_CLIP_PLANE4
                     GL_CLIP_PLANE5))
  (provide clip-plane)
  (define (clip-plane x y)
    (let ((v (clip-plane-table-lookup x)))
      (if v
          (glClipPlane v y)
          (raise-type-error 'clip-plane clip-plane-syms 0 x y))))
  
  (provide raster-pos raster-pos-v)
  (multi-type raster-pos glRasterPos (2 3 4) (i d) #t)
  (multi-type-v raster-pos-v glRasterPos (2 3 4) (dv iv fv sv) #t)
  
  (define-values (front-face-table-lookup front-face-syms)
    (make-enum-table GL_CCW GL_CW))
  (provide front-face)
  (define (front-face x)
    (let ((v (front-face-table-lookup x)))
      (if v
          (glFrontFace v)
          (raise-type-error 'front-face front-face-syms 0 x))))

  (provide material material-v)
  (define-values (material-face-table-lookup material-face-syms)
    (make-enum-table GL_FRONT
                     GL_BACK
                     GL_FRONT_AND_BACK))
  (define-values (material-pname-table-lookup material-pname-syms)
    (make-enum-table GL_SHININESS))
  
  (define (material face pname num)
    (let ((v0 (material-face-table-lookup face))
          (v1 (material-pname-table-lookup pname)))
      (cond
        ((and v0 v1)
         (cond
           ((and (real? num) (exact? num) (integer? num))
            (glMateriali v0 v1 num))
           ((real? num)
            (glMaterialf v0 v1 num))
           (else
            (raise-type-error 'material "exact integer or real number" 2 face pname num))))
        (v0 (raise-type-error 'material material-pname-syms 1 face pname num))
        (else (raise-type-error 'material material-face-syms 0 material pname num)))))
    
  (define-values (material-v-pname-table-lookup material-v-pname-syms)
    (make-enum-table GL_SHININESS
                     GL_COLOR_INDEXES
                     GL_EMISSION
                     GL_SPECULAR
                     GL_DIFFUSE
                     GL_AMBIENT
                     GL_AMBIENT_AND_DIFFUSE))
  
  (define (material-v face pname num-v)
    (let ((v0 (material-face-table-lookup face))
          (v1 (material-v-pname-table-lookup pname)))
      (cond
        ((and v0 v1)
         (cond
           ((gl-int-vector? num-v) (glMaterialiv v0 v1 num-v))
           ((gl-float-vector? num-v) (glMaterialfv v0 v1 num-v))
           (else
            (raise-type-error 'material-v "gl-int-vector or gl-float-vector" 2 face pname num-v))))
        (v0 (raise-type-error 'material-v material-v-pname-syms 1 light pname num-v))
        (else (raise-type-error 'material-v material-face-syms 0 material pname num-v)))))
  
  
  
  (provide light light-v)
  (define-values (light-light-table-lookup light-light-syms)
    (make-enum-table GL_LIGHT0
                     GL_LIGHT1
                     GL_LIGHT2
                     GL_LIGHT3
                     GL_LIGHT4
                     GL_LIGHT5
                     GL_LIGHT6
                     GL_LIGHT7))
  (define-values (light-pname-table-lookup light-pname-syms)
    (make-enum-table GL_SPOT_EXPONENT
                     GL_SPOT_CUTOFF
                     GL_CONSTANT_ATTENUATION
                     GL_LINEAR_ATTENUATION
                     GL_QUADRATIC_ATTENUATION))
  (define (light light pname num)
    (let ((v0 (light-light-table-lookup light))
          (v1 (light-pname-table-lookup pname)))
      (cond
        ((and v0 v1)
         (cond
           ((and (real? num) (exact? num) (integer? num))
            (glLighti v0 v1 num))
           ((real? num)
            (glLightf v0 v1 num))
           (else
            (raise-type-error 'light "exact integer or real number" 2 light pname num))))
        (v0 (raise-type-error 'light light-pname-syms 1 light pname num))
        (else (raise-type-error 'light light-light-syms 0 light pname num)))))
           
  (define-values (light-v-pname-table-lookup light-v-pname-syms)
    (make-enum-table GL_SPOT_EXPONENT
                     GL_SPOT_CUTOFF
                     GL_CONSTANT_ATTENUATION
                     GL_LINEAR_ATTENUATION
                     GL_QUADRATIC_ATTENUATION
                     GL_SPOT_DIRECTION
                     GL_POSITION
                     GL_SPECULAR
                     GL_DIFFUSE
                     GL_AMBIENT))
  (define (light-v light pname num-v)
    (let ((v0 (light-light-table-lookup light))
          (v1 (light-v-pname-table-lookup pname)))
      (cond
        ((and v0 v1)
         (cond
           ((gl-int-vector? num-v) (glLightiv v0 v1 num-v))
           ((gl-float-vector? num-v) (glLightfv v0 v1 num-v))
           (else
            (raise-type-error 'light-v "gl-int-vector or gl-float-vector" 2 light pname num-v))))
        (v0 (raise-type-error 'light-v light-pname-syms 1 light pname num-v))
        (else (raise-type-error 'light-v light-light-syms 0 light pname num-v)))))
      
      
  (provide light-model light-model-v)
  (define-values (light-model-table-lookup light-model-syms)
    (make-enum-table GL_LIGHT_MODEL_COLOR_CONTROL
                     GL_LIGHT_MODEL_LOCAL_VIEWER
                     GL_LIGHT_MODEL_TWO_SIDE))

  (define-values (light-model-v-table-lookup light-model-v-syms)
    (make-enum-table GL_LIGHT_MODEL_AMBIENT
                     GL_LIGHT_MODEL_COLOR_CONTROL
                     GL_LIGHT_MODEL_LOCAL_VIEWER
                     GL_LIGHT_MODEL_TWO_SIDE))

  (define (light-model pname num)
      (let ((v (light-model-table-lookup pname)))
        (if v
            (cond
              ((and (real? num) (exact? num) (integer? num))
               (glLightModeli v num))
              ((real? num)
               (glLightModelf v num))
              (else
               (raise-type-error 'light-model "exact integer or real number" 1 pname num)))
            (raise-type-error 'light-model light-model-syms 0 pname num))))

  (define (light-model-v pname num-v)
      (let ((v (light-model-table-lookup pname)))
        (if v
            (cond
              ((gl-int-vector? num-v)
               (glLightModeliv v num-v))
              ((gl-float-vector? num-v)
               (glLightModelfv v num-v))
              (else
               (raise-type-error 'light-model-v "gl-int-vector or gl-float-vector" 1 pname num-v)))
            (raise-type-error 'light-model-v light-model-syms 0 pname num-v))))
  
  (define-values (color-material-table-lookup0 color-material-syms0)
    (make-enum-table GL_FRONT
                     GL_BACK
                     GL_FRONT_AND_BACK))
  (define-values (color-material-table-lookup1 color-material-syms1)
    (make-enum-table GL_EMISSION
                     GL_AMBIENT
                     GL_DIFFUSE
                     GL_SPECULAR
                     GL_AMBIENT_AND_DIFFUSE))
  (provide color-material)
  (define (color-material x y)
    (let ((v0 (color-material-table-lookup0 x))
          (v1 (color-material-table-lookup1 y)))
      (cond
        ((and v0 v1) (glColorMaterial v0 v1))
        (v0 (raise-type-error 'color-material color-material-syms1 1 x y))
        (else (raise-type-error 'color-material color-material-syms0 0 x y)))))
  
  (define-values (shade-model-table-lookup shade-model-syms)
    (make-enum-table GL_FLAT GL_SMOOTH))
  (provide shade-model)
  (define (shade-model x)
    (let ((v (shade-model-table-lookup x)))
      (if v
          (glShadeModel v)
          (raise-type-error 'shade-model shade-model-syms 0 x))))

  (define-values (cull-face-table-lookup cull-face-syms)
    (make-enum-table GL_FRONT GL_BACK GL_FRONT_AND_BACK))
  (provide cull-face)
  (define (cull-face x)
    (let ((v (cull-face-table-lookup x)))
      (if v
          (glCullFace v)
          (raise-type-error 'cull-face cull-face-syms 0 x))))
  (define-values (polygon-mode-table-lookup0 polygon-mode-syms0)
    (make-enum-table GL_FRONT
                     GL_BACK
                     GL_FRONT_AND_BACK))
  (define-values (polygon-mode-table-lookup1 polygon-mode-syms1)
    (make-enum-table GL_POINT
                     GL_LINE
                     GL_FILL))
  (provide polygon-mode)
  (define (polygon-mode x y)
    (let ((v0 (polygon-mode-table-lookup0 x))
          (v1 (polygon-mode-table-lookup1 y)))
      (cond
        ((and v0 v1) (polygon-mode v0 v1))
        (v0 (raise-type-error 'polygon-mode polygon-mode-syms1 1 x y))
        (else (raise-type-error 'polygon-mode polygon-mode-syms0 0 x y)))))

  ;; pixel-store
  ;; pixel-transfer
  ;; pixel-map
  ;; color-table 1.2
  ;; color-table-parameter 1.2
  ;; copy-color-table 1.2
  ;; color-sub-table 1.2
  ;; copy-color-sub-table 1.2
  ;; convolution-filter-2D 1.2
  ;; convolution-filter-1D 1.2
  ;; separable-filter-2D 1.2
  ;; copy-convolution-filter-2D 1.2
  ;; copy-convolution-filter-1D 1.2
  ;; histogram 1.2
  ;; minmax 1.2
  ;; draw-pixels
  ;; convolution-parameter 1.2
  ;; tex-image-3D 1.2
  ;; tex-image-2D
  ;; tex-image-1D
  ;; copy-tex-image-2D
  ;; copy-tex-image-1D
  ;; tex-sub-image-3D 1.2
  ;; tex-sub-image-2D
  ;; tex-sub-image-1D
  ;; copy-tex-sub-image-3D 1.2
  ;; copy-tex-sub-image-2D
  ;; copy-tex-sub-image-1D
  ;; tex-parameter
  ;; bind-texture
  ;; tex-env
  ;; fog
  ;; sample-coverage 1.3
  ;; alpha-func
  ;; stencil-func
  ;; stencil-op
  ;; depth-func
  ;; blend-color 1.2
  ;; blend-equation 1.2
  ;; blend-func
  ;; logic-op
  ;; draw-buffer
  
  (define-values (clear-table-lookup clear-syms)
    (make-enum-table GL_ACCUM_BUFFER_BIT
                     GL_COLOR_BUFFER_BIT
                     GL_DEPTH_BUFFER_BIT
                     GL_STENCIL_BUFFER_BIT))
  (provide clear)
  (define clear
    (lambda x
      (glClear
       (let loop ((vals x)
                  (i 0))
         (cond
           ((null? vals) 0)
           (else
            (let ((v (clear-table-lookup (car vals))))
              (if v
                  (bitwise-ior v (loop (cdr vals) (add1 i)))
                  (apply raise-type-error (list* 'clear clear-syms i x))))))))))
  
  ;; accum
  ;; read-pixels
  ;; copy-pixels
  ;; map
  
  (provide eval-coord eval-coord-v)
  (multi-type eval-coord glEvalCoord (1 2) (d) #t)
  (multi-type-v eval-coord-v glEvalCoord (1 2) (dv fv) #t)
  
  ;; map-grid
  ;; eval-mesh
  ;; eval-point
  ;; render-mode
  ;; feedback-buffer

  (provide new-list)
  (define-values (new-list-table-lookup new-list-syms)
    (make-enum-table GL_COMPILE GL_COMPILE_AND_EXECUTE))
  
  (define (new-list n mode)
    (let ((v (new-list-table-lookup mode)))
      (if v
          (glNewList n v)
          (raise-type-error 'new-list new-list-syms 1 n mode))))

           
  ;; call-lists
  ;; hint
  ;; push-attrib
  ;; push-client-attrib
  
  (define-values (enable/disable-table-lookup enable/disable-syms)
    (make-enum-table GL_NORMALIZE
                     GL_TEXTURE_GEN_S
                     GL_TEXTURE_GEN_T
                     GL_TEXTURE_GEN_R
                     GL_TEXTURE_GEN_Q
                     GL_CLIP_PLANE0
                     GL_CLIP_PLANE1
                     GL_CLIP_PLANE2
                     GL_CLIP_PLANE3
                     GL_CLIP_PLANE4
                     GL_CLIP_PLANE5
                     GL_LIGHTING
                     GL_LIGHT0
                     GL_LIGHT1
                     GL_LIGHT2
                     GL_LIGHT3
                     GL_LIGHT4
                     GL_LIGHT5
                     GL_LIGHT6
                     GL_LIGHT7
                     GL_COLOR_MATERIAL
                     GL_POINT_SMOOTH
                     GL_LINE_SMOOTH
                     GL_LINE_STIPPLE
                     GL_POLYGON_SMOOTH
                     GL_CULL_FACE
                     GL_POLYGON_STIPPLE
                     GL_POLYGON_OFFSET_POINT
                     GL_POLYGON_OFFSET_LINE
                     GL_POLYGON_OFFSET_FILL
                     GL_TEXTURE_1D
                     GL_TEXTURE_2D
                     GL_FOG
                     GL_SCISSOR_TEST
                     GL_ALPHA_TEST
                     GL_STENCIL_TEST
                     GL_DEPTH_TEST
                     GL_BLEND
                     GL_DITHER
                     GL_INDEX_LOGIC_OP
                     GL_LOGIC_OP
                     GL_COLOR_LOGIC_OP
                     GL_MAP1_VERTEX_3
                     GL_MAP1_VERTEX_4
                     GL_MAP1_INDEX
                     GL_MAP1_COLOR_4
                     GL_MAP1_NORMAL
                     GL_MAP1_TEXTURE_COORD_1
                     GL_MAP1_TEXTURE_COORD_2
                     GL_MAP1_TEXTURE_COORD_3
                     GL_MAP1_TEXTURE_COORD_4
                     GL_MAP2_VERTEX_3
                     GL_MAP2_VERTEX_4
                     GL_MAP2_INDEX
                     GL_MAP2_COLOR_4
                     GL_MAP2_NORMAL
                     GL_MAP2_TEXTURE_COORD_1
                     GL_MAP2_TEXTURE_COORD_2
                     GL_MAP2_TEXTURE_COORD_3
                     GL_MAP2_TEXTURE_COORD_4
                     GL_AUTO_NORMAL))
  (provide enable)
  (define (enable x)
    (let ((v (enable/disable-table-lookup x)))
      (if v
          (glEnable v)
          (raise-type-error 'enable enable/disable-syms 0 x))))
  (provide disable)
  (define (disable x)
    (let ((v (enable/disable-table-lookup x)))
      (if v
          (glDisable v)
          (raise-type-error 'disable enable/disable-syms 0 x))))
  
  ;; is-enabled
  ;; get-clip-plane
  ;; get-light
  ;; get-material
  ;; get-polygon-stipple
  ;; get-color-table-parameter 1.2
  ;; get-convolution-parameter 1.2
  ;; reset-histogram
  ;; get-histogram-parameter
  ;; reset-minmax
  ;; get-minmax-parameter

  (define-values (get-string-table-lookup get-string-syms)
    (make-enum-table GL_VENDOR
                     GL_RENDERER
                     GL_VERSION
                     GL_EXTENSIONS))
  (provide get-string)
  (define (get-string x)
    (let ((v (get-string-table-lookup x)))
      (if v
          (glGetString v)
          (raise-type-error 'get-string get-string-syms 0 x))))
  

  ) 