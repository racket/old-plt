(module gl mzscheme
  (require (lib "foreign.ss")
           "gl-types.ss"
           "gl-info.ss"
           "gl-vectors.ss")
  
  (provide (all-from-except "gl-info.ss"
                            gl-byte-size gl-ubyte-size
                            gl-short-size gl-ushort-size
                            gl-int-size gl-uint-size
                            gl-float-size gl-double-size
                            gl-boolean-size gl-sizei-size
                            gl-clampf-size gl-clampd-size
                            gl-enum-size gl-bitfield-size))
  
  (unsafe!)
  (define gl-lib (case (system-type)
		   [(windows) (ffi-lib "opengl32")]
                   [(macosx) (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL")]
		   [else (ffi-lib "libGL")]))
  (define glu-lib (case (system-type)
		   [(windows) (ffi-lib "glu32")]
                   [(macosx) (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGLU")]
		   [else (ffi-lib "libGLU")]))
  
  (define-syntax define-foreign-lib
    (syntax-rules (->)
      ((_ lib name type ... ->)
       (define-foreign-lib lib name type ... -> _void))
      ((_ lib name type ...)
       (begin
         ;(printf "~a~n" 'name)
         (provide name)
         (define name
           (get-ffi-obj 'name lib (_fun type ...)
                        (lambda ()
                          (lambda x
                            (error 'name "unavailable on this system")))))))))

  (define-syntax define-foreign
    (syntax-rules ()
      ((_ args ...)
       (define-foreign-lib gl-lib args ...))))
      
  (define-for-syntax (get-type x err)
    (case (syntax-object->datum x)
      ((b) #'_gl-byte)
      ((s) #'_gl-short)
      ((i) #'_gl-int)
      ((f) #'_gl-float)
      ((d) #'_gl-double)
      ((ub) #'_gl-ubyte)
      ((us) #'_gl-ushort)
      ((ui) #'_gl-uint)
      ((bv) #'_gl-bytev)
      ((sv) #'_gl-shortv)
      ((iv) #'_gl-intv)
      ((fv) #'_gl-floatv)
      ((dv) #'_gl-doublev)
      ((ubv) #'_gl-ubytev)
      ((usv) #'_gl-ushortv)
      ((uiv) #'_gl-uintv)
      (else (raise-syntax-error #f "unknown GL type abbreviation" err x))))

  (define-for-syntax (get-vtype x err)
    (case (syntax-object->datum x)
      ((bv) #'_gl-byte)
      ((sv) #'_gl-short)
      ((iv) #'_gl-int)
      ((fv) #'_gl-float)
      ((dv) #'_gl-double)
      ((ubv) #'_gl-ubyte)
      ((usv) #'_gl-ushort)
      ((uiv) #'_gl-uint)
      (else (raise-syntax-error #f "unknown GL type abbreviation" err x))))
  
  (define-for-syntax (type-map type convert)
    (syntax-case type (: =)
      ((label : type) #`(label : #,(type-map #'type convert)))
      ((type = expr) #`(#,(type-map #'type convert) = expr))
      ((label : type = expr) #`(label : #,(type-map #'type convert) = expr))
      (_
       (convert type))))
                                 
  (define-syntax (define-foreign-tparm stx)
    (syntax-case stx (->)
      ((_ name (suffix ...) type ...)
       (let* ((name-sym (syntax-object->datum #'name))
              (build-def
               (lambda (suffix)
                 (with-syntax ((new-name
                                (datum->syntax-object
                                 #'name
                                 (string->symbol
                                  (format "~a~a"
                                          name-sym 
                                          (syntax-object->datum suffix)))
                                 #'name))
                               ((new-type ...)
                                (map
                                 (lambda (type)
                                   (type-map
                                    type
                                    (lambda (type)
                                      (syntax-case type (T outT)
                                        (T (get-type suffix stx))
                                        ((T n)
                                         #`(_cvector o
                                                     #,(get-vtype suffix stx)
                                                     n))
                                        (_ type)))))
                                 (syntax->list #'(type ...)))))
                   #'(define-foreign new-name new-type ...)))))
         (with-syntax (((defs ...)
                        (map build-def (syntax->list #'(suffix ...)))))
           #'(begin defs ...))))))
  
  ;; 2.5
  (define-foreign glGetError -> _gl-enum)
  
  ;; 2.6.1
  (define-foreign glBegin _gl-enum ->)
  (define-foreign glEnd ->)
  
  ;; 2.6.2
  (define-foreign glEdgeFlag _gl-boolean ->)
  (define-foreign glEdgeFlagv _gl-booleanv ->)

  ;; 2.7
  (define-foreign-tparm glVertex2 (s i f d) T T ->)
  (define-foreign-tparm glVertex3 (s i f d) T T T ->)
  (define-foreign-tparm glVertex4 (s i f d) T T T T ->)
  (define-foreign-tparm glVertex2 (sv iv fv dv) T ->)
  (define-foreign-tparm glVertex3 (sv iv fv dv) T ->)
  (define-foreign-tparm glVertex4 (sv iv fv dv) T ->)
  (define-foreign-tparm glTexCoord1 (s i f d) T ->)
  (define-foreign-tparm glTexCoord2 (s i f d) T T ->)
  (define-foreign-tparm glTexCoord3 (s i f d) T T T ->)
  (define-foreign-tparm glTexCoord4 (s i f d) T T T T ->)
  (define-foreign-tparm glTexCoord1 (sv iv fv dv) T ->)
  (define-foreign-tparm glTexCoord2 (sv iv fv dv) T ->)
  (define-foreign-tparm glTexCoord3 (sv iv fv dv) T ->)
  (define-foreign-tparm glTexCoord4 (sv iv fv dv) T ->)
  (define-foreign-tparm glMultiTexCoord1 (s i f d) _gl-enum T ->)
  (define-foreign-tparm glMultiTexCoord2 (s i f d) _gl-enum T T ->)
  (define-foreign-tparm glMultiTexCoord3 (s i f d) _gl-enum T T T ->)
  (define-foreign-tparm glMultiTexCoord4 (s i f d) _gl-enum T T T T ->)
  (define-foreign-tparm glMultiTexCoord1 (sv iv fv dv) _gl-enum T ->)
  (define-foreign-tparm glMultiTexCoord2 (sv iv fv dv) _gl-enum T ->)
  (define-foreign-tparm glMultiTexCoord3 (sv iv fv dv) _gl-enum T ->)
  (define-foreign-tparm glMultiTexCoord4 (sv iv fv dv) _gl-enum T ->)
  (define-foreign-tparm glNormal3 (b s i f d) T T T ->)
  (define-foreign-tparm glNormal3 (bv sv iv fv dv) T ->)
  (define-foreign-tparm glFogCoord (f d) T ->)
  (define-foreign-tparm glFogCoord (fv dv) T ->)
  (define-foreign-tparm glColor3 (b s i f d ub us ui) T T T ->)
  (define-foreign-tparm glColor4 (b s i f d ub us ui) T T T T ->)
  (define-foreign-tparm glColor3 (bv sv iv fv dv ubv usv uiv) T ->)
  (define-foreign-tparm glColor4 (bv sv iv fv dv ubv usv uiv) T ->)
  (define-foreign-tparm glSecondaryColor3 (b s i f d ub us ui) T T T ->)
  (define-foreign-tparm glSecondaryColor3 (bv sv iv fv dv ubv usv uiv) T ->)
  (define-foreign-tparm glIndex (s i f d ub) T ->)
  (define-foreign-tparm glIndex (sv iv fv dv ubv) T ->)
  
  ;; 2.8
  #|
  (define-foreign glVertexPointer _gl-int _gl-enum _gl-sizei _gl-voidv ->)
  (define-foreign glNormalPointer _gl-enum _gl-sizei _gl-voidv ->)
  (define-foreign glColorPointer _gl-int _gl-enum _gl-sizei _gl-voidv ->)
  (define-foreign glSecondaryColorPointer
                  _gl-int _gl-enum _gl-sizei _gl-voidv ->)
  (define-foreign glIndexPointer _gl-enum _gl-sizei _gl-voidv ->)
  (define-foreign glFogCoordPointer _gl-enum _gl-sizei _gl-voidv ->)
  (define-foreign glTexCoordPointer _gl-int _gl-enum _gl-sizei _gl-voidv ->)
  (define-foreign glEdgeFlagPointer _gl-sizei _gl-voidv ->)
  (define-foreign glEnableClientState _gl-enum ->)
  (define-foreign glDisableClientState _gl-enum ->)
  (define-foreign glClientActiveTexture _gl-enum ->)
  (define-foreign glArrayElement _gl-int ->)
  (define-foreign glDrawArrays _gl-enum _gl-int _gl-sizei ->)
  (define-foreign glMultiDrawArrays _gl-enum _gl-intv _gl-sizeiv _gl-sizei ->)
  (define-foreign glDrawElements _gl-enum _gl-sizei _gl-enum _gl-voidv ->)
  (define-foreign glMultiDrawElements
                  _gl-enum _gl-sizeiv _gl-enum _gl-voidvv _gl-sizei ->)
  (define-foreign glDrawRangeElements
                  _gl-enum _gl-uint _gl-uint _gl-sizei _gl-enum _gl-voidv ->)
  (define-foreign glInterleavedArrays _gl-enum _gl-sizei _gl-voidv)
  |#
  
  ;; 2.9
  #|
  (define-foreign glBindBuffer _gl-enum _gl-uint ->)
  (define-foreign glDeleteBuffers _gl-sizei gl_uintv ->)
  (define-foreign glGenBuffers _gl-sizei gl_uintv ->)
  (define-foreign glBufferData _gl-enum _gl-sizeiptr _gl_voidv _gl-enum ->)
  (define-foreign glBufferSubData _gl-enum _gl-intptr _gl-sizeiptr _gl_voidv ->)
  (define-foreign glMapBuffer _gl-enum _gl-enum -> _gl-voidv)
  (define-foreign glUnmapBuffer _gl-enum -> _gl-boolean)
  |#
  
  ;; 2.10
  (define-foreign-tparm glRect (s i f d) T T T T ->)
  (define-foreign-tparm glRect (sv iv fv dv) T ->)
  
  ;; 2.11.1
  (define-foreign glDepthRange _gl-clampd _gl-clampd ->)
  (define-foreign glViewport _gl-int _gl-int _gl-sizei _gl-sizei ->)

  ;; 2.11.2
  (define-foreign glMatrixMode _gl-enum ->)
  (define-foreign glLoadMatrixf _gl-floatv ->)
  (define-foreign glLoadMatrixd _gl-doublev ->)
  (define-foreign glMultMatrixf _gl-floatv ->)
  (define-foreign glMultMatrixd _gl-doublev ->)
  (define-foreign glLoadTransposeMatrixf _gl-floatv ->)
  (define-foreign glLoadTransposeMatrixd _gl-doublev ->)
  (define-foreign glMultTransposeMatrixf _gl-floatv ->)
  (define-foreign glMultTransposeMatrixd _gl-doublev ->)
  (define-foreign glLoadIdentity ->)
  (define-foreign-tparm glRotate (f d) T T T T ->)
  (define-foreign-tparm glTranslate (f d) T T T ->)
  (define-foreign-tparm glScale (f d) T T T ->)
  (define-foreign glFrustum _gl-double _gl-double _gl-double
                            _gl-double _gl-double _gl-double ->)
  (define-foreign glOrtho _gl-double _gl-double _gl-double
                          _gl-double _gl-double _gl-double ->)
  (define-foreign glActiveTexture _gl-enum ->)
  (define-foreign glPushMatrix ->)
  (define-foreign glPopMatrix ->)
  
  ;; 2.11.3
  (define-foreign glEnable _gl-enum ->)
  (define-foreign glDisable _gl-enum ->)
  
  ;; 2.11.4
  (define-foreign-tparm glTexGen (i f d) _gl-enum _gl-enum T ->)
  (define-foreign-tparm glTexGen (iv fv dv) _gl-enum _gl-enum T ->)
  
  ;; 2.12
  (define-foreign glClipPlane _gl-enum _gl-doublev ->)
  
  ;; 2.13
  (define-foreign-tparm glRasterPos2 (s i f d) T T ->)
  (define-foreign-tparm glRasterPos3 (s i f d) T T T ->)
  (define-foreign-tparm glRasterPos4 (s i f d) T T T T ->)
  (define-foreign-tparm glRasterPos2 (sv iv fv dv) T ->)
  (define-foreign-tparm glRasterPos3 (sv iv fv dv) T ->)
  (define-foreign-tparm glRasterPos4 (sv iv fv dv) T ->)
  (define-foreign-tparm glWindowPos2 (s i f d) T T ->)
  (define-foreign-tparm glWindowPos3 (s i f d) T T T ->)
  (define-foreign-tparm glWindowPos2 (sv iv fv dv) T ->)
  (define-foreign-tparm glWindowPos3 (sv iv fv dv) T ->)
  
  ;; 2.14.1
  (define-foreign glFrontFace _gl-enum ->)
  
  ;; 2.14.2
  (define-foreign-tparm glMaterial (i f) _gl-enum _gl-enum T ->)
  (define-foreign-tparm glMaterial (iv fv) _gl-enum _gl-enum T ->)
  (define-foreign-tparm glLight (i f) _gl-enum _gl-enum T ->)
  (define-foreign-tparm glLight (iv fv) _gl-enum _gl-enum T ->)
  (define-foreign-tparm glLightModel (i f) _gl-enum T ->)
  (define-foreign-tparm glLightModel (iv fv) _gl-enum T ->)
  
  ;; 2.14.3
  (define-foreign glColorMaterial _gl-enum _gl-enum ->)
  
  ;; 2.14.7
  (define-foreign glShadeModel _gl-enum ->)
  
  ;; 3.3
  (define-foreign glPointSize _gl-float ->)
  (define-foreign-tparm glPointParameter (i f) _gl-enum _gl-float ->)
  (define-foreign-tparm glPointParameter (iv fv) _gl-enum _gl-floatv ->)
  
  ;; 3.4
  (define-foreign glLineWidth _gl-float ->)
  
  ;; 3.4.2
  (define-foreign glLineStipple _gl-int _gl-ushort ->)
  
  ;; 3.5.1
  (define-foreign glCullFace _gl-enum ->)
  
  ;; 3.5.2
  (define-foreign glPolygonStipple _gl-ubytev ->)
  
  ;; 3.5.4
  (define-foreign glPolygonMode _gl-enum _gl-enum ->)
  
  ;; 3.5.5
  (define-foreign glPolygonOffset _gl-float _gl-float ->)
  
  ;; 3.6.1
  (define-foreign-tparm glPixelStore (i f) _gl-enum T ->)
  
  ;; 3.6.3
  (define-foreign-tparm glPixelTransfer (i f) _gl-enum T ->)
  (define-foreign-tparm glPixelMap (uiv usv fv)
                        _gl-enum (_gl-sizei = (cvector-length l)) (l : T) ->)
  (define-foreign glColorTable
                  _gl-enum _gl-enum _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign-tparm glColorTableParameter (iv fv) _gl-enum _gl-enum T ->)
  (define-foreign glCopyColorTable
                  _gl-enum _gl-enum _gl-int _gl-int _gl-sizei ->)
  (define-foreign glColorSubTable
                  _gl-enum _gl-sizei _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glCopyColorSubTable
                  _gl-enum _gl-sizei _gl-int _gl-int _gl-sizei ->)
  (define-foreign glConvolutionFilter2D _gl-enum _gl-enum _gl-sizei _gl-sizei
                                        _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign-tparm glConvolutionParameter (iv fv) _gl-enum _gl-enum T ->)
  (define-foreign glConvolutionFilter1D
                  _gl-enum _gl-enum _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glSeparableFilter2D _gl-enum _gl-enum _gl-sizei _gl-sizei
                                      _gl-enum _gl-enum _gl-voidv _gl-voidv ->)
  (define-foreign glCopyConvolutionFilter2D
                  _gl-enum _gl-enum _gl-int _gl-int _gl-sizei _gl-sizei ->)
  (define-foreign glCopyConvolutionFilter1D
                  _gl-enum _gl-enum _gl-int _gl-int _gl-sizei ->)
  (define-foreign glHistogram _gl-enum _gl-sizei _gl-enum _gl-boolean ->)
  (define-foreign glMinmax _gl-enum _gl-enum _gl-boolean ->)
  
  ;; 3.6.4
  (define-foreign glDrawPixels
                  _gl-sizei _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glPixelZoom _gl-float _gl-float ->)
  
  ;; 3.6.5
  (define-foreign-tparm  glConvolutionParameter (i f) _gl-enum _gl-enum T ->)

  ;; 3.7
  (define-foreign glBitmap _gl-sizei _gl-sizei _gl-float _gl-float _gl-float
                           _gl-float _gl-ubytev ->)

  ;; 3.8.1
  (define-foreign glTexImage3D _gl-enum _gl-int _gl-int _gl-sizei _gl-sizei
                               _gl-sizei _gl-int _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glTexImage2D _gl-enum _gl-int _gl-int _gl-sizei _gl-sizei
                               _gl-int _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glTexImage1D _gl-enum _gl-int _gl-int _gl-sizei _gl-int 
                               _gl-enum _gl-enum _gl-voidv ->)

  ;; 3.8.2
  (define-foreign glCopyTexImage2D _gl-enum _gl-int _gl-enum _gl-int _gl-int
                                   _gl-sizei _gl-sizei _gl-int ->)
  (define-foreign glCopyTexImage1D _gl-enum _gl-int _gl-enum _gl-int _gl-int
                                   _gl-sizei _gl-int ->)
  (define-foreign glTexSubImage3D
                  _gl-enum _gl-int _gl-int _gl-int _gl-int _gl-sizei _gl-sizei 
                  _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glTexSubImage2D
                  _gl-enum _gl-int _gl-int _gl-int _gl-sizei _gl-sizei 
                  _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glTexSubImage1D _gl-enum _gl-int _gl-int _gl-sizei 
                                  _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glCopyTexSubImage3D
                  _gl-enum _gl-int _gl-int _gl-int _gl-int _gl-int _gl-int 
                  _gl-sizei _gl-sizei ->)
  (define-foreign glCopyTexSubImage2D
                  _gl-enum _gl-int _gl-int _gl-int _gl-int _gl-int
                  _gl-sizei _gl-sizei ->)
  (define-foreign glCopyTexSubImage1D
                  _gl-enum _gl-int _gl-int _gl-int _gl-int _gl-sizei ->)

  ;; 3.8.3
  (define-foreign glCompressedTexImage1D
                  _gl-enum _gl-int _gl-enum _gl-sizei _gl-int _gl-sizei
                  _gl-voidv ->)
  (define-foreign glCompressedTexImage2D
                  _gl-enum _gl-int _gl-enum _gl-sizei _gl-sizei _gl-int
                  _gl-sizei _gl-voidv ->)
  (define-foreign glCompressedTexImage3D
                  _gl-enum _gl-int _gl-enum _gl-sizei _gl-sizei _gl-sizei
                  _gl-int _gl-sizei _gl-voidv ->)
  (define-foreign glCompressedTexSubImage1D
                  _gl-enum _gl-int _gl-int _gl-sizei _gl-enum _gl-sizei
                  _gl-voidv ->)
  (define-foreign glCompressedTexSubImage2D
                  _gl-enum _gl-int _gl-int _gl-int _gl-sizei _gl-sizei _gl-enum
                  _gl-sizei _gl-voidv ->)
  (define-foreign glCompressedTexSubImage3D
                  _gl-enum _gl-int _gl-int _gl-int _gl-int _gl-sizei _gl-sizei
                  _gl-sizei _gl-enum _gl-sizei _gl-voidv ->)
  
  ;; 3.8.4
  (define-foreign-tparm glTexParameter (i f) _gl-enum _gl-enum T ->)
  (define-foreign-tparm glTexParameter (iv fv) _gl-enum _gl-enum T ->)
  
  ;; 3.8.12
  (define-foreign glBindTexture _gl-enum _gl-uint ->)
  (define-foreign glDeleteTextures
                  (_gl-sizei = (cvector-length v)) (v : _gl-uintv) ->)
  (define-foreign glGenTextures
                  (n : _gl-sizei) (r : (_cvector o _gl-uint n)) -> _void -> r)
  (define-foreign glAreTexturesResident
                  (n : _gl-sizei = (cvector-length v)) (v : _gl-uintv)
                  (r : (_cvector o _gl-boolean n)) -> 
                  (r2 : _gl-boolean) -> (values r2 r))
  
  ;; 3.8.13
  (define-foreign-tparm glTexEnv (i f) _gl-enum _gl-enum T ->)
  (define-foreign-tparm glTexEnv (iv fv) _gl-enum _gl-enum T ->)
  
  ;; 3.10
  (define-foreign-tparm glFog (i f) _gl-enum T ->)
  (define-foreign-tparm glFog (iv fv) _gl-enum T ->)

  ;; 4.1.2
  (define-foreign glScissor _gl-int _gl-int _gl-sizei _gl-sizei ->)
  
  ;; 4.1.3
  (define-foreign glSampleCoverage _gl-clampf _gl-boolean ->)
  
  ;; 4.1.4
  (define-foreign glAlphaFunc _gl-enum _gl-clampf ->)
  
  ;; 4.1.5
  (define-foreign glStencilFunc _gl-enum _gl-int _gl-uint ->)
  (define-foreign glStencilOp _gl-enum _gl-enum _gl-enum ->)
  
  ;; 4.1.6
  (define-foreign glDepthFunc _gl-enum ->)
  
  ;; 4.1.7
  (define-foreign glBeginQuery _gl-enum _gl-uint ->)
  (define-foreign glEndQuery _gl-enum ->)
  (define-foreign glGenQueries
                  (n : _gl-sizei) (r : (_cvector o _gl-uint n)) -> _void -> r)
  (define-foreign glDeleteQueries  (_gl-sizei = (cvector-length v)) (v : _gl-uintv) ->)
  
  ;; 4.1.8
  (define-foreign glBlendEquation _gl-enum ->)
  (define-foreign glBlendFuncSeparate _gl-enum _gl-enum _gl-enum _gl-enum ->)
  (define-foreign glBlendFunc _gl-enum _gl-enum ->)
  (define-foreign glBlendColor _gl-clampf _gl-clampf _gl-clampf _gl-clampf ->)
  
  ;; 4.1.10
  (define-foreign glLogicOp _gl-enum ->)
  
  ;; 4.2.1 
  (define-foreign glDrawBuffer _gl-enum ->)
  
  ;; 4.2.2
  (define-foreign glIndexMask _gl-uint ->)
  (define-foreign glColorMask
                  _gl-boolean _gl-boolean _gl-boolean _gl-boolean ->)
  (define-foreign glDepthMask _gl-boolean ->)
  (define-foreign glStencilMask _gl-uint ->)
  
  ;; 4.2.3
  (define-foreign glClear _gl-bitfield ->)
  (define-foreign glClearColor _gl-clampf _gl-clampf _gl-clampf _gl-clampf ->)
  (define-foreign glClearIndex _gl-float ->)
  (define-foreign glClearDepth _gl-clampd ->)
  (define-foreign glClearStencil _gl-int ->)
  (define-foreign glClearAccum _gl-float _gl-float _gl-float _gl-float ->)
  
  ;; 4.2.4
  (define-foreign glAccum _gl-enum _gl-float ->)
  
  ;; 4.3.2
  (define-foreign glReadPixels _gl-int _gl-int _gl-sizei _gl-sizei
                               _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glReadBuffer _gl-enum ->)
  
  ;; 4.3.3
  (define-foreign glCopyPixels _gl-int _gl-int _gl-sizei _gl-sizei _gl-enum ->)
  
  ;; 5.1
  ;; Map1 and 2 appear to have a bug in the spec where the last argument is not 
  ;; declared as a pointer
  (define-foreign-tparm glMap1 (f) _gl-enum T T _gl-int _gl-int _gl-floatv ->)
  (define-foreign-tparm glMap1 (d) _gl-enum T T _gl-int _gl-int _gl-doublev ->)
  (define-foreign-tparm glMap2 (f)
                        _gl-enum T T _gl-int _gl-int T T _gl-int _gl-int
                        _gl-floatv ->)
  (define-foreign-tparm glMap2 (d)
                        _gl-enum T T _gl-int _gl-int T T _gl-int _gl-int
                        _gl-doublev ->)
  (define-foreign-tparm glEvalCoord1 (f d) T ->)
  (define-foreign-tparm glEvalCoord2 (f d) T T ->)
  (define-foreign-tparm glEvalCoord1 (fv dv) T ->)
  (define-foreign-tparm glEvalCoord2 (fv dv) T ->)
  (define-foreign-tparm glMapGrid1 (f d) _gl-int T T ->)
  (define-foreign-tparm glMapGrid2 (f d) _gl-int T T _gl-int T T ->)
  (define-foreign glEvalMesh1 _gl-enum _gl-int _gl-int ->)
  (define-foreign glEvalMesh2 _gl-enum _gl-int _gl-int _gl-int _gl-int ->)
  (define-foreign glEvalPoint1 _gl-int ->)
  (define-foreign glEvalPoint2 _gl-int _gl-int ->)
  
  ;; 5.2
  (define-foreign glInitNames ->)
  (define-foreign glPopName ->)
  (define-foreign glPushName _gl-uint ->)
  (define-foreign glLoadName _gl-uint ->)
  (define-foreign glRenderMode _gl-enum -> _gl-int)
  (define-struct select-buffer-object (ptr len))
  (provide select-buffer->gl-uint-vector)
  (define (select-buffer->gl-uint-vector sbo)
    (unless (select-buffer-object? sbo)
      (raise-type-error 'select-buffer->gl-uint-vector "select-buffer-object" sbo))
    (let* ((l (select-buffer-object-len sbo))
           (p (select-buffer-object-ptr sbo))
           (v (make-gl-uint-vector l)))
      (let loop ((i 0))
        (when (< i l)
          (gl-vector-set! v i (ptr-ref p _gl-uint i))
          (loop (add1 i))))
      v))
  
  (define-foreign glSelectBuffer
                  (n : _gl-sizei) (mem : _pointer = (malloc n _gl-uint 'raw)) ->
                  _void ->
                  (let ((o (make-select-buffer-object mem n)))
                    (register-finalizer o (lambda (sbo)
                                            (free (select-buffer-object-ptr sbo))))
                    o))

                  
  ;; 5.3
  (define-struct feedback-buffer-object (ptr len))
  (provide feedback-buffer->gl-float-vector)
  (define (feedback-buffer->gl-float-vector fbo)
    (unless (feedback-buffer-object? fbo)
      (raise-type-error 'feedback-buffer->gl-uint-vector "feedback-buffer-object" fbo))
    (let* ((l (feedback-buffer-object-len fbo))
           (p (feedback-buffer-object-ptr fbo))
           (v (make-gl-float-vector l)))
      (let loop ((i 0))
        (when (< i l)
          (gl-vector-set! v i (ptr-ref p _gl-float i))
          (loop (add1 i))))
      v))
  (define-foreign glFeedbackBuffer
                  (n : _gl-sizei) _gl-enum (mem : _pointer = (malloc _gl-float n 'raw)) ->
                  _void ->
                  (let ((o (make-feedback-buffer-object mem n)))
                    (register-finalizer o (lambda (fbo)
                                            (free (feedback-buffer-object-ptr fbo))))
                    o))
  (define-foreign glPassThrough _gl-float ->)
  
  ;; 5.4
  (define-foreign glNewList _gl-uint _gl-enum ->)
  (define-foreign glEndList ->)
  (define-foreign glCallList _gl-uint ->)
  (define-foreign glCallLists _gl-sizei _gl-enum _gl-voidv ->)
  (define-foreign glListBase _gl-uint ->)
  (define-foreign glGenLists _gl-sizei -> _gl-uint)
  (define-foreign glIsList _gl-uint -> _gl-boolean)
  (define-foreign glDeleteLists _gl-uint _gl-sizei ->)
  
  ;; 5.5
  (define-foreign glFlush ->)
  (define-foreign glFinish ->)

  ;; 5.6
  (define-foreign glHint _gl-enum _gl-enum ->)

  ;; 6.1.1
  (define-foreign glGetBooleanv
                  _gl-enum (n : _?) (r : (_cvector o _gl-boolean n)) ->
                  _void -> r)
  (define-foreign glGetIntegerv
                  _gl-enum (n : _?) (r : (_cvector o _gl-int n)) -> _void -> r)
  (define-foreign glGetFloatv
                  _gl-enum (n : _?) (r : (_cvector o _gl-float n)) ->
                  _void -> r)
  (define-foreign glGetDoublev
                  _gl-enum (n : _?) (r : (_cvector o _gl-double n)) ->
                  _void -> r)
  (define-foreign glIsEnabled _gl-enum -> _gl-boolean)
  
  ;; 6.1.3
  (define-foreign glGetClipPlane
                  _gl-enum (r : (_cvector o _gl-double 4)) -> _void -> r)
  (define-foreign-tparm glGetLight (iv fv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)
  (define-foreign-tparm glGetMaterial (iv fv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)
  (define-foreign-tparm glGetTexEnv (iv fv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)
  (define-foreign-tparm glGetTexGen (iv fv dv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)
  (define-foreign-tparm glGetTexParameter (iv fv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)
  (define-foreign-tparm glGetTexLevelParameter (iv fv)
                        _gl-enum _gl-int _gl-enum (n : _?) (r : (T n)) ->
                        _void -> r)
  (define-foreign-tparm glGetPixelMap (uiv usv fv)
                        _gl-enum (n : _?) (r : (T n)) -> _void -> r)
  (define-foreign-tparm glGetMap (iv fv dv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)
  (define-foreign-tparm glGetBufferParameter (iv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)


  ;; 6.1.4
  (define-foreign glGetTexImage _gl-enum _gl-int _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glGetCompressedTexImage _gl-enum _gl-int _gl-voidv ->)
  (define-foreign glIsTexture _gl-uint -> _gl-boolean)
  
  ;; 6.1.5
  (define-foreign glGetPolygonStipple _gl-voidv ->)
  
  ;; 6.1.7
  (define-foreign glGetColorTable _gl-enum _gl-enum _gl-enum _gl-voidv ->)
  
  ;; 6.1.8
  (define-foreign glGetConvolutionFilter
                  _gl-enum _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glGetSeparableFilter
                  _gl-enum _gl-enum _gl-enum _gl-voidv _gl-voidv _gl-voidv ->)
  (define-foreign-tparm glGetConvolutionParameter (iv fv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)

  ;; 6.1.9
  (define-foreign glGetHistogram
                  _gl-enum _gl-boolean _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glResetHistogram _gl-enum ->)
  (define-foreign-tparm glGetHistogramParameter (iv fv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)

  ;; 6.1.10
  (define-foreign glGetMinmax
                  _gl-enum _gl-boolean _gl-enum _gl-enum _gl-voidv ->)
  (define-foreign glResetMinmax _gl-enum ->)
  (define-foreign-tparm glGetMinmaxParameter (iv fv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)

  ;; 6.1.11
  #|
  (define-foreign glGetPointerv _gl-enum _gl-voidvv ->)
  |#
  (define-foreign glGetString _gl-enum -> _string)

  ;; 6.1.12
  (define-foreign glIsQuery _gl-uint -> _gl-boolean)
  (define-foreign-tparm glGetQuery (iv)
                        _gl-enum _gl-enum (n : _?) (r : (T n)) -> _void -> r)
  (define-foreign-tparm glGetQueryObject (iv uiv)
                        _gl-uint _gl-enum (n : _?) (r : (T n)) -> _void -> r)

  ;; 6.1.13
  (define-foreign glIsBuffer _gl-uint -> _gl-boolean)
  #|
  (define-foreign glGetBufferSubData
                  _gl-enum _gl-intptr _gl-sizeiptr _gl-voidv ->)
  (define-foreign glGetBufferPointerv _gl-enum _gl-enum _gl-voidvv ->)
  |#

  ;; 6.1.14
  (define-foreign glPushAttrib _gl-bitfield ->)
  (define-foreign glPushClientAttrib _gl-bitfield ->)
  (define-foreign glPopAttrib ->)
  (define-foreign glPopClientAttrib ->)

  
  (define-syntax define-foreignu
    (syntax-rules ()
      ((_ args ...) (define-foreign-lib glu-lib args ...))))
  
  ;; 2
  (define-foreignu gluGetString _gl-enum -> _string)
  (define-foreignu gluCheckExtension _string _string -> _gl-boolean)
  
  ;; 3.1
  (define-foreignu gluScaleImage _gl-enum _gl-sizei _gl-sizei _gl-enum _gl-voidv
                                 _gl-sizei _gl-sizei _gl-enum _gl-voidv ->)

  ;; 3.2
  (define-foreignu gluBuild1DMipmaps
                   _gl-enum _gl-int _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
  (define-foreignu gluBuild2DMipmaps
                   _gl-enum _gl-int _gl-sizei _gl-sizei _gl-enum _gl-enum
                   _gl-voidv ->)
  (define-foreignu gluBuild3DMipmaps
                   _gl-enum _gl-int _gl-sizei _gl-sizei _gl-sizei 
                   _gl-enum _gl-enum _gl-voidv ->)
  (define-foreignu gluBuild1DMipmapLevels
                   _gl-enum _gl-int _gl-sizei _gl-enum _gl-enum 
                   _gl-int _gl-int _gl-int _gl-voidv ->)
  (define-foreignu gluBuild2DMipmapLevels
                   _gl-enum _gl-int _gl-sizei _gl-sizei _gl-enum _gl-enum
                   _gl-int _gl-int _gl-int _gl-voidv ->)
  (define-foreignu gluBuild3DMipmapLevels
                   _gl-enum _gl-int _gl-sizei _gl-sizei _gl-sizei
                   _gl-enum _gl-enum _gl-int _gl-int _gl-int _gl-voidv ->)

  ;; 4.1
  (define-foreignu gluOrtho2D _gl-double _gl-double _gl-double _gl-double ->)
  (define-foreignu gluPerspective
                   _gl-double _gl-double _gl-double _gl-double ->)
  (define-foreignu gluLookAt _gl-double _gl-double _gl-double
                             _gl-double _gl-double _gl-double
                             _gl-double _gl-double _gl-double ->)
  (define-foreignu gluPickMatrix _gl-double _gl-double _gl-double _gl-double _gl-intv ->)

  ;; 4.2
  (define-foreignu gluProject _gl-double _gl-double _gl-double
                              _gl-doublev _gl-doublev _gl-intv
                              (r1 : (_ptr o _gl-double))
                              (r2 : (_ptr o _gl-double))
                              (r3 : (_ptr o _gl-double)) ->
                              _void -> (gl-double-vector r1 r2 r3))
  (define-foreignu gluUnProject _gl-double _gl-double _gl-double
                                _gl-doublev _gl-doublev _gl-intv
                                (r1 : (_ptr o _gl-double))
                                (r2 : (_ptr o _gl-double))
                                (r3 : (_ptr o _gl-double)) ->
                                _void -> (gl-double-vector r1 r2 r3))
  (define-foreignu gluUnProject4 _gl-double _gl-double _gl-double _gl-double
                                 _gl-doublev _gl-doublev _gl-intv
                                 _gl-clampd _gl-clampd
                                 (r1 : (_ptr o _gl-double))
                                 (r2 : (_ptr o _gl-double))
                                 (r3 : (_ptr o _gl-double))
                                 (r4 : (_ptr o _gl-double)) ->
                                 _void -> (gl-double-vector r1 r2 r3 r4))
  
  ;; 5.1
  #|
  (define-foreignu gluNewTess -> _glu-tessalator*)
  (define-foreignu gluDeleteTess _glu-tessalator* ->)
  |#
  
  ;; 5.2
  #|
  (define-foreignu gluTessBeginPolygon _glu-tessalator* _gl-voidv ->)
  (define-foreignu gluTessBeginContour _glu-tessalator* ->)
  (define-foreignu gluTessVertex _glu-tessalator* _gl-doublev _gl-voidv ->)
  (define-foreignu gluTessEndContour _glu-tessalator* ->)
  (define-foreignu gluTessEndPolygon _glu-tessalator* ->)
  |#
  
  ;; 5.3
  #|
  (define-foreignu gluTessCallback _glu-tessalator _gl-enum ??? ->)
  |#

  ;; 5.4
  #|
  (define-foreignu gluTessProperty _glu-tessalator* _gl-enum _gl-double ->)
  (define-foreignu gluGetTessProperty _glu-tessalator* _gl-enum _gl-doublev ->)
  (define-foreignu gluTessNormal _glu-tessalator* _gl-double _gl-double _gl-double ->)
  |#

  ;; 5.7
  #|
  (define-foreignu gluBeginPolygon _glu-tessalator* ->)
  (define-foreignu gluNextContour _glu-tessalator* _gl-enum ->)
  (define-foreignu gluEndPolygon _glu-tessalator* ->)
  |#

  ;; 6.1
  (define _glu-quadric
    (_cpointer 'quadric _pointer
               #f
               (lambda (q*)
                 (register-finalizer q* gluDeleteQuadric)
                 q*)))
  
  (define-foreignu gluNewQuadric -> _glu-quadric)

  ;; Don't use define-foreign, because this shouldn't be provided
  (define gluDeleteQuadric
    (with-handlers ((exn:fail:filesystem?
                     (lambda (ex)
                       (lambda x
                         (error 'gluDeleteQuadric
                                "unavailable on this system")))))
      (get-ffi-obj 'gluDeleteQuadric glu-lib (_fun _glu-quadric -> _void))))
  ;; 6.2
  #|
  (define-foreignu gluQuadricCallback _glu-quadric _gl-enum ??? ->)
  |#

  ;; 6.3
  (define-foreignu gluQuadricNormals _glu-quadric _gl-enum ->)
  (define-foreignu gluQuadricTexture _glu-quadric _gl-boolean ->)
  (define-foreignu gluQuadricOrientation _glu-quadric _gl-enum ->)
  (define-foreignu gluQuadricDrawStyle _glu-quadric _gl-enum ->)

  ;; 6.4
  (define-foreignu gluSphere _glu-quadric _gl-double _gl-int _gl-int ->)
  (define-foreignu gluCylinder _glu-quadric _gl-double _gl-double _gl-double
                               _gl-int _gl-int ->)
  (define-foreignu gluDisk
                   _glu-quadric _gl-double _gl-double _gl-int _gl-int ->)
  (define-foreignu gluPartialDisk
                   _glu-quadric _gl-double _gl-double _gl-int _gl-int
                   _gl-double _gl-double ->)

  ;; 7.1
  #|
  (define-foreignu gluNewNurbsRenderer -> _glu-nurbs*)
  (define-foreignu gludeleteNurbsRenderer _glu-nurbs* ->)
  |#
  
  ;; 7.2
  #|
  (define-foreignu gluNurbsCallback _glu-nurbs* _gl-enum ??? ->)
  (define-foreignu gluNurbsCallbackData _glu-nurbs* _gl-voidv ->)
  |#

  ;; 7.3
  #|
  (define-foreignu gluBeginCurve _glu-nurbs* ->)
  (define-foreignu gluNurbsCurve _glu-nurbs* _gl-int _gl-floatv _gl-int _gl-floatv _gl-int _gl-enum ->)
  (define-foreignu gluEndCurve _glu-nurbs* ->)
  |#

  ;; 7.4
  #|
  (define-foreignu gluBeginSurface _glu-nurbs* ->)
  (define-foreignu gluNurbsSurface _glu-nurbs* _gl-int _gl-floatv _gl-int _gl-floatv _gl-int
                                              _gl-int _gl-floatv _gl-int _gl-int _gl-enum ->)
  (define-foreignu gluEndSurface _glu-nurbs* ->)
  |#

  ;; 7.5
  #|
  (define-foreignu gluBeginTrim _glu-nurbs* ->)
  (define-foreignu gluPwlCurve _glu-nurbs* _gl-int _gl-floatv _gl-int _gl-enum ->)
  (define-foreignu gluEndTrim _glu-nurbs* ->)
  |#

  ;; 7.6
  #|
  (define-foreignu gluNurbsProperty _glu-nurbs* _gl-enum _gl-float ->)
  (define-foreignu gluLoadSamplingMatrix _glu-nurbs* _gl-floatv _gl-floatv _gl-intv ->)
  (define-foreignu gluGetNurbsProperty _glu-nurbs* _gl-enum _gl-floatv ->)
  |#

  ;; 8
  (define-foreignu gluErrorString _gl-enum -> _string)
  )
