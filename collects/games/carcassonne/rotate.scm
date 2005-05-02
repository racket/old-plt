#cs
(module rotate mzscheme 

  ;; a function for creating the rotated images 

  (require "if.scm" ;; for comments only 
           (lib "mred.ss" "mred")
           (prefix srfi: (lib "1.ss" "srfi"))
           (file "Testing/testing.scm")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "class.ss"))
  
  (provide 
   orientation->rotation ;; Orientation Bitmap -> Bitmap
   )
  
  ;; --- EXPORTS --- 
  (define (orientation->rotation o) (vector-ref rotate (quotient o 90)))
  ;; --- exports --- 

  ;; --- VERSION 208/9 ADAPTATION --- 
;  (define make-bytes make-string) 
;  (define bytes-set! string-set!)
;  (define bytes-ref  string-ref)
;  (define bytes-length string-length)
;  (define ZERO #\space)
  ;; --- version 208/9 adaptation ---   
  
  ;; --- VERSION 299 ADAPTATION
  (define ZERO 0)
  
  ;; --- MATRIX CODE ---
  
  (define-struct pixel (a r g b) (make-inspector))
  ;; Pixel  = (make-pixel Byte Byte Byte Byte)
  
  ;; Listof[Char] -> Pixel 
  (define (chars->pixel l) (apply make-pixel l))
    
  ;; Pixel -> Listof[Char]
  (define (pixel->chars p) (list (pixel-a p) (pixel-r p) (pixel-g p) (pixel-b p)))

  ;; Matrix[X]

  ;; Number Number -> Matrix[Number]
  (define (make-matrix w h)
    (build-vector h (lambda (i) (build-vector w (lambda (j) (list i j))))))
  
  ;; Matrix[X] Number Number -> X 
  (define (matrix-ref m x y) (vector-ref (vector-ref m x) y))
  
  ;; Matrix[X] Number Number X -> Void 
  (define (matrix-set! m x y v) (vector-set! (vector-ref m x) y v))

  ;; Pixels = String ;; (= (modulo (string-length <pixels>) 4) 0)
  
  ;; Pixels Number Number -> Matrix[Pixel]
  (define (pixels->matrix b w h)
    ;; Listof[Char] -> Vectorof[Pixel]
    (define (row->list r)
      (list->vector 
       (let loop ([r r])
         (cond
           [(null? r) '()]
           [else 
            (cons (chars->pixel (srfi:take r 4)) (loop (srfi:drop r 4)))]))))
    (list->vector 
     (let L ([b (bytes->list b)])
       (cond
         [(null? b) '()]
         [else 
          (cons (row->list (srfi:take b (* 4 w))) (L (srfi:drop b (* 4 w))))]))))

  ;; Matrix Number Number (Number Number ->* Number Number) -> Matrix 
  ;; transform a (square) matrix 
  (define (matrix-rotate m w h transform)
    (define new-m (make-matrix w h))
    (for-each (lambda (x) 
                (for-each (lambda (y) 
                            (let-values ([(x1 y1) (transform x y w h)])
                              (matrix-set! new-m x1 y1 (matrix-ref m x y))))
                          (build-list h identity)))
              (build-list w identity))
    new-m)
  
  (define (matrix-rotate90 m w h)
    (matrix-rotate m w h (lambda (x y w h) (values y (- h x 1)))))
  
  (define (matrix-rotate180 m w h)
    (matrix-rotate m w h (lambda (x y w h) (values (- w x 1) (- h y 1)))))
  
  (define (matrix-rotate270 m w h)
    (matrix-rotate m w h (lambda (x y w h) (values (- w y 1) x))))
  
  ;; Matrix[Pixel] -> Pixels
  (define (matrix->pixels m)
    (define l (map vector->list (vector->list m)))
    (define k (apply append (map (lambda (r) (apply append (map pixel->chars r))) l)))
    (list->bytes k))
  
  #| TEST
  
  (test== (make-matrix 1 1) 
          (vector (vector (list 0 0))))
  (test== (make-matrix 2 2)
          (vector (vector (list 0 0) (list 0 1))
                  (vector (list 1 0) (list 1 1))))
  
  (define M (vector (vector 0 1)
                    (vector 2 3)))
  (test== (matrix-ref M 0 0) 0)
  (test== (matrix-ref M 0 1) 1)
  (test== (matrix-ref M 1 0) 2)
  (test== (matrix-ref M 1 1) 3)
  
  (test== (begin (matrix-set! M 0 0 'a) M) (vector (vector 'a 1)
                                                   (vector 2 3)))
  (test== (begin (matrix-set! M 0 1 'b) M) (vector (vector 'a 'b)
                                                   (vector 2 3)))
  (test== (begin (matrix-set! M 1 0 'c) 
                 (matrix-set! M 1 1 'd)
                 M)
          (vector (vector 'a 'b)
                  (vector 'c 'd)))
  
  
  (test== (matrix-rotate90 (make-matrix 2 2) 2 2)
          (vector (vector (list 1 0) (list 0 0))
                  (vector (list 1 1) (list 0 1))) "rotate90")
  
  (test== (matrix-rotate180 (make-matrix 2 2) 2 2)
          (vector (vector (list 1 1) (list 1 0))
                  (vector (list 0 1) (list 0 0))) "rotate180")
  
  (test== (matrix-rotate180 (vector (vector 'a 'b 'c)
                                    (vector 'd 'e 'f)
                                    (vector 'g 'h 'i))
                            3 3)
          (vector (vector 'i 'h 'g)
                  (vector 'f 'e 'd)
                  (vector 'c 'b 'a)) "rotate180/2")
  
  (test== (matrix-rotate270 (make-matrix 2 2) 2 2)
          (vector (vector (list 0 1) (list 1 1))
                  (vector (list 0 0) (list 1 0))) "rotate270")
  
  (test== (pixels->matrix (bytes 0 1 2 3) 1 1)
          (vector (vector (make-pixel 0 1 2 3)))
          "pixels-matrix-1x1")
  (define bytes1 (bytes 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 
  (define matrix1 
    (vector (vector (make-pixel 0 1 2 3) (make-pixel 4 5 6 7))
            (vector (make-pixel 8 9 10 11) (make-pixel 12 13 14 15))))
  (test== (pixels->matrix bytes1 2 2)
          matrix1
          "pixels->matrix 2x2")
  (test== (matrix->pixels matrix1)
          bytes1
          "pixes->matrix 3")
 |# 
  
  ;; --- matrix code --- 
  
  ;; Bitmap% (Number Number Number Number ->* Number Number) -> Bitmap%
  (define (transform-bitmap bm rot)
    (define w         (send bm get-width))
    (define h         (send bm get-height))
    (define new-bm    (make-object bitmap% w h))
    (define bdc       (make-object bitmap-dc% bm))
    (define bytes     (make-bytes (* w h 4) ZERO))
    (define _         (send bdc get-argb-pixels 0 0 w h bytes))
    (define m:bytes   (pixels->matrix bytes w h))
    (define m:rotated (rot m:bytes w h))
    (define new-bytes (matrix->pixels m:rotated))
    
    (send bdc set-bitmap new-bm)
    (send bdc set-argb-pixels 0 0 w h new-bytes)
    (send bdc set-bitmap #f)
    
    new-bm)
  
  ;; Bitmap% -> Bitmap% 
  (define (rotate90 bm) (transform-bitmap bm matrix-rotate90))
  
  (define (rotate180 bm) (transform-bitmap bm matrix-rotate180))
  
  (define (rotate270 bm) (transform-bitmap bm matrix-rotate270))
  
  (define rotate (vector identity rotate90 rotate180 rotate270))

  #| VISUAL TESTS:
  
  ;; String Listof[bitmap%] -> Void
  (define (show-bitmaps index . bm)
    (define f (new frame% (label  (string-append "Bitmap" index))))
    (define w (cons 0 (map (lambda (x) (send x get-width)) bm)))
    (define c (new canvas%
                   (parent f)
                   (min-width   (apply + w))
                   (min-height  (send (car bm) get-height)) ; assume same height
                   (paint-callback
                    (lambda (c dc)
                      (define width 0)
                      (for-each (lambda (i)
                                  (set! width (+ (car w) width))
                                  (set! w (cdr w))
                                  (send dc draw-bitmap i width 0))
                                bm)))))
    (send f show #t))
  
  ;; Index -> bitmap%
  (define (index->bitmap index)
    (make-object bitmap% (build-path "Tiles" (format "tile~a.jpg" index))))
  
  ;; Index -> Void 
  (define (visualize index)
    (let ([test-bm (index->bitmap index)])
      (apply show-bitmaps index (map (lambda (f) (f test-bm)) (vector->list rotate)))))

  (visualize "8")

  (for-each visualize (build-list 24 (lambda (i) (number->string (add1 i)))))
  |#
  )
