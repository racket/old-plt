(module image mzscheme

  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   "../posn.ss"
	   "color.ss")

  (provide image?
	   image=?

	   image-width
	   image-height
	   image+
	   offset-image+
	   offset-masked-image+

	   solid-box
	   outline-box
	   solid-dot
	   outline-dot

	   image-inside?
	   find-image

	   image->color-list
	   color-list->image

	   ;; For implementing other teachpacks
	   ;; (not for students)
	   snip-size
	   image-snip->dc+bitmap
	   image-snip->bitmap)

  ;; ----------------------------------------

  (define (snip-size a)
    (let ([bm (send a get-bitmap)])
      (values (send bm get-width)
	      (send bm get-height))))

  (define (image-snip->bitmap a)
    (send a get-bitmap))
  
  ;; Converts a bitmap snip to a bitmap
  (define (image-snip->dc+bitmap a)
    (let-values ([(w h) (snip-size a)])
      (let ([a-dc (make-object bitmap-dc% (make-object bitmap% w h))])
	(send a-dc draw-bitmap (image-snip->bitmap a) 0 0)
	a-dc)))


  (define (check name p? v desc)
    (unless (p? v)
      (raise-type-error
       name
       desc
       v)))
   
  (define black (make-object color% "black"))

  (define (dc->snip a-dc)
    (let ([a-bm (send a-dc get-bitmap)])
      (send a-dc set-bitmap #f)
      (make-object image-snip% a-bm)))

  (define (snip->mask b)
    (let-values ([(w h) (snip-size b)])
      (let ([s (make-string (* 4 w h))]
	    [dc (image-snip->dc+bitmap b)])
	(send dc get-argb-pixels 0 0 w h s)
	(let loop ([i (* 4 w h)])
	  (unless (zero? i)
	    (let ([r (- i 3)]
		  [g (- i 2)]
		  [b (- i 1)])
	      (unless (and (eq? #\377 (string-ref s r))
			   (eq? #\377 (string-ref s g))
			   (eq? #\377 (string-ref s b)))
		(string-set! s r #\000)
		(string-set! s g #\000)
		(string-set! s b #\000))
	      (loop (- i 4)))))
	(send dc set-argb-pixels 0 0 w h s)
	(begin0
	 (send dc get-bitmap)
	 (send dc set-bitmap #f)))))
  
  ;; ----------------------------------------

  (define (image? a)
    (is-a? a image-snip%))

  (define (image=? a b)
    (unless (image? a) (raise-type-error 'image=? "image" 0 a b))
    (unless (image? b) (raise-type-error 'image=? "image" 1 a b))
    ;; If the same size...
    (let-values ([(aw ah) (snip-size a)]
		 [(bw bh) (snip-size b)])
      (and (= aw bw)
	   (= ah bh)
	   (let ([a-dc (image-snip->dc+bitmap a)]
		 [b-dc (image-snip->dc+bitmap b)])
	     (let* ([s1 (make-string (* aw ah 4))]
		    [s2 (make-string (* bw bh 4))])
	       (send a-dc get-argb-pixels 0 0 aw ah s1)
	       (send b-dc get-argb-pixels 0 0 bw bh s2)
	       (string=? s1 s2))))))

  (define (image-width a)
    (check 'image-width image? a "image")
    (let-values ([(w h) (snip-size a)])
      (inexact->exact (ceiling w))))

  (define (image-height a)
    (check 'image-height image? a "image")
    (let-values ([(w h) (snip-size a)])
      (inexact->exact (ceiling h))))

  (define (image+ a b)
    (check 'image+ image? a "image")
    (check 'image+ image? b "image")
    (offset-image+ a 0 0 b))

  (define (offset-image+ a dx dy b)
    (check 'offset-image+ image? a "image")
    (check 'offset-image+ real? dx "real number")
    (check 'offset-image+ real? dy "real number")
    (check 'offset-image+ image? b "image")
    (let ([a-dc (image-snip->dc+bitmap a)])
      (send a-dc draw-bitmap 
	    (image-snip->bitmap b) dx dy
	    'solid black (snip->mask b))
      (dc->snip a-dc)))

  (define (offset-masked-image+ a x y mask b)
    (check 'offset-masked-image+ image? a "image")
    (check 'offset-masked-image+ real? x "real number")
    (check 'offset-masked-image+ real? y "real number")
    (check 'offset-masked-image+ image? mask "image")
    (check 'offset-masked-image+ image? b "image")
    (let ([a-dc (image-snip->dc+bitmap a)]
	  [b-bm (image-snip->bitmap b)]
	  [mask-bm (image-snip->bitmap mask)])
      (unless (and (= (send b-bm get-width)
		      (send mask-bm get-width))
		   (= (send b-bm get-height)
		      (send mask-bm get-height)))
	(error 'offset-masked-image+
	       "cannot use a mask and last image with different sizes"))
      (send a-dc draw-bitmap b-bm
	    x y 'solid black
	    mask-bm)
      (dc->snip a-dc)))

  ;; ------------------------------------------------------------

  (define (new-dc+bm who w h color brush pen)
    (check who real? w "positive integer")
    (check who real? h "positive integer")
    (check who symbol? color "symbol")
    (unless (and (< 0 w 10000) (< 0 h 10000))
      (error (format "cannot make ~a x ~a image" w h)))
    (let* ([bm (make-object bitmap% (inexact->exact (ceiling w)) (inexact->exact (ceiling h)))]
	   [dc (make-object bitmap-dc% bm)])
      (unless (send bm ok?)
	(error (format "cannot make ~a x ~a image" w h)))
      (send dc clear)
      (send dc set-brush (or (send the-brush-list find-or-create-brush (symbol->string color) brush)
			     (error (format "unknown color: ~a" color))))
      (send dc set-pen (or (send the-pen-list find-or-create-pen (symbol->string color) 1 pen)
			   (error (format "unknown color: ~a" color))))
      dc))

  (define (a-box who w h color brush pen)
    (let ([dc (new-dc+bm who w h color brush pen)])
      (send dc draw-rectangle 0 0 w h)
      (dc->snip dc)))

  (define (solid-box w h color)
    (a-box 'solid-box w h color 'solid 'transparent))

  (define (outline-box w h color)
    (a-box 'outline-box w h color 'transparent 'solid))

  (define (a-dot who w h color brush pen)
    (let ([dc (new-dc+bm who w h color brush pen)])
      (send dc draw-ellipse 0 0 w h)
      (dc->snip dc)))

  (define (solid-dot w h color)
    (a-dot 'solid-dot w h color 'solid 'transparent))

  (define (outline-dot w h color)
    (a-dot 'solid-dot w h color 'transparent 'solid))

  ;; ------------------------------------------------------------

  ;; Checks whether the non-white part of a appears in b
  ;;  at offset bd
  (define (first-in-second? a x xd)
    (let loop ([i (string-length a)])
      (or (zero? i)
	  (let ([r (- i 3)]
		[g (- i 2)]
		[b (- i 1)])
	    (let ([rv (string-ref a r)]
		  [gv (string-ref a g)]
		  [bv (string-ref a b)])
	      (and (or (and (eq? #\377 rv)
			    (eq? #\377 gv)
			    (eq? #\377 bv))
		       (and (eq? (string-ref x (+ xd r)) rv)
			    (eq? (string-ref x (+ xd g)) gv)
			    (eq? (string-ref x (+ xd b)) bv)))
		   (loop (- i 4))))))))
		 
  (define (locate-image who i a)
    (check who image? i "image")
    (check who image? a "image")
    (let* ([i-dc (image-snip->dc+bitmap i)]
	   [a-dc (image-snip->dc+bitmap a)]
	   [i (send i-dc get-bitmap)]
	   [a (send a-dc get-bitmap)])
      (let ([iw (send i get-width)]
	    [ih (send i get-height)]
	    [aw (send a get-width)]
	    [ah (send a get-height)])
	(and (iw . >= . aw)
	     (ih . >= . ah)
	     (let ([is (make-string (* 4 iw ih))]
		   [as (make-string (* 4 aw ah))])
	       (send i-dc get-argb-pixels 0 0 iw ih is)
	       (send a-dc get-argb-pixels 0 0 aw ah as)
	       (let* ([al (let loop ([offset 0])
			    (cond
			     [(= offset (* ah aw 4)) null]
			     [else (cons (substring as offset (+ offset (* 4 aw)))
					 (loop (+ offset (* 4 aw))))]))])
		 (let yloop ([dy 0])
		   (and (dy . <= . (- ih ah))
			(let xloop ([dx 0])
			  (if (dx . <= . (- iw aw))
			      (if (let loop ([al al][dd 0])
				    (or (null? al)
					(and (first-in-second?
					      (car al) 
					      is
					      (* 4 (+ (* (+ dy dd) iw) dx)))
					     (loop (cdr al) (add1 dd)))))
				  (make-posn dx dy)
				  (xloop (add1 dx)))
			      (yloop (add1 dy))))))))))))
		
  (define (image-inside? i a)
    (and (locate-image 'image-inside? i a) #t))

  (define (find-image i a)
    (or (locate-image 'find-image i a)
	(error 'find-image
	       "the second image does not appear within the first image")))

  ;; ----------------------------------------

  (define (image->color-list i)
    (check 'image->color-list image? i "image")
    (let* ([i-dc (image-snip->dc+bitmap i)]
	   [i (send i-dc get-bitmap)])
      (let ([iw (send i get-width)]
	    [ih (send i get-height)])
	(let ([is (make-string (* 4 iw ih))]
	      [cols (make-vector (* iw ih))])
	  (send i-dc get-argb-pixels 0 0 iw ih is)
	  (let yloop ([y 0][pos 0])
	    (unless (= y ih)
	      (let xloop ([x 0][pos pos])
		(if (= x iw)
		    (yloop (add1 y) pos)
		    (begin
		      (vector-set! cols (+ x (* y iw))
				   (make-color (char->integer (string-ref is (+ 1 pos)))
					       (char->integer (string-ref is (+ 2 pos)))
					       (char->integer (string-ref is (+ 3 pos)))))
		      (xloop (add1 x) (+ pos 4)))))))
	  (vector->list cols)))))

  (define (color-list? l)
    (and (list? l) (andmap color? l)))

  (define (posi? i)
    (and (number? i) (integer? i) (positive? i) (exact? i)))

  (define (color-list->image cl w h)
    (check 'color-list->image color-list? cl "list-of-colors")
    (check 'color-list->image posi? w "positive exact integer")
    (check 'color-list->image posi? h "positive exact integer")
    (unless (and (< 0 w 10000) (< 0 h 10000))
      (error (format "cannot make ~a x ~a image" w h)))
    (unless (= (* w h) (length cl))
      (error (format "given width times given height is ~a, but the given color list has ~a items"
		     (* w h) (length cl))))
    (let* ([bm (make-object bitmap% w h)]
	   [dc (make-object bitmap-dc% bm)])
      (unless (send bm ok?)
	(error (format "cannot make ~a x ~a image" w h)))
      (let ([is (make-string (* 4 w h) #\000)]
	    [cols (list->vector cl)])
	(let yloop ([y 0][pos 0])
	  (unless (= y h)
	    (let xloop ([x 0][pos pos])
	      (if (= x w)
		  (yloop (add1 y) pos)
		  (let ([col (vector-ref cols (+ x (* y w)))])
		    (string-set! is (+ 1 pos) (integer->char (min 255 (max 0 (color-red col)))))
		    (string-set! is (+ 2 pos) (integer->char (min 255 (max 0 (color-green col)))))
		    (string-set! is (+ 3 pos) (integer->char (min 255 (max 0 (color-blue col)))))
		    (xloop (add1 x) (+ pos 4)))))))
	(send dc set-argb-pixels 0 0 w h is))
      (send dc set-bitmap #f)
      (make-object image-snip% bm))))
