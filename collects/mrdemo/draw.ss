
(module draw mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "string.ss"))

  (provide draw-snip%
	   ;; the name `snip-class' isimportant!
	   (rename draw-snip-class snip-class))

  ;; A simple snip class that makes an empty square of a certain
  ;; size. Try (make-object draw-snip% 100 100) in DrScheme to get an
  ;; empty box (100 pixels x 100 pixles) as the result.

  (define draw-snip%
    (class snip%
      (inherit get-admin set-snipclass set-count)
      (init-field w h)
      (override*
	[get-extent  ; called by an editor to get the snip's size
	 (lambda (dc x y wbox hbox descentbox spacebox lspacebox rspacebox)
	   (when hbox
	     (set-box! hbox h))
	   (when wbox
	     (set-box! wbox w))
	   (when descentbox
	     (set-box! descentbox 0))
	   (when spacebox
	     (set-box! spacebox 0))
	   (when rspacebox
	     (set-box! rspacebox 0))
	   (when lspacebox
	     (set-box! lspacebox 0)))]
	[draw  ; called by an editor to draw the snip
	 (lambda (dc x y . other)
	   (let* ((xw (sub1 (+ x w)))
		  (yh (sub1 (+ y h)))
		  (x (add1 x))
		  (y (add1 y)))
	     (send dc draw-line x y xw y)
	     (send dc draw-line xw y xw yh)
	     (send dc draw-line x yh xw yh)
	     (send dc draw-line x y x yh)))]
	[copy  ; clones the snip
	 (lambda ()
	   (make-object draw-snip% w h))]
	[write  ; marshals the snip to a text stream
	 (lambda (stream)
	   (send stream << w)
	   (send stream << h))]
	[resize  ; called by a pasetboard editor to resize the snip
	 (lambda (w-in h-in)
	   (set! w w-in)
	   (set! h h-in)
	   ;; send resize notification to the editor containing the snip
	   (let ([admin (get-admin)])
	     (when admin
	       (send admin resized this #t)))
	   #t)])
      (super-instantiate ())
      ;; Need to set the "class" for unmarshaling from text stream
      (set-snipclass (send (get-the-snip-class-list) find "(lib \"draw.ss\" \"mrdemo\")"))
      (set-count 1)))

  ; The snip "class" is used for unmarshaling a snip from a text stream
  (define draw-snip-class
    (make-object
     (class snip-class%
       (inherit set-classname)
       (override*
	[read
	 (lambda (stream)
	   (let ([w-box (box 0)]
		 [h-box (box 0)])
	     (send stream >> w-box)
	     (send stream >> h-box)
	     (make-object draw-snip% (unbox w-box) (unbox h-box))))])
       (super-instantiate ())
       (set-classname "(lib \"draw.ss\" \"mrdemo\")")))))
