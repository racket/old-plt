;; mysterx.ss

(require-library "function.ss")
(require-library "string.ss")

(load-extension "mysterx.dll")

(define mx-element%
  (class null (dhtml-element)

	 (private
	  [elt dhtml-element])

	 (public
	  [insert-html
	   (lambda (s)
	     (element-insert-html elt s))]
	  [append-html
	   (lambda (s)
	     (element-append-html elt s))]
	  [insert-text 
	   (lambda (s)
	     (element-insert-text elt s))]
	  [append-text
	   (lambda (s)
	     (element-append-text elt s))]
	  [attribute
	   (lambda (s)
	     (element-attribute elt s))]
	  [set-attribute!
	   (lambda (a v)
	     (element-set-attribute! elt a v))]
          [click
	   (lambda ()
	     (element-click elt))]
	  [tag
	   (lambda ()
	     (element-tag elt))]
	  [font-family
	   (lambda ()
	     (element-font-family elt))]
	  [set-font-family!
	   (lambda (s)
	     (element-set-font-family! elt s))]
	  [font-style
	   (lambda ()
	     (element-font-style elt))]
	  [set-font-style!
	   (lambda (s)
	     (element-set-font-style! elt s))]
	  [font-variant
	   (lambda ()
	     (element-font-variant elt))]
	  [set-font-variant!
	   (lambda (s)
	     (element-set-font-variant! elt s))]
	  [font-weight
	   (lambda ()
	     (element-font-weight elt))]
	  [set-font-weight!
	   (lambda (s)
	     (element-set-font-weight! elt s))]
	  [font
	   (lambda ()
	     (element-font elt))]
	  [set-font!
	   (lambda (s)
	     (element-set-font! elt s))]
	  [background
	   (lambda ()
	     (element-background elt))]
	  [set-background!
	   (lambda (s)
	     (element-set-background! elt s))]
	  [background-image
	   (lambda ()
	     (element-background-image elt))]
	  [set-background-image!
	   (lambda (s)
	     (element-set-background-image! elt s))]
	  [background-repeat
	   (lambda ()
	     (element-background-repeat elt))]
	  [set-background-repeat!
	   (lambda (s)
	     (element-set-background-repeat! elt s))]
	  [background-position
	   (lambda ()
	     (element-background-position elt))]
	  [set-background-position!
	   (lambda (s)
	     (element-set-background-position! elt s))]
	  [text-decoration
	   (lambda ()
	     (element-text-decoration elt))]
	  [set-text-decoration!
	   (lambda (s)
	     (element-text-decoration! elt s))]
	  [text-transform
	   (lambda ()
	     (element-text-transform elt))]
	  [set-text-transform!
	   (lambda (s)
	     (element-text-transform! elt s))]
	  [text-align
	   (lambda ()
	     (element-text-align elt))]
	  [set-text-align!
	   (lambda (s)
	     (element-text-align! elt s))]
	  [margin
	   (lambda ()
	     (element-margin elt))]
	  [set-margin!
	   (lambda (s)
	     (element-set-margin! elt s))]
	  [padding
	   (lambda ()
	     (element-padding elt))]
	  [set-padding!
	   (lambda (s)
	     (element-set-padding! elt s))]
	  [border
	   (lambda ()
	     (element-border elt))]
	  [set-border!
	   (lambda (s)
	     (element-set-border! elt s))]
	  [border-top
	   (lambda ()
	     (element-border-top elt))]
	  [set-border-top!
	   (lambda (s)
	     (element-set-border-top! elt s))]
	  [border-bottom
	   (lambda ()
	     (element-border-bottom elt))]
	  [set-border-bottom!
	   (lambda (s)
	     (element-set-border-bottom! elt s))]
	  [border-left
	   (lambda ()
	     (element-border-left elt))]
	  [set-border-left!
	   (lambda (s)
	     (element-set-border-left! elt s))]
	  [border-right
	   (lambda ()
	     (element-border-right elt))]
	  [set-border-right!
	   (lambda (s)
	     (element-set-border-right! elt s))]
	  [border-color
	   (lambda ()
	     (element-border-color elt))]
	  [set-border-color!
	   (lambda (s)
	     (element-set-border-color! elt s))]
	  [border-width
	   (lambda ()
	     (element-border-width elt))]
	  [set-border-width!
	   (lambda (s)
	     (element-set-border-width! elt s))]
	  [border-style
	   (lambda ()
	     (element-border-style elt))]
	  [set-border-style!
	   (lambda (s)
	     (element-set-border-style! elt s))]
	  [border-top-style
	   (lambda ()
	     (element-border-top-style elt))]
	  [set-border-top-style!
	   (lambda (s)
	     (element-set-border-top-style! elt s))]
	  [border-bottom-style
	   (lambda ()
	     (element-border-bottom-style elt))]
	  [set-border-bottom-style!
	   (lambda (s)
	     (element-set-border-bottom-style! elt s))]
	  [border-left-style
	   (lambda ()
	     (element-border-left-style elt))]
	  [set-border-left-style!
	   (lambda (s)
	     (element-set-border-left-style! elt s))]
	  [border-right-style
	   (lambda ()
	     (element-border-right-style elt))]
	  [set-border-right-style!
	   (lambda (s)
	     (element-set-border-right-style! elt s))]
	  [style-float
	   (lambda ()
	     (element-style-float elt))]
	  [set-style-float!
	   (lambda (s)
	     (element-set-style-float! elt s))]
	  [clear
	   (lambda ()
	     (element-clear elt))]
	  [set-clear!
	   (lambda (s)
	     (element-set-clear! elt s))]
	  [display
	   (lambda ()
	     (element-display elt))]
	  [set-display!
	   (lambda (s)
	     (element-set-display! elt s))]
	  [visibility
	   (lambda ()
	     (element-visibility elt))]
	  [set-visibility!
	   (lambda (s)
	     (element-set-visibility! elt s))]
	  [list-style-type
	   (lambda ()
	     (element-list-style-type elt))]
	  [set-list-style-type!
	   (lambda (s)
	     (element-set-list-style-type! elt s))]
	  [list-style-position
	   (lambda ()
	     (element-list-style-position elt))]
	  [set-list-style-position!
	   (lambda (s)
	     (element-set-list-style-position! elt s))]
	  [list-style-image
	   (lambda ()
	     (element-list-style-image elt))]
	  [set-list-style-image!
	   (lambda (s)
	     (element-set-list-style-image! elt s))]
	  [list-style
	   (lambda ()
	     (element-list-style elt))]
	  [set-list-style!
	   (lambda (s)
	     (element-set-list-style! elt s))]
	  [whitespace
	   (lambda ()
	     (element-whitespace elt))]
	  [set-whitespace!
	   (lambda (s)
	     (element-set-whitespace! elt s))]
	  [position
	   (lambda ()
	     (element-position elt))]
	  [overflow
	   (lambda ()
	     (element-overflow elt))]
	  [set-overflow!
	   (lambda (s)
	     (element-set-overflow! elt s))]
	  [pagebreak-before
	   (lambda ()
	     (element-pagebreak-before elt))]
	  [set-pagebreak-before!
	   (lambda (s)
	     (element-set-pagebreak-before! elt s))]
	  [pagebreak-after
	   (lambda ()
	     (element-pagebreak-after elt))]
	  [set-pagebreak-after!
	   (lambda (s)
	     (element-set-pagebreak-after! elt s))]
	  [css-text
	   (lambda ()
	     (element-css-text elt))]
	  [set-css-text!
	   (lambda (s)
	     (element-set-css-text! elt s))]
	  [cursor
	   (lambda ()
	     (element-cursor elt))]
	  [set-cursor!
	   (lambda (s)
	     (element-set-cursor! elt s))]
	  [clip
	   (lambda ()
	     (element-clip elt))]
	  [set-clip!
	   (lambda (s)
	     (element-set-clip! elt s))]
	  [filter
	   (lambda ()
	     (element-filter elt))]
	  [set-filter!
	   (lambda (s)
	     (element-set-filter! elt s))]
	  [style-string
	   (lambda ()
	     (element-style-string elt))]
	  [text-decoration-none
	   (lambda ()
	     (element-text-decoration-none elt))]
	  [set-text-decoration-none!
	   (lambda (s)
	     (element-set-text-decoration-none! elt s))]
	  [text-decoration-underline
	   (lambda ()
	     (element-text-decoration-underline elt))]
	  [set-text-decoration-underline!
	   (lambda (s)
	     (element-set-text-decoration-underline! elt s))]
	  [text-decoration-overline
	   (lambda ()
	     (element-text-decoration-overline elt))]
	  [set-text-decoration-overline!
	   (lambda (s)
	     (element-set-text-decoration-overline! elt s))]
	  [text-decoration-linethrough
	   (lambda ()
	     (element-text-decoration-linethrough elt))]
	  [set-text-decoration-linethrough!
	   (lambda (s)
	     (element-set-text-decoration-linethrough! elt s))]
	  [text-decoration-blink
	   (lambda ()
	     (element-text-decoration-blink elt))]
	  [set-text-decoration-blink!
	   (lambda (s)
	     (element-set-text-decoration-blink! elt s))]
	  [pixel-top
	   (lambda ()
	     (element-pixel-top elt))]
	  [set-pixel-top!
	   (lambda (s)
	     (element-set-pixel-top! elt s))]
	  [pixel-left
	   (lambda ()
	     (element-pixel-left elt))]
	  [set-pixel-left!
	   (lambda (s)
	     (element-set-pixel-left! elt s))]
	  [pixel-width
	   (lambda ()
	     (element-pixel-width elt))]
	  [set-pixel-width!
	   (lambda (s)
	     (element-set-pixel-width! elt s))]
	  [pixel-height
	   (lambda ()
	     (element-pixel-height elt))]
	  [set-pixel-height!
	   (lambda (s)
	     (element-set-pixel-height! elt s))]
	  [pos-top
	   (lambda ()
	     (element-pos-top elt))]
	  [set-pos-top!
	   (lambda (s)
	     (element-set-pos-top! elt s))]
	  [pos-left
	   (lambda ()
	     (element-pos-left elt))]
	  [set-pos-left!
	   (lambda (s)
	     (element-set-pos-left! elt s))]
	  [pos-width
	   (lambda ()
	     (element-pos-width elt))]
	  [set-pos-width!
	   (lambda (s)
	     (element-set-pos-width! elt s))]
	  [pos-height
	   (lambda ()
	     (element-pos-height elt))]
	  [set-pos-height!
	   (lambda (s)
	     (element-set-pos-height! elt s))]
	  [font-size
	   (lambda ()
	     (element-font-size elt))]
	  [set-font-size!
	   (lambda (s)
	     (element-set-font-size! elt s))]
	  [color
	   (lambda ()
	     (element-color elt))]
	  [set-color!
	   (lambda (s)
	     (element-set-color! elt s))]
	  [background-color
	   (lambda ()
	     (element-background-color elt))]
	  [set-background-color!
	   (lambda (s)
	     (element-set-background-color! elt s))]
	  [background-position-x
	   (lambda ()
	     (element-background-position-x elt))]
	  [set-background-position-x!
	   (lambda (s)
	     (element-set-background-position-x! elt s))]
	  [background-position-y
	   (lambda ()
	     (element-background-position-y elt))]
	  [set-background-position-y!
	   (lambda (s)
	     (element-set-background-position-y! elt s))]
	  [word-spacing
	   (lambda ()
	     (element-word-spacing elt))]
	  [set-word-spacing!
	   (lambda (s)
	     (element-set-word-spacing! elt s))]
	  [letter-spacing
	   (lambda ()
	     (element-letter-spacing elt))]
	  [set-letter-spacing!
	   (lambda (s)
	     (element-set-letter-spacing! elt s))]
	  [vertical-align
	   (lambda ()
	     (element-vertical-align elt))]
	  [set-vertical-align!
	   (lambda (s)
	     (element-set-vertical-align! elt s))]
	  [text-indent
	   (lambda ()
	     (element-text-indent elt))]
	  [set-text-indent!
	   (lambda (s)
	     (element-set-text-indent! elt s))]
	  [line-height
	   (lambda ()
	     (element-line-height elt))]
	  [set-line-height!
	   (lambda (s)
	     (element-set-line-height! elt s))]
	  [margin-top
	   (lambda ()
	     (element-margin-top elt))]
	  [set-margin-top!
	   (lambda (s)
	     (element-set-margin-top! elt s))]
	  [margin-bottom
	   (lambda ()
	     (element-margin-bottom elt))]
	  [set-margin-bottom!
	   (lambda (s)
	     (element-set-margin-bottom! elt s))]
	  [margin-left
	   (lambda ()
	     (element-margin-left elt))]
	  [set-margin-left!
	   (lambda (s)
	     (element-set-margin-left! elt s))]
	  [margin-right
	   (lambda ()
	     (element-margin-right elt))]
	  [set-margin-right!
	   (lambda (s)
	     (element-set-margin-right! elt s))]
	  [padding-top
	   (lambda ()
	     (element-padding-top elt))]
	  [set-padding-top!
	   (lambda (s)
	     (element-set-padding-top! elt s))]
	  [padding-bottom
	   (lambda ()
	     (element-padding-bottom elt))]
	  [set-padding-bottom!
	   (lambda (s)
	     (element-set-padding-bottom! elt s))]
	  [padding-left
	   (lambda ()
	     (element-padding-left elt))]
	  [set-padding-left!
	   (lambda (s)
	     (element-set-padding-left! elt s))]
	  [padding-right
	   (lambda ()
	     (element-padding-right elt))]
	  [set-padding-right!
	   (lambda (s)
	     (element-set-padding-right! elt s))]
	  [border-top-color
	   (lambda ()
	     (element-border-top-color elt))]
	  [set-border-top-color!
	   (lambda (s)
	     (element-set-border-top-color! elt s))]
	  [border-bottom-color
	   (lambda ()
	     (element-border-bottom-color elt))]
	  [set-border-bottom-color!
	   (lambda (s)
	     (element-set-border-bottom-color! elt s))]
	  [border-left-color
	   (lambda ()
	     (element-border-left-color elt))]
	  [set-border-left-color!
	   (lambda (s)
	     (element-set-border-left-color! elt s))]
	  [border-right-color
	   (lambda ()
	     (element-border-right-color elt))]
	  [set-border-right-color!
	   (lambda (s)
	     (element-set-border-right-color! elt s))]
	  [border-top-width
	   (lambda ()
	     (element-border-top-width elt))]
	  [set-border-top-width!
	   (lambda (s)
	     (element-set-border-top-width! elt s))]
	  [border-bottom-width
	   (lambda ()
	     (element-border-bottom-width elt))]
	  [set-border-bottom-width!
	   (lambda (s)
	     (element-set-border-bottom-width! elt s))]
	  [border-left-width
	   (lambda ()
	     (element-border-left-width elt))]
	  [set-border-left-width!
	   (lambda (s)
	     (element-set-border-left-width! elt s))]
	  [border-right-width
	   (lambda ()
	     (element-border-right-width elt))]
	  [set-border-right-width!
	   (lambda (s)
	     (element-set-border-right-width! elt s))]
	  [width
	   (lambda ()
	     (element-width elt))]
	  [set-width!
	   (lambda (s)
	     (element-set-width! elt s))]
	  [height
	   (lambda ()
	     (element-height elt))]
	  [set-height!
	   (lambda (s)
	     (element-set-height! elt s))]
	  [top
	   (lambda ()
	     (element-top elt))]
	  [set-top!
	   (lambda (s)
	     (element-set-top! elt s))]
	  [left
	   (lambda ()
	     (element-left elt))]
	  [set-left!
	   (lambda (s)
	     (element-set-left! elt s))]
	  [z-index
	   (lambda ()
	     (element-z-index elt))]
	  [set-z-index!
	   (lambda (s)
	     (element-set-z-index! elt s))])))

(define mx-event%
  (class null (dhtml-event)

	 (private
	  [event dhtml-event])

	 (public

	  ; predicates

	  [keypress? (lambda () (event-keypress? event))]
	  [keydown? (lambda () (event-keydown? event))]
	  [keyup? (lambda () (event-keyup? event))] 
	  [mousedown? (lambda () (event-mousedown? event))] 
	  [mousemove? (lambda () (event-mousemove? event))] 
	  [mouseover? (lambda () (event-mouseover? event))] 
	  [mouseout? (lambda () (event-mouseout? event))] 
	  [mouseup? (lambda () (event-mouseup? event))] 
	  [click? (lambda () (event-click? event))] 
	  [dblclick? (lambda () (event-dblclick? event))] 
	  [error? (lambda () (event-error? event))]
	  
	  ; attributes

	  [tag (lambda () (event-tag event))]
	  [id (lambda () (event-id event))]
	  [from-tag (lambda () (event-from-tag event))]
	  [from-id (lambda () (event-id event))]
	  [to-tag (lambda () (event-to-tag event))]
	  [to-id (lambda () (event-to-id event))]
	  [x (lambda () (event-x event))]
	  [y (lambda () (event-y event))])))

(define mx-document%
  (class null 

	 ((label "MysterX")
	  (width 'default)
	  (height 'default)
	  (x 'default)
	  (y 'default)
	  (style-options null))

	 (private
	  [doc (make-document label width height x y style-options)]
	  [thread-sem (make-semaphore 1)]   ; protects *handler-threads*
	  [handler-sem (make-semaphore 1)]  ; protects *handler-table* and its contained hash tables
	  [handler-table (make-hash-table)]
	  [handler-thread #f]
	  [block-until-event ; busy-wait loop -- until Mz threads allow blocking
	   (lambda ()
	     (let loop () 
	       (unless (event-available? doc)
		       (sleep 0.1)
		       (loop))))]
	  [make-event-key 
	   (lambda (tag id) ; string x string -> symbol
	     (let ([new-tag (string-copy tag)]
		   [new-id (string-copy id)])
	       (string-uppercase! new-tag)
		  (string-uppercase! new-id)
		  (string->symbol
		   (string-append new-tag "@" new-id))))])

	 (public
	  [show 
	   (lambda (b) 
	     (document-show doc b))]
	  [find-element
	   (lambda (tag id)
	     (make-object mx-element% (document-find-element doc tag id)))]
	  [objects
	   (lambda () 
	     (document-objects doc))]
	  [insert-html 
	   (lambda (html-string)
	     (document-insert-html doc html-string))]
	  [append-html 
	   (lambda (html-string)
	     (document-append-html doc html-string))]
	  [replace-html 
	   (lambda (html-string)
	     (document-replace-html doc html-string))]
	  [register-event-handler
	   (lambda (elt fn)
	     (semaphore-wait handler-sem)
	     (let* ([tag (send elt tag)]
		    [id (send elt attribute "id")]) 
	       (let ([key (make-event-key tag id)])
		 (hash-table-remove! handler-table key)
		 (hash-table-put! handler-table key fn)))
	     (semaphore-post handler-sem))]
	  [unregister-event-handler
	   (lambda (elt)
	     (semaphore-wait handler-sem)
	     (let* ([tag (send elt tag)]
		    [id (send elt attribute "id")])
	       (let ([key (make-event-key tag id)])
		 (hash-table-remove! handler-table key)))
	     (semaphore-post handler-sem))]
	   [append-object 
	    (lambda (object)
	      (append-html (coclass->html object))
	      (car (document-objects doc)))]
	   [insert-object 
	    (lambda (object)
	      (insert-html (coclass->html object))
	      (car (document-objects doc)))]
	   [handle-events 
	    (lambda ()
	      (semaphore-wait thread-sem)
	      ; no-op if existing handler-thread
	      (unless handler-thread
		      (semaphore-wait handler-sem)
		      (let* ([handler-thunk
			      (lambda ()
				(let loop ()
				  (block-until-event)
				     (let* ([event (make-object mx-event% 
								(get-event doc))]
					    [tag (send event tag)]
					    [id (send event id)]
					    [key (make-event-key tag id)]
					    [handler (hash-table-get handler-table key void)])
				       (unless (void? handler)
					       (handler event))
				       (loop))))])
			(set! handler-thread (thread handler-thunk)))
		      (semaphore-post handler-sem))
	      (semaphore-post thread-sem))]
	  [stop-handling-events 
	   (lambda ()
	     (semaphore-wait thread-sem)
	     (when handler-thread
		   (kill-thread handler-thread))
	     (set! handler-thread #f)
	     (semaphore-post thread-sem))])))

       
       
    
	


