;; mysterxe.ss

(unit/sig mysterx:mysterx^
  (import 
   [mzlib : mzlib:function^]
   mzlib:string^
   [mxprims : mysterx:prims^]
   [style : mysterx:style^]
   mysterx:filter^
   mysterx:properties^
   mysterx:util^)

  (define com-invoke mxprims:com-invoke)
  (define com-set-property! mxprims:com-set-property!)
  (define com-get-property mxprims:com-get-property)
  (define com-methods mxprims:com-methods)
  (define com-get-properties mxprims:com-get-properties)
  (define com-set-properties mxprims:com-set-properties)
  (define com-events mxprims:com-events)
  (define com-method-type mxprims:com-method-type)
  (define com-get-property-type mxprims:com-get-property-type)
  (define com-set-property-type mxprims:com-set-property-type)
  (define com-event-type mxprims:com-event-type)
  (define com-object-type mxprims:com-object-type)
  (define com-is-a? mxprims:com-is-a?)
  (define com-help mxprims:com-help)
  (define com-register-event-handler mxprims:com-register-event-handler)
  (define com-unregister-event-handler mxprims:com-unregister-event-handler)
  (define com-all-coclasses mxprims:com-all-coclasses)
  (define com-all-controls mxprims:com-all-controls)
  (define coclass->html mxprims:coclass->html)
  (define cocreate-instance mxprims:cocreate-instance)
  (define com-object-eq? mxprims:com-object-eq?)
  (define com-object? mxprims:com-object?)
  (define com-omit mxprims:com-omit)

  (define make-css-percentage style:make-css-percentage)
  (define css-percentage? style:css-percentage?) 
  (define css-percentage-num style:css-percentage-num)
  (define make-css-length style:make-css-length)
  (define css-length? style:css-length?)
  (define css-length-num style:css-length-num)
  (define css-length-units style:css-length-units)

  (define html-sem (make-semaphore 1))   ; protects HTML insertions
  (define html-wait (lambda () (semaphore-wait html-sem)))
  (define html-post (lambda () (semaphore-post html-sem)))

  (define mx-element%
    (class object% (document dhtml-element)
	   (private
	    [elt dhtml-element]
	    [doc document]
	    [get-string-as-symbol
	     (lambda (f name)
		(let ([s (f elt)])
		  (if (empty-string? s)	
		      (empty-property-error name)
		      (string->symbol s))))]
	    [set-symbol-as-string
	     (lambda (sym vals f name)
	       (unless (member sym vals)
		       (error 
			(format "~a: Expected value in '~a, got ~a" 
				name vals sym)))
	       (f elt (symbol->string sym)))])
	   (public
	    [insert-html
	     (lambda (s)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:element-insert-html elt s))
		html-post))]
	    [append-html
	     (lambda (s)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:element-append-html elt s))
		html-post))]
	    [replace-html
	     (lambda (s)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:element-replace-html elt s))
		html-post))]
	    [insert-text 
	     (lambda (s)
	       (mxprims:element-insert-text elt s))]
	    [append-text
	     (lambda (s)
	       (mxprims:element-append-text elt s))]
	    [insert-object 
	     (opt-lambda (object width height [size 'pixels])
	       (dynamic-wind
		html-wait
		(lambda () 
		  (let ([old-objects (mxprims:document-objects doc)])
		    (mxprims:element-insert-html 
		     elt 
		     (coclass->html object width height size))
		       (let* ([new-objects (mxprims:document-objects doc)]
			      [obj (car (mzlib:remove* old-objects new-objects
						 com-object-eq?))])
			 (mxprims:com-register-object obj)
			 obj)))
		html-post))]
	    [append-object 
	     (opt-lambda (object width height [size 'pixels])
	       (dynamic-wind
		html-wait
		(lambda ()
		  (let* ([old-objects (mxprims:document-objects doc)])
		    (mxprims:element-append-html 
		     elt 
		     (coclass->html object width height size))
		       (let* ([new-objects (mxprims:document-objects doc)]
			      [obj (car (mzlib:remove* old-objects
						 new-objects
						 com-object-eq?))])
			 (mxprims:com-register-object obj)
			 obj)))
		html-post))]
	    [attribute
	     (lambda (s)
	       (mxprims:element-attribute elt s))]
	    [set-attribute!
	     (lambda (a v)
	       (mxprims:element-set-attribute! elt a v))]
	    [click
	     (lambda ()
	       (mxprims:element-click elt))]
	    [tag
	     (lambda ()
	       (mxprims:element-tag elt))]
	    [font-family
	     (lambda ()
	       (let ([s (mxprims:element-font-family elt)])
		 (if (empty-string? s)
		     (empty-property-error "font-family")
		     (style:string->font-families s))))]
	    [font-family-native
	     (lambda ()
	       (mxprims:element-font-family elt))]
	    [set-font-family!
	     (lambda (ff)
	       (unless (and (list? ff)
			    (andmap string? ff))
		       (error "set-font-family!: Expected list of strings, got"
			      ff))
	       (mxprims:element-set-font-family! 
		elt 
		(style:font-families->string ff)))]
	    [set-font-family-native!
	     (lambda (s)
	       (mxprims:element-set-font-family! elt s))]
	    [font-style
	     (lambda ()
	       (get-string-as-symbol 
		mxprims:element-font-style "font-style"))]
	    [font-style-native
	     (lambda ()
	       (mxprims:element-font-style elt))]
	    [set-font-style!
	     (lambda (sym)
	       (set-symbol-as-string sym *font-styles* 
				     mxprims:element-set-font-style!
				     "set-font-style!"))]
	    [set-font-style-native!
	     (lambda (s)
	       (mxprims:element-set-font-style! elt s))]
	    [font-variant
	     (lambda ()
	       (get-string-as-symbol mxprims:element-font-variant
				     "font-variant"))]
	    [font-variant-native
	     (lambda ()
	       (mxprims:element-font-variant elt))]
	    [set-font-variant!
	     (lambda (sym)
	       (set-symbol-as-string
		sym *font-variants* mxprims:element-set-font-variant! 
		"set-font-variant!"))]
	    [set-font-variant-native!
	     (lambda (s)
	       (mxprims:element-set-font-variant! elt s))]
	    [font-weight
	     (lambda ()
	       (let ([s (mxprims:element-font-weight elt)])
		 (if (empty-string? s)
		     (empty-property-error "font-weight")
		     (let ([c (string-ref s 0)])
		       (if (char-numeric? c)
			   (string->number s)
			   (string->symbol s))))))]
	    [font-weight-native
	     (lambda ()
	       (mxprims:element-font-weight elt))]
	    [set-font-weight!
	     (lambda (w)
	       (unless (member w 
			       '(bold bolder lighter normal
				 100 200 300 400 500 600 700 800 900))
		       (error 
			(string-append 
			 "Expected value in "
			 "'(bold bolder lighter normal "
			 "100 200 300 400 500 600 700 800 900),"
			 "got ~a")
			w))
	       (let ([s (if (number? w)
			    (number->string w)
			    (symbol->string w))])
		 (mxprims:element-set-font-weight! elt s)))]
	    [set-font-weight-native!
	     (lambda (s)
	       (mxprims:element-set-font-weight! elt s))]
	    [font-native
	     (lambda ()
	       (mxprims:element-font elt))]
	    [set-font-native!
	     (lambda (s)
	       (mxprims:element-set-font! elt s))]
	    [background-native
	     (lambda ()
	       (mxprims:element-background elt))]
	    [set-background-native!
	     (lambda (s)
	       (mxprims:element-set-background! elt s))]
	    [background-attachment
	     (lambda ()
	       (get-string-as-symbol mxprims:element-background-attachment
				     "background-attachment"))]
	    [background-attachment-native
	     (lambda ()
	       (mxprims:element-background-attachment elt))]
	    [set-background-attachment!
	     (lambda (sym)
	       (set-symbol-as-string
		sym *background-attachments*
		mxprims:element-set-background-attachment! 
		"set-background-attachment!"))]
	    [set-background-attachment-native!
	     (lambda (s)
	       (mxprims:element-set-background-attachment! elt s))]
	    [background-image
	     (lambda ()
		(let ([s (mxprims:element-background-image elt)])
		  (cond
		   [(empty-string? s)
		    (empty-property-error "background-image")]
		   [(string=? s "none") 'none]
		   [(string-ci=? (substring s 0 3) "url")
		    (list->string
		     (mzlib:filter (lambda (c)
			       (not (member c '(#\( #\)))))
			     (string->list 
			      (substring s 3 (string-length s)))))]
		   [else (error "Unknown background-image value: ~a"
				s)])))]
	    [background-image-native
	     (lambda ()
	       (mxprims:element-background-image elt))]
	    [set-background-image!
	     (lambda (image)
	       (cond
		[(eq? image 'none)
		 (mxprims:element-set-background-image! elt "none")]
		[(string? image)
		 (mxprims:element-set-background-image! 
		  elt 
		  (string-append "url(" image ")"))]
		[else
		 (error "Expected 'none or string, got: ~a" image)]))]
	    [set-background-image-native!
	     (lambda (s)
	       (mxprims:element-set-background-image! elt s))]
	    [background-repeat
	     (lambda ()
	       (get-string-as-symbol mxprims:element-background-repeat
				     "background-repeat"))]
	    [background-repeat-native
	     (lambda ()
	       (mxprims:element-background-repeat elt))]
	    [set-background-repeat!
	     (lambda (sym)
	       (set-symbol-as-string
		sym *background-repeats*
		mxprims:element-set-background-repeat!
		"set-background-repeat!"))]
	    [set-background-repeat-native!
	     (lambda (s)
	       (mxprims:element-set-background-repeat! elt s))]
	    [background-position
	     (lambda ()
	       (let ([s (mxprims:element-background-position elt)])
		 (if (empty-string? s)
		     (empty-property-error "background-position")
		     (style:string->background-position s))))]
	    [background-position-native
	     (lambda ()
	       (mxprims:element-background-position elt))]
	    [set-background-position!
	     (lambda (pos)
		(cond
		 [(and (list? pos) (= (length pos) 2))
		  (if (andmap symbol? pos)
		      (let ([elt-1 (car pos)]
			    [elt-2 (cadr pos)])
			(if (or (and (horizontal? elt-1)
				     (vertical? elt-2))
				(and (vertical? elt-1)
				     (horizontal? elt-2)))
			    (mxprims:element-set-background-position! 
			     elt 
			     (string-append (symbol->string elt-1)
					    " "
					    (symbol->string elt-2)))
			    (error 
			     (format 
			      (string-append 
			       "One symbol must be from "
			       "'~a, other from "
			      "'~a, got: ~a") 
			      *horizontals* *verticals* pos))))
		      (if (andmap style:percentage-or-length? pos)
			  (mxprims:element-set-background-position! 
			   elt 
			   (string-append
			    (style:percentage-or-length->string (car pos))
			    " "
			    (style:percentage-or-length->string (cadr pos))))
			  (error 
			   (format 
			    (string-append 
			     "Two elements of list "
			     " must be either a percentage or "
			     " CSS length, got: ~a") pos))))]
		 [(style:percentage-or-length? pos)
		  (mxprims:element-set-background-position! 
		   elt 
		   (style:percentage-or-length->string pos))]
		 [else
		  (error 
		   (format 
		    (string-append 
		     "Expected any of "
		     "1) a list of two symbols, one "
		     "from '~a, the other from '~a, or "
		     "2) a two element list, where each element is a "
		     "percentage or CSS length, or "
		     "3) a percentage, or "
		     "4) a CSS length.  Got: ~a") 
		    *horizontals* *verticals* pos))]))]
	    [set-background-position-native!
	     (lambda (s)
	       (mxprims:element-set-background-position! elt s))]
	    [text-decoration
	     (lambda ()
	       (style:validated-string->symbols
		(mxprims:element-text-decoration elt)
		"text-decoration" style:parse-decoration))]
	    [text-decoration-native
	     (lambda ()
	       (mxprims:element-text-decoration elt))]
	    [set-text-decoration!
	     (lambda (decs)
	       (unless 
		(andmap decoration? decs)
		(error 
		 (format "Expected text decorations from ~a, got: ~a"
			 *decorations* decs)))
	       (mxprims:element-set-text-decoration! elt 
                   (symbols->string decs)))]
	    [set-text-decoration-native!
	     (lambda (s)
	       (mxprims:element-set-text-decoration! elt s))]
	    [text-transform
	     (lambda ()
	       (get-string-as-symbol mxprims:element-text-transform
				     "text-transform"))]
	    [text-transform-native
	     (lambda ()
	       (mxprims:element-text-transform elt))]
	    [set-text-transform!
	     (lambda (sym)
	       (set-symbol-as-string
		sym *text-transforms* mxprims:element-set-text-transform!
		"set-text-transforms!"))]
	    [set-text-transform-native!
	     (lambda (s)
	       (mxprims:element-set-text-transform! elt s))]
	    [text-align
	     (lambda ()
	       (get-string-as-symbol 
		mxprims:element-text-align
		"text-align"))]
	    [text-align-native
	     (lambda ()
	       (mxprims:element-text-align elt))]
	    [set-text-align!
	     (lambda (sym)
	       (set-symbol-as-string
		sym *text-aligns*
		mxprims:element-set-text-align!
		"set-text-align!"))]
	    [set-text-align-native!
	     (lambda (s)
	       (mxprims:element-set-text-align! elt s))]
	    [margin
	     (lambda ()
	       (let ([s (mxprims:element-margin elt)])
		 (if (empty-string? s)
		     (empty-property-error "margin")
		     (style:string->margin s))))]
	    [margin-native
	     (lambda ()
	       (mxprims:element-margin elt))]
	    [set-margin!
	     (lambda (lst)
	       (let ([len (length lst)])
		 (when (or (< len 1) (> len 4))
		       (error 
			"Expected one to four margin values, got"
			lst)))
	       (mxprims:element-set-margin! elt (style:margin->string lst)))]
	    [set-margin-native!
	     (lambda (s)
	       (mxprims:element-set-margin! elt s))]
	    [padding
	     (lambda ()
	       (let ([s (mxprims:element-padding elt)])
		 (if (empty-string? s)
		     (empty-property-error "padding")
		     (style:string->padding s))))]
	    [padding-native
	     (lambda ()
	       (mxprims:element-padding elt))]
	    [set-padding!
	     (lambda (pads)
	       (unless (and (list? pads)
			    (let ([len (length pads)])
			      (and (>= len 1) (<= len 4))) 
			    (andmap style:percentage-or-length? pads))
		       (error (string-append 
			       "set-padding: expected list of "
			       "1 to 4 css-percentages or "
			       "css-lengths, got") pads))
	       (mxprims:element-set-padding! 
		elt 
		(style:padding->string pads)))]
	    [set-padding-native!
	     (lambda (s)
	       (mxprims:element-set-padding! elt s))]
	    [border
	     (style:make-border-getter elt mxprims:element-border "border")]
	    [border-native
	     (lambda (s)
	       (mxprims:element-border elt s))]
	    [set-border!
	     (lambda (cs)
	       (style:set-border-with-fun 
		elt cs mxprims:element-set-border!))]
	    [set-border-native!
	     (lambda (s)
	       (mxprims:element-set-border! elt s))]
	    [border-top
	     (style:make-border-getter 
	      elt mxprims:element-border-top "border-top")]
	    [border-top-native
	     (lambda ()
	       (mxprims:element-border-top elt))]
	    [set-border-top!
	     (lambda (cs)
	       (style:set-border-with-fun 
		elt cs mxprims:element-set-border-top!))]
	    [set-border-top-native!
	     (lambda (s)
	       (mxprims:element-set-border-top! elt s))]
	    [border-bottom
	     (style:make-border-getter 
	      elt mxprims:element-border-bottom "border-bottom")]
	    [border-bottom-native
	     (lambda ()
	       (mxprims:element-border-bottom elt))]
	    [set-border-bottom!
	     (lambda (cs)
	       (style:set-border-with-fun 
		elt cs mxprims:element-set-border-bottom!))]
	    [set-border-bottom-native!
	     (lambda (s)
	       (mxprims:element-set-border-bottom! elt s))]
	    [border-left
	     (style:make-border-getter 
	      elt mxprims:element-border-left "border-left")]
	    [border-left-native
	     (lambda ()
	       (mxprims:element-border-left elt))]
	    [set-border-left!
	     (lambda (cs)
	       (style:set-border-with-fun 
		elt cs mxprims:element-set-border-left!))]
	    [set-border-left-native!
	     (lambda (s)
	       (mxprims:element-set-border-left! elt s))]
	    [border-right
	     (style:make-border-getter 
	      elt mxprims:element-border-right "border-right")]
	    [border-right-native
	     (lambda ()
	       (mxprims:element-border-right elt))]
	    [set-border-right!
	     (lambda (cs)
	       (style:set-border-with-fun 
		elt cs mxprims:element-set-border-right!))]
	    [set-border-right-native!
	     (lambda (s)
	       (mxprims:element-set-border-right! elt s))]
	    [border-color
	     (style:make-color-getter 
	      elt 
	      mxprims:element-border-color
	      "border-color")]
	    [border-color-native
	     (lambda ()
	       (mxprims:element-border-color elt))]
	    [set-border-color!
	     (style:make-color-setter 
	      elt mxprims:element-set-border-color!
	      "set-border-color!")]
	    [set-border-color-native!
	     (lambda (s)
	       (mxprims:element-set-border-color! elt s))]
	    [border-width
	     (style:make-border-width-getter 
	      elt mxprims:element-border-width "border-width")]
	    [border-width-native
	     (lambda ()
	       (mxprims:element-border-width elt))]
	    [set-border-width!
	     (lambda (s)
	       (unless (style:border-width? s)
		       (error 
			(format "border-width: Expected element of ~a or CSS length, got: ~a"
				*border-widths* s)))
	       (mxprims:element-set-border-width! 
		elt 
		(style:border-width->string s)))]
	    [set-border-width-native!
	     (lambda (s)
	       (mxprims:element-set-border-width! elt s))]
	    [border-style
	     (style:make-border-style-getter 
	      elt mxprims:element-border-style 
	      "border-style")]
	    [border-style-native
	     (lambda ()
	       (mxprims:element-border-style elt))]
	    [set-border-style!
	     (style:make-border-style-setter 
	      elt mxprims:element-set-border-style! 
	      "set-border-style!")]
	    [set-border-style-native!
	     (lambda (s)
	       (mxprims:element-set-border-style! elt s))]
	    [border-top-style
	     (style:make-border-style-getter 
	      elt mxprims:element-border-top-style 
	      "border-top-style")]
	    [border-top-style-native
	     (lambda ()
	       (mxprims:element-border-top-style elt))]
	    [set-border-top-style!
	     (style:make-border-style-setter 
	      elt mxprims:element-set-border-top-style! 
	      "set-border-top-style!")]
	    [set-border-top-style-native!
	     (lambda (s)
	       (mxprims:element-set-border-top-style! elt s))]
	    [border-bottom-style
	     (style:make-border-style-getter 
	      elt mxprims:element-border-bottom-style 
	      "border-bottom-style")]
	    [border-bottom-style-native
	     (lambda ()
	       (mxprims:element-border-bottom-style elt))]
	    [set-border-bottom-style!
	     (style:make-border-style-setter 
	      elt mxprims:element-set-border-bottom-style! 
	      "set-border-bottom-style!")]
	    [set-border-bottom-style-native!
	     (lambda (s)
	       (mxprims:element-set-border-bottom-style! elt s))]
	    [border-left-style
	     (style:make-border-style-getter 
	      elt mxprims:element-border-left-style 
	      "border-left-style")]
	    [border-left-style-native
	     (lambda ()
	       (mxprims:element-border-left-style elt))]
	    [set-border-left-style!
	     (style:make-border-style-setter 
	      elt mxprims:element-set-border-left-style! 
	      "set-border-left-style!")]
	    [set-border-left-style-native!
	     (lambda (s)
	       (mxprims:element-set-border-left-style! elt s))]
	    [border-right-style
	     (style:make-border-style-getter 
	      elt mxprims:element-border-right-style 
	      "border-right-style")]
	    [border-right-style-native
	     (lambda ()
	       (mxprims:element-border-right-style elt))]
	    [set-border-right-style!
	     (style:make-border-style-setter elt
	      mxprims:element-set-border-right-style! 
	      "set-border-right-style!")]
	    [set-border-right-style-native!
	     (lambda (s)
	       (mxprims:element-set-border-right-style! elt s))]
	    [border-top-color
	     (style:make-color-getter 
	      elt mxprims:element-border-top-color
	      "border-top-color")]
	    [border-top-color-native!
	     (lambda ()
	       (mxprims:element-border-top-color elt))]
	    [set-border-top-color!
	     (style:make-color-setter 
	      elt mxprims:element-set-border-top-color!
	      "set-border-top-color!")]
	    [set-border-top-color-native!
	     (lambda (s)
	       (mxprims:element-set-border-top-color! elt s))]
	    [border-bottom-color
	     (style:make-color-getter 
	      elt mxprims:element-border-bottom-color
	      "border-bottom-color")]
	    [border-bottom-color-native
	     (lambda ()
	       (mxprims:element-border-bottom-color elt))]
	    [set-border-bottom-color!
	     (style:make-color-setter 
	      elt mxprims:element-set-border-bottom-color!
	      "set-border-bottom-color!")]
	    [set-border-bottom-color-native!
	     (lambda (s)
	       (mxprims:element-set-border-bottom-color! elt s))]
	    [border-left-color
	     (style:make-color-getter 
	      elt mxprims:element-border-left-color
	      "border-left-color")]
	    [border-left-color-native
	     (lambda ()
	       (mxprims:element-border-left-color elt))]
	    [set-border-left-color!
	     (style:make-color-setter 
	      elt mxprims:element-set-border-left-color!
	      "set-border-left-color!")]
	    [set-border-left-color-native!
	     (lambda (s)
	       (mxprims:element-set-border-left-color! elt s))]
	    [border-right-color
	     (style:make-color-getter 
	      elt mxprims:element-border-right-color
	      "border-right-color")]
	    [border-right-color-native
	     (lambda ()
	       (mxprims:element-border-right-color elt))]
	    [set-border-right-color!
	     (style:make-color-setter 
	      elt mxprims:element-set-border-right-color!
	      "set-border-right-color!")]
	    [set-border-right-color-native!
	     (lambda (s)
	       (mxprims:element-set-border-right-color! elt s))]
	    [border-top-width
	     (style:make-border-width-getter 
	      elt mxprims:element-border-top-width "border-top-width")]
	    [border-top-width-native
	     (lambda ()
	       (mxprims:element-border-top-width elt))]
	    [set-border-top-width!
	     (style:make-border-width-setter 
	      elt mxprims:element-set-border-top-width!
	      "set-border-top-width!")]
	    [set-border-top-width-native!
	     (lambda (s)
	       (mxprims:element-set-border-top-width! elt s))]
	    [border-bottom-width
	     (style:make-border-width-getter 
	      elt mxprims:element-border-bottom-width "border-bottom-width")]
	    [border-bottom-width-native
	     (lambda ()
	       (mxprims:element-border-bottom-width elt))]
	    [set-border-bottom-width!
	     (style:make-border-width-setter 
	      elt mxprims:element-set-border-bottom-width!
	      "set-border-bottom-width!")]
	    [set-border-bottom-width-native!
	     (lambda (s)
	       (mxprims:element-set-border-bottom-width! elt s))]
	    [border-left-width
	     (style:make-border-width-getter 
	      elt mxprims:element-border-left-width "border-left-width")]
	    [border-left-width-native
	     (lambda ()
	       (mxprims:element-border-left-width elt))]
	    [set-border-left-width!
	     (style:make-border-width-setter 
	      elt mxprims:element-set-border-left-width!
	      "set-border-left-width!")]
	    [set-border-left-width-native!
	     (lambda (s)
	       (mxprims:element-set-border-left-width! elt s))]
	    [border-right-width
	     (style:make-border-width-getter 
	      elt mxprims:element-border-right-width "border-right-width")]
	    [border-right-width-native
	     (lambda ()
	       (mxprims:element-border-right-width elt))]
	    [set-border-right-width!
	     (style:make-border-width-setter 
	      elt mxprims:element-set-border-right-width!
	      "set-border-right-width!")]
	    [set-border-right-width-native!
	     (lambda (s)
	       (mxprims:element-set-border-right-width! elt s))]
	    [style-float
	     (style:make-element-getter 
	      elt 
	      mxprims:element-style-float "style-float" )]
	    [style-float-native
	     (lambda ()
	       (mxprims:element-style-float elt))]
	    [set-style-float!
	     (style:make-element-setter elt 
				  style-float? 
				  *style-floats* 
				  mxprims:element-set-style-float!)] 
	    [set-style-float-native!
	     (lambda (s)
	       (mxprims:element-set-style-float! elt s))]
	    [clear
	     (style:make-element-getter 
	      elt mxprims:element-clear "clear")]
	    [clear-native
	     (lambda ()
	       (mxprims:element-clear elt))]
	    [set-clear!
	     (style:make-element-setter elt
				  clear? 
				  *clears* 
				  mxprims:element-set-clear!)] 
	    [set-clear-native!
	     (lambda (s)
	       (mxprims:element-set-clear! elt s))]
	    [display
	     (style:make-element-getter 
	      elt mxprims:element-display "display")]
	    [display-native
	     (lambda ()
	       (mxprims:element-display elt))]
	    [set-display!
	     (style:make-element-setter elt 
				  display? 
				  *displays* 
				  mxprims:element-set-display!)] 
	    [set-display-native!
	     (lambda (s)
	       (mxprims:element-set-display! elt s))]
	    [visibility
	     (style:make-element-getter 
	      elt mxprims:element-visibility
	      "visibility")] 
	    [visibility-native
	     (lambda ()
	       (mxprims:element-visibility elt))]
	    [set-visibility!
	     (style:make-element-setter elt
				  visibility? 
				  *visibilities* 
				  mxprims:element-set-visibility!)] 
	    [set-visibility-native!
	     (lambda (s)
	       (mxprims:element-set-visibility! elt s))]
	    [list-style-type
	     (style:make-element-getter 
	      elt mxprims:element-list-style-type
	      "list-style-type")]
	    [list-style-type-native
	     (lambda ()
	       (mxprims:element-list-style-type elt))]
	    [set-list-style-type!
	     (style:make-element-setter elt
				  list-style-type? 
				  *list-style-types* 
				  mxprims:element-set-list-style-type!)] 
	    [set-list-style-type-native!
	     (lambda (s)
	       (mxprims:element-set-list-style-type! elt s))]
	    [list-style-position
	     (style:make-element-getter 
	      elt mxprims:element-list-style-position
	      "list-style-position")]
	    [list-style-position-native
	     (lambda ()
	       (mxprims:element-list-style-position elt))]
	    [set-list-style-position!
	     (style:make-element-setter elt 
				  list-style-position? 
				  *list-style-positions* 
				  mxprims:element-set-list-style-position!)] 
	    [set-list-style-position-native!
	     (lambda (s)
	       (mxprims:element-set-list-style-position! elt s))]
	    [list-style-image
	     (lambda ()
	       (let ([s (mxprims:element-list-style-image  elt)])
		 (when (empty-string? s)
		       (empty-property-error "list-style-image"))
		 (cond
		  [(string-ci=? s "none") 'none]
		  [(string-ci=? (substring s 0 4) "url(")
		   (style:url->string s)]
		  [else 
		   (error 
		    (format 
		     "list-style-image: Expected 'none or URL, got: ~a" s))])))]
	    [list-style-image-native
	     (lambda ()
	       (mxprims:element-list-style-image elt))]
	    [set-list-style-image!
	     (lambda (s)
	       (let ([str (if (eq? s 'none)
			      "none"
			      (style:string->url s))])
		 (mxprims:element-set-list-style-image! elt str)))]
	    [set-list-style-image-native!
	     (lambda (s)
	       (mxprims:element-set-list-style-image! elt s))]
	    [list-style
	     (lambda ()
	       (let* ([s (mxprims:element-list-style elt)]
		      [elts (style:parse-string s)])
		 (map (style:string->list-style-item elts))))]
	    [list-style-native
	     (lambda ()
	       (mxprims:element-list-style elt))]
	    [set-list-style!
	     (lambda (items)
	       (mxprims:element-set-list-style! 
		elt
		(fold-strings-with-spaces 
		 (map style:list-style-item->string items))))]
	    [set-list-style-native!
	     (lambda (s)
	       (mxprims:element-set-list-style! elt s))]
	    [position
	     (style:make-element-getter 
	      elt mxprims:element-position
	      "position")]
	    [position-native
	     (lambda ()
	       (mxprims:element-position elt))]
	    [overflow
	     (style:make-element-getter 
	      elt mxprims:element-overflow "overflow")]
	    [overflow-native
	     (lambda ()
	       (mxprims:element-overflow elt))]
	    [set-overflow!
	     (style:make-element-setter elt 
				  overflow?
				  *overflows* 
				  mxprims:element-set-overflow!)]
	    [set-overflow-native!
	     (lambda (s)
	       (mxprims:element-set-overflow! elt s))]
	    [pagebreak-before
	     (style:make-pagebreak-getter 
	      elt 
	      mxprims:element-pagebreak-before)]
	    [pagebreak-before-native
	     (lambda ()
	       (mxprims:element-pagebreak-before elt))]
	    [set-pagebreak-before!
	     (style:make-pagebreak-setter elt
	      mxprims:element-set-pagebreak-before!
	      "set-pagebreak-before!")]
	    [set-pagebreak-before-native!
	     (lambda (s)
	       (mxprims:element-set-pagebreak-before! elt s))]
	    [pagebreak-after
	     (style:make-pagebreak-getter 
	      elt mxprims:element-pagebreak-after)]
	    [pagebreak-after-native
	     (lambda ()
	       (mxprims:element-pagebreak-after elt))]
	    [set-pagebreak-after!
	     (style:make-pagebreak-setter elt 
	      mxprims:element-set-pagebreak-after!
	      "set-pagebreak-after!")]
	    [css-text-native
	     (lambda ()
	       (mxprims:element-css-text elt))]
	    [set-css-text-native!
	     (lambda (s)
	       (mxprims:element-set-css-text! elt s))]
	    [cursor
	     (style:make-element-getter 
	      elt mxprims:element-cursor "cursor")]
	    [cursor-native
	     (lambda ()
	       (mxprims:element-cursor elt))]
	    [set-cursor!
	     (style:make-element-setter elt 
				  cursor?
				  *cursors* 
				  mxprims:element-set-cursor!)]
	    [set-cursor-native!
	     (lambda (s)
	       (mxprims:element-set-cursor! elt s))]
	    [clip
	     (lambda ()
	       (let ([s (mxprims:element-clip elt)])
		 (cond
		  [(empty-string? s)
		   (empty-property-error "clip")]
		  [(string-ci=? s "auto")
		   'auto]
		  [(style:clip-rect? s)
		   (style:clip-rect->symbols s)]
		  [else 
		   (error 
		    (format "clip: Expected clip string, got: ~a" s))])))]
	    [clip-native
	     (lambda ()
	       (mxprims:element-clip elt))]
	    [set-clip!
	     (lambda (s)
	       (let ([str (cond
			   [(eq? s 'auto) "auto"]
			   [(and (list? s) 
				 (= (length s) 4) 
				 (andmap
				  (lambda (elt)
				    (or (eq? elt 'auto)
					(css-length? elt)))
				  s))
			    (string-append
			     "rect("
			    (fold-strings-with-spaces
			     (map 
			      (lambda (elt) 
				(if (eq? elt 'auto)
				    "auto"
				    (style:css-length->string elt)))
			      s))
                             ")")]
			   [else
			    (error
			     (format 
			      (string-append 
			       "Expected 'auto or 4-element list of "
			       "CSS lengths, with elements "
			       "possibly replaced by 'auto. Got ~a")
			      s))])])
		 (mxprims:element-set-clip! elt str)))]
	    [set-clip-native!
	     (lambda (s)
	       (mxprims:element-set-clip! elt s))]
	    [filter
	     (lambda ()
	       (let ([s (mxprims:element-filter elt)])
		 (if (empty-string? s)
		     (empty-property-error "filter")
		     (string->filter s))))]
	    [filter-native
	     (lambda ()
	       (mxprims:element-filter elt))]
	    [set-filter!
	     (lambda (filter . options)
		(let ([s (filter->string filter options)])		 
		  (mxprims:element-set-filter! elt s)))]
	    [set-filter-native!
	     (lambda (s)
	       (mxprims:element-set-filter! elt s))]
	    [style-string
	     (lambda ()
	       (mxprims:element-style-string elt))]
	    ; the text decoration, blink attributes are boolean
	    ; hence no conversion to/from strings
	    [text-decoration-none
	     (lambda ()
	       (mxprims:element-text-decoration-none elt))]
	    [set-text-decoration-none!
	     (lambda (s)
	       (mxprims:element-set-text-decoration-none! elt s))]
	    [text-decoration-underline
	     (lambda ()
	       (mxprims:element-text-decoration-underline elt))]
	    [set-text-decoration-underline!
	     (lambda (s)
	       (mxprims:element-set-text-decoration-underline! elt s))]
	    [text-decoration-overline
	     (lambda ()
	       (mxprims:element-text-decoration-overline elt))]
	    [set-text-decoration-overline!
	     (lambda (s)
	       (mxprims:element-set-text-decoration-overline! elt s))]
	    [text-decoration-linethrough
	     (lambda ()
	       (mxprims:element-text-decoration-linethrough elt))]
	    [set-text-decoration-linethrough!
	     (lambda (s)
	       (mxprims:element-set-text-decoration-linethrough! elt s))]
	    [text-decoration-blink
	     (lambda ()
	       (mxprims:element-text-decoration-blink elt))]
	    [set-text-decoration-blink!
	     (lambda (s)
	       (mxprims:element-set-text-decoration-blink! elt s))]
            ; pixel attributes are all longs
	    ; hence, no conversion to/from strings
	    [pixel-top
	     (lambda ()
	       (mxprims:element-pixel-top elt))]
	    [set-pixel-top!
	     (lambda (s)
	       (mxprims:element-set-pixel-top! elt s))]
	    [pixel-left
	     (lambda ()
	       (mxprims:element-pixel-left elt))]
	    [set-pixel-left!
	     (lambda (s)
	       (mxprims:element-set-pixel-left! elt s))]
	    [pixel-width
	     (lambda ()
	       (mxprims:element-pixel-width elt))]
	    [set-pixel-width!
	     (lambda (s)
	       (mxprims:element-set-pixel-width! elt s))]
	    [pixel-height
	     (lambda ()
	       (mxprims:element-pixel-height elt))]
	    [set-pixel-height!
	     (lambda (s)
	       (mxprims:element-set-pixel-height! elt s))]
            ; position attributes are all floats
	    ; hence no conversion to/from strings
	    [pos-top
	     (lambda ()
	       (mxprims:element-pos-top elt))]
	    [set-pos-top!
	     (lambda (s)
	       (mxprims:element-set-pos-top! elt s))]
	    [pos-left
	     (lambda ()
	       (mxprims:element-pos-left elt))]
	    [set-pos-left!
	     (lambda (s)
	       (mxprims:element-set-pos-left! elt s))]
	    [pos-width
	     (lambda ()
	       (mxprims:element-pos-width elt))]
	    [set-pos-width!
	     (lambda (s)
	       (mxprims:element-set-pos-width! elt s))]
	    [pos-height
	     (lambda ()
	       (mxprims:element-pos-height elt))]
	    [set-pos-height!
	     (lambda (s)
	       (mxprims:element-set-pos-height! elt s))]
	    [font-size
	     (lambda ()
	       (let ([s (mxprims:element-font-size elt)])
		 (if (empty-string? s)
		     (empty-property-error "font-size")
		     (style:string->font-size s))))]
	    [font-size-native
	     (lambda ()
	       (mxprims:element-font-size elt))]
	    [set-font-size!
	     (lambda (sz)
	       (let ([s (cond
			 [(font-size? sz)
			  (symbol->string sz)]
			 [(style:percentage-or-length? sz)
			  (style:percentage-or-length->string sz)]
			 [else
			  (error 
			   (format (string-append
				    "set-font-size!: Expected element of ~a, "
				    "a CSS length, or CSS percentage. Got: ~a")
				    *font-sizes* sz))])])
		 (mxprims:element-set-font-size! elt s)))]
	    [set-font-size-native!
	     (lambda (s)
	       (mxprims:element-set-font-size! elt s))]
	    [color
	     (style:make-color-getter elt mxprims:element-color "color")]
	    [color-native
	     (lambda ()
	       (mxprims:element-color elt))]
	    [set-color!
	     (style:make-color-setter 
	      elt mxprims:element-set-color! "set-color!")]
	    [set-color-native!
	     (lambda (s)
	       (mxprims:element-set-color! elt s))]
	    [background-color
	     (style:make-color-getter 
	      elt mxprims:element-background-color 
	      "background-color")]
	    [background-color-native
	     (lambda ()
	       (mxprims:element-background-color elt))]
	    [set-background-color!
	     (style:make-color-setter 
	      elt mxprims:element-set-background-color! 
	      "set-background-color!")]
	    [set-background-color-native!
	     (lambda (s)
	       (mxprims:element-set-background-color! elt s))]
	    [background-position-x
	     (style:make-bg-pos-getter 
	      elt 
	      mxprims:element-background-position-x
	      "background-position-x")]
	    [background-position-x-native
	     (lambda ()
	       (mxprims:element-background-position-x elt))]
	    [set-background-position-x!
	     (style:make-bg-pos-setter 
	      elt
	      mxprims:element-set-background-position-x! 
	      horizontal? *horizontals*
	      "x")]
	    [set-background-position-x-native!
	     (lambda (n)
	       (mxprims:element-set-background-position-x! elt n))]
	    [background-position-y
	     (style:make-bg-pos-getter 
	      elt 
	      mxprims:element-background-position-y
	      "background-position-y")]
	    [background-position-y-native
	     (lambda ()
	       (mxprims:element-background-position-y elt))]
	    [set-background-position-y!
	     (style:make-bg-pos-setter 
	      elt
	      mxprims:element-set-background-position-y! 
	      vertical? *verticals*
	      "y")]
	    [set-background-position-y-native!
	     (lambda (s)
	       (mxprims:element-set-background-position-y! elt s))]
	    [letter-spacing
	     (style:make-normal-or-css-getter elt
	      mxprims:element-letter-spacing
	      "letter-spacing")]
	    [letter-spacing-native
	     (lambda ()
	       (mxprims:element-letter-spacing elt))]
	    [set-letter-spacing!
	     (style:make-normal-or-css-setter elt
	      mxprims:element-set-letter-spacing!
	      "set-letter-spacing!")]
	    [set-letter-spacing-native!
	     (lambda (s)
	       (mxprims:element-set-letter-spacing! elt s))]
	    [vertical-align
	     (lambda ()
	       (let ([s (mxprims:element-vertical-align elt)])
		 (when (empty-string? s)
		       (empty-property-error "vertical-align"))
		 (string->symbol s)))]   
	    [vertical-align-native
	     (lambda ()
	       (mxprims:element-vertical-align elt))]
	    [set-vertical-align!
	     (lambda (sym)
	       (unless (vertical-align? sym)
		       (error 
			(format 
			 (string-append "set-vertical-align!: "
					"Expected element of ~a, got ~a")
			 *vertical-aligns* sym)))
	       (mxprims:element-set-vertical-align! 
		elt 
		(symbol->string sym)))]
	    [set-vertical-align-native!
	     (lambda (s)
	       (mxprims:element-set-vertical-align! elt s))]
	    [text-indent
	     (style:make-css-getter elt
	      mxprims:element-text-indent "text-indent")]
	    [text-indent-native
	     (lambda ()
	       (mxprims:element-text-indent elt))]
	    [set-text-indent!
	     (style:make-css-setter elt
	      mxprims:element-set-text-indent! "set-text-indent!")]
	    [set-text-indent-native!
	     (lambda (s)
	       (mxprims:element-set-text-indent! elt s))]
	    [line-height
	     (style:make-normal-or-css-getter elt
	      mxprims:element-line-height
	      "line-height")]
	    [line-height-native
	     (lambda ()
	       (mxprims:element-line-height elt))]
	    [set-line-height!
	     (style:make-normal-or-css-setter elt
	      mxprims:element-set-line-height!
	      "set-line-height!")]
	    [set-line-height-native!
	     (lambda (s)
	       (mxprims:element-set-line-height! elt s))]
	    [margin-top
	     (style:make-auto-or-css-getter elt
	      mxprims:element-margin-top
	      "margin-top")]
	    [margin-top-native
	     (lambda ()
	       (mxprims:element-margin-top elt))]
	    [set-margin-top!
	     (style:make-auto-or-css-setter elt
	      mxprims:element-set-margin-top!
	      "set-margin-top!")]
	    [set-margin-top-native!
	     (lambda (s)
	       (mxprims:element-set-margin-top! elt s))]
	    [margin-bottom
	     (style:make-auto-or-css-getter elt
	      mxprims:element-margin-bottom
	      "margin-bottom")]
	    [margin-bottom-native
	     (lambda ()
	       (mxprims:element-margin-bottom elt))]
	    [set-margin-bottom!
	     (style:make-auto-or-css-setter elt
	      mxprims:element-set-margin-bottom!
	      "set-margin-bottom!")]
	    [set-margin-bottom-native!
	     (lambda (s)
	       (mxprims:element-set-margin-bottom! elt s))]
	    [margin-left
	     (style:make-auto-or-css-getter elt
	      mxprims:element-margin-left
	      "margin-left")]
	    [margin-left-native
	     (lambda ()
	       (mxprims:element-margin-left elt))]
	    [set-margin-left!
	     (style:make-auto-or-css-setter elt
	      mxprims:element-set-margin-left!
	      "set-margin-left!")]
	    [set-margin-left-native!
	     (lambda (s)
	       (mxprims:element-set-margin-left! elt s))]
	    [margin-right
	     (style:make-auto-or-css-getter elt
	      mxprims:element-margin-right
	      "margin-right")]
	    [margin-right-native
	     (lambda ()
	       (mxprims:element-margin-right elt))]
	    [set-margin-right!
	     (style:make-auto-or-css-setter elt
	      mxprims:element-set-margin-right!
	      "set-margin-right!")]
	    [set-margin-right-native!
	     (lambda (s)
	       (mxprims:element-set-margin-right! elt s))]
	    [padding-top
	     (style:make-css-getter elt
	      mxprims:element-padding-top "padding-top")]
	    [padding-top-native
	     (lambda ()
	       (mxprims:element-padding-top elt))]
	    [set-padding-top!
	     (style:make-css-setter elt
	      mxprims:element-set-padding-top! "set-padding-top!")]
	    [set-padding-top-native!
	     (lambda (s)
	       (mxprims:element-set-padding-top! elt s))]
	    [padding-bottom
	     (style:make-css-getter elt
	      mxprims:element-padding-bottom "padding-bottom")]
	    [padding-bottom-native
	     (lambda ()
	       (mxprims:element-padding-bottom elt))]
	    [set-padding-bottom!
	     (style:make-css-setter elt
	      mxprims:element-set-padding-bottom! "set-padding-bottom!")]
	    [set-padding-bottom-native!
	     (lambda (s)
	       (mxprims:element-set-padding-bottom! elt s))]
	    [padding-left
	     (style:make-css-getter elt
	      mxprims:element-padding-left "padding-left")]
	    [padding-left-native
	     (lambda ()
	       (mxprims:element-padding-left elt))]
	    [set-padding-left!
	     (style:make-css-setter elt
	      mxprims:element-set-padding-left! "set-padding-left!")]
	    [set-padding-left-native!
	     (lambda (s)
	       (mxprims:element-set-padding-left! elt s))]
	    [padding-right
	     (style:make-css-getter elt
	      mxprims:element-padding-right "padding-right")]
	    [padding-right-native
	     (lambda ()
	       (mxprims:element-padding-right elt))]
	    [set-padding-right!
	     (style:make-css-setter elt
	      mxprims:element-set-padding-right! "set-padding-right!")]
	    [set-padding-right-native!
	     (lambda (s)
	       (mxprims:element-set-padding-right! elt s))]
	    [width
	     (style:make-auto-or-css-getter 
	      elt mxprims:element-width "width")]
	    [width-native
	     (lambda ()
	       (mxprims:element-width elt))]
	    [set-width!
	     (style:make-auto-or-css-setter 
	      elt mxprims:element-set-width!
	      "set-width!")]
	    [set-width-native!
	     (lambda (s)
	       (mxprims:element-set-width! elt s))]
	    [height
	     (style:make-auto-or-css-getter 
	      elt mxprims:element-height "height")]
	    [height-native
	     (lambda ()
	       (mxprims:element-height elt))]
	    [set-height!
	     (style:make-auto-or-css-setter 
	      elt mxprims:element-set-height!
	      "set-height!")]
	    [set-height-native!
	     (lambda (s)
	       (mxprims:element-set-height! elt s))]
	    [top
	     (style:make-auto-or-css-getter 
	      elt mxprims:element-top "top")]
	    [top-native
	     (lambda ()
	       (mxprims:element-top elt))]
	    [set-top!
	     (style:make-auto-or-css-setter 
	      elt mxprims:element-set-top!
	      "set-top!")]
	    [set-top-native!
	     (lambda (s)
	       (mxprims:element-set-top! elt s))]
	    [left
	     (style:make-auto-or-css-getter 
	      elt mxprims:element-left "left")]
	    [left-native
	     (lambda ()
	       (mxprims:element-left elt))]
	    [set-left!
	     (style:make-auto-or-css-setter 
	      elt
	      mxprims:element-set-left!
	      "set-left!")]
	    [set-left-native!
	     (lambda (s)
	       (mxprims:element-set-left! elt s))]
	    [z-index 
	     (lambda ()
	       (let ([s (mxprims:element-z-index elt)])
		 (when (empty-string? s)
		       (empty-property-error "z-index"))
		 (if (and (string? s) (string=? s "auto"))
		     'auto
		     s)))]
	    [z-index-native
	     (lambda ()
	       (mxprims:element-z-index elt))]
	    [set-z-index!
	     (lambda (zi)
	       (let ([s (cond
			 [(eq? zi 'auto) "auto"]
			 [(and (number? zi)
			       (exact? zi)) zi]
			 [else
			  (error 
			   (string-append "set-z-index!: "
					  "Expected 'auto or exact integer, "
					  "got")
			   zi)])])
		 (mxprims:element-set-z-index! elt s)))]
	    [set-z-index-native!
	     (lambda (s)
	       (mxprims:element-set-z-index! elt s))])
	   (sequence (super-init))))

  (define mx-event%
    (class object% (dhtml-event)

	   (private
	    [event dhtml-event])

	   (public

            ; predicates

	    [keypress? (lambda () (mxprims:event-keypress? event))]
	    [keydown? (lambda () (mxprims:event-keydown? event))]
	    [keyup? (lambda () (mxprims:event-keyup? event))] 
	    [mousedown? (lambda () (mxprims:event-mousedown? event))] 
	    [mousemove? (lambda () (mxprims:event-mousemove? event))] 
	    [mouseover? (lambda () (mxprims:event-mouseover? event))] 
	    [mouseout? (lambda () (mxprims:event-mouseout? event))] 
	    [mouseup? (lambda () (mxprims:event-mouseup? event))] 
	    [click? (lambda () (mxprims:event-click? event))] 
	    [dblclick? (lambda () (mxprims:event-dblclick? event))] 
	    [error? (lambda () (mxprims:event-error? event))]
	    
            ; attributes

	    [tag (lambda () (mxprims:event-tag event))]
	    [id (lambda () (mxprims:event-id event))]
	    [from-tag (lambda () (mxprims:event-from-tag event))]
	    [from-id (lambda () (mxprims:event-id event))]
	    [to-tag (lambda () (mxprims:event-to-tag event))]
	    [to-id (lambda () (mxprims:event-to-id event))]
	    [keycode (lambda () (mxprims:event-keycode event))]
	    [shiftkey (lambda () (mxprims:event-shiftkey event))]
	    [ctrlkey (lambda () (mxprims:event-ctrlkey event))]
	    [altkey (lambda () (mxprims:event-altkey event))]
	    [x (lambda () (mxprims:event-x event))]
	    [y (lambda () (mxprims:event-y event))])

	   (sequence (super-init))))


  (define mx-document%
    (class object%

	   ((label "MysterX")
	    (width 'default)
	    (height 'default)
	    (x 'default)
	    (y 'default)
	    (style-options null))

	   (private
	    [doc (mxprims:make-document label width height x y style-options)]
	    [thread-sem (make-semaphore 1)]   ; protects *handler-threads*
	    [thread-wait (lambda () (semaphore-wait thread-sem))]
	    [thread-post (lambda () (semaphore-post thread-sem))]
	    [handler-sem (make-semaphore 1)]  ; protects *handler-table* and its contained hash tables
	    [handler-wait (lambda () (semaphore-wait handler-sem))]
	    [handler-post (lambda () (semaphore-post handler-sem))]
	    [handler-table (make-hash-table)]
	    [handler-thread #f]
	    [block-until-event 
	     (lambda () (mxprims:block-until-event doc))]
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
	       (mxprims:document-show doc b))]
	    [find-element
	     (lambda (tag id)
	       (make-object mx-element% doc (mxprims:document-find-element doc tag id)))]
	    [objects
	     (lambda () 
	       (mxprims:document-objects doc))]
	    [insert-html 
	     (lambda (html-string)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:document-insert-html doc html-string))
		html-post))]
	    [append-html 
	     (lambda (html-string)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:document-append-html doc html-string))
		html-post))]
	    [replace-html 
	     (lambda (html-string)
	       (dynamic-wind
		html-wait
		(lambda () (mxprims:document-replace-html doc html-string))
		html-post))]
	    [register-event-handler
	     (lambda (elt fn)
	       (dynamic-wind
		handler-wait
		(lambda () 
		  (let* ([tag (send elt tag)]
			 [id (send elt attribute "id")]) 
		    (let ([key (make-event-key tag id)])
		      (hash-table-remove! handler-table key)
			 (hash-table-put! handler-table key fn))))
		handler-post))]
	    [unregister-event-handler
	     (lambda (elt)
	       (dynamic-wind
		handler-wait
		(lambda () 
		  (let* ([tag (send elt tag)]
			 [id (send elt attribute "id")])
		    (let ([key (make-event-key tag id)])
		      (hash-table-remove! handler-table key))))
		handler-post))]
	    [insert-object 
	     (opt-lambda (object width height [size 'pixels])
	       (dynamic-wind 
		html-wait
		(lambda ()
		  (mxprims:document-insert-html 
		   doc 
		   (coclass->html object width height size))
		  (car (mxprims:document-objects doc)))
		html-post))]
	    [append-object 
	     (opt-lambda (object width height [size 'pixels])
	       (dynamic-wind
		html-wait
		(lambda ()
		  (mxprims:document-append-html 
		   doc 
		   (coclass->html object width height size))
		  (car (mzlib:last-pair (mxprims:document-objects doc))))
		html-post))]
	    [handle-events 
	     (lambda ()
	       (dynamic-wind
		thread-wait 
                (lambda ()	; no-op if existing handler-thread
		  (unless handler-thread
			  (dynamic-wind
			   handler-wait
			   (lambda ()
			     (let* ([handler-thunk
				     (lambda ()
				       (let loop ()
					 (block-until-event)
				         (let* ([prim-event
						 (with-handlers
						  ([void 
						    (lambda (e) 
						      (printf "~a~n" (exn-message e))
						      (loop))])
						    (mxprims:get-event doc))]
						[event (make-object mx-event% prim-event)]
						[tag (send event tag)]
						[id (send event id)]
						[key (make-event-key tag id)]
						[handler (hash-table-get handler-table key void)])
					   (unless (void? handler)
						   (handler event))
					   (loop))))])
			       (set! handler-thread (thread handler-thunk))))
			   handler-post)))
		thread-post))]
	    [stop-handling-events 
	     (lambda ()
	       (dynamic-wind
		thread-wait
		(lambda () 
		  (when handler-thread
			(kill-thread handler-thread))
		  (set! handler-thread #f))
		thread-post))])

	   (sequence 
	     (super-init))))
	     
  (thread	
   (lambda () 
     (let loop ()
       (mxprims:process-win-events)
	  (sleep)
	  (loop))))

  (let ([old-exit-handler (exit-handler)])
    (exit-handler 
     (lambda (arg)
       (for-each 
	(lambda (obj) 
	  (let ([val (cdr obj)])
	    (cond
	     [(com-object? val)
	      (mxprims:com-release-object (cdr obj))]
	     
             ; rely on GC to release interfaces in documents, elements
	     ; not entirely reliable, since collector is conservative
	     
	     [(or (is-a? val mx-document%)
		  (is-a? val mx-element%))
	      (undefine (car obj))])))
	(make-global-value-list))
       (collect-garbage)
       (mxprims:release-type-table)
       (mxprims:com-terminate)
       (old-exit-handler arg)))))


