; Dan Grossman
; Hyper Edit 
; July 17, 1995

(define mred:hyper-edit@
  (unit/sig mred:hyper-edit^
    (import [mred:debug : mred:debug^]
	    [mred:edit : mred:edit^]
	    [mred:hyper-dialog : mred:hyper-dialog^]
	    [mzlib:file : mzlib:file^]
	    [mzlib:string : mzlib:string^])
	    
    (mred:debug:printf 'invoke "mred:hyper-edit@")

    ; for cut and paste:
    (define hyper-buffer-data%
      (class-asi wx:buffer-data%
		 (private
		  links-list
		  tags-list)
		 (public
		  [get-links-list (lambda () links-list)]
		  [set-links-list  (lambda (l) (set! links-list l))]
		  [get-tags-list (lambda () tags-list)]
		  [set-tags-list  (lambda (l) (set! tags-list l))])))
    
    (define hyper-data-class 
      (make-object
	  (class wx:buffer-data-class% ()
		 (inherit set-classname)
		 (sequence
		   (super-init)
		   (set-classname  "hyper-data-class")
		   (send wx:the-buffer-data-class-list add this)))))

    (define-struct hyperlink (anchor-start 
			      anchor-end
			      reference-file 
			      reference-tag))
    
    (define-struct hypertag (name position))

    (define make-hyper-edit%
      (lambda (super%)
	(class super% ([keep-locked? #t] . args)
	  (inherit get-start-position get-end-position last-position set-position  
		   begin-edit-sequence end-edit-sequence lock
		   change-style  get-style-list
		   find-snip get-snip-position line-start-position
		   get-visible-line-range scroll-to-position position-line
		   begin-write-header-footer-to-file end-write-header-footer-to-file
		   write-to-file set-filename
		   set-clickback  remove-clickback set-file-format
		   get-frame insert-image)
	  (rename [super-get-filename get-filename]
		  [super-do-edit do-edit]
		  [super-save-file save-file]
		  [super-get-region-data get-region-data]
		  [super-set-region-data set-region-data]
		  [super-load-file load-file]
		  [super-read-footer-from-file read-footer-from-file]
		  [super-write-footers-to-file write-footers-to-file])
	  (public  
	    [auto-set-wrap? #t]
	    [hypertags-list (list (make-hypertag "top" 0))]
	    [hyperlinks-list ()]
	    [follow-on-click? #t] ;; when false, someone designing hyper-text
	    ;; can see a link rather than jumping there.
	    ;; when (), links are uninstalled.
	    [set-follow-on-click 
	     (lambda (val)
	       (set! follow-on-click? val))]
	    [directory (current-directory)] ;; changes when file is saved.
	    ;; to be called by on-insert and on-delete (which then return #t):     
	    [adjust-lists    
	     (lambda (place amt)
	       (set! hypertags-list
		     (let tags-loop ([tags-left hypertags-list])
		       (if (null? tags-left)
			   ()
			   (let ([next-tag (hypertag-position (car tags-left))])
			     (cond 
			       [(<= next-tag place) tags-left]
			       [(< next-tag (- place amt))
				(tags-loop (cdr tags-left))] 
			       [else 
				(set-hypertag-position! 
				 (car tags-left) (+ next-tag amt))
				(cons (car tags-left)(tags-loop(cdr tags-left)))])))))
	       (set! hyperlinks-list
		     (let links-loop ([links-left hyperlinks-list])
		       (if (null? links-left)
			   ()
			   (let ([next-start 
				  (hyperlink-anchor-start (car links-left))]
				 [next-end 
				  (hyperlink-anchor-end (car links-left))])
			     (cond 
			       [(<= next-end place) links-left]
			       [(< next-start place)
				(set-hyperlink-anchor-end!
				 (car links-left) (if (<= next-end (- place amt))
						      place
						      (+ next-end amt)))
				links-left]
			       [(<= next-end (- place amt))
				(links-loop (cdr links-left))]
			       [else
				(set-hyperlink-anchor-start!
				 (car links-left) (if (<= next-start (- place amt))
						      place
						      (+ next-start amt)))
				(set-hyperlink-anchor-end!
				 (car links-left) (+ next-end amt))
				(cons (car links-left)(links-loop (cdr links-left)))]
			       ))))))]
	    [map-shift-style 
	     (lambda (start end shift-style)
	       (let loop ([pos start])
		 (unless (>= pos end)
		   (let* ([curr-snip (find-snip pos wx:const-snip-after-or-null)]
			  [curr-snip-end (unless (null? curr-snip)
					   (+ (get-snip-position curr-snip)
					      (send curr-snip get-count)))])
		     (unless (null? curr-snip)
		       (change-style 
			(send (get-style-list) find-or-create-join-style 
			      (send curr-snip get-style) shift-style)
			pos (min curr-snip-end end))
		       (loop curr-snip-end))))))]
	    [remove-h-link-style 
	     (lambda (start end)
	       (let* ([style-list (get-style-list)]
		      [h-style (send style-list find-named-style "h-link-style")]
		      [h-index (send style-list style-to-index h-style)]
		      [root (send style-list basic-style)]
		      [root-index (send style-list style-to-index root)])
		 (let snip-loop ([pos start])
		   (unless (>= pos end)
		     (let* ([curr-snip (find-snip pos wx:const-snip-after-or-null)]
			    [curr-style (unless (null? curr-snip)
					  (send curr-snip get-style))]
			    [curr-snip-end (unless (null? curr-snip)
					     (+ (get-snip-position curr-snip)
						(send curr-snip get-count)))])
		       (unless (null? curr-snip)
			 (let([new-style-stack
			       (let down-loop ([stack ()][style curr-style])
				 (if(= (send style-list style-to-index style) root-index)
				    #f
				    (if (send style is-join?)
					(let ([shift (send style get-shift-style)]
					      [base (send style get-base-style)])
					  (if (= (send style-list style-to-index shift)
						 h-index)
					      (cons base stack) 
					      (or 
					       (down-loop (cons (cons #t shift) stack)
							  base)
					       (down-loop (cons (cons #f base) stack) 
							  shift))))
					(down-loop
					 (let ([delta (make-object wx:style-delta%)])
					   (send style get-delta delta)
					   (cons delta stack)) 
					 (send style get-base-style)))))])
			   (unless (not new-style-stack)
			     (let ([real-end (min curr-snip-end end)])
			       (change-style  (car new-style-stack) 
					      pos real-end)
			       (let up-loop ([stack (cdr new-style-stack)])
				 (cond
				   [(null? stack) #t]
				   [(pair? (car stack))
				    (if (caar stack)
					(begin
					  (change-style 
					   (send (get-style-list) 
						 find-or-create-join-style 
						 (send (find-snip 
							pos wx:const-snip-after)
						       get-style)
						 (cdar stack))
					   pos real-end)
					  (up-loop (cdr stack)))
					(begin
					  (change-style 
					   (send (get-style-list)
						 find-or-create-join-style 
						 (cdar stack)
						 (send (find-snip
							pos wx:const-snip-after)
						       get-style))
					   pos real-end)
					  (up-loop (cdr stack))))]
				   [else
				    (change-style 
				     (car stack) pos real-end)
				    (up-loop (cdr stack))]))))
			   (snip-loop curr-snip-end))))))))]
	    [make-link-style       
	     (lambda (start end)
	       (map-shift-style  
		start end (send (get-style-list) find-named-style "h-link-style")))]
	    [write-footers-to-file
	     (lambda (stream)
	       (super-write-footers-to-file stream)
	       (let ([buffer (box 0)])
		 (begin-write-header-footer-to-file stream "wx-hypertags" buffer)
		 (send stream put (length hypertags-list))
		 (let loop ([tags-left hypertags-list])
		   (unless (null? tags-left)
		     (let ([next (car tags-left)])
		       (send* stream 
			      (put (string-length (hypertag-name next)))
			      (put (string-length (hypertag-name next))
				   (hypertag-name next))
			      (put (hypertag-position next))))
		     (loop (cdr tags-left))))
		 (end-write-header-footer-to-file stream (unbox buffer)))
	       (let ([buffer (box 0)])
		 (begin-write-header-footer-to-file stream "wx-hyperlinks" buffer)
		 (send stream put (length hyperlinks-list))
		 (let loop ([links-left hyperlinks-list])
		   (unless (null? links-left)
		     (let ([next (car links-left)])
		       (send stream put (hyperlink-anchor-start next))
		       (send stream put (hyperlink-anchor-end next))
		       (send stream put 
			     (if (string? (hyperlink-reference-file next))
				 (string-length(hyperlink-reference-file next))
				 0))
		       (send stream put 
			     (string-length(hyperlink-reference-tag next)))
		       (if (string? (hyperlink-reference-file next))
			   (send stream put 
				 (string-length (hyperlink-reference-file next))
				 (hyperlink-reference-file next))
			   (send stream put 0 ""))
		       (send stream put 
			     (string-length (hyperlink-reference-tag next))
			     (hyperlink-reference-tag next)))
		     (loop (cdr links-left))))
		 (end-write-header-footer-to-file stream (unbox buffer)))
	       #t)]
	    [read-footer-from-file  
	     (lambda (stream name)
	       (cond 
		 [(string=? name "wx-hypertags")
		  (let ([num-tagsb (box 0)]
			[name-lengthb (box 0)]
			[positionb (box 0)])
		    (send stream get num-tagsb)
		    (set! 
		     hypertags-list
		     (let loop ([num-tags-left (unbox num-tagsb)])
		       (if (zero? num-tags-left)
			   ()
			   (cons (begin
				   (send stream get name-lengthb)
				   (let ([name-str (send stream get-string)])
				     (send stream get positionb)
				     (make-hypertag name-str (unbox positionb))))
				 (loop (sub1 num-tags-left)))))))]
		 [(string=? name "wx-hyperlinks")
		  (let ([num-linksb (box 0)]
			[anchor-startb (box 0)][anchor-endb (box 0)]
			[file-lengthb (box 0)][tag-lengthb (box 0)])
		    (send stream get num-linksb)
		    (set! 
		     hyperlinks-list
		     (let loop ([num-links-left (unbox num-linksb)])
		       (if (zero? num-links-left)
			   ()
			   (cons (begin
				   (send* stream 
					  (get anchor-startb)
					  (get anchor-endb)
					  (get file-lengthb)
					  (get tag-lengthb))
				   (let ([file-str (send stream get-string)]
					 [tag-str (send stream get-string)])
				     (make-hyperlink
				      (unbox anchor-startb)(unbox anchor-endb)
				      (if (> (string-length file-str) 0) file-str #f)
				      tag-str)))
				 (loop (sub1 num-links-left)))))))]
		 [else (super-read-footer-from-file stream name)]))]
	    [make-clickback-funct
	     (lambda (filename tag)
	       (lambda (edit start end)
		 (catch-errors 
		  (lambda (err-msg)
		    (show-message "Unable to find destination position."
				  "Error"))
		  (lambda () #f)
		  (if follow-on-click?
		      (let ([filename  
			     (if (not filename) 
				 (get-filename)
				 (mzlib:file:normalize-path filename directory))])
			(send (get-frame) open-file filename tag))
		      (show-message 
		       (string-append "Filename: " (if filename
						       (mzlib:string:expr->string filename) 
						       "this file. ")
				      " Tag: " (mzlib:string:expr->string tag))
		       "Link")))))]
	    
	    [install-clickbacks 
	     (lambda ()
	       (let install-loop ([links-left hyperlinks-list])
		 (unless (null? links-left)
		   (set-clickback  (hyperlink-anchor-start (car links-left))
				   (hyperlink-anchor-end (car links-left))
				   (make-clickback-funct
				    (hyperlink-reference-file (car links-left))
				    (hyperlink-reference-tag (car links-left))))
		   (install-loop (cdr links-left)))))]
	    [uninstall-clickbacks 
	     (lambda ()
	       (let uninstall-loop ([links-left hyperlinks-list])
		 (unless (null? links-left)
		   (remove-clickback  (hyperlink-anchor-start (car links-left))
				      (hyperlink-anchor-end (car links-left)))
		   (uninstall-loop (cdr links-left)))))]
	    [show-message
	     (lambda (str title)
	       (wx:message-box str title wx:const-ok))])
	  
	  (public
	    [keep-locked keep-locked?]
	    [hyper-delta (make-object wx:style-delta% 
				      wx:const-change-underline 1)])
	  (sequence
	    (let ([mult (send hyper-delta get-foreground-mult)]
		  [add (send hyper-delta get-foreground-add)])
	      (send mult set 0 0 0)
	      (send add set 0 0 255)))
	  
	  (public
	    [add-h-link-style
	     (lambda ()
	       (let ([style-list (get-style-list)])
		 (send style-list replace-named-style  "h-link-style"
		       (send style-list find-or-create-style  
			     (send style-list find-named-style "standard") 
			     hyper-delta))))]
	    
	    [add-tag 
	     (lambda (name pos)
	       (if (let name-check ([tags-left hypertags-list])
		     (cond [(null? tags-left) #t]
			   [(string=? name (hypertag-name (car tags-left))) 
			    (if (= wx:const-yes
				   (wx:message-box
				    (string-append
				     "There is an existing tag with the name \""
				     name "\". Replace it?")
				    "Error" wx:const-yes-no))
				(begin (remove-tag  name) #t)
				#f)]
			   [else (name-check (cdr tags-left))]))
		   (let ([new-tag (make-hypertag name pos)])
		     (set! hypertags-list
			   (let insert-loop ([tags-left hypertags-list])
			     (cond [(null? tags-left)(cons new-tag ())]
				   [(> pos (hypertag-position (car tags-left)))
				    (cons new-tag tags-left)]
				   [else (cons (car tags-left)
					       (insert-loop (cdr tags-left)))]))))))]
	    [remove-tag 
	     (lambda (name)
	       (set! hypertags-list
		     (let remove-loop ([tags-left hypertags-list])
		       (cond 
			 [(null? tags-left) (show-message "Tag not found.""")()]
			 [(string=? name (hypertag-name (car tags-left)))
			  (cdr tags-left)]
			 [else (cons (car tags-left) 
				     (remove-loop (cdr tags-left)))]))))]
	    [show-tag
	     (lambda (name)
	       (if (not name)
		   (set-position 0)
		   (unless (null? name)
		     (let tags-loop ([tags-left hypertags-list])
		       (cond [(null? tags-left)(show-message "Tag not found.""")]
			     [(string=? name (hypertag-name (car tags-left)))
			      (set-position  (hypertag-position (car tags-left)))]
			     [else (tags-loop (cdr tags-left))])))))]
	    [add-link 
	     (lambda (start end filename tag)
	       (let* ([new-link (make-hyperlink start end filename tag)]
		      [set-clickback  (if (not (null? follow-on-click?))
					  set-clickback 
					  (lambda args #t))]
		      [new-links-list
		       (let insert-loop ([links-left hyperlinks-list])
			 (if (null? links-left)
			     (begin(set-clickback  start end 
						   (make-clickback-funct filename tag)) 
				   (cons new-link ()))
			     (cond
			       [(<= end (hyperlink-anchor-start(car links-left)))
				(cons (car links-left)(insert-loop (cdr links-left)))]
			       [(>= start (hyperlink-anchor-end (car links-left)))
				(set-clickback  start end 
						(make-clickback-funct filename tag)) 
				(cons new-link links-left)]
			       [else (show-message 
				      "A new link cannot overlap with a current link"
				      "Error") 
				     #f])))])
		 (if new-links-list
		     (set! hyperlinks-list new-links-list))
		 new-links-list))]
	    
	    [remove-link    ;; walking links-list twice but it should be short?!
	     (lambda (pos)
	       (let ([the-link
		      (let find-loop ([links-left hyperlinks-list])
			(cond [(or (null? links-left)
				   (< (hyperlink-anchor-end (car links-left))
				      pos))
			       (show-message 
				"Selection must be completely inside a current link."
				"Error")
			       #f]
			      [(<= (hyperlink-anchor-start (car links-left))
				   pos)
			       (car links-left)]
			      [else (find-loop (cdr links-left))]))])
		 (when the-link
		   (let ([start (hyperlink-anchor-start the-link)]
			 [end (hyperlink-anchor-end the-link)])
		     (set! hyperlinks-list
			   (let remove-loop ([links-left hyperlinks-list])
			     (if (= start
				    (hyperlink-anchor-start (car links-left)))
				 (cdr links-left)
				 (cons (car links-left)
				       (remove-loop (cdr links-left))))))
		     (remove-clickback  start end)
		     (remove-h-link-style  start end)))
		 the-link))]
	    [get-link-file
	     (lambda ()
	       (let* ([result 'cancel])
		 (make-object
		  (make-class wx:dialog-box%
			      (inherit show new-line fit)
			      (private
				[NAME-WIDTH 250]
				where-toggle
				filename
				[on-ok
				 (lambda args
				   (if (zero? (send where-toggle get-selection))
				       (set! result #f)
				       (set! result (send filename get-value)))
				   (show #f))]
				[on-cancel
				 (lambda args
				   (set! result 'cancel)
				   (show #f))])
			      (lambda ()
				(super-init () "Link to File" #t)
				(set! where-toggle
				      (make-object wx:radio-box% this
						   (lambda args #f)
						   ""
						   -1 -1 -1 -1
						   (list "Local Link"
							 "External link to file (relative path):")))
				(new-line)
				(set! filename
				      (make-object wx:text% this
						   (lambda (self event)
						     (if (= wx:const-event-type-text-enter-command
							    (send event get-event-type))
							 (on-ok)
							 (send where-toggle set-selection 1)))
						   ""
						   ""
						   50 -1
						   NAME-WIDTH))
				(new-line)
				(make-object wx:button% this
					     on-ok
					     "Ok")
				(make-object wx:button% this
					     on-cancel
					     "Cancel")
				(new-line)
				(fit)
				(show #t))))
		 result))]
	    [change-link 
	     (lambda (pos)
	       (let ([old (remove-link  pos)])
		 (if old
		     (make-link  (hyperlink-anchor-start old)
				 (hyperlink-anchor-end old)))))]
	    [make-link 
	     (lambda (start end)
	       (if (= start end)
		   (wx:message-box "Link selection must span at least one character."
				   "Error")
		   (call/cc 
		    (lambda (break)
		      (let ([filename (get-link-file)])
			(if (add-link  
			     start end
			     (if (or (not filename) (string? filename))
				 filename 
				 (break #f))
			     (let ([tag-name 
				    (send (make-object 
					   mred:hyper-dialog:hyper-tag-dialog% 
					   (if (not filename)
					       (reverse
						(map hypertag-name hypertags-list))
					       (mred:hyper-dialog:hyper-get-current-tags 
						directory filename)))
					  get-answer)])
			       (if tag-name tag-name (break #f))))
			    (make-link-style  start end)))))))]
	    [make-tag 
	     (lambda (pos)
	       (call/cc
		(lambda (break)
		  (add-tag  (let ([name (wx:get-text-from-user
					 "Enter a name for this tag." "Tag Name")])
			      (if (null? name)
				  (break #f)
				  name))
			    pos))))]
	    [get-filename
	     (opt-lambda ([tmp? null])
	       (let ([fn (super-get-filename tmp?)])
		 (if (string? fn)
		     (mzlib:file:normalize-path fn)
		     ())))]
	    [update-directory
	     (opt-lambda ([new-dir 
			   (if (string? (get-filename))
			       (mzlib:file:path-only (mzlib:file:normalize-path (get-filename)))
			       directory)])
	       (set! directory new-dir))]
	    [do-edit
	     (lambda (op)
	       (if (= op wx:const-edit-insert-image)
		   (let ([filename
			  (wx:get-text-from-user
			   "Enter the relative path name for the image file"
			   "File Name")])
		     (if (string? filename)
			 (insert-image filename -1 #t)))
		   (super-do-edit op)))]
	    [load-file 
	     (opt-lambda ([filename ()][format wx:const-media-ff-guess][relative? #f])
	       (set! hyperlinks-list ())
	       (set! hypertags-list ())
	       (lock #f)
	       (let* ([filename 
		       (if (or (null? filename)
			       (not relative?))
			   filename
			   (build-path directory filename))])
		 (if (super-load-file filename format)
		     (begin (if keep-locked (lock #t))
			    (update-directory)
			    (install-clickbacks )
			    (add-h-link-style)
			    (set-file-format wx:const-media-ff-std)
			    #t)
		     (begin (set-filename filename)
			    (set! hypertags-list 
				  (list (make-hypertag "top" 0)))
			    #f))))]
	    [save-file
	     (opt-lambda ([filename ()][format wx:const-media-ff-same])
	       (super-save-file filename format)
	       (update-directory))]
	    [get-region-data
	     (lambda (start end)
	       (let ([data-obj (make-object hyper-buffer-data%)])
		 (send data-obj set-dataclass  hyper-data-class)
		 (send data-obj set-next  (super-get-region-data start end))
		 (send data-obj set-links-list 
		       (let links-loop ([links-left hyperlinks-list])
			 (if (null? links-left)
			     ()
			     (let* ([next-link (car links-left)]
				    [next-start 
				     (hyperlink-anchor-start next-link)]
				    [next-end 
				     (hyperlink-anchor-end next-link)])
			       (cond
				 [(<= next-end start) ()]
				 [(>= next-start end)(links-loop (cdr links-left))]
				 [else
				  (let ([new-start (max start next-start)]
					[new-end (min end next-end)])
				    (cons (make-hyperlink 
					   (- new-start start)(- new-end start)
					   (hyperlink-reference-file next-link)
					   (hyperlink-reference-tag next-link))
					  (links-loop (cdr links-left))))])))))
		 (send data-obj set-tags-list 
		       (let tags-loop ([tags-left hypertags-list])
			 (if (null? tags-left)
			     ()
			     (let* ([next-tag (car tags-left)]
				    [next-pos (hypertag-position next-tag)])
			       (cond 
				 [(< next-pos start) ()]
				 [(or (> next-pos end) 
				      (and (= next-pos end)
					   (not (= end (send this last-position)))))
				  (tags-loop (cdr tags-left))]
				 [else
				  (cons (make-hypertag 
					 (hypertag-name next-tag)
					 (- next-pos start))
					(tags-loop (cdr tags-left)))])))))
		 data-obj))]
	    [set-region-data 
	     (lambda (start end data)
	       (unless (null? data)
		 (if (eq? (send data get-dataclass) hyper-data-class) 
		     (begin
		       (let links-loop ([links-left (send data get-links-list)])
			 (unless (null? links-left)
			   (let ([next-link (car links-left)])
			     (add-link  
			      (+ start (hyperlink-anchor-start next-link))
			      (+ start (hyperlink-anchor-end next-link))
			      (hyperlink-reference-file next-link)
			      (hyperlink-reference-tag next-link))
			     (links-loop (cdr links-left)))))
		       (let tags-loop ([tags-left (send data get-tags-list)])
			 (unless (null? tags-left)
			   (let ([next-tag (car tags-left)])
			     (add-tag  (hypertag-name next-tag)
				       (+ start (hypertag-position next-tag)))
			     (tags-loop (cdr tags-left)))))
		       (send this set-region-data  start end (send data get-next)))
		     (super-set-region-data  start end data))))]
	    [on-insert
	     (lambda (start size)
	       (adjust-lists  start size)
	       (begin-edit-sequence)
	       #t)]
	    [after-insert
	     (lambda (start size)
	       (let links-loop ([links-left hyperlinks-list])
		 (if (not (null? links-left))
		     (let ([curr-end (hyperlink-anchor-end (car links-left))])
		       (cond
			 [(> curr-end start)(links-loop (cdr links-left))]
			 [(< curr-end start) #t]
			 [else (let*([prev-snip(find-snip(hyperlink-anchor-start
							  (car links-left))
							 wx:const-snip-before-or-null)]
				     [correct-style 
				      (if (null? prev-snip)
					  (send (get-style-list) find-named-style
						"standard")
					  (send prev-snip get-style))])
				 (change-style  correct-style start (+ start size)))]
			 ))))
	       (end-edit-sequence))]
	    [on-delete
	     (lambda (start size)
	       (adjust-lists  start (- 0 size))
	       #t)])
	  (sequence
	    (apply super-init args)
	    (add-h-link-style)))))

    (define hyper-edit% (make-hyper-edit% mred:edit:edit%))))
