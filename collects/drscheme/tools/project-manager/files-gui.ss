(begin-elaboration-time
 (require-library "files.ss")
 (require-library "frameworks.ss" "framework")
 (require-library "pconvers.ss"))

(unit/sig ()
  (import (top-horizontal-panel
	   show/hide-rep
           is-changed my-explode-path
           this
           show-menu
           project-dir project-aware-frame<%>)
	  mred^
          framework^
	  [file : mzlib:file^]
          mzlib:print-convert^
          [pretty-print : mzlib:pretty-print^])
  
    (define (anything-shown-in-top-half?)
      '(or elaboration-files-shown? to-load-files-shown? loaded-files-shown?))

    (define (anything-shown?)
      '(or elaboration-files-shown?
	   to-load-files-shown?
	   loaded-files-shown?
	   rep-shown?))

  (define (make-gui files set-files label-str)
    (define files-menu-item
      (make-object menu-item% "Show Project Files" show-menu 
		   (lambda xxx (show/hide-files))))
    
    (define files-outer-panel (make-object vertical-panel% top-horizontal-panel))
    
    (define files-panel (make-object horizontal-panel% files-outer-panel
					     '(border)))
    (define vp (make-object vertical-panel% files-panel))
    (define message (make-object message% label-str vp))
    (define files-list-box (make-object list-box% #f null vp
					(lambda (lb evt)
					  (files-list-box-callback
					   (send evt get-event-type)))
					'(single)))

    (define button-panel (make-object vertical-panel% files-panel))
    (define up-button (make-object button% "Up" button-panel
				   (lambda x (move-file-up))))
    (define down-button (make-object button% "Down" button-panel
				     (lambda x (move-file-down))))
    
    (define spacer (make-object horizontal-panel% button-panel))

    (define open-button (make-object button% "Open" button-panel
				     (lambda x (open-file))
				     '(border)))
    (define remove-button (make-object button% "Remove" button-panel
				       (lambda x (remove-file))))
    (define pathize-button (make-object button% "Make Abs" button-panel
					(lambda x (swap-abs/rel-file))))
    
    (define (get-file-list-box-string file)
      (let ([sp (open-output-string)])
	(parameterize ([current-output-port sp]
		       [pretty-print:pretty-print-columns 'infinity])
	  (pretty-print:pretty-print (cons (car file) (map print-convert (cdr file)))))
	(get-output-string sp)))
    
    (define (refresh-files-list-box)
      (send files-list-box clear)
      (for-each
       (lambda (file)
	 (send files-list-box append (get-file-list-box-string file)))
       files))
    
    
    (define (swap index)
      (is-changed)
      (let loop ([n index]
		 [files files]
		 [previous-pair #f])
	(cond
	 [(zero? n)
	  (let ([tmp (car previous-pair)])
	    (set-car! previous-pair (car files))
	    (set-car! files tmp))
	  (let ([tmp (send files-list-box get-string index)])
	    (send files-list-box set-string index (send files-list-box get-string (- index 1)))
	    (send files-list-box set-string (- index 1) tmp))]
	 [else
	  (loop (- n 1)
		(cdr files)
		files)])))

    (define (move-file-down)
      (let ([selection (car (send files-list-box get-selections))])
	(swap (+ selection 1))
	(send files-list-box select (+ selection 1))
	(update-buttons)))

    (define (move-file-up)
      (let ([selection (car (send files-list-box get-selections))])
	(swap selection)
	(send files-list-box select (- selection 1))
	(update-buttons)))

    (define (open-file)
      (let* ([file (list-ref files (car (send files-list-box get-selections)))]
	     [filename (case (car file)
			 [(build-path)
			  (let ([p (apply build-path (cdr file))])
			    (if (relative-path? p)
				(file:normalize-path (build-path project-dir p))
				p))]
			 [(require-library)
			  (build-path (apply collection-path (cddr file))
				      (cadr file))])]
	     [frame (handler:edit-file filename)])
	(when (is-a? frame project-aware-frame<%>)
	  (send frame project:set-project-window this))))
    
    (define (remove-file)
      (let* ([index (car (send files-list-box get-selections))])
	(set-files
	 (let loop ([n index]
		    [files files])
	   (cond
	    [(null? files) null]
	    [(zero? n) (cdr files)]
	    [else (cons (car files) (loop (- n 1) (cdr files)))])))
	(send files-list-box delete index)
	(let ([max (send files-list-box get-number)])
	  (cond
	   [(= 0 max) (void)]
	   [(<= 0 index (- max 1))
	    (send files-list-box select index)]
	   [else
	    (send files-list-box select (- max 1))]))
	(update-buttons)
	(is-changed)))
    
    (define (swap-abs/rel-file)
      (let* ([index (send files-list-box get-selection)]
	     [file (list-ref files index)]
	     [path (apply build-path (cdr file))]
	     [new-path
	      (cond
	       [(relative-path? path)
		(file:normalize-path (build-path project-dir path))]
	       [(absolute-path? path)
		(if project-dir
		    (file:find-relative-path project-dir path)
		    (begin (bell)
			   path))])])
	(set-cdr! file (my-explode-path new-path))
	(send files-list-box set-string index (get-file-list-box-string file))
	(send files-list-box set-selection index)
	(is-changed)
	(update-buttons)))

    (define (update-buttons)
      (let ([selection-list (send files-list-box get-selections)])
	(if (null? selection-list)
	    (begin
	      (send pathize-button set-label "Make ...")
	      (send pathize-button enable #f)
	      (send down-button enable #f)
	      (send up-button enable #f)
	      (send remove-button enable #f)
	      (send open-button enable #f))
	    (let ([selection (car selection-list)])
	      (send open-button enable #t)
	      (send remove-button enable #t)

	      (let ([file (list-ref files (car selection-list))])
		(case (car file)
		  [(build-path)
		   (send pathize-button enable #t)
		   (if (absolute-path? (apply build-path (cdr file)))
		       (send pathize-button set-label "Make Rel")
		       (send pathize-button set-label "Make Abs"))]
		  [(require-library)
		   (send pathize-button enable #f)]))

	      (cond
	       [(= 1 (send files-list-box get-number))
		(send down-button enable #f)
		(send up-button enable #f)]
	       [(= 0 selection)
		(send down-button enable #t)
		(send up-button enable #f)]
	       [(= selection (- (send files-list-box get-number) 1))
		(send down-button enable #f)
		(send up-button enable #t)]
	       [else
		(send down-button enable #t)
		(send up-button enable #t)])))))

    (define (files-list-box-callback selection)
      (case selection
	[(list-box-dclick) (open-file)]
	[(list-box)
	 (update-buttons)]))

    (define files-shown? #t)
    (define (update-files-shown)
      (send files-menu-item set-label
	    (if files-shown?
		"Hide Project Files"
		"Show Project Files"))
      (send files-outer-panel change-children
	    (lambda (l)
	      (if files-shown?
		  (list files-panel)
		  null)))
      (send files-outer-panel stretchable-height files-shown?)
      (send files-outer-panel stretchable-width files-shown?)
      (send top-horizontal-panel stretchable-width (anything-shown-in-top-half?))
      (send top-horizontal-panel stretchable-height (anything-shown-in-top-half?)))

    (define (show/hide-files)
      (set! files-shown? (not files-shown?))
      (is-changed)
      (update-files-shown)
      (unless (anything-shown?)
	(show/hide-rep)))

    (send spacer stretchable-height #f)
    (send spacer min-height 16)
    (send button-panel stretchable-width #f)
    (send button-panel set-alignment 'center 'center)))