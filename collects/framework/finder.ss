;;; finder.ss

;;; Authors: Matthew Flatt, Robby Findler, Paul Steckler 

(unit/sig framework:finder^
  (import mred^
	  [preferences : framework:preferences^]
	  [gui-utils : framework:gui-utils^]
	  [keymap : framework:keymap^]
	  [mzlib:string : mzlib:string^]
	  [mzlib:function : mzlib:function^]
	  [mzlib:file : mzlib:file^])

  (rename [-put-file put-file]
	  [-get-file get-file])
  
  (define dialog-parent-parameter (make-parameter #f))

  (define filter-match?
    (lambda (filter name msg)
      (let-values ([(base name dir?) (split-path name)])
	(if (mzlib:string:regexp-match-exact? filter name)
	    #t
	    (begin
	      (message-box "Error" msg)
	      #f)))))
  
  (define last-directory #f)
  
  (define make-relative
    (lambda (s) s))
  
  (define current-find-file-directory
    (opt-lambda ([dir 'get])
      (cond
	[(eq? dir 'get)
	 (if (not last-directory)
	     (set! last-directory (current-directory)))
	 last-directory]
	[(and (string? dir)
	      (directory-exists? dir))
	 (set! last-directory dir)
	 #t]
	[else #f])))
  
  (define build-updir
    (lambda (dir)
      (let-values ([(base _1 _2) (split-path dir)])
	(or base dir))))

  (define default-extension
    (let ([val #f])
      (case-lambda
       [() val]
       [(x) 
	(unless (or (string? x)
		    (not x))
	  (error 'finder:default-extension
		 "expected a string or #f, got: ~e"
		 x))
	(set! val x)])))
  
  ; the finder-dialog% class controls the user interface for dialogs
  
  (define finder-dialog%
    (class dialog% (parent-win
			save-mode? 
			replace-ok? 
			multi-mode? 
			result-box 
			start-dir 
			start-name 
			prompt 
			file-filter 
			file-filter-msg)
      
      (inherit center show)
      
      (private
	[default-width 500]
	[default-height 400]
	dirs 
	current-dir
	last-selected)
      
      (private
	
	[set-directory ; sets directory in listbox 
	 
	 (lambda (dir) ; dir is normalized
	   (when (directory-exists? dir)
	     (gui-utils:show-busy-cursor
	      (lambda ()
		(set! current-dir dir)
		(set! last-directory dir)
		(let-values 
		    ([(dir-list menu-list)
		      (let loop ([this-dir dir]
				 [dir-list null]
				 [menu-list null])
			(let-values ([(base-dir in-dir dir?) 
				      (split-path this-dir)])
			  (if (eq? (system-type) 'windows)
			      (mzlib:string:string-lowercase! in-dir))
			  (let* ([dir-list (cons this-dir dir-list)]
				 [menu-list (cons in-dir menu-list)])
			    (if base-dir
				(loop base-dir dir-list menu-list)
				; No more
				(values dir-list menu-list)))))])
		  (set! dirs (reverse dir-list))
		  (send dir-choice clear)
		  (let loop ([choices (reverse menu-list)])
		    (unless (null? choices)
		      (send dir-choice append (car choices))
		      (loop (cdr choices))))
		  (send dir-choice set-selection 0))
		
		(send name-list clear)
		(send name-list set
		      (mzlib:function:quicksort
		       (let ([no-periods? 
			      (not (preferences:get
				    'framework:show-periods-in-dirlist))])
			 (let loop ([l (directory-list dir)])
			   (if (null? l)
			       null
			       (let ([s (car l)]
				     [rest (loop (cdr l))])
				 (cond
				   [(and no-periods?
					 (<= 1 (string-length s))
					 (char=? (string-ref s 0) #\.))
				    rest]
				   [(directory-exists? (build-path dir s))
				    (cons s rest)]
				   [(or (not file-filter)
					(mzlib:string:regexp-match-exact? 
					 file-filter s))
				    (cons s rest)]
				   [else rest])))))
		       (if (eq? (system-type) 'unix) string<? string-ci<?)))
		(send name-list set-selection-and-edit 0)
		(set! last-selected -1)))))]
	
	[set-edit
	 (lambda ()
	   (let* ([file (send name-list get-string-selection)])
	     (send directory-field set-value
		   (if file
		       (build-path current-dir file)
		       current-dir))))])
      
      (public
	
	[do-period-in/exclusion
	 (lambda (check-box event)
	   (preferences:set
	    'framework:show-periods-in-dirlist
	    (send check-box get-value))
	   (set-directory current-dir))]
	
	[do-dir
	 (lambda (choice event)
	   (let ([which (send choice get-selection)])
	     (if (< which (length dirs))
		 (set-directory (list-ref dirs which)))))]
	
	[do-name-list
	 (lambda (list-box evt)
	   (if (eq? (send evt get-event-type) 'list-box-dclick)
	       (let ([dir (send directory-field get-value)])
		 (if (directory-exists? dir)
		     (set-directory (mzlib:file:normalize-path dir))
		     (if multi-mode?
			 (do-add)
			 (do-ok))))
	       (when (send list-box get-string-selection)
		 (set-edit))))]
	
	[do-result-list
	 (lambda args #f)]
	
	[do-ok
	 (lambda args

	   (if multi-mode?

	       (let loop ([n (sub1 (send result-list get-number))]
			  [result null])
		 (if (< n 0)
		     (begin
		       (set-box! result-box result)
		       (show #f))
		     (loop (sub1 n) 
			   (cons (send result-list get-string n)
				 result))))
	       ; not multi-mode

	       (let ([name (send name-list get-string-selection)]
		     [non-empty? (> (send name-list get-number) 0)])

		 (cond

		   [(and save-mode? 
			 non-empty?
			 (not (string? name))) 'nothing-selected]

		   [(and save-mode? 
			 non-empty?
			 (string=? name ""))
		    (let ([file (send directory-field get-value)])
		      (if (directory-exists? file)
			  (set-directory (mzlib:file:normalize-path file))
			  (message-box 
			   "Error"
			   "You must specify a file name")))]

		   [(and save-mode? 
			 non-empty?
			 file-filter 
			 (not (mzlib:string:regexp-match-exact? file-filter name)))
		    (message-box "Error" file-filter-msg)]

		   [else
		    
		    ; if dir in edit box, go to that dir
		    
		    (let ([dir-name (send directory-field get-value)])
		      
		      (if (directory-exists? dir-name)
			  (set-directory (mzlib:file:normalize-path dir-name))
			  
			  ; otherwise, try to return absolute path
			  
			  (let* ([relative-name (make-relative name)]
				 [file-in-edit (file-exists? dir-name)]
				 [file (if (or file-in-edit
					       (not relative-name)
					       save-mode?)
					   dir-name
					   (build-path current-dir relative-name))])
			    
			    ; trying to open a file that doesn't exist
			    
			    (if (and (not save-mode?) (not file-in-edit))
				(message-box 
				 "Error"
				 (string-append "The file \"" 
						dir-name
						"\" does not exist."))
				
				; saving a file, which may exist, or
				; opening an existing file
				
				(if (or (not save-mode?)
					(not (file-exists? file))
					replace-ok?
					(eq? (message-box "Warning"
							  (string-append
							   "The file " 
							   file
							   " already exists. "
							   "Replace it?")
							  #f
							  '(yes-no))
					     'yes))
				    (let ([normal-path
					   (with-handlers 
					       ([(lambda (_) #t)
						 (lambda (_)
						   (message-box
						    "Warning" 
						    (string-append
						     "The file " 
						     file
						     " contains nonexistent directory or cycle."))
						   #f)])
					     (mzlib:file:normalize-path file))])
				      (when normal-path 
					(set-box! result-box normal-path)
					(show #f))))))))]))))]
	
	[add-one
	 (lambda (name)
	   (unless (or (directory-exists? name)
		       (send result-list find-string name))
	     (send result-list append (mzlib:file:normalize-path name))))]
	
	[do-add
	 (lambda args
	   (let ([name (send name-list get-string-selection)])
	     (if (string? name)
		 (let ([name (build-path current-dir
					 (make-relative name))])
		   (add-one name)))))]
	
	[do-add-all
	 (lambda args
	   (let loop ([n 0])
	     (when (< n (send name-list get-number))
	       (let ([name (send name-list get-string n)])
		 (let ([name (build-path current-dir
					 (make-relative name))])
		   (add-one name)
		   (loop (add1 n)))))))]
	
	[do-remove
	 (lambda args
	   (let loop ([n 0])
	     (if (< n (send result-list get-number))
		 (if (send result-list is-selected? n)
		     (begin
		       (send result-list delete n)
		       (loop n))
		     (loop (add1 n))))))]
	
	[do-cancel
	 (lambda args
	   (set-box! result-box #f)
	   (show #f))])
	
      (override
	[on-close (lambda () #f)])
      
      (sequence
	(super-init (if save-mode? "Put file" "Get file")
		    parent-win 
		    default-width 
		    default-height))
      
      (private
	
	[main-panel (make-object vertical-panel% this)]
	
	[top-panel (make-object horizontal-panel% main-panel)]
	
	[_1 (make-object message% prompt top-panel)]
	
	[dir-choice (make-object choice% #f null top-panel do-dir)]
	
	[middle-panel (make-object horizontal-panel% main-panel)]
	[left-middle-panel (make-object vertical-panel% middle-panel)]
	[right-middle-panel (when multi-mode? 
			      (make-object vertical-panel% middle-panel))]

	[name-list%
	 
	 (class-asi list-box%
	   
	   (inherit 
	    get-string-selection
	    get-string
	    get-selection
	    get-number
	    get-first-visible-item
	    number-of-visible-items
	    set-first-visible-item
	    set-selection)

	   (override
	    [on-subwindow-char
	     
	     (lambda (_ key) 
	       (let ([code (send key get-key-code)]
		     [num-items (get-number)]
		     [curr-pos (get-selection)])
		 (cond 
		  [(or (equal? code 'numpad-return)
		       (equal? code #\return))
		   (if multi-mode?
		       (do-add)
		       (do-ok))]
		  
		  ; look for letter at beginning of a filename
		  [(char? code)
		   (let loop ([pos (add1 curr-pos)])
		     (unless (>= pos num-items)
		       (let ([first-char (string-ref (get-string pos) 0)])
			 (if (char=? code first-char)
			     (set-selection-and-edit pos)
			     (loop (add1 pos))))))]
		  
		  ; movement keys
		  [(and (eq? code 'up) 
			(> curr-pos 0))
		   (set-selection-and-edit (sub1 curr-pos))]
		  
		  [(and (eq? code 'down)
			(< curr-pos (sub1 num-items)))
		   (let* ([num-vis (number-of-visible-items)] 
			  [curr-first (get-first-visible-item)]
			  [new-curr-pos (add1 curr-pos)]
			  [new-first (if (< new-curr-pos (+ curr-first num-vis))
					 curr-first ; no scroll needed
					 (add1 curr-first))])
		     (set-first-visible-item new-first)
		     (set-selection-and-edit new-curr-pos))]
		  
		  [(and (eq? code 'prior)
			(> curr-pos 0))
		   (let* ([num-vis (number-of-visible-items)]
			  [new-first (- (get-first-visible-item) num-vis)])
		     (set-first-visible-item (max new-first 0))
		     (set-selection-and-edit (max 0 (- curr-pos num-vis))))]
		  
		  [(and (eq? code 'next)
			(< curr-pos (sub1 num-items)))
		   (let* ([num-vis (number-of-visible-items)]
			  [new-first (+ (get-first-visible-item) num-vis)])
		     (set-first-visible-item
		      (min new-first (- (get-number) num-vis)))
		     (set-selection-and-edit 
		      (min (sub1 num-items) (+ curr-pos num-vis))))]
		  
		  [else #f])))])
	   
	   (public
	    [set-selection-and-edit
	     (lambda (pos)
	       (when (> (get-number) 0)
		 (let* ([first-item (get-first-visible-item)]
			[last-item (sub1 (+ (number-of-visible-items) 
					    first-item))])
		   (if (or (< pos first-item) (> pos last-item))
		       (set-first-visible-item pos))
		   (set-selection pos)))
	       (set-edit))]
	     [on-default-action
	      (lambda ()
		(when (> (get-number) 0)
		  (let* ([which (get-string-selection)]
			 [dir (build-path current-dir
					  (make-relative which))])
		    (if (directory-exists? dir)
			(set-directory (mzlib:file:normalize-path dir))
			(if multi-mode?
			    (do-add)
			    (do-ok))))))]))]
	
	[name-list (make-object name-list%
				#f null left-middle-panel do-name-list
				'(single))]
	
	[set-focus-to-name-list
	 (lambda ()
	   (send name-list focus))]
	
	[save-panel (when save-mode? (make-object horizontal-panel% main-panel))]
	
	[directory-panel (make-object horizontal-panel% main-panel)]
	
	[dot-panel (when (eq? 'unix (system-type))
		     (make-object horizontal-panel% main-panel))]
	
	[bottom-panel (make-object horizontal-panel% main-panel)]
	
	[directory-field
	 (keymap:call/text-keymap-initializer
	  (lambda ()
	    (make-object text-field%
	      "Full pathname"
	      directory-panel
	      (lambda (txt evt)
		(when (eq? (send evt get-event-type) 'text-field-enter)
		  (let ([dir (send directory-field get-value)])
		    (if (directory-exists? dir)
			(set-directory (mzlib:file:normalize-path dir))
			(if multi-mode?
			    (do-add)
			    (do-ok)))))))))]

	[result-list
	 (when multi-mode?
	   (make-object list-box%
			#f
			null
			right-middle-panel
			do-result-list
			'(multiple)))]
	[add-panel 
	 (when multi-mode? 
	   (make-object horizontal-panel% left-middle-panel))]
	
	[remove-panel 
	 (when multi-mode? 
	   (make-object horizontal-panel% right-middle-panel))]
	
	[do-updir
	 (lambda () 
	   (set-directory (build-updir current-dir))
	   (set-focus-to-name-list))])
      
      (sequence
	
	(when (eq? (system-type) 'unix)
	  (let ([dot-cb
		 (make-object check-box%
		   "Show files and directories that begin with a dot"
		   dot-panel
		   do-period-in/exclusion)])
	    (send dot-panel stretchable-height #f)
	    (send dot-cb set-value 
		  (preferences:get 'framework:show-periods-in-dirlist))))
	
	(send directory-panel stretchable-height #f)
	
	(when multi-mode?
	  (send add-panel stretchable-height #f)
	  (send remove-panel stretchable-height #f)
	  (send result-list stretchable-width #t))
	
	(make-object button% 
		     "Up directory"
		     top-panel
		     (lambda (button evt) (do-updir)))
	
	(send dir-choice stretchable-width #t)
	(send name-list stretchable-width #t)
	(send top-panel stretchable-height #f)
	(send bottom-panel stretchable-height #f)
	
	(when save-mode?
	  (send save-panel stretchable-height #f)))
      
      (private
	
	[add-button (when multi-mode?
		      (make-object horizontal-panel% add-panel)
		      (make-object button%
				   "Add"
				   add-panel
				   do-add))]
	[add-all-button (when multi-mode?
			  (begin0
			    (make-object button%
					 "Add all"
					 add-panel do-add-all)
			    (make-object horizontal-panel% add-panel)))]
	[remove-button (when multi-mode?
			 (make-object horizontal-panel% remove-panel)
			 (begin0
			   (make-object button% "Remove" remove-panel do-remove)
			   (make-object horizontal-panel% remove-panel)))])
      (sequence
	(make-object vertical-panel% bottom-panel)) 
      (private
	[ok-button
	 (make-object button% "OK" bottom-panel do-ok (if multi-mode? '() '(border)))]
	[cancel-button (make-object button% "Cancel" bottom-panel do-cancel)])
      (sequence
	(cond
	  [(and start-dir
		(directory-exists? start-dir))
	   (set-directory (mzlib:file:normalize-path start-dir))]
	  [last-directory (set-directory last-directory)]
	  [else (set-directory (current-directory))])
	
	(send ok-button min-width (send cancel-button get-width))
	
	(center 'both)
	
	(show #t))))
  
  ; make-common takes a dialog-maker
  ; used to make one dialog object per session, now created each time
  
  (define make-common
    (lambda (make-dialog)
      (lambda args
	(let ([result-box (box #f)])
	  (apply make-dialog result-box args)
	  (unbox result-box)))))
  
  ; the common versions of these functions have their visual
  ; interfaces under Scheme control
  
  (define common-put-file
    (make-common
     (opt-lambda (result-box 
		  [name #f]
		  [in-directory #f]
		  [replace? #f]
		  [prompt "Select file"]
		  [filter #f]
		  [filter-msg "Invalid form"]
		  [parent-win (dialog-parent-parameter)])
       (let* ([directory (if (and (not in-directory)
				  (string? name))
			     (mzlib:file:path-only name)
			     in-directory)]
	      [saved-directory last-directory]
	      [name (or (and (string? name)
			     (mzlib:file:file-name-from-path name))
			name)])
	 (make-object finder-dialog% 
	   parent-win
	   #t 
	   replace? 
	   #f 
	   result-box 
	   directory 
	   name 
	   prompt 
	   filter 
	   filter-msg)
	 (when in-directory (set! last-directory saved-directory))))))
  
  (define common-get-file
    (make-common
     (opt-lambda
	 (result-box 
	  [directory #f]
	  [prompt "Select file"]
	  [filter #f]
	  [filter-msg "Bad name"]
	  [parent-win (dialog-parent-parameter)])
       (let ([saved-directory last-directory])
	 (make-object finder-dialog% 
	   parent-win     ; parent window
	   #f             ; save-mode?
	   #f             ; replace-ok?
	   #f             ; multi-mode?
	   result-box     ; boxed results
	   directory      ; start-dir
	   #f             ; start-name
	   prompt         ; prompt
	   filter         ; file-filter
	   filter-msg)    ; file-filter-msg
	 (when directory (set! last-directory saved-directory))))))
  
  (define common-get-file-list
    (make-common
     (opt-lambda (result-box 
		  [directory #f]
		  [prompt "Select files"]
		  [filter #f]
		  [filter-msg "Bad name"]
		  [parent-win (dialog-parent-parameter)])
       (make-object 
	finder-dialog% 
	parent-win  ; parent window
	#f          ; save-mode?
	#f          ; replace-ok?
	#t          ; multi-mode?
	result-box  ; boxed results
	directory   ; directory
	#f          ; start-name
	prompt      ; prompt
	filter      ; file-filter
	filter-msg  ; file-filter-msg
	))))
  
  ; the std- and common- forms both have opt-lambda's, with the same
  ; list of args.  Should the opt-lambda's be placed in the dispatching function?
  
  (define std-put-file
    (opt-lambda ([name #f]
		 [directory #f]
		 [replace? #f]
		 [prompt "Select file"]
		 [filter #f]
		 [filter-msg "That filename does not have the right form."]
		 [parent-win (dialog-parent-parameter)])
      (let* ([directory (if (and (not directory)
				 (string? name))
			    (mzlib:file:path-only name)
			    directory)]
	     [name (or (and (string? name)
			    (mzlib:file:file-name-from-path name))
		       name)]
	     [f (put-file 
		 prompt 
		 parent-win
		 directory 
		 name
		 (default-extension))])
 
	(if (or (not f)
		(and filter 
		     (not (filter-match? filter 
					 f
					 filter-msg))))
	    #f
	    (let* ([f (mzlib:file:normalize-path f)]
		   [dir (mzlib:file:path-only f)]
		   [name (mzlib:file:file-name-from-path f)])
	      (cond
		[(not (and (string? dir) (directory-exists? dir)))
		 (message-box "Error" "That directory does not exist.")
		 #f]
		[(or (not name) (equal? name ""))
		 (message-box "Error" "Empty filename.")
		 #f]
		[else f]))))))
  
  (define std-get-file
    (opt-lambda ([directory #f]
		 [prompt "Select file"]
		 [filter #f]
		 [filter-msg "That filename does not have the right form."]
		 [parent-win (dialog-parent-parameter)])
      (let ([f (get-file
		prompt 
		parent-win
		directory)])
	(if f
	    (if (or (not filter) (filter-match? filter f filter-msg))
		(let ([f (mzlib:file:normalize-path f)])
		  (cond
		    [(directory-exists? f)
		     (message-box "Error" "That is a directory name.")
		     #f]
		    [(not (file-exists? f))
		     (message-box "Error" "File does not exist.")
		     #f]
		    [else f]))
		#f)
	    #f))))
  
  ; external interfaces to file functions
  
  (define -put-file
    (lambda args
      (let ([actual-fun 
	     (case (preferences:get 'framework:file-dialogs)
	       [(std) std-put-file]
	       [(common) common-put-file])])
	(apply actual-fun args))))
  
  (define -get-file
    (lambda args
      (let ([actual-fun
	     (case (preferences:get 'framework:file-dialogs)
	       [(std) std-get-file]
	       [(common) common-get-file])])
	(apply actual-fun args)))))
