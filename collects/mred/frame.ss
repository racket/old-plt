
(define mred:frame@
  (unit/sig mred:frame^
    (import [mred:debug : mred:debug^]
	    [mred:preferences : mred:preferences^]
	    [mred:edit : mred:edit^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:icon : mred:icon^]
	    [mred:menu : mred:menu^] 
	    [mred:group : mred:group^]
	    [mred:finder : mred:finder^]
	    [mred:find-string : mred:find-string^]
	    [mred:handler : mred:handler^]
	    [mred:exit : mred:exit^]
	    [mred:autosave : mred:autosave^]
	    [mred:panel : mred:panel^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:file : mzlib:file^])
	    
    (mred:debug:printf 'invoke "mred:frame@")

    (define frame-name "MrEd")

    (define frame-width 600)
    (define frame-height 600)
    (let ([w (box 0)]
	  [h (box 0)])
      (wx:display-size w h)
      (if (< (unbox w) frame-width)
	  (set! frame-width (- (unbox w) 65)))
      (if (< (unbox h) frame-height)
	  (set! frame-height (- (unbox h) 65))))

    (define empty-frame%
      (class mred:container:frame% args
	(rename [super-pre-on-char pre-on-char]
		[super-pre-on-event pre-on-event])
	(rename [super-show show])
	(sequence (mred:debug:printf 'creation "creating a frame"))
	(public
	  [get-panel% 
	   (lambda ()
	     (class-asi mred:container:vertical-panel%
	       (public
		 [default-spacing-width 0]
		 [default-border-width 2])))]
	  [on-close (lambda () #t)])
	(sequence 
	  (mred:debug:printf 'super-init "before empty-frame%")
	  (apply super-init args)
	  (mred:debug:printf 'super-init "after empty-frame%"))
	(public
	  [shown #f]
	  [show (lambda (x) 
		  (set! shown x)
		  (super-show x))]
	  [keymap (make-object wx:keymap%)]
	  [make-root-panel
	   (lambda (% parent)
	     (make-object % parent))]
	  [panel (make-root-panel (get-panel%) this)])
	(public
	  [pre-on-char
	   (lambda (receiver event)
	     (let ([ans (send keymap handle-key-event this event)])
	       (or ans
		   (super-pre-on-char receiver event))))]
	  [pre-on-event
	   (lambda (receiver event)
	     (or (send keymap handle-mouse-event this event)
		 (super-pre-on-event receiver event)))])))

    (define make-menu-frame%
      (lambda (super%)
	(class super% args
	  (inherit set-menu-bar get-menu-bar keymap)
	  (public
	    [menu% mred:menu:menu%]
	    [menu-bar% mred:menu:menu-bar%])
	  (public
	    [get-menu-bar% (lambda () menu-bar%)]
	    [get-menu% (lambda () menu%)]
	    [next-menu-id (lambda () 0)]
	    [make-menu-bar
	     (lambda ()
	       (make-object (get-menu-bar%)))]
	    [make-menu
	     (lambda ()
	       (make-object (get-menu%)))]
	    [on-menu-command
	     (lambda (op)
	       (send (get-menu-bar) dispatch op))])
	  (sequence
	    (mred:debug:printf 'super-init "before mred:menu-frame%")
	    (apply super-init args)
	    (mred:debug:printf 'super-init "after mred:menu-frame%")
	    ; Build and install the menu bar:
	    (let ([menu-bar (make-menu-bar)])
	      (set-menu-bar menu-bar)
	      (send menu-bar set-frame this))))))

    (define menu-frame% (make-menu-frame% empty-frame%))

    (define tab (string #\tab))

    (define make-standard-menus-frame%
      (lambda (super%)
	(let ([join (opt-lambda (base special [suffix ""])
		      (if (string=? special "")
			  (string-append base suffix)
			  (string-append base " " special suffix)))])
	  (let-expansion-time 
	   build-class-macro
	   (let-struct between (menu name separator)
	     (let-struct an-item (name help-string proc key menu-string-before menu-string-after)
	       (letrec* ([build-id
			  (lambda (name post)
			    (let ([name-string (symbol->string name)])
			      (string->symbol (string-append name-string post))))]
			 [build-public-clause
			  (opt-lambda (item)
			    (let ([name (an-item-name item)]
				  [help-string (an-item-help-string item)]
				  [proc (an-item-proc item)])
			      `(public 
				 [,name ,proc]
				 [,(build-id name "-id") #f]
				 [,(build-id name "-string") ""]
				 [,(build-id name "-help-string") ,help-string])))]
			 [build-proc-clause
			  (lambda (item)
			    (let* ([name (an-item-name item)]
				   [name-string (symbol->string name)]
				   [menu-before-string (an-item-menu-string-before item)]
				   [menu-after-string (an-item-menu-string-after item)]
				   [key (an-item-key item)]
				   [file-menu? (string=? (substring name-string 0 9) "file-menu")])
			      `(when ,name
				 (set! ,(build-id name "-id")
				       (send ,(if file-menu? 'file-menu 'edit-menu) append-item 
					     (join ,menu-before-string 
						   ,(build-id name "-string")
						   ,menu-after-string)
					     ,name ,(build-id name "-help-string")
					     #f ,key)))))]
			 [build-between-ivar
			  (lambda (between)
			    (string->symbol 
			     (string-append (symbol->string (between-menu between))
					    ":"
					    (symbol->string (between-name between)))))]
			 [build-between-proc-clause
			  (lambda (between)
			    `(,(build-between-ivar between) ,(between-menu between)))]
			 [build-between-public-clause
			  (lambda (between)
			    `(public
			       [,(build-between-ivar between) 
				,(if (between-separator between)
				     '(lambda (m) (send m append-separator))
				     '(lambda (x) (void)))]))]
			 [build-make-menu-bar
			  (lambda (items)
			    (let ([mb (gensym "mb")])
			      `(lambda ()
				 (let ([,mb (super-make-menu-bar)])
				   (set! file-menu (make-menu))
				   (set! edit-menu (make-menu))
				   (send* ,mb (append file-menu
						      (if (eq? wx:platform 'windows)
							  "&File" "F&ile"))
				     (append edit-menu "&Edit"))
				   ,@(map (lambda (x)
					    (if (between? x)
						(build-between-proc-clause x)
						(build-proc-clause x)))
					  items)
				   ,mb))))]
			 [items
			  (list (make-an-item 'file-menu:new "Open a new file"
					      '(lambda () (mred:handler:edit-file #f) #t)
					      "n" "&New" "")
				(make-between 'file-menu 'between-new-and-open #f)
				(make-an-item 'file-menu:open "Open a file from disk"
					      'mred:handler:open-file
					      "n" "&Open" "...")
				(make-between 'file-menu 'between-open-and-save #f)
				(make-an-item 'file-menu:revert 
					      "Revert this file to the copy on disk"
					      #f #f "&Revert" "")
				(make-an-item 'file-menu:save "" #f "s" "&Save" "")
				(make-an-item 'file-menu:save-as "" #f "n" "Save" " &As...")
				(make-between 'file-menu 'between-save-and-print #t)
				(make-an-item 'file-menu:print "" #f "p" "&Print..." "")
				(make-between 'file-menu 'between-print-and-close #t)
				(make-an-item 'file-menu:close "" 
					      '(lambda () (when (on-close) (show #f)) #t)
					      "w" "&Close" "")
				(make-between 'file-menu 'between-close-and-quit #f)
				(make-an-item 'file-menu:quit "" '(lambda () (mred:exit:exit))
					      (if (eq? 'wx:platform 'macintosh) "q" "x")
					      (if (eq? wx:platform 'macintosh) "Quit" "E&xit") "")
				(make-between 'file-menu 'after-quit #f)
				
				(make-an-item 'edit-menu:undo "" #f "z" "&Undo" "")
				(make-an-item 'edit-menu:redo "" #f #f "&Redo" "")
				(make-between 'edit-menu 'between-redo-and-cut #t)
				(make-an-item 'edit-menu:cut "" #f "x" "Cu&t" "")
				(make-between 'edit-menu 'between-cut-and-copy #f)
				(make-an-item 'edit-menu:copy "" #f "c" "&Copy" "")
				(make-between 'edit-menu 'between-copy-and-paste #f)
				(make-an-item 'edit-menu:paste "" #f "v" "&Paste" "")
				(make-between 'edit-menu 'between-paste-and-clear #f)
				(make-an-item 'edit-menu:clear "" #f #f
					      (if (eq? wx:platform 'macintosh)
						  "Clear"
						  "&Delete")
					      "")
				(make-between 'edit-menu 'between-clear-and-select-all #f)
				(make-an-item 'edit-menu:select-all "" #f "z" "Select A&ll" "")
				(make-between 'edit-menu 'between-select-all-and-find #t)
				(make-an-item 'edit-menu:find "Search for a string in the buffer"
					      '(lambda () (send this search))
					      "z" "Find" "")
				(make-an-item 'edit-menu:replace "Search and replace a string in the buffer"
					      #f "z" "Replace" "")
				(make-between 'edit-menu 'between-replace-and-preferences #t)
				(make-an-item 'edit-menu:preferences ""
					      '(lambda () (mred:preferences:show-preferences-dialog))
					      #f "Preferences..." "")
				(make-between 'edit-menu 'after-standard-items #f))])
		 (lambda ()
		   `(class-asi super%
		      (inherit make-menu on-close show)
		      (rename [super-make-menu-bar make-menu-bar])
		      (public [file-menu 'file-menu-uninitialized]
			      [edit-menu 'edit-menu-uninitialized])
		      ,@(map (lambda (x)
			       (if (between? x)
				   (build-between-public-clause x)
				   (build-public-clause x)))
			     items)
		      (public
			[make-menu-bar
			 ,(build-make-menu-bar items)]))))))
	   (let-macro build-class
	     (lambda ()
	       ((local-expansion-time-value 'build-class-macro)))
	     (build-class))))))

    (define standard-menus-frame% (make-standard-menus-frame% menu-frame%))

    (define make-simple-frame%
      (lambda (super%)
	(class super% ([name frame-name])
	  (inherit panel get-client-size set-icon get-menu-bar
		   make-menu on-close show)
	  (rename [super-on-close on-close]
		  [super-set-title set-title])
	  (public
	    [WIDTH frame-width]
	    [HEIGHT frame-height])

	  (public
	    [get-panel% (lambda () mred:panel:vertical-edit-panel%)]
	    [title-prefix "MrEd"])
	  
	  (private
	    [title ""]
	    [do-title
	     (lambda ()
	       (let ([t (if (or (string=? "" title)
				(string=? "" title-prefix))
			    (string-append title-prefix title)
			    (string-append title-prefix ": " title))])
		 '(printf "setting-title to ~a~n" t)
		 (super-set-title t)))])

	  ; methods
	  (public
	    [on-frame-active (lambda () (void))]
	    [get-title (lambda () title)]
	    [set-title
	     (lambda (t)
	       (when (and (string? t)
			  (not (string=? t title)))
		 (set! title t)
		 (do-title)))]
	    [set-title-prefix
	     (lambda (s)
	       (when (and (string? s)
			  (not (string=? s title-prefix)))
		 (set! title-prefix s)
		 (do-title)))]
	    [get-canvas% (lambda () mred:canvas:simple-frame-canvas%)]
	    [get-edit% (lambda () mred:edit:edit%)]
	    [make-edit
	     (lambda ()
	       (let ([% (get-edit%)])
		 (make-object %)))])

	  (public
	    [save-as
	     (opt-lambda ([format wx:const-media-ff-same])
	       (let ([file (mred:finder:put-file)])
		 (when file
		   (send (active-edit) save-file file format))))]
	    [file-menu:new (lambda () (mred:handler:edit-file #f)
				   #t)]
	    [file-menu:revert 
	     (lambda () 
	       (let* ([b (box #f)]
		      [filename (send edit get-filename b)])
		 (if (or (null? filename) (unbox b))
		     (wx:bell)
		     (send edit load-file filename))
		 #t))]
	    [file-menu:save (lambda () (send edit save-file)
				    #t)]
	    [file-menu:save-as (lambda () (save-as) #t)]
	    [file-menu:between-print-and-close
	     (lambda (file-menu)
	       (send file-menu append-separator)
	       (let ([split
		      (lambda (panel%)
			(let ([% (class-asi panel%
				   (public
				     [default-spacing-width 0]
				     [default-border-width 0]))])
			  (lambda ()
			    (when (active-canvas)
			      (send panel split (active-canvas) %)))))])
		 (send file-menu append-item "Split Horizontally" (split mred:container:horizontal-panel%))
		 (send file-menu append-item "Split Vertically" (split mred:container:vertical-panel%))
		 (send file-menu append-item "Collapse"
		       (lambda ()
			 (when (active-canvas)
			   (send panel collapse (active-canvas))))))
	       (send file-menu append-separator))]
	    [file-menu:close (lambda () 
			       (when (on-close) (show #f))
			       #t)]
	    [file-menu:print (lambda () (send edit print '()) #t)])


	  (private
	    [edit-menu:do (lambda (const)
			    (lambda () (send edit do-edit const)
				    #t))])

	  (public
	    [edit-menu:undo (edit-menu:do wx:const-edit-undo)]
	    [edit-menu:redo (edit-menu:do wx:const-edit-redo)]
	    [edit-menu:cut (edit-menu:do wx:const-edit-cut)]
	    [edit-menu:clear (edit-menu:do wx:const-edit-clear)]
	    [edit-menu:copy (edit-menu:do wx:const-edit-copy)]
	    [edit-menu:paste (edit-menu:do wx:const-edit-paste)]
	    [edit-menu:select-all (edit-menu:do wx:const-edit-select-all)]
	    [edit-menu:replace (lambda ()
				 (when (active-canvas)
				   (mred:find-string:find-string
				    (active-canvas)
				    (active-edit)
				    -1 -1 (list 'replace 'ignore-case))))]

	    [edit-menu:between-replace-and-preferences
	     (lambda (edit-menu)
	       (send edit-menu append-separator)
	       (send edit-menu append-item "Insert Text Box"
		     (edit-menu:do wx:const-edit-insert-text-box))
	       (send edit-menu append-item "Insert Graphic Box"
		     (edit-menu:do wx:const-edit-insert-graphic-box))
	       (send edit-menu append-item "Insert Image..."
		     (edit-menu:do wx:const-edit-insert-image))
	       (send edit-menu append-item "Toggle Wrap Text"
		     (lambda ()
		       (let ([edit (active-edit)])
			 (when edit
			   (send edit set-auto-set-wrap (not (ivar edit auto-set-wrap?)))
			   (send (active-canvas) force-redraw)))))
	       (send edit-menu append-separator))])

	  (sequence
	    (mred:debug:printf 'super-init "before simple-frame%")
	    (super-init () name -1 -1 WIDTH HEIGHT
			(+ wx:const-default-frame wx:const-sdi)
			name)
	    (mred:debug:printf 'super-init "after simple-frame%"))

	  (public
	    [canvas (make-object (get-canvas%) panel)]
	    [canvases (list canvas)]
	    [active-canvas
	     (let ([last-active-canvas canvas])
	       (lambda ()
		 (if (send last-active-canvas is-focus-on?)
		     last-active-canvas
		     (let ([ans
			    (let loop ([item panel])
			      (cond
			       [(is-a? item wx:media-canvas%)
				(and (send item is-focus-on?)
				     item)]
			       [(is-a? item wx:panel%)
				(ormap loop
				       (ivar item children))]
			       [else #f]))])
		       (when ans
			 (set! last-active-canvas ans))
		       ans))))]
	    [active-edit
	     (lambda ()
	       (let ([c (active-canvas)])
		 (and c
		      (send c get-media))))])
	  
	  (public
	    [last-focus-canvas #f] ; Does this need to be inited during make-canvas?
	    [edit (make-edit)])
	  (sequence
	    (send* canvas (set-frame this) (set-media edit))
	    (when (send mred:icon:icon ok?)
	      (set-icon mred:icon:icon))
	    (do-title)))))

    (define simple-menu-frame% (make-simple-frame% standard-menus-frame%))))



	    '[remarkable-menu #f]
	    '[init-remarkable-menu
	     (lambda ()
	       '(unless remarkable-menu
		 (let ([mb (get-menu-bar)])
		   (set! remarkable-menu (make-menu))
		   (send mb append remarkable-menu "&Show"))))]
		 
	    '[add-remarkable-edit/panel
	     (lambda (panel/edit menu-string)
	       '(set! remarkable-edits/panels (cons panel/edit remarkable-edits/panels))
	       '(init-remarkable-menu)
	       '(letrec* ([remarkable-top-level (make-object mred:container:vertical-panel%
							    remarkable-panel)]
			 [insert-child
			  (lambda (current-children all-children)
			    (printf "insert-child;~n current: ~a~n     all: ~a~n" current-children all-children)
			    (cond
			      [(null? current-children)
			       (printf "case 1~n")
			       (list remarkable-top-level)]
			      [(null? all-children)
			       (printf "case 2~n")
			       (list remarkable-top-level)]
			      [(eq? (car all-children) remarkable-top-level)
			       (printf "case 3~n")
			       (cons remarkable-top-level current-children)]
			      [(eq? (car current-children) (car all-children))
			       (printf "case 4~n")
			       (cons (car current-children) (insert-child (cdr current-children) 
									  (cdr all-children)))]
			      [else
			       (printf "case 5~n")
			       (insert-child current-children (cdr all-children))]))]
			 [menu-id
			  (send remarkable-menu append-item menu-string
				(lambda ()
				  (if (send remarkable-menu checked? menu-id)
				      (send remarkable-panel change-children 
					    (lambda (l)
					      (insert-child l remarkable-children)))
				      (send remarkable-panel change-children
					    (lambda (l)
					      (mzlib:function:remq remarkable-top-level l)))))
				   "" #t)])
		 (send remarkable-menu check menu-id #t)
		 (set! remarkable-children (append remarkable-children
						   (list remarkable-top-level)))
		 (if (is-a? panel/edit wx:media-edit%)
		     (let ([canvas (make-object (get-canvas%) remarkable-top-level)])
		       (send canvas set-frame this)
		       (send canvas set-media panel/edit))
		     (panel/edit remarkable-top-level))))]
