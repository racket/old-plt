
  (unit/sig mred:frame^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:preferences : mred:preferences^]
	    [mred:edit : mred:edit^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:icon : mred:icon^]
	    [mred:menu : mred:menu^] 
	    [mred:group : mred:group^]
	    [mred:finder : mred:finder^]
	    [mred:find-string : mred:find-string^]
	    [mred:hyper-frame : mred:hyper-frame^]
	    [mred:handler : mred:handler^]
	    [mred:keymap : mred:keymap^]
	    [mred:exit : mred:exit^]
	    [mred:autosave : mred:autosave^]
	    [mred:panel : mred:panel^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mred:application : mred:application^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:file : mzlib:file^]
	    [mzlib:date : mzlib:date^])
	    
    (mred:debug:printf 'invoke "mred:frame@")

    (define frame-width 600)
    (define frame-height 650)
    (let ([w (box 0)]
	  [h (box 0)])
      (wx:display-size w h)
      (when (< (unbox w) frame-width)
	(set! frame-width (- (unbox w) 65)))
      (when (< (unbox h) frame-height)
	(set! frame-height (- (unbox h) 65))))

    (define empty-frame%
      (class mred:container:frame% args
	(rename [super-on-frame-active on-frame-active]
		[super-pre-on-char pre-on-char]
		[super-on-close on-close]
		[super-pre-on-event pre-on-event])
	(sequence (mred:debug:printf 'creation "creating a frame"))
	(inherit active-canvas)
	(rename [super-show show])
	(public
	  [get-panel% (lambda () mred:container:vertical-panel%)]
	  [show
	   (lambda (on?)
	     (when on?
	       (unless (member this (send mred:group:the-frame-group get-frames))
		 (send mred:group:the-frame-group insert-frame this)))
	     (super-show on?))]
	  [on-close
	   (lambda ()
	     (and (send mred:group:the-frame-group remove-frame this)
		  (super-on-close)))])
	(sequence 
	  (mred:debug:printf 'super-init "before empty-frame%")
	  (apply super-init args)
	  (mred:debug:printf 'super-init "after empty-frame%"))
	(public
	  [on-frame-active
	   (lambda ()
	     (super-on-frame-active)
	     (send mred:group:the-frame-group set-active-frame this))]
	  [keymap (make-object wx:keymap%)]
	  [make-root-panel
	   (lambda (% parent)
	     (make-object % parent))]
	  [panel (make-root-panel (get-panel%) this)])
	(public
	  [pre-on-char
	   (lambda (receiver event)
	     (let* ([canvas (active-canvas)]
		    [edit-should-handle?
		     (and canvas (send canvas is-focus-on?))]
		    [handled? (if edit-should-handle?
				  #f
				  (send keymap handle-key-event this event))])
	       '(printf "canvas ~a focus? ~a edit-should-handle? ~a handled? ~a~n"
		       canvas (send canvas is-focus-on?)
		       edit-should-handle? handled?)
	       (or handled?
		   (super-pre-on-char receiver event))))]
	  [pre-on-event
	   (lambda (receiver event)
	     (let* ([canvas (active-canvas)]
		    [canvas-should-handle?
		     (and canvas (send canvas is-focus-on?))]
		    [handled? (if canvas-should-handle?
				  #f
				  (send keymap handle-mouse-event this event))])
	       (or handled?
		   (super-pre-on-event receiver event))))])))

    (define make-menu-frame%
      (lambda (super%)
	(class super% args
	  (inherit set-menu-bar get-menu-bar)
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

    (mred:preferences:set-preference-default
     'mred:menu-bindings #t 
     (lambda (x) 
       (or (eq? x #t)
	   (eq? x #f))))

    (define make-standard-menus-frame%
      (lambda (super%)
	(let ([join (opt-lambda (base special [suffix ""])
		      (if (string=? special "")
			  (string-append base suffix)
			  (string-append base " " special suffix)))])
          (begin-elaboration-time (reference-library "macro.ss"))
	  (begin-elaboration-time
            (let-struct between (menu name separator)
              (let-struct an-item (name help-string proc key menu-string-before menu-string-after)
                (letrec* ([wx:platform
			   ;; mrspidey hack
			   (if (defined? 'wx:platform)
			       (global-defined-value 'wx:platform)
			       'unix)]
			  [build-id
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
					      #f 
					      (and (mred:preferences:get-preference
						    'mred:menu-bindings)
						   ,key))))))]
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
					       '(lambda () (mred:handler:open-file) #t)
					       "o" "&Open" "...")
				 (make-an-item 'file-menu:open-url "Open a Uniform Resource Locater"
					       '(lambda () (mred:handler:open-url) #t)
					       #f "Open &Url" "...")
				 (make-between 'file-menu 'between-open-and-save #f)
				 (make-an-item 'file-menu:revert 
					       "Revert this file to the copy on disk"
					       #f #f "&Revert" "")
				 (make-an-item 'file-menu:save "" #f "s" "&Save" "")
				 (make-an-item 'file-menu:save-as "" #f #f "Save" " &As...")
				 (make-between 'file-menu 'between-save-and-print #t)
				 (make-an-item 'file-menu:print "" #f "p" "&Print" "...")
				 (make-between 'file-menu 'between-print-and-close #t)
				 (make-an-item 'file-menu:close "" 
					       '(lambda () (when (on-close) (show #f)) #t)
					       "w" "&Close" "")
				 (make-between 'file-menu 'between-close-and-quit #f)
				 (make-an-item 'file-menu:quit "" '(lambda () (mred:exit:exit))
					       "q"
					       '(if (eq? wx:platform 'windows) "E&xit" "Quit")
					       "")
				 (make-between 'file-menu 'after-quit #f)
				 
				 (make-an-item 'edit-menu:undo "" #f "z" "&Undo" "")
				 (make-an-item 'edit-menu:redo "" #f "y" "&Redo" "")
				 (make-between 'edit-menu 'between-redo-and-cut #t)
				 (make-an-item 'edit-menu:cut "" #f "x" "Cu&t" "")
				 (make-between 'edit-menu 'between-cut-and-copy #f)
				 (make-an-item 'edit-menu:copy "" #f "c" "&Copy" "")
				 (make-between 'edit-menu 'between-copy-and-paste #f)
				 (make-an-item 'edit-menu:paste "" #f "v" "&Paste" "")
				 (make-between 'edit-menu 'between-paste-and-clear #f)
				 (make-an-item 'edit-menu:clear "" #f #f
					       '(if (eq? wx:platform 'macintosh)
						   "Clear"
						   "&Delete")
					       "")
				 (make-between 'edit-menu 'between-clear-and-select-all #f)
				 (make-an-item 'edit-menu:select-all "" #f "a" "Select A&ll" "")
				 (make-between 'edit-menu 'between-select-all-and-find #t)
				 (make-an-item 'edit-menu:find "Search for a string in the buffer"
					       '(lambda () (send this search 1) #t)
					       "f" "Find" "")
				 (make-an-item 'edit-menu:replace "Search and replace a string in the buffer"
					       #f #f "Replace" "")
				 (make-between 'edit-menu 'between-replace-and-preferences #t)
				 (make-an-item 'edit-menu:preferences ""
					       '(lambda () (mred:preferences:show-preferences-dialog) #t)
					       #f "Preferences..." "")
				 (make-between 'edit-menu 'after-standard-items #f))])
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
                         ,(build-make-menu-bar items)])))))))))

    (define standard-menus-frame% (make-standard-menus-frame% menu-frame%))

    (define make-simple-frame%
      (lambda (super%)
	(class super% ([name mred:application:app-name])
	  (inherit panel get-client-size set-icon get-menu-bar
		   make-menu show active-edit active-canvas)
	  (rename [super-on-close on-close]
		  [super-make-menu-bar make-menu-bar]
		  [super-set-title set-title])
	  (public
	    [WIDTH frame-width]
	    [HEIGHT frame-height])
	  
	  (public
	    [on-close
	     (lambda ()
	       (and (send (get-edit) on-close)
		    (super-on-close)))])

	  (public
	    [get-panel%  (lambda () mred:panel:vertical-edit-panel%)]
	    [title-prefix name])
	  
	  (private
	    [title ""]
	    [do-title
	     (lambda ()
	       (mred:debug:printf 'matthew "do-title.1~n")
	       (let ([t (if (or (string=? "" title)
				(string=? "" title-prefix))
			    (string-append title-prefix title)
			    (string-append title " - " title-prefix))])
		 (mred:debug:printf 'matthew "do-title.2~n")
		 (super-set-title t)
		 (mred:debug:printf 'matthew "super-set-title finished~n")))])
	  
	  (public
	    [get-title (lambda () title)]
	    [set-title
	     (lambda (t)
	       (mred:debug:printf 'matthew "set-title~n")
	       (when (and (string? t)
			  (not (string=? t title)))
		 (set! title t)
		 (do-title))
	       (mred:debug:printf 'matthew "end set-title~n"))]
	    [set-title-prefix
	     (lambda (s)
	       (when (and (string? s)
			  (not (string=? s title-prefix)))
		 (set! title-prefix s)
		 (mred:debug:printf 'matthew "set-title-prefix calling do-title~n")
		 (do-title)
		 (mred:debug:printf 'matthew "set-title-prefix returned from do-title~n")))]
	    [get-canvas% (lambda () mred:canvas:frame-title-canvas%)]
	    [get-edit% (lambda () mred:edit:backup-autosave-edit%)]
	    [make-edit (lambda () (make-object (get-edit%)))])
	  
	  (public
	    [save-as
	     (opt-lambda ([format wx:const-media-ff-same])
	       (let ([file (mred:finder:put-file)])
		 (when file
		   (send (get-edit) save-file file format))))]
	    [file-menu:revert 
	     (lambda () 
	       (let* ([b (box #f)]
		      [edit (get-edit)]
		      [filename (send edit get-filename b)]
		      [start (send edit get-start-position)]
		      [end (send edit get-end-position)])
		 (if (or (null? filename) (unbox b))
		     (wx:bell)
		     (begin (send edit begin-edit-sequence)
			    (send edit load-file filename)
			    (send edit set-position start end)
			    (send edit end-edit-sequence)))
		 #t))]
	    [file-menu:save (lambda ()
			      (send (get-edit) save-file)
			      #t)]
	    [file-menu:save-as (lambda () (save-as) #t)]
	    [file-menu:between-print-and-close
	     (lambda (file-menu)
	       (send file-menu append-separator)
	       (let ([split
		      (lambda (panel%)
			(lambda ()
			  (when (active-canvas)
			    (send panel split (active-canvas) panel%))))])
		 (send file-menu append-item "Split Horizontally" (split mred:container:horizontal-panel%))
		 (send file-menu append-item "Split Vertically" (split mred:container:vertical-panel%))
		 (send file-menu append-item "Collapse"
		       (lambda ()
			 (when (active-canvas)
			   (send panel collapse (active-canvas))))))
	       (send file-menu append-separator))]
	    [file-menu:print (lambda () (send (get-edit) print '()) #t)])
	  
	  
	  (private
	    [edit-menu:do (lambda (const)
			    (lambda () 
			      (let ([edit (active-edit)])
				(when edit
				  (send edit do-edit const)))
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

	  (public
	    [make-menu-bar
	     (let ([reg (regexp "<TITLE>(.*)</TITLE>")])
	       (lambda ()
		 (let* ([mb (super-make-menu-bar)]
			[help-menu (make-menu)]
			[dir (build-path (global-defined-value 
					  'mred:plt-home-directory)
					 "doc")])
		   (if (directory-exists? dir)
		       (let* ([dirs (directory-list dir)]
			      [find-title
			       (lambda (name)
				 (lambda (port)
				   (let loop ([l (read-line port)])
				     (if (eof-object? l)
					 name
					 (let ([match (regexp-match reg l)])
					   (if match
					       (cadr match)
					       (loop (read-line port))))))))]
			      [build-item
			       (lambda (local-dir output)
				 (let* ([f (build-path dir local-dir "index.htm")])
				   (if (file-exists? f)
				       (let ([title (call-with-input-file f (find-title local-dir))])
					 (cons 
					  (list title
						(lambda ()
						  (let* ([f (make-object mred:hyper-frame:hyper-view-frame%
									 (string-append "file:" f))])
						    (send f set-title-prefix title)
						    f)))
					  output))
				       (begin (mred:debug:printf 'help-menu "couldn't find ~a" f)
					      output))))]
			      [item-pairs 
			       (mzlib:function:quicksort
				(mzlib:function:foldl build-item null dirs)
				(lambda (x y) (string-ci<? (car x) (car y))))])
			 (unless (null? item-pairs)
			   (send mb append help-menu 
				 (if (= (length item-pairs) 1)
				     "Manual"
				     "Manuals")))
			 (for-each (lambda (x) (apply (ivar help-menu append-item) x))
				   item-pairs))
		       (mred:debug:printf 'help-menu "couldn't find PLTHOME/doc directory"))
		   mb)))])
	  
	  (sequence
	    (mred:debug:printf 'super-init "before simple-frame%")
	    (super-init () name -1 -1 WIDTH HEIGHT
			(+ wx:const-default-frame wx:const-sdi)
			name)
	    (mred:debug:printf 'super-init "after simple-frame%"))
	  
	  (public
	    [get-canvas (let ([c (make-object (get-canvas%) panel)])
			  (lambda () c))]
	    [get-edit (let ([e (make-edit)]) 
			(send (get-canvas) set-media e)
			(lambda () e))])
	  (sequence
	    (let ([icon (mred:icon:get-icon)])
	      (when (send icon ok?)
		(set-icon icon)))
	    (mred:debug:printf 'matthew "initial call to do-title~n")
	    (do-title)
	    (mred:debug:printf 'matthew "finished initial call to do-title~n")
	    (let ([canvas (get-canvas)])
	      (mred:debug:printf 'matthew "got canvas.1 ~a~n" canvas)
	      (mred:debug:printf 'matthew "got canvas.2 ~a~n" (ivar canvas set-focus))
	      (send canvas set-focus))
	    (mred:debug:printf 'matthew "after initial call to set-focus~n")))))

    (define simple-menu-frame% (make-simple-frame% standard-menus-frame%))

    (define make-searchable-frame%
      (let* ([anchor 0]
	     [searching-direction 1]
	     [old-highlight void]
	     [get-active-embedded-edit
	      (lambda (edit)
		(let loop ([edit edit])
		  (let ([snip (send edit get-focus-snip)])
		    (if (or (null? snip)
			    (not (is-a? snip wx:media-snip%)))
			edit
			(loop (send snip get-this-media))))))]
	     [clear-highlight
	      (lambda ()
		(begin (old-highlight)
		       (set! old-highlight void)))]
	     [reset-anchor
	      (let ([color (make-object wx:colour% "BLUE")])
		(lambda (edit)
		  (old-highlight)
		  (let ([position 
			 (if (= 1 searching-direction)
			     (send edit get-end-position)
			     (send edit get-start-position))])
		    (set! anchor position)
		    (set! old-highlight
			  (send edit highlight-range position position color)))))]
	     [replace-edit (make-object mred:edit:media-edit%)]
	     [find-edit
	      (make-object
	       (class-asi mred:edit:media-edit%
		 (inherit get-text)
		 (rename [super-after-insert after-insert]
			 [super-after-delete after-delete]
			 [super-on-focus on-focus])
		 (public
		   [searching-frame #f]
		   [set-searching-frame
		    (lambda (frame)
		      (set! searching-frame frame))]
		   [get-searching-edit
		    (lambda ()
		      (get-active-embedded-edit
		       (send searching-frame get-edit-to-search)))]
		   [search
		    (opt-lambda ([reset-anchor? #t] [beep? #t])
		      (when searching-frame
			(let* ([string (get-text)]
			       [searching-edit (get-searching-edit)]
			       [not-found
				(lambda ()
				  (when beep?
				    (wx:bell))
				  #f)]
			       [found
				(lambda (edit first-pos)
				  (let ([last-pos (+ first-pos (* searching-direction 
								  (string-length string)))])
				    (send* edit 
				      (set-caret-owner null wx:const-focus-display)
				      (set-position
					  (min first-pos last-pos)
					  (max first-pos last-pos)))
				    #t))])
			  (when reset-anchor?
			    (reset-anchor searching-edit))
			  (let-values ([(found-edit first-pos)
					(send searching-edit
					      find-string-embedded
					      string 
					      searching-direction
					      anchor
					      -1 #t #t #t)])
			    (cond
			      [(= -1 first-pos)
			       (let-values ([(found-edit pos)
					     (send searching-edit
						   find-string-embedded
						   string 
						   searching-direction
						   (if (= 1 searching-direction)
						       0
						       (send searching-edit last-position)))])
				 (if (= -1 pos)
				     (begin (send found-edit set-position anchor)
					    (not-found))
				     (found found-edit pos)))]
			      [else
			       (found found-edit first-pos)])))))]
		   [on-focus
		    (lambda (on?)
		      (when on?
			(reset-anchor (get-searching-edit)))
		      (super-on-focus on?))]
		   [after-insert
		    (lambda args
		      (apply super-after-insert args)
		      (search #f))]
		   [after-delete
		    (lambda args
		      (apply super-after-delete args)
		      (search #f))])))]
	     [canvas% 
	      (class-asi mred:canvas:one-line-canvas%
		(inherit get-parent frame)
		(rename [super-on-set-focus on-set-focus])
		(public
		  [lines 2]
		  [style-flags wx:const-mcanvas-hide-h-scroll]
		  [on-set-focus
		   (lambda ()
		     (mred:debug:printf 'matthew "searching frame::on-set-focus.1~n")
		     (send find-edit set-searching-frame frame)
		     (mred:debug:printf 'matthew "searching frame::on-set-focus.2~n")
		     (super-on-set-focus)
		     (mred:debug:printf 'matthew "searching frame::on-set-focus.3~n"))]))])
	(for-each (lambda (keymap)
		    (send keymap chain-to-keymap
			  mred:keymap:global-search-keymap
			  #t))
		  (list (send find-edit get-keymap)
			(send replace-edit get-keymap)))
	(lambda (super%)
	  (class super% args
	    (inherit active-edit active-canvas get-edit)
	    (rename [super-make-root-panel make-root-panel]
		    [super-on-activate on-activate]
		    [super-on-close on-close])
	    (private
	      [super-root 'unitiaialized-super-root])
	    (public
	      [make-root-panel
	       (lambda (% parent)
		 (let* ([s-root (super-make-root-panel
				 mred:container:vertical-panel%
				 parent)]
			[root (make-object % s-root)])
		   (set! super-root s-root)
		   root))])
	    (public
	      [on-activate
	       (lambda (on?)
		 (unless hidden?
		   (if on?
		       (reset-anchor (get-edit-to-search))
		       (clear-highlight)))
		 (super-on-activate on?))]
	      [get-edit-to-search
	       (lambda () 
		 (get-edit))]
	      [hide-search
	       (opt-lambda ([startup? #f])
		 (send super-root delete-child search-panel)
		 (clear-highlight)
		 (unless startup?
		   (send (send (get-edit-to-search)
			       get-canvas) set-focus))
		 (set! hidden? #t))]
	      [unhide-search
	       (lambda ()
		 (set! hidden? #f)
		 (send super-root add-child search-panel)
		 (reset-anchor (get-edit-to-search))
		 (send search-panel set-focus))])
	    (public
	      [on-close
	       (lambda ()
		 (and (super-on-close)
		      (begin (let ([close-canvas
				    (lambda (canvas edit)
				      (send edit remove-canvas canvas)
				      (send canvas set-media ()))])
			       (close-canvas find-canvas find-edit)
			       (close-canvas replace-canvas replace-edit))
			     (if (eq? this (ivar find-edit searching-frame))
				 (send find-edit set-searching-frame #f))
			     #t)))]
	      [set-search-direction 
	       (lambda (x) 
		 (set! searching-direction x)
		 (send dir-radio set-selection (if (= x 1) 0 1)))]
	      [replace&search
	       (lambda ()
		 (when (replace)
		   (search)))]
	      [replace-all
	       (lambda ()
		 (let* ( [replacee-edit (get-edit-to-search)]
                         [pos (if (= searching-direction 1)
                                (send replacee-edit get-start-position)
                                (send replacee-edit get-end-position))]
                         [get-pos 
                           (if (= searching-direction 1)
                               (ivar replacee-edit get-end-position)
			       (ivar replacee-edit get-start-position))]
                         [done? (if (= 1 searching-direction)
                                  <=
                                  >=)])
		   (send* replacee-edit 
		     (begin-edit-sequence)
		     (set-position pos))
		   (when (search)
		     (send replacee-edit set-position pos)
		     (let loop ([last-pos pos])
		       (search searching-direction #f)
		       (let ([current-pos (get-pos)])
			 (if (done? current-pos last-pos)
			     (send replacee-edit set-position last-pos)
			     (begin (replace)
				    (loop current-pos))))))
		   (send replacee-edit end-edit-sequence)))]
	      [replace
	       (lambda ()
		 (let* ([search-text (send find-edit get-text)]
			[replacee-edit (get-edit-to-search)]
			[replacee (send replacee-edit get-text 
					(send replacee-edit get-start-position)
					(send replacee-edit get-end-position))])
		   (if (string=? replacee search-text)
		       (begin (send replacee-edit insert (send replace-edit get-text))
			      #t)
		       #f)))]
	      [search
	       (opt-lambda ([direction searching-direction] [beep? #t])
		 (send find-edit set-searching-frame this)
		 (if hidden?
		     (unhide-search)
		     (begin
		       (set-search-direction direction)
		       (send find-edit search #t beep?))))])
	    (sequence
	      (mred:debug:printf 'super-init "before searchable-frame%")
	      (apply super-init args)
	      (mred:debug:printf 'super-init "after searchable-frame%"))
            (private
	      [search-panel (make-object mred:container:horizontal-panel% super-root)]

	      [left-panel (make-object mred:container:vertical-panel% search-panel)]
	      [find-canvas (make-object canvas% left-panel)]
	      [replace-canvas (make-object canvas% left-panel)]

	      [middle-panel (make-object mred:container:horizontal-panel% search-panel)]

	      [middle-left-panel (make-object mred:container:vertical-panel% middle-panel)]
	      [middle-middle-panel (make-object mred:container:vertical-panel% middle-panel)]
	      [middle-right-panel (make-object mred:container:vertical-panel% middle-panel)]

	      [spacing1 (make-object mred:container:horizontal-panel% middle-left-panel)]
	      [spacing2 (make-object mred:container:horizontal-panel% middle-middle-panel)]
	      [search-button (make-object mred:container:button% middle-left-panel 
					  (lambda args (search)) "Search")]

	      [replace&search-button (make-object mred:container:button% middle-middle-panel 
						  (lambda x (replace&search)) "Replace && Search")]
	      [replace-button (make-object mred:container:button% middle-left-panel (lambda x (replace)) "Replace")]
	      [replace-all-button (make-object mred:container:button% middle-middle-panel
					       (lambda x (replace-all)) "Replace All")]
	      [spacing3 (make-object mred:container:horizontal-panel% middle-left-panel)]
	      [spacing4 (make-object mred:container:horizontal-panel% middle-middle-panel)]

	      [dir-radio (make-object mred:container:radio-box% middle-right-panel
				      (lambda (dir-radio evt)
					(let ([forward (if (= 0 (send evt get-command-int))
							   1
							   -1)])
					  (set-search-direction forward)
					  (reset-anchor (get-edit-to-search))))
				      null
				      -1 -1 -1 -1
				      (list "Forward" "Backward"))]
	      [close-button (make-object mred:container:button% middle-right-panel
					 (lambda args (hide-search)) "Close")]
	      [hidden? #f])
	    (sequence
	      (let ([align
		     (lambda (x y)
		       (let ([m (max (send x get-width)
				     (send y get-width))])
			 (send x user-min-width m)
			 (send y user-min-width m)))])
		(align search-button replace-button)
		(align replace&search-button replace-all-button))
	      (send search-panel stretchable-in-y #f)
	      (for-each (lambda (x) (send* x (stretchable-in-y #f) (stretchable-in-x #f)))
			(list middle-panel))
	      (for-each (lambda (x) (send x stretchable-in-y #f))
			(list search-panel left-panel))
	      (send find-canvas set-media find-edit)
	      (send replace-canvas set-media replace-edit) 
	      (send find-edit add-canvas find-canvas)
	      (send replace-edit add-canvas replace-canvas)
	      (hide-search #t))))))

    (define searchable-frame% (make-searchable-frame% simple-menu-frame%))

    (let ([boolean? 
	   (lambda (x)
	     (or (not x)
		 (eq? x #t)))])
      (mred:preferences:set-preference-default 'mred:show-status-line
					       #t
					       boolean?)
      (mred:preferences:set-preference-default 'mred:line-offsets
					       #t
					       boolean?))
    

    (define make-info-frame%
      (let* ([time-edit (make-object mred:edit:media-edit%)]
	     [time-semaphore (make-semaphore 1)]
	     [magic-space 25]
	     [wide-time "00:00pm"]
	     [_ (send time-edit lock #t)]
	     [update-time
	      (lambda ()
		(dynamic-wind
		 (lambda ()
		   (semaphore-wait time-semaphore)
		   (send time-edit lock #f))
		 (lambda ()
		   (send* time-edit 
		     (erase)
		     (insert 
		      (let* ([date (seconds->date
				    (current-seconds))]
			     [hours (date-hour date)]
			     [minutes (date-minute date)])
			(format "~a:~a~a~a"
				(cond
				  [(= hours 0) 12]
				  [(<= hours 12) hours]
				  [else (- hours 12)])
				(quotient minutes 10)
				(modulo minutes 10)
				(if (< hours 12) "am" "pm"))))))
		 (lambda ()
		   (send time-edit lock #t)
		   (semaphore-post time-semaphore))))]
	     [determine-width
	      (lambda (string canvas edit)
		(send edit set-autowrap-bitmap null)
		(send canvas call-as-primary-owner
		      (lambda ()
			(let ([lb (box 0)]
			      [rb (box 0)])
			  (send edit erase)
			  (send edit insert string)
			  (send edit position-location 
				(send edit last-position)
				rb)
			  (send edit position-location 0 lb)
			  '(printf "width: ~a ~a ~a ~a~n" 
				  (send canvas user-min-width)
				  lb rb (send time-edit last-position))
			  (send canvas user-min-width 
				(+ magic-space (- (unbox rb) (unbox lb))))))))]
	     [time-thread
	      (thread
	       (rec loop
		    (lambda ()
		      (update-time)
		      (sleep 30)
		      (loop))))])
	(lambda (super-info%)
	  (class super-info% args
	    (rename [super-make-root-panel make-root-panel])
	    (private
	      [rest-panel 'unitiaialized-root]
	      [super-root 'unitiaialized-super-root])
	    (public
	      [make-root-panel
	       (lambda (% parent)
		 (let* ([s-root (super-make-root-panel
				 mred:container:vertical-panel%
				 parent)]
			[r-root (make-object % s-root)])
		   (set! super-root s-root)
		   (set! rest-panel r-root)
		   r-root))])
	    
	    (rename [super-on-close on-close])
	    (private
	      [close-panel-callback
	       (mred:preferences:add-preference-callback
		'mred:show-status-line
		(lambda (p v)
		  (if v 
		      (register-gc-blit)
		      (wx:unregister-collecting-blit gc-canvas))
		  (send super-root change-children
			(lambda (l)
			  (if v
			      (list rest-panel info-panel)
			      (list rest-panel))))))]
	      [remove-pref-callback
	       (mred:preferences:add-preference-callback
		'mred:line-offsets
		(lambda (p v)
		  (edit-position-changed-offset v)
		  #t))])
	    (public
	      [on-close
	       (lambda ()
		 (and (super-on-close)
		      (begin (send time-canvas set-media null)
			     (wx:unregister-collecting-blit gc-canvas)
			     (remove-pref-callback)
			     (close-panel-callback)
			     #t)))])
	    
	    (public
	      [overwrite-status-changed
	       (let ([last-state? #f])
		 (lambda ()
		   (let ([overwrite-now? (send (get-info-edit)
					       get-overwrite-mode)])
		     (unless (eq? overwrite-now? last-state?)
		       (send overwrite-message
			     show
			     overwrite-now?)
		       (set! last-state? overwrite-now?)))))]
	      [anchor-status-changed
	       (let ([last-state? #f])
		 (lambda ()
		   (let ([anchor-now? (send (get-info-edit)
					    get-anchor)])
		     (unless (eq? anchor-now? last-state?)
		       (send anchor-message
			     show
			     anchor-now?)
		       (set! last-state? anchor-now?)))))]
	      [lock-status-changed
	       (let ([icon-currently-locked? #f])
		 (lambda ()
		   (let ([locked-now? (ivar (get-info-edit) locked?)])
		     (unless (eq? locked-now? icon-currently-locked?)
		       (mred:debug:printf 'lock-icon 
					  "lock-icon: setting to: ~a"
					  locked-now?)
		       (set! icon-currently-locked? locked-now?)
		       (let ([bitmap
			      (if locked-now?
				  (mred:icon:get-lock-bitmap)
				  (mred:icon:get-unlock-bitmap))])
			 (send lock-message
			       set-label
			       (if (send bitmap ok?)
				   bitmap
				   (if locked-now? "Locked" "Unlocked"))))))))]
	      
	      [edit-position-changed-offset
	       (lambda (offset?)
		 (let ([edit (get-info-edit)])
		   (let ([start (send edit get-start-position)]
			 [end (send edit get-end-position)]
			 [make-one
			  (lambda (pos)
			    (let* ([line (send edit position-line pos)]
				   [line-start (send edit line-start-position line)]
				   [char (- pos line-start)])
			      (format "~a:~a"
				      (if offset?
					  (add1 line)
					  line)
				      (if offset?
					  (add1 char)
					  char))))])
		     (when (object? position-edit)
		       (send* position-edit
			      (lock #f)
			      (erase)
			      (insert 
			       (if (= start end)
				   (make-one start)
				   (string-append (make-one start)
						  "-"
						  (make-one end))))
 			      (lock #t))))))]
	      [edit-position-changed
	       (lambda ()
		 (edit-position-changed-offset
		  (mred:preferences:get-preference 'mred:line-offsets)))])

	    (inherit get-edit)
	    (public
	      [get-info-edit
	       (lambda ()
		 (get-edit))])
	    (public
	      [update-info
	       (lambda ()
		 (overwrite-status-changed)
		 (anchor-status-changed)
		 (edit-position-changed)
		 (lock-status-changed))])
	    (sequence 
	      (apply super-init args))
	    
	    (private
	      [info-panel (make-object mred:container:horizontal-panel% 
				       super-root)]
	      [space (make-object mred:container:horizontal-panel% info-panel)]
	      [anchor-message 
	       (make-object mred:container:canvas-message%
			    info-panel
			    (let ([b (mred:icon:get-anchor-bitmap)])
			      (if (send b ok?)
				  b
				  "Anchor"))
			    -1 -1 wx:const-border)]
	      [overwrite-message 
	       (make-object mred:container:canvas-message%
			    info-panel
			    "Overwrite"
			    -1 -1 wx:const-border)]
	      [lock-message (make-object mred:container:canvas-message%
			      info-panel 
			      (let ([b (mred:icon:get-unlock-bitmap)])
				(if (send b ok?)
				    b
				    "Unlocked"))
			      -1 -1 wx:const-border)]
	      [position-canvas (make-object mred:canvas:one-line-canvas%
					    info-panel)]
	      [time-canvas (make-object mred:canvas:one-line-canvas% 
					info-panel)]
	      [gc-canvas (make-object mred:container:canvas% info-panel
				      -1 -1 -1 -1 wx:const-border)]
	      [position-edit (make-object mred:edit:media-edit%)]
	      [register-gc-blit
	       (lambda ()
		 (let ([mdc (mred:icon:get-gc-on-dc)])
		   (when (send mdc ok?)
		     (wx:register-collecting-blit gc-canvas 
						  0 0
						  (mred:icon:get-gc-width)
						  (mred:icon:get-gc-height)
						  (mred:icon:get-gc-on-dc)
						  (mred:icon:get-gc-off-dc)))))])

	    (sequence
	      (unless (mred:preferences:get-preference 'mred:show-status-line)
		(send super-root change-children
		      (lambda (l)
			(list rest-panel))))
	      (register-gc-blit)

	      (let ([bw (box 0)]
		    [bh (box 0)]
		    [gc-width (mred:icon:get-gc-width)]
		    [gc-height (mred:icon:get-gc-height)])
		(send* gc-canvas
		  (set-size 0 0 gc-width gc-height)
		  (get-client-size bw bh))
		(send* gc-canvas
		  (user-min-client-width gc-width)
		  (user-min-client-height gc-height)
		  (stretchable-in-x #f)
		  (stretchable-in-y #f)))
	      (send* info-panel 
		(stretchable-in-y #f)
		(spacing 3)
		(border 3))
	      (send anchor-message show #f)
	      (send overwrite-message show #f)
	      (send* position-canvas
		(set-media position-edit)
		(stretchable-in-x #f))
	      (send* time-canvas 
		(set-media time-edit)
		(stretchable-in-x #f))
	      (determine-width "0000:000-0000:000" 
			       position-canvas
			       position-edit)
	      (edit-position-changed)
	      (send position-edit lock #t)
	      (semaphore-wait time-semaphore)
	      (determine-width wide-time time-canvas time-edit)
	      (semaphore-post time-semaphore)
	      (update-time))))))
    
    (define info-frame% (make-info-frame% searchable-frame%)))
