
  (unit/sig mred:menu^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mzlib:function : mzlib:function^])
	    
    (mred:debug:printf 'invoke "mred:menu@")

    (define max-manual-menu-id 1000)

    (define generate-menu-id
      (let ([id (add1 max-manual-menu-id)])
	(lambda ()
	  (begin0 id
		  (set! id (add1 id))))))

    (define parse-key
      (lambda (str)
	(letrec ([find-next
		  (lambda (l p)
		    (cond [(null? l) (values null null)]
			  [else (if (p (car l))
				    (values null (cdr l))
				    (let-values ([(token rest) (find-next (cdr l) p)])
				      (values (cons (car l) token) rest)))]))]
		 [handle-modifier
		  (lambda (s)
		    (cond
		      [(string=? "c" s) (case wx:platform
					  [(macintosh) (error 'parse-key "cannot bind control to a menu-item on the macintosh")]
					  [else "Ctrl+"])]
		      [(string=? "d" s) (case wx:platform
					  [(macintosh) "Cmd+"]
					  [else (error 'parse-key "cannot bind command to a menu-item except on the macintosh")])]
		      [(string=? "s" s) (case wx:platform
					  [(macintosh) (error 'parse-key "cannot bind shift to a menu-item on the macintosh")]
					  [else "Shft+"])]
		      [(string=? "m" s) (case wx:platform
					  [(macintosh) (error 'parse-key "cannot bind meta to a menu-item on the macintosh")]
					  [else "Meta+"])]
		      [(string=? "a" s) (case wx:platform
					  [(macintosh) (error 'parse-key "cannot bind alt to a menu-item on the macintosh")]
					  [else "Alt+"])]
		      [else (error 'parse-key "unrecognised modifier key: ~a in string: ~a" s str)]))]
		 [capitalize 
		  (lambda (s)
		    (cond
		      [(and (= (length s) 1) (eq? wx:platform 'unix)) s]
		      [(null? s) null]
		      [else (cons (char-upcase (car s)) (cdr s))]))]
		 [handle-key-combo
		  (lambda (l)
		    (let-values ([(modifier rest) (if (member #\: l)
						      (find-next l (lambda (x) (char=? x #\:)))
						      (values null l))])
		      (let-values ([(key rest) (find-next rest (lambda (x) (char=? x #\;)))])
			(let ([mod (if (null? modifier) 
				       ""
				       (handle-modifier (apply string modifier)))])
			  
			  (values (string-append mod
						 (apply string (capitalize key)))
				  rest)))))]
		 [handle-string
		  (lambda (l)
		    (let-values ([(combo rest) (find-next l (lambda (x) (char=? x #\;)))])
		      (let-values ([(combo-string rest2) (handle-key-combo combo)])
			(unless (null? rest2)
			  (error 'parse-key "uncomprehended key-string: ~a (problems from ~a)" str rest2))
			(if (null? rest)
			    combo-string
			    (string-append combo-string " " (handle-string rest))))))])
	  (handle-string (string->list str)))))

    (define make-menu%
      (lambda (super%)
	(class super% ([title null] [func #f])
	  (inherit check)
	  (rename [super-append append]
		  [super-delete delete])
	  (private
	    [submenus ()]
	    [callbacks ()])
	  (public
	    [menu-bar #f]
	    [set-menu-bar
	     (lambda (mb)
	       (set! menu-bar mb))]
	    [append
	     (lambda (id . args)
	       (let ([id (if (negative? id)
			     (generate-menu-id)
			     id)])
		 (apply super-append id args)
		 id))]
	    [append-item
	     (opt-lambda (label callback [help ()] [checkable? #f] [key #f])
	       (let* ([key-proc (cond
				 [(procedure? key) key]
				 [(string? key) (lambda (platform)
						  (case platform
						    [(macintosh) (string-append "d:" key)]
						    [(windows) (string-append "c:" key)]
						    [else (string-append "c:m;" key)]))]
				 [(not key) (lambda (s) #f)]
				 [else (error 'mred:menu% "append-item: last arg (key) must be either #f, a procedure or a string. Args were: ~a"
					      (list label callback help checkable? key))])]
		      [this-key (key-proc wx:platform)]
		      [platforms (list 'unix  'windows  'macintosh)]
		      [label-with-key (if this-key 
					  (string-append label 
							 (string #\tab) 
							 (parse-key this-key))
					  label)]
		      [id (append -1 label-with-key help checkable?)])
		 (when (and (not menu-bar)
			    (ormap key-proc platforms))
		   (error 'mred:menu% "append-item: must add the menu to a menubar before appending items when keybings are involved"))
		 (set! callbacks (cons (cons id callback) callbacks))
		 (when menu-bar
		   (for-each (let ([keymap-string (string-append "append-item:" (number->string id) "/")])
			       (lambda (symbol)
				 (let ([keymap (send menu-bar get-platform-menu-keymap symbol)]
				       [key (key-proc symbol)])
				   (when key
				     (let ([name (string-append keymap-string key)])
				       (send keymap add-key-function name (lambda (x y) (callback)))
				       (send keymap map-function key name))))))
			     platforms))
		 id))]
	    [append-menu
	     (opt-lambda (label menu [help ()])
	       (let ([id (append -1 label menu help)])
		 (set! submenus (cons (cons id menu) submenus))
		 (when menu-bar
		   (send menu set-menu-bar menu-bar))
		 id))]
            [set-callback
	     (lambda (id cb)
                (let [(pair (assoc id callbacks))]
                  (and pair (set-cdr! pair cb))))]
	    [append-check-set
	     (opt-lambda (name-tag-list callback [initial 0] [help ()])
	       (let* ([id-list
		       (map (lambda (name-tag)
			      (let ([name (if (pair? name-tag)
					      (car name-tag)
					      name-tag)])
				(append-item name 'tmp help #t)))
			    name-tag-list)]
		      [old-selected-id 0]
		      [make-item-callback
		       (lambda (name-tag id)
			 (let ([tag (if (pair? name-tag)
					(cdr name-tag)
					name-tag)])
			   (lambda ()
			     (check id #t)
			     (unless (= old-selected-id id)
			       (check old-selected-id #f)
			       (set! old-selected-id id)
			       (callback tag)))))])
		 (map (lambda (name-tag id)
			(let ([cb (make-item-callback name-tag id)]
			      [pair (assoc id callbacks)])
			  (set-cdr! pair cb)))
		      name-tag-list id-list)
		 (set! old-selected-id (list-ref id-list initial))
		 (check old-selected-id #t)
		 id-list))]
	    [delete
	     (lambda (id)
	       (super-delete id)
	       (set! submenus (mzlib:function:remove id submenus
						     (lambda (id pair)
						       (= (car pair) id))))
	       (set! callbacks (mzlib:function:remove id callbacks
						      (lambda (id pair)
							(= (car pair) id)))))]
	    [dispatch
	     (lambda (id)
	       (or (ormap (lambda (pair)
			    (send (cdr pair) dispatch id))
			  submenus)
		   (let ([v (assoc id callbacks)])
		     (if v
			 ((cdr v))
			 #f))))])

	  (sequence
	    (super-init title (or func
				  (lambda (menu evt)
				    (dispatch (send evt get-command-int)))))))))

    (define menu% (make-menu% wx:menu%))

    (define make-menu-bar%
      (lambda (super%)
	(class super% ([init-menus null]
		       [init-titles null])
	  (inherit enable-top)
	  (rename [super-append append]
		  [super-delete delete])
	  (private
	    [menus init-menus]
	    [keymap%
	     (class-asi wx:keymap%
	       (rename [super-map-function map-function])
	       (public
		 [map-function
		  (lambda args
		    '(printf "map-functinon: ~a~n" args)
		    (apply super-map-function args))]))]
	    [macintosh-keymap (make-object keymap%)]
	    [windows-keymap (make-object keymap%)]
	    [unix-keymap (make-object keymap%)])
	  (sequence
	    (send macintosh-keymap set-error-callback 
		  (lambda (x) (error 'macintosh-menu-keymap x)))
	    (send windows-keymap set-error-callback 
		  (lambda (x) (error 'windows-menu-keymap x)))
	    (send unix-keymap set-error-callback 
		  (lambda (x) (error 'unix-menu-keymap x))))

	  (public
	    [frame #f]
	    [set-frame (lambda (f) 
			 (set! frame f)
			 (send (ivar frame keymap) chain-to-keymap menu-keymap #t))]
	    [get-platform-menu-keymap
	     (lambda (sym)
	       (case sym
		 [(macintosh) macintosh-keymap]
		 [(windows) windows-keymap]
		 [(unix) unix-keymap]
		 [else (error 'menu-frame% "get-platform-menu-keymap, unrecognised platform: ~a"
			      sym)]))]
	    [menu-keymap (get-platform-menu-keymap wx:platform)]
	    [append
	     (lambda (menu name)
	       (when (not (ivar menu menu-bar))
		 (super-append menu name)
		 (set! menus (#%append menus (list menu)))
		 (send menu set-menu-bar this)))]
	    [get-nth-menu
	     (lambda (pos)
	       (if (< pos (length menus))
		   (list-ref menus pos)
		   (error 'get-nth-menu "arg too large, got: ~a, only ~ a menus."
			  pos (length menus))))]
	    [delete
	     (opt-lambda (menu [pos 0])
	       (if (null? menu)
		   (when (< pos (length menus))
		     (delete (list-ref menus pos)))
		   (when (member menu menus)
		     (super-delete menu)
		     (send menu set-menu-bar #f)
		     (set! menus (mzlib:function:remove menu menus)))))]
	    [enable-all
	     (lambda (on?)
	       (let loop ([i (length menus)])
		 (unless (zero? i)
		   (enable-top (sub1 i) on?)
		   (loop (sub1 i)))))]
	    [dispatch
	     (lambda (op)
	       (let loop ([menus menus])
		 (if (null? menus)
		     #f
		     (if (send (car menus) dispatch op)
			 #t
			 (loop (cdr menus))))))])
	  (sequence
	    (super-init init-menus init-titles)))))
    (define menu-bar% (make-menu-bar% wx:menu-bar%)))
