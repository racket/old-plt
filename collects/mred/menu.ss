(define mred:menu@
  (unit/sig mred:menu^
    (import [mred:debug : mred:debug^]
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
						      (values null l))]
				 [(key rest) (find-next rest (lambda (x) (char=? x #\;)))])
		      (let ([mod (if (null? modifier) 
				     ""
				     (handle-modifier (apply string modifier)))])
			
			(values (string-append mod
					       (apply string (capitalize key)))
				rest))))]
		 [handle-string
		  (lambda (l)
		    (let-values ([(combo rest) (find-next l (lambda (x) (char=? x #\;)))]
				 [(combo-string rest2) (handle-key-combo combo)])
		      (unless (null? rest2)
			(error 'parse-key "uncomprehended key-string: ~a (problems from ~a)" str rest2))
		      (if (null? rest)
			  combo-string
			  (string-append combo-string " " (handle-string rest)))))])
	  (handle-string (string->list str)))))

    (define make-menu%
      (lambda (super%)
	(class-asi super%
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
		      [label-with-key (if this-key 
					  (string-append label 
							 (string #\tab) 
							 (parse-key this-key))
					  label)]
		      [id (append -1 label-with-key help checkable?)])
		 (unless menu-bar
		   (error 'mred:menu% "append-item: must set the menubar before appending items"))
		 (set! callbacks (cons (cons id callback) callbacks))
		 (for-each (let ([keymap-string (string-append "append-item:" (number->string id) "/")])
			     (lambda (symbol)
			       (let ([keymap (send menu-bar get-platform-menu-keymap symbol)]
				     [key (key-proc symbol)])
				 (when key
				   (let ([name (string-append keymap-string key)])
				     (send keymap add-key-function name (lambda (x y) (callback)))
				     (send keymap map-function key name))))))
			   (list 'unix  'windows  'macintosh))
		 id))]
	    [append-menu
	     (opt-lambda (label menu [help ()])
	       (let ([id (append -1 label menu help)])
		 (set! submenus (cons (cons id menu) submenus))
		 id))]
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
			 #f))))]))))

    (define menu% (make-menu% wx:menu%))

    (define make-menu-bar%
      (lambda (super%)
	(class-asi super%
	  (inherit enable-top)
	  (rename [super-append append]
		  [super-delete delete])
	  (private
	    [menus ()]
	    [macintosh-keymap (make-object wx:keymap%)]
	    [windows-keymap (make-object wx:keymap%)]
	    [unix-keymap (make-object wx:keymap%)])
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
	    [delete
	     (opt-lambda (menu [pos 0])
	       (if (null? menu)
		   (if (< pos (length menus))
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
			 (loop (cdr menus))))))]))))
    (define menu-bar% (make-