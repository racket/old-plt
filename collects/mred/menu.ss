(define mred:menu@
  (unit/sig mred:menu^
    (import [mred:debug : mred:debug^]
	    [mzlib:function : mzlib:function^])
	    
    (mred:debug:printf 'invoke "mred:menu@")

    (define max-manual-menu-id 1000)

    (define generate-menu-id
      (let ([id (add1 max-manual-menu-id)])
	(lambda ()
	  (begin0
	   id
	   (set! id (add1 id))))))

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
	     (opt-lambda (label callback [help ()] [checkable? #f])
	       (let ([id (append -1 label help checkable?)])
		 (set! callbacks (cons (cons id callback)
				       callbacks))
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
						     (lambda (pair)
						       (= (car pair) id))))
	       (set! callbacks (mzlib:function:remove id callbacks
						      (lambda (pair)
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
		    [menus ()])
		   (public
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
    (define menu-bar% (make-menu-bar% wx:menu-bar%))
    ))



			
	   
       
	       