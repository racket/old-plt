#|

A bundle-table% is a mapping from names to bundles. It supports these methods:
  
  extend : (symbol bundle-manager% -> void)
    adds a new binding from the symbol to a bundle.
  
  lookup : (symbol -> (union #f bundle-manager%))
    looks up a bundle
  
  create-view : (-> void)
    opens a frame displaying the mappings of names to bundles in
    this table. If a frame has already been created, just shows that
    frame.
    
    All changes to the table of the bundle rare reflected in the frame.

-- 

A bundle-table-frame% provides the view of a bundle-table%

It's single initialization is argument is a bundle-table%. It supports:

   extend : ((symbol bundle-manager%) -> void)
   
      called when the bundle-table is extended

   delete : (symbol -> void)
   
      called when an entry in the bundle-table is removed

      
--    

A bundle-manager% is an manager class. It keeps track of an instance
of a Bundle Contents and the views associated with that instance.

It takes no initialization arguments.

It supports three methods:

  get-bundle : (-> (union #f bundle<%>))
    returns the bundle that this manager manages or #f
    if this manager is not yet managing a bundle.

  set-bundle : ((union #f bundle<%>) -> void)
    sets the bundle that this manager manages;
    #f indicates that no bundle is managed.

  create-view : (bundle-table-frame% (snip -> void) -> void)
    constructs a snip that displays the Bundle that this manager
    manages. Calls its argument with the snip, which must insert
    the snip into an editor%.
  
  bundle-changed : (-> void)
    notifies all views of this bundle to update themselves.

-- 

A Bundle is a tree representing the structure of the bundle,
using the interpreter pattern:

            bundle%
              |
       +--------------+
       |              |
 node-bundle%   leaf-bundle%
  

 bundle% supports:
   

    set-bundle-manager : (bundle-manager<%> -> void)
      sets the anchor (used to notify views of modifications)

    traverse : ((bundle A -> A) A -> A)
      visits the entire tree (leaves and nodes) by calling the function argument
      at each node. The second argument is used as the initial second argument to
      the function argument, after that the result of one call to the function is used
      as the input for the next call to the function.

    get-flat-names : (-> (list-of symbols))
      

  leaf-bundle% supports:
  
     get-names : (-> (list-of symbol))
     set-names : ((list-of symbol) -> void)

  node-bundle% supports:
    
    get-label     : (-> symbol)
    set-label     : (symbol -> void)
    set-children  : ((list-of bundle%) -> void)
    get-children  : (-> (list-of bundle%))
    add-child     : (bundle% -> void)

--

bundle-frame%

    enable-children : (boolean -> void)
      Enables or disables the buttons that allow creation of new children nodes.
    
bundle-pasteboard%

leaf-bundle-snip%
node-bundle-snip%
    
|#

(unit/sig drscheme:bundle:bundle^
  (import mred^
          drscheme:bundle:misc^)
  
  (define bundle-manager<%>
    (interface ()
      get-bundle
      create-view
      bundle-changed))
  
  (define bundle-manager%
    (class* object% (bundle-manager<%>) ()
      (private
        [contents #f]
        [views null]
        [interior-height-addition 10]
        [calculate-tree-size
         (lambda (view)
           (let o-loop ([contents-snip (send view get-contents-snip)])
             (let ([contents (send contents-snip get-bundle)])
               (cond
                 [(is-a? contents-snip leaf-bundle-snip%)
                  (let ([xl (box 0)]
                        [xr (box 0)]
                        [yt (box 0)]
                        [yb (box 0)])
                    (send view get-snip-location contents-snip xl yt #f)
                    (send view get-snip-location contents-snip xr yb #t)
                    (let ([w (- (unbox xr) (unbox xl))]
                          [h (- (unbox yb) (unbox yt))])
                      (send contents set-tree-width w)
                      (send contents set-tree-height h)
                      (values w h)))]
                 [(is-a? contents-snip node-bundle-snip%)
                  (let i-loop ([bundle
                                (send contents-snip get-bundle-snips)]
                               [width 0]
                               [height 0])
                    (cond
                      [(null? bundle)
                       (let ([xl (box 0)]
                             [yt (box 0)]
                             [xr (box 0)]
                             [yb (box 0)])
                         (send view get-snip-location contents-snip xl yt #f)
                         (send view get-snip-location contents-snip xr yb #t)
                         (let ([w (max width (- (unbox xr) (unbox xl)))]
                               [h (+ height interior-height-addition
                                     (- (unbox yb) (unbox yt)))])
                           (send contents set-tree-width w)
                           (send contents set-tree-height h)
                           (values w h)))]
                      [else (let*-values ([(c-width c-height) (o-loop (car bundle))])
                              (i-loop (cdr bundle)
                                      (+ c-width width)
                                      (max c-height height)))]))]
                 [else (error 'position-view-contents
                              "fell off cond: ~e~n"
                              contents-snip)]))))]
        [position-snips
         (lambda (view)
           (let o-loop ([contents-snip (send view get-contents-snip)]
                        [x 0]
                        [y 0])
             (cond
               [(is-a? contents-snip leaf-bundle-snip%)
                (send view move-to contents-snip x y)]
               [(is-a? contents-snip node-bundle-snip%)
                
                ;; set this snips position
                (let* ([bundle (send contents-snip get-bundle)]
                       [tree-width (send bundle get-tree-width)]
                       [width (send contents-snip get-width)])
                  (send view move-to contents-snip (+ x (/ (- tree-width width) 2)) y))
                
                ;; loop over children
                (let ([text-space (let ([yt (box 0)]
                                        [yb (box 0)])
                                    (send view get-snip-location contents-snip #f yt #f)
                                    (send view get-snip-location contents-snip #f yb #t)
                                    (- (unbox yb) (unbox yt)))])
                  (let i-loop ([bundle-snips (send contents-snip get-bundle-snips)]
                               [x x])
                    (cond
                      [(null? bundle-snips) (void)]
                      [else (let* ([bundle-content-snip (car bundle-snips)]
                                   [bundle-content (send bundle-content-snip get-bundle)]
                                   
                                   [tree-width (send bundle-content get-tree-width)]
                                   [tree-height (send bundle-content get-tree-width)])
                              (o-loop bundle-content-snip 
                                      x 
                                      (+ y interior-height-addition text-space))
                              (i-loop (cdr bundle-snips)
                                      (+ x tree-width)))])))]
               [else (error 'position-snips "fell off cond: ~e~n" contents-snip)])))]
        [build-view-contents
         (lambda (view)
           (let loop ([contents contents])
             (cond
               [(is-a? contents leaf-bundle%)
                (let ([snip (make-object leaf-bundle-snip% contents)])
                  (send view insert snip 0 0)
                  snip)]
               [(is-a? contents node-bundle%)
                (let ([snip (make-object node-bundle-snip%
                                         contents
                                         (map loop (send contents get-children)))])
                  (send view insert snip 0 0)
                  snip)]
               [else (error 'create-view "fell off cond: ~e~n" contents)])))])
      (public
        [get-bundle (lambda () contents)]
        [set-bundle (lambda (b) 
                      (set! contents b)
                      (message-box "0" (format "setting view ~s" b))
                      (bundle-changed))]
        [create-view
         (lambda (frame insert-into-editor)
           (let* ([view (make-object bundle-pasteboard% frame this)]
                  [snip (make-object editor-snip% view)])
             (insert-into-editor snip)
             (when contents
               (send view set-contents-snip (build-view-contents view))
               (calculate-tree-size view)
               (position-snips view))
             (set! views (cons view views))
             snip))]
        [bundle-changed
         (lambda ()
           (let ([update-view
                  (lambda (view)
                    (message-box "1" (format "updating view ~s~n" view))
                    '(send view begin-edit-sequence)
                    (let ([old-snips
                           (let loop ([snip (send view find-first-snip)])
                             (if snip
                                 (cons snip (loop (send snip next)))
                                 null))])
                      
                      ;; delete the old snips (why does this break things?)
                      (for-each
                       (lambda (snip) (send view delete snip))
                       old-snips)
                      
                      ;; build new ones
                      (send view set-contents-snip (build-view-contents view))
                      (calculate-tree-size view)
                      (position-snips view))
                    '(send view end-edit-sequence))])
             
             (for-each update-view views)))])
      (sequence
        (super-init)
        (when contents
          (send contents traverse
                (lambda (c x) (send c set-bundle-manager this))
                (void))))))
  
  (define bundle<%>
    (interface ()
      set-bundle-manager
      get-flat-names
      traverse ; ((bundle A -> A) A -> A)
      ))
  
  (define bundle%
    (class* object% (bundle<%>) ()
      
      (private
        [tree-width 0]
        [tree-height 0])
      (public
        [get-tree-width (lambda () tree-width)]
        [get-tree-height (lambda () tree-height)]
        [set-tree-width (lambda (w) (set! tree-width w))]
        [set-tree-height (lambda (h) (set! tree-height h))])
      
      (private
        [bundle-manager #f])
      (public
        [get-bundle-manager
         (lambda ()
           bundle-manager)]
        [set-bundle-manager
         (lambda (b)
           (unless (is-a? b bundle-manager<%>)
             (error 'set-bundle-manager "expected a bundle-manager<%>, got: ~e"
                    b))
           (set! bundle-manager b))]
        [traverse
         (lambda (f init)
           (void))])
      (sequence
        (super-init))
      
      (public
        [get-flat-names
         (lambda ()
           (error 'get-flat-names "abstract-method"))])))
  
  (define leaf-bundle<%>
    (interface (bundle<%>)
      get-names
      set-names))
  
  (define leaf-bundle%
    (class* bundle% (leaf-bundle<%>) (_names)
      (private
        [names _names])
      (override
	[traverse
	 (lambda (f init)
	   (f this init))])
      (public
        [get-names
         (lambda ()
           names)]
        [set-names
         (lambda (n)
           (unless (and (list? n)
                        (andmap symbol? n))
             (error 'set-names "expected a list of symbols, got: ~e"
                    n))
           (set! names n))])
      (sequence
        (super-init)
        (set-names _names))))
  
  (define node-bundle<%>
    (interface (bundle<%>)
      get-label
      set-label
      set-children
      get-children
      add-child))
  
  (define node-bundle%
    (class bundle% (_label _children)
      (override
	[traverse
	 (lambda (f init)
	   (let loop ([contents children]
		      [init init])
	     (cond
	       [(null? contents) (f this init)]
	       [else (loop
		      (cdr contents)
		      (send (car contents)
			    traverse
			    (lambda (object init) (f object init))
			    init))])))])
      (private
        [label _label]
        [children _children])
      (public
        [get-label
         (lambda ()
           label)]
        [set-label
         (lambda (s)
           (unless (symbol? s)
             (error 'set-names "expected a list of symbols, got: ~e"
                    s))
           (set! label s))]
        [get-children
         (lambda ()
           children)]
        [set-children
         (lambda (chils)
           (unless (and (list? chils)
                        (andmap (lambda (x) (is-a? x bundle<%>)) chils))
             (error 'set-names "expected a list of bundle<%> objects, got: ~e" chils))
           (set! children chils))])
      
      (inherit get-bundle-manager)
      (public
        [add-child
         (lambda (new-child)
           (set! children (cons new-child children))
           (send (get-bundle-manager) bundle-changed))])
      (sequence
        (super-init)
        (set-label _label)
        (set-children _children))))
  
  (define bundle-pasteboard%
    (class pasteboard% (frame bundle-manager)
      (private
        [contents-snip #f])
      (public
        [get-bundle-manager (lambda () bundle-manager)]
        [get-contents-snip (lambda () contents-snip)]
        [set-contents-snip (lambda (c) (set! contents-snip c))])
      
      (inherit get-snip-location)
      (private
        [get-snip-x-location
         (lambda (snip)
           (let ([xl (box 0)]
                 [xr (box 0)])
             (get-snip-location snip xl #f #f)
             (get-snip-location snip xr #f #t)
             (floor (+ (unbox xl) (/ (- (unbox xr) (unbox xl)) 2)))))]
        [get-snip-top-location
         (lambda (snip)
           (let ([yt (box 0)])
             (get-snip-location snip #f yt #f)
             (unbox yt)))]
        [get-snip-bottom-location
         (lambda (snip)
           (let ([yb (box 0)])
             (get-snip-location snip #f yb #t)
             (unbox yb)))])


      (inherit find-first-snip)
      (rename [super-on-focus on-focus])
      (override
	[on-focus
	 (lambda (on?)
           (unless (find-first-snip)
             (send frame enable-children on?))
	   (super-on-focus on?))]
        [after-select
         (lambda (snip on?)
           (super-after-select snip on?)
           (cond
             [(not on?)
              (send frame enable-children #f)]
             [(is-a? snip node-bundle-snip%)
              (send frame enable-children #t)]
             [else (void)]))])

      (inherit invalidate-bitmap-cache)
      (rename [super-after-select after-select])
      (override
	[after-move-to
	 (lambda (snip x y dragging)
	   (invalidate-bitmap-cache))]
	[on-paint
	 (lambda (before? dc left top right bottom dx dy draw-caret)
	   (when (and contents-snip
		      (not before?))
	     (let ([pen (send dc get-pen)])
	       (set-dc-pen dc "BLUE" 1 'solid)
	       (let o-loop ([contents-snip contents-snip])
		 (cond
		   [(is-a? contents-snip leaf-bundle-snip%) (void)]
		   [(is-a? contents-snip node-bundle-snip%)
		    (let ([x (get-snip-x-location contents-snip)]
			  [y (get-snip-bottom-location contents-snip)])
		      (let i-loop ([bundle-snips
				    (send contents-snip get-bundle-snips)])
			(cond
			  [(null? bundle-snips) (void)]
			  [else
			   (let* ([bundle-content-snip (car bundle-snips)])
			     (let ([bx (get-snip-x-location bundle-content-snip)]
				   [by (get-snip-top-location bundle-content-snip)])
			       (send dc draw-line (+ x dx) (+ y dy) (+ bx dx) (+ by dy))
			       (o-loop bundle-content-snip)
			       (i-loop (cdr bundle-snips))))])))]
		   [else (error 'on-paint "fell off cond: ~e~n" contents-snip)]))
	       (send dc set-pen pen))))])
      (sequence
        (super-init))))
  
  (define leaf-bundle-snip%
    (class editor-snip% (leaf-bundle)
      (public
        [get-bundle
         (lambda ()
           leaf-bundle)])
      (private
        [text (make-object text%)]
        [update-text
         (lambda ()
           ;(send text begin-edit-sequence)
           (let ([names (send leaf-bundle get-names)])
             (unless (null? names)
               (send text insert (symbol->string (car names)))
               (for-each (lambda (name)
                           (send text insert #\newline)
                           (send text insert (symbol->string name)))
                         (cdr names))))
           ;(send text end-edit-sequence)
           )])
      (sequence
        (update-text)
        (super-init text))))
  
  (define node-bundle-snip%
    (class snip% (node-bundle bundle-snips)
      (public
        [get-bundle
         (lambda ()
           node-bundle)]
        [get-bundle-snips
         (lambda ()
           bundle-snips)])
      (private
        [width 10]
        [height 10])
      (public
        [get-width (lambda () width)]
        [get-height (lambda () height)])
      (override
        [get-extent
	 (lambda (dc x y w h descent space lspace rspace)
	   
	   (let-values ([(tw th _1 _2)
			 (send dc get-text-extent
			       (symbol->string (send node-bundle get-label)))])
	     (set! width tw)
	     (set! height th))
	   
	   (set-box/f! w width)
	   (set-box/f! h height)
	   (set-box/f! descent 0)
	   (set-box/f! space 0)
	   (set-box/f! lspace 0)
	   (set-box/f! rspace 0))]
	[draw
	 (lambda (dc x y left top right bottom dx dy draw-caret)
	   (let ([foreground (send dc get-text-foreground)]
		 [background (send dc get-text-background)]
		 [mode (send dc get-text-mode)])
	     (send dc set-text-foreground black)
	     (send dc set-text-background white)
	     (send dc set-text-mode 'solid)
	     (send dc draw-text (symbol->string (send node-bundle get-label)) x y)
	     (send dc set-text-mode mode)
	     (send dc set-text-foreground foreground)
	     (send dc set-text-background background)))])
      (sequence
        (super-init))))
  
  (define bundle-table%
    (class object% ()
      (private
       [table null]
       [view #f])
      (public
        [extend
         (lambda (name contents)
           (when view
             (send view extend name contents))
           (set! table (cons (cons name contents) table)))]
        [remove
         (lambda (name)
           (when view
             (send view remove name))
           (set! table
                 (let loop ([table table])
                   (cond
                     [(null? table) null]
                     [else (let ([ent (car table)])
                             (if (eq? (car ent) name)
                                 (cdr table)
                                 (cons (car table)
                                       (loop (cdr table)))))]))))]
        [lookup
         (lambda (name)
           (let ([ans (memq name table)])
             (if ans
                 (cdr ans)
                 #f)))])
      (public
        [create-view
         (lambda ()
           (set! view (make-object bundle-table-frame% this))
           (send view show #t))])
      (sequence (super-init))))
           
  
  (define bundle-table-frame%
    (class frame% (bundle-table)
      (public
        [enable-children
         (lambda (x)
           (send new-leaf-button enable x)
           (send new-node-button enable x))])
      (sequence
        (super-init "Bundles" #f 400 400))
      (private
        [button-panel (make-object horizontal-panel% this)]
        [text (make-object text%)]
        [new-bundle-button (make-object button%
                             "New Bundle"
                             button-panel
                             (lambda x
                               (new-bundle)))]
        [new-leaf-button (make-object button% "New Leaf" button-panel (lambda x (new-leaf)))]
        [new-node-button (make-object button% "New Node" button-panel (lambda x (new-node)))]
        [canvas (make-object editor-canvas% this text)]
        
        [new-leaf
         (lambda ()
           (let/ec k
             (message-box "-2" "new-leaf")
             (let ([out (lambda () (bell) (k #f))]
                   [snip (send text get-focus-snip)])
               (unless (is-a? snip editor-snip%)
                 (out))
               (let ([pb (send snip get-editor)])
                 (unless (is-a? pb bundle-pasteboard%)
                   (out))
                 (let ([snip (send pb find-next-selected-snip #f)])
                   (cond
                     [(not snip)
                      (let ([manager (send pb get-bundle-manager)])
                        (message-box "-1" (format "manager: ~s" (list manager (send manager get-bundle))))
                        (when (send manager get-bundle)
                          (out))
                        (send manager set-bundle (make-object leaf-bundle% '())))]
                     [(and (is-a? snip node-bundle-snip%)
                           (not (send pb find-next-selected-snip snip)))
                      (let ([node-bundle (send snip get-bundle)])
                        (send node-bundle add-child (make-object leaf-bundle% '())))]
                     [else (out)]))))))]
        
        [new-node void]
        
        [new-bundle
         (lambda ()
           (let ([name (get-text-from-user "New bundle" "Name of new bundle")]
                 [bundle-manager (make-object bundle-manager%)])
             (send text insert name)
             (send bundle-manager create-view this (lambda (snip) (send text insert snip)))
             (send text insert #\newline)))])
      (sequence
        (send button-panel stretchable-height #f)
        (enable-children #f))))

  (define (new-bundle-table-frame)
    (send (make-object bundle-table-frame% (make-object bundle-table%)) show #t))
  )