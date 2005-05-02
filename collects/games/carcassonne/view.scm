;; a global graphical view of the Carcassonne map and the game messages
#cs
(module view mzscheme
  (require "if.scm"
           "tiles.scm"
           "tile-info.scm"
           (lib "mred.ss" "mred")
           (lib "pretty.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss"))
  
  (define STILE 50)
  (define WTEXT 200)
  (define SIZE  (+ (* 10 STILE) WTEXT))
  
  ;; the pasteboard for the map 
  (define carcassonne-map%
    (class* pasteboard% (observer<%>)
      (super-new)
      (init-field admin)
      (field [graph0 (send admin get-graph)])
      
      ;; --- lock down the pieces ---  
      (send this set-dragable #f)
      (define/augment (can-interactive-resize? snip) #f)
      (define/augment (can-interactive-move? me) #f)
      (define/augment (can-select? x b) #f)
      
      ;(rename [super-insert insert])
      (define/override (insert t)
        (define x (* STILE (- (send t get-x) x0)))
        (define y  (* STILE (- (send t get-y) y0)))
        (super insert (send t snip) x y))
      
      ;; -> String
      ;; effect: save the map part in a file
      ;; produce name 
      (define/public (to-file) 
        (define bm   (make-object bitmap% 400 400 #f))
        (define bmdc (new bitmap-dc% (bitmap bm)))
        (define file 
          (let ([file (put-file)])
            (if (and (string? file) (regexp-match "/([a-zA-Z0-9]*).jpg" file))
                file
                ; default: 
                "example.jpg")))
        (define name 
          (cond
            [(regexp-match "/([a-zA-Z0-9]*).jpg" file) => cadr]
            [else file]))                      
        (printf "despositing image in ~a~n" file)
        (send this print-to-dc bmdc)
        (send bm save-file file 'jpeg 75)
        (string->symbol name))
      
      ;; the leftmost coordinate of the last argument to draw-graph 
      (define x0 0)
      ;; the topmost coordinate of the last argument to draw-graph 
      (define y0 0)
      
      ;; drawable-graph<%> -> Void
      ;; draw a graph into the editor 
      (define/public (draw-graph)
        (set! x0 (- (send graph0 left) 1))
        (set! y0 (- (send graph0 top) 1))
        (send this begin-edit-sequence)
        (send this erase)
        (for-each (lambda (t) (insert t)) (send graph0 list-of-tiles))
        (send this end-edit-sequence))
      
      (define/public (placed-tile t) 
        (define g (send admin get-graph))
        (let* ([x (- (send g left) 1)]
               [y (- (send g top) 1)])
          (if (and (= x x0) (= y y0))
              (insert t)
              (begin (set! graph0 g) (draw-graph)))))
      
      (define/public (placed-follower t f p) (insert t))

      (define/public (other-score-and-token . x) (void))))
  
  ;; --- the actual GUI ---
  
  ;; admin<%>  -> Void
  ;; set up a frame for displaying the graph during a Carcassonne game 
  (define view%
    (class* object% (observer<%>)
      (super-new)
      (init-field admin)
      
      (define fram (new frame% (label "Carcassonne") (width SIZE) (height SIZE)))
      ;; --- menu bar ---
      (define mb (new menu-bar% (parent fram)))
      (define m (new menu% (label "File") (parent mb)))
      (define s1 (new menu-item% 
                      (label "Save")
                      (parent m) 
                      (callback (lambda _ (send cmap to-file)))))
      (define s2 (new menu-item% 
                      (label "Export") 
                      (parent m)
                      (callback (lambda _ (send (send admin get-graph) creation 'example)))))
      ;; --- pane --- 
      (define pane (new horizontal-pane% (parent fram) (alignment '(left top))))
      ;; --- the player status in pane --- 
      (define stat (new vertical-pane% 
                        (parent pane)
                        (min-width WTEXT)
                        (stretchable-width #f)
                        (alignment '(left top))))
      (define mesg (new message% (parent stat) (label "Player Actions")))
      (define slab (let ([t (new text-field% 
                                 (parent stat)
                                 (label #f)                                 
                                 (callback void)
                                 (style '(multiple)))])
                     (define (add-text s)
                       (define old (send t get-value))
                       (send t set-value (string-append "\n" s old)))
                     add-text))
      
      ;; --- the map in pane --- 
      (define cmap (let ([c (new editor-canvas% (parent pane))]
                         [cmap (new carcassonne-map% [admin admin])])
                     (send c set-editor cmap)
                     cmap))
      ;; --- the observer interface --- 
      (define/public (placed-follower t f p)
        (slab 
         (format "player ~a~n tile (~s,~s)~n position ~s~n" 
                 f (send t get-x) (send t get-y) p))
        (send cmap placed-follower t p f))
      (define/public (placed-tile t)
        (slab
         (format "tile (~s,~s)~n"
                 (send t get-x) (send t get-y)))
        (send cmap placed-tile t))
      (define/public (other-score-and-token f s n)
        (slab
         (format "player ~a~n scored ~s points~n got ~s followers back~n" f s n)))
      
      (send admin register-observer this)
      (send fram show #t)))
  
  ;; drawable-graph<%> -> Void
  (define (show-me a)
    (define set-cmap-graph0! [class-field-mutator carcassonne-map% graph0])
    (define fram (new frame% (label "Carcassonne") (width SIZE) (height SIZE)))
    (define cmap (let ([c (new editor-canvas% (parent fram))]
                       [cmap (new carcassonne-map% [admin (new 
							    (class object% 
							      (super-new)
							      (define/public (get-graph) a)))])])
                   (send c set-editor cmap)
                   cmap))
    (send fram show #t)
    (set! show-me (lambda (g)
                    (set-cmap-graph0! cmap g)
                    (send cmap draw-graph)))
    (show-me a))

  
  (provide view% show-me))
