
(module language-configuration mzscheme
  (require (lib "unitsig.ss")
           (lib "hierlist.ss" "hierlist")
           (lib "class.ss")
	   (lib "contracts.ss")
           "drsig.ss"
           (lib "string-constant.ss" "string-constants")
	   (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "file.ss")
           (lib "pconvert.ss")
           (lib "getinfo.ss" "setup")
           (lib "toplevel.ss" "syntax"))
  
  (define original-output (current-output-port))
  (define (printfo . args) (apply fprintf original-output args))
  
  (provide language-configuration@)
  
  (define language-configuration@
    (unit/sig drscheme:language-configuration/internal^
      (import [drscheme:unit : drscheme:unit^]
              [drscheme:rep : drscheme:rep^]
              [drscheme:teachpack : drscheme:teachpack^]
              [drscheme:init : drscheme:init^]
              [drscheme:language : drscheme:language^]
              [drscheme:app : drscheme:app^]
              [drscheme:tools : drscheme:tools^])
      
      ;; settings-preferences-symbol : symbol
      ;; the preferences key for the language settings.
      ;; depends on the version number, so people can use multiple versions
      ;; of drscheme and maintain separate language settings for each
      ;; of them.
      (define settings-preferences-symbol 
        (string->symbol (format "drscheme:~a-settings" (version:version))))
      
      ;; get-settings-preferences-symbol : -> symbol
      (define (get-settings-preferences-symbol) settings-preferences-symbol)

      ;; default-language-position : (listof string)
      ;; if a language is registered with this position, it is
      ;; considered the default language
      (define default-language-position
        (list (string-constant how-to-design-programs)
              (string-constant beginning-student)))

      ;; languages : (listof (instanceof language<%>))
      ;; all of the languages supported in DrScheme
      (define languages null)

      ;; add-language : (instanceof language%) -> void
      ;; only allows addition on phase2
      ;; effect: updates `languages'
      (define add-language
        (opt-lambda (language [front? #f])
          
          (drscheme:tools:only-in-phase 'drscheme:language:add-language 'phase2)
          (for-each
           (lambda (i<%>)
             (unless (is-a? language i<%>)
               (error 'drscheme:language:add-language "expected language ~e to implement ~e, forgot to use drscheme:language:get-default-mixin ?" language i<%>)))
           (drscheme:language:get-language-extensions))
          
          (set! languages 
                (if front? 
                    (cons language languages)
                    (append languages (list language))))))
      
      ;; get-languages : -> (listof languages)
      (define (get-languages) 
        (drscheme:tools:only-in-phase
         'drscheme:language-configuration:get-languages
         'init-complete)
        languages)
      
      ;; get-default-language-settings : -> language-settings
      ;; uses `default-language-position' to find the default language.
      ;; if that language is not available, just takes the first language.
      ;; if there are no languages defined yet, signal an error -- drscheme is in trouble.
      (define (get-default-language-settings)
	(when (null? languages)
	  (error 'get-default-language-settings "no languages registered!"))
	(let ([lang (or (ormap (lambda (x)
				 (and (equal? (send x get-language-position)
					      default-language-position)
				      x))
			       (get-languages))
			(first (get-languages)))])
	  (make-language-settings lang (send lang default-settings))))

      ;; type language-settings = (make-language-settings (instanceof language<%>) settings)
      (define-struct language-settings (language settings))
       
      
                                                        
 ;;;                                                    
   ;                                                    
   ;                                                    
   ;    ;;;;  ; ;;;    ;;; ;;;  ;;  ;;;;    ;;; ;  ;;;  
   ;        ;  ;;  ;  ;   ;  ;   ;      ;  ;   ;  ;   ; 
   ;     ;;;;  ;   ;  ;   ;  ;   ;   ;;;;  ;   ;  ;;;;; 
   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;     
   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
 ;;;;;;  ;;; ;;;;  ;;  ;;;;   ;;; ;  ;;; ;  ;;;;   ;;;  
                          ;                    ;        
                          ;                    ;        
                       ;;;                  ;;;         

                                          
    ;;    ;           ;;;                 
     ;                  ;                 
     ;                  ;                 
  ;;;;  ;;;    ;;;;     ;     ;;;    ;;; ;
 ;   ;    ;        ;    ;    ;   ;  ;   ; 
 ;   ;    ;     ;;;;    ;    ;   ;  ;   ; 
 ;   ;    ;    ;   ;    ;    ;   ;  ;   ; 
 ;   ;    ;    ;   ;    ;    ;   ;  ;   ; 
  ;;; ; ;;;;;   ;;; ; ;;;;;;  ;;;    ;;;; 
                                        ; 
                                        ; 
                                     ;;;  

      
      ;; language-dialog : (boolean language-setting -> language-setting)
      ;;                   (boolean language-setting (union #f (instanceof top-level-window%))
      ;;                    -> language-setting)
      ;; allows the user to configure their language. The input language-setting is used
      ;; as the defaults in the dialog and the output language setting is the user's choice
      ;; todo: when button is clicked, ensure language is selected
      (define language-dialog
        (opt-lambda (show-welcome? language-settings-to-show [parent #f])
          (define ret-dialog%
            (class dialog%
              (rename [super-on-subwindow-char on-subwindow-char])
              (define/override (on-subwindow-char receiver evt)
                (case (send evt get-key-code)
                  [(escape) (cancel-callback)]
                  [(#\return numpad-enter) (ok-callback)]
                  [else (super-on-subwindow-char receiver evt)]))
              (super-instantiate ())))
          
	  (define dialog (make-object ret-dialog% 
                           (if show-welcome?
                               (string-constant welcome-to-drscheme)
                               (string-constant language-dialog-title))
                           parent #f #f #f #f '(resize-border)))
          (define welcome-before-panel (instantiate horizontal-panel% ()
                                         (parent dialog)
                                         (stretchable-height #f)))
          (define language-dialog-meat-panel (make-object vertical-panel% dialog))

          (define welcome-after-panel (instantiate vertical-panel% () 
                                        (parent dialog)
                                        (stretchable-height #f)))
          
          (define button-panel (make-object horizontal-panel% dialog))
          
          (define-values (get-selected-language get-selected-language-settings)
            (fill-language-dialog language-dialog-meat-panel button-panel language-settings-to-show))
          
          ;; cancelled? : boolean
          ;; flag that indicates if the dialog was cancelled.
          (define cancelled? #t)
          
	  ;; ok-callback : -> void
          (define (ok-callback)
            (cond
              [(get-selected-language)
               (set! cancelled? #f)
               (send dialog show #f)]
              [else
               (message-box (string-constant drscheme)
                            (string-constant please-select-a-language))]))
          
          ;; cancel-callback : -> void
	  (define (cancel-callback)
            (send dialog show #f))

          (define show-details-label (string-constant show-details-button-label))
          (define hide-details-label (string-constant hide-details-button-label))
          
          (define button-gap (make-object horizontal-panel% button-panel))
	  (define-values (ok-button cancel-button)
            (gui-utils:ok/cancel-buttons
             button-panel
             (lambda (x y) (ok-callback))
             (lambda (x y) (cancel-callback))))
          (define grow-box-spacer (make-object grow-box-spacer-pane% button-panel))
          
          (when show-welcome?
            (add-welcome dialog welcome-before-panel welcome-after-panel))

          (send dialog stretchable-width #f)
          (send dialog stretchable-height #f)

          (unless parent
            (send dialog center 'both))
          (send dialog show #t)
          (if cancelled?
              language-settings-to-show
              (make-language-settings
               (get-selected-language)
               (get-selected-language-settings)))))
      
      ;; fill-language-dialog :    (vertical-panel panel language-setting -> language-setting)
      ;;                        -> (-> (union #f language<%>)) (-> settings[corresponding to fst thnk result])
      ;; allows the user to configure their language. The input language-setting is used
      ;; as the defaults in the dialog and the output language setting is the user's choice
      (define fill-language-dialog
        (opt-lambda (parent show-details-parent language-settings-to-show)

	  (define language-to-show (language-settings-language language-settings-to-show))
	  (define settings-to-show (language-settings-settings language-settings-to-show))
          
          ;; hier-list items that implement this interface correspond to
	  ;; actual language selections
	  (define hieritem-language<%>
	    (interface (hierarchical-list-item<%>)
	      selected))

	  ;; language-container-mixin : (implements hierlist<%>) -> (implements hierlist<%>)
	  ;; a mixin that delegates item clicks to the item clicked on, if it
	  ;; is a language.
	  (define selectable-hierlist%
            (class hierarchical-list%
              (init parent)

              (rename [super-on-char on-char])
              (inherit get-selected)
              (define/override (on-char evt)
                (let ([code (send evt get-key-code)])
                  (cond
                    [(eq? code 'up) (select-next sub1 (lambda (v) (- (vector-length v) 1)))]
                    [(eq? code 'down) (select-next add1 (lambda (v) 0))]
                    [else (super-on-char evt)])))
              
              (inherit get-items)
              
              ;; select-next : (int -> int) (vector -> int)
              ;; finds the next leaf after the selected child,
              ;; using `inc' and `start' to control the direction of the traversal.
              (define/private (select-next inc start)
                (let ([fst-selected (get-selected)])
                  (when fst-selected
                    (let loop ([item fst-selected])
		      (when item
			(let* ([parent (send item get-parent)]
			       [siblings (list->vector (if parent
							   (send parent get-items)
							   (get-items)))])
			  (let sibling-loop ([index (inc (find-index item siblings))])
			    (cond
			      [(and (<= 0 index)
				    (< index (vector-length siblings)))
			       (let ([sibling (vector-ref siblings index)])
				 (cond
				   [(find-first-leaf sibling inc start)
				    =>
				    (lambda (child)
				      (send fst-selected select #f)
				      (send child select #t)
				      (open-parents child))]
				   [else (sibling-loop (inc index))]))]
			      [else (loop parent)]))))))))
              
              ;; find-first-leaf : item (int -> int) (vec -> int)
              ;; finds the first child, using `inc' and `start' to control
              ;; the traversal over the children.
              (define/private (find-first-leaf item inc start)
                (let loop ([item item])
                  (cond
                    [(is-a? item hierarchical-list-compound-item<%>)
                     (let ([children (list->vector (send item get-items))])
                       (let child-loop ([i (start children)])
                         (cond
                           [(and (<= 0 i) (< i (vector-length children)))
                            (or (loop (vector-ref children i))
                                (child-loop (inc i)))]
                           [else #f])))]
                    [else item])))
              
              ;; find-index : tst (vectorof tst) -> int
              ;; returns the index of `item' in `vec'
              (define/private (find-index item vec)
                (let loop ([i 0])
                  (cond
                    [(< i (vector-length vec))
                     (if (eq? (vector-ref vec i) item)
                         i
                         (loop (+ i 1)))]
                    [else (error 'find-index "didn't find ~e in ~e" item vec)])))

              ;; open-parents : item -> void
              ;; selects the item and opens all of its parents.
              (define/private (open-parents item)
                (let loop ([item (send item get-parent)])
                  (when item
                    (send item open)
                    (loop (send item get-parent)))))
              
              (define/override (on-select i)
                (cond
		  [(and i (is-a? i hieritem-language<%>))
		   (send i selected)]
                  [else
		   (nothing-selected)]))
              (define/override (on-click i)
                (when (and i (is-a? i hierarchical-list-compound-item<%>))
                  (send i toggle-open/closed)))
              (super-instantiate (parent))))

          (define outermost-panel (make-object horizontal-panel% parent))
          (define languages-hier-list (make-object selectable-hierlist% outermost-panel))
          (define details-outer-panel (make-object vertical-pane% outermost-panel))
	  (define details-panel (make-object panel:single% details-outer-panel))
          (define one-line-summary-message (instantiate message% ()
                                             (parent parent)
                                             (label "")
                                             (stretchable-width #t)))
          
	  (define no-details-panel (make-object vertical-panel% details-panel))

          (define languages-table (make-hash-table))
          (define languages (get-languages))

	  ;; selected-language : (union (instanceof language<%>) #f)
	  ;; invariant: selected-language and get/set-selected-language-settings
	  ;;            match the user's selection in the languages-hier-list.
	  ;;            or #f if the user is not selecting a language.
	  (define selected-language #f)
	  ;; get/set-selected-language-settings (union #f (-> settings))
	  (define get/set-selected-language-settings #f)

	  ;; language-mixin : (implements language<%>) (implements area-container<%>) get/set ->
          ;;                  ((implements hierlist<%>) -> (implements hierlist<%>))
	  ;; a mixin that responds to language selections and updates the details-panel
	  (define (language-mixin language language-details-panel get/set-settings)
            (lambda (%)
              (class* % (hieritem-language<%>)
                (init-rest args)
                (public selected)
                (define (selected)
                  (send details-panel active-child language-details-panel)
                  (send one-line-summary-message set-label (send language get-one-line-summary))
                  (send revert-to-defaults-button enable #t)
		  (set! get/set-selected-language-settings get/set-settings)
		  (set! selected-language language))
                (apply super-make-object args))))

	  ;; nothing-selected : -> void
	  ;; updates the GUI and selected-language and get/set-selected-language-settings
	  ;; for when no language is selected.
	  (define (nothing-selected)
            (send revert-to-defaults-button enable #f)
	    (send details-panel active-child no-details-panel)
            (send one-line-summary-message set-label "")
	    (set! get/set-selected-language-settings #f)
	    (set! selected-language #f))

          ;; add-language-to-dialog : (instanceof language<%>) -> void
	  ;; adds the language to the dialog
	  ;; opens all of the turn-down tags
          ;; when `language' matches language-to-show, update the settings
          ;;   panel to match language-to-show, otherwise set to defaults.
          (define (add-language-to-dialog language)
            (let ([positions (send language get-language-position)]
                  [numbers (send language get-language-numbers)])
              (unless (and (list? positions)
                           (list? numbers)
                           (pair? positions)
                           (pair? numbers)
                           (andmap number? numbers)
                           (andmap string? positions)
                           (= (length positions) (length numbers)))
                (error 'drscheme:language
                       "languages position and numbers must be lists of strings and numbers, respectively, and must have the same length, got: ~e ~e"
                       positions numbers))
              (let add-sub-language ([ht languages-table]
                                     [hier-list languages-hier-list]
                                     [positions positions]
                                     [numbers numbers])
                (cond
                  [(null? (cdr positions))
                   (let-values ([(language-details-panel get/set-settings)
                                 (make-details-panel language)])
                     (let* ([position (car positions)]
                            [number (car numbers)]
                            [item
                             (send hier-list new-item
                                   (compose
                                    number-mixin
                                    (language-mixin language language-details-panel get/set-settings)))]
                            [text (send item get-editor)]
                            [delta (send language get-style-delta)])
                       (send item set-number number)
                       (send text insert position)
                       (when delta
                         (cond
                           [(list? delta)
                            (for-each (lambda (x)
                                        (send text change-style 
                                              (car x)
                                              (cadr x)
                                              (caddr x)))
                                      delta)]
                           [(is-a? delta style-delta%)
                            (send text change-style 
                                  (send language get-style-delta)
                                  0
                                  (send text last-position))])))
                     (cond
                       [(equal? (send language-to-show get-language-position)
                                (send language get-language-position))
                        (get/set-settings settings-to-show)]
                       [else
                        (get/set-settings (send language default-settings))]))]
                  [else (let* ([position (car positions)]
                               [number (car numbers)]
                               [sub-ht/sub-hier-list
                                (hash-table-get
                                 ht
                                 (string->symbol position)
                                 (lambda ()
                                   (let* ([new-list (send hier-list new-list number-mixin)]
                                          [x (cons (make-hash-table) new-list)])
                                     (send new-list set-number number)
                                     (send new-list set-allow-selection #f)
                                     (send new-list open)
                                     (send (send new-list get-editor) insert position)
                                     (hash-table-put! ht (string->symbol position) x)
                                     x)))])
                          (add-sub-language (car sub-ht/sub-hier-list)
                                            (cdr sub-ht/sub-hier-list)
                                            (cdr positions)
                                            (cdr numbers)))]))))

          ;; number-mixin : (extends object%) -> (extends object%)
          ;; adds the get/set-number methods to this class
          (define (number-mixin %)
            (class %
              (field (number 0))
              (define/public (get-number) number)
              (define/public (set-number _number) (set! number _number))
              (super-instantiate ())))
              
          ;; make-details-panel : ((instanceof language<%>) -> (values panel (case-> (-> settings) (settings -> void))))
	  ;; adds a details panel for `language', using
	  ;; the language's default settings, unless this is
	  ;; the to-show language.
	  (define (make-details-panel language)
	    (let ([panel (instantiate vertical-panel% ()
                           (parent details-panel)
                           (stretchable-width #f)
                           (stretchable-height #f))])
	      (values
	       panel
	       (send language config-panel panel))))

	  ;; close-all-languages : -> void
	  ;; closes all of the tabs in the language hier-list.
	  (define (close-all-languages)
	    (define (close-children list)
	      (for-each close-this-one (send list get-items)))
	    (define (close-this-one item)
	      (cond
		[(is-a? item hierarchical-list-compound-item<%>)
		 (send item close)
		 (close-children item)]
		[else (void)]))
	    (close-children languages-hier-list))

	  ;; open-current-language : -> void
	  ;; opens the tabs that lead to the current language
	  ;; and selects the current language
	  (define (open-current-language)
	    (let loop ([hi languages-hier-list]
		       [first-pos (car (send language-to-show get-language-position))]
		       [position (cdr (send language-to-show get-language-position))])
		 (let ([child
			;; know that this `car' is okay by construction of the dialog
			(car 
			 (filter (lambda (x)
				   (equal? (send (send x get-editor) get-text)
					   first-pos))
				 (send hi get-items)))])
		   (cond
		     [(null? position)
		      (send child select #t)]
		     [else
		      (send child open)
		      (loop child (car position) (cdr position))]))))

          ;; docs-callback : -> void
          (define (docs-callback)
            (void))
          
          ;; details-shown? : boolean
          ;; indicates if the details are currently visible in the dialog
          (define details-shown? (not (send language-to-show default-settings? settings-to-show)))

          ;; details-callback : -> void
          ;; flips the details-shown? flag and resets the GUI
          (define (details-callback)
            (set! details-shown? (not details-shown?))
            (update-show/hide-details))
          
          ;; show/hide-details : -> void
          ;; udpates the GUI based on the details-shown? flag
          (define (update-show/hide-details)
            (send details-button set-label 
                  (if details-shown? hide-details-label show-details-label))
            (send revert-to-defaults-outer-panel change-children
                  (lambda (l)
                    (if details-shown? (list revert-to-defaults-button) null)))
            (send details-outer-panel change-children
                  (lambda (l)
                    (if details-shown? (list details-panel) null))))

          ;; revert-to-defaults-callback : -> void
          (define (revert-to-defaults-callback)
            (when selected-language
              (get/set-selected-language-settings 
               (send selected-language default-settings))))

          (define show-details-label (string-constant show-details-button-label))
          (define hide-details-label (string-constant hide-details-button-label))
          (define details-button (make-object button% 
                                   (if (show-details-label . system-font-space->= . hide-details-label)
                                       show-details-label
                                       hide-details-label)
                                   show-details-parent
                                   (lambda (x y)
                                     (details-callback))))
          
          (define revert-to-defaults-outer-panel (make-object horizontal-pane% show-details-parent))
          (define revert-to-defaults-button (make-object button% 
                                              (string-constant revert-to-language-defaults)
                                              revert-to-defaults-outer-panel
                                              (lambda (_1 _2)
                                                (revert-to-defaults-callback))))

          (send details-outer-panel stretchable-width #t)
          (send details-outer-panel stretchable-width #f)
          (send revert-to-defaults-outer-panel stretchable-width #f)
          (send revert-to-defaults-outer-panel stretchable-height #f)
          (send outermost-panel set-alignment 'center 'center)

          (update-show/hide-details)

          (for-each add-language-to-dialog languages)
          (send languages-hier-list sort (lambda (x y) (< (send x get-number) (send y get-number))))
	  (send languages-hier-list stretchable-width #t)
	  (send parent reflow-container)
          (send languages-hier-list min-client-width (text-width (send languages-hier-list get-editor)))
	  (send languages-hier-list min-client-height (text-height (send languages-hier-list get-editor)))
          (close-all-languages)
	  (open-current-language)
          (get/set-selected-language-settings settings-to-show)
          (send languages-hier-list focus)
          (values
           (lambda () selected-language)
           (lambda () (get/set-selected-language-settings)))))

      (define (add-welcome dialog welcome-before-panel welcome-after-panel)
        (let* ([outer-pb%
                (class pasteboard%
                  (define/override (can-interactive-move? evt)
                    #f)
                  (super-instantiate ()))]
               [outer-pb (make-object outer-pb%)]
               [bitmap 
                (make-object bitmap%
                  (build-path (collection-path "icons") 
                              "plt-small-shield.gif"))]
               [image-snip
                (make-object image-snip% 
                  (build-path (collection-path "icons") 
                              "plt-small-shield.gif"))]
               [before-text (make-object text%)]
               [before-snip (make-object editor-snip% before-text #f)]
               [before-ec%
                (class editor-canvas% 
                  (inherit get-client-size)
                  (define (update-size)
                    (let-values ([(cw ch) (get-client-size)])
                      (unless (or (zero? cw)
                                  (zero? ch))
                        (let ([image-l-box (box 0)]
                              [image-r-box (box 0)])
                          (send before-text get-snip-location image-snip image-l-box #f #f)
                          (send before-text get-snip-location image-snip image-r-box #f #t)
                          (let* ([image-w (send bitmap get-width)]
                                 [before-snip-space (- cw image-w)]
                                 [before-snip-w (- before-snip-space
                                                   5 5 ;; space before and after inside snip 
                                                   2   ;; space at end of outer editor
                                                   1   ;; space at beginning of outer editor
                                                   1   ;; space between image and snip
                                                   -5  ;; unknown space
                                                   )])
                            (send before-text set-max-width (max 0 before-snip-w)))))))
                  (rename [super-on-superwindow-show on-superwindow-show])
                  (define/override (on-superwindow-show shown?)
                    (update-size)
                    (super-on-superwindow-show shown?))
                  (rename [super-on-size on-size])
                  (define/override (on-size w h)
                    (update-size)
                    (super-on-size w h))
                  (super-instantiate ()))]
               [before-ec (instantiate before-ec% ()
                            (parent welcome-before-panel)
                            (editor outer-pb)
                            (stretchable-height #f)
                            (style '(no-vscroll no-hscroll)))]
               [first-line-style-delta (make-object style-delta% 'change-bold)])
          (send first-line-style-delta set-delta-foreground (make-object color% 150 0 150))
          (send before-ec min-width 550)

          (let-values ([(cw ch) (send before-ec get-client-size)]
                       [(w h) (send before-ec get-size)])
            (send before-ec min-height 
                  (+ (send bitmap get-height) 
                     8  ;; pasteboards apparently want some space here....
                     (- h ch))))

          (send outer-pb insert image-snip)
          (send outer-pb insert before-snip)
          (send outer-pb move image-snip 0 0)
          (send outer-pb move before-snip (send bitmap get-width) 0)
          (send outer-pb set-selection-visible #f)
          (send outer-pb lock #t)

          ;(send before-snip set-align-top-line #t)
          (send before-text insert 
                (format (string-constant welcome-to-drscheme-version/language)
                        (version:version)
                        (this-language)))
          (send before-text insert #\newline)
          (send before-text insert (string-constant introduction-to-language-dialog))
          (send before-text change-style 
                first-line-style-delta
                0
                (send before-text paragraph-end-position 0))
          (send before-text auto-wrap #t)

          (send before-text lock #t)
          (send before-text hide-caret #t)

          (for-each (lambda (native-lang-string language)
                      (unless (equal? (this-language) language)
                        (instantiate button% ()
                          (label native-lang-string)
                          (parent welcome-after-panel)
                          (stretchable-width #t)
                          (callback (lambda (x1 x2) (drscheme:app:switch-language-to dialog language))))))
                    (string-constants is-this-your-native-language)
                    (all-languages))))

      ;; system-font-space->= : string string -> boolean
      ;; determines which string is wider, when drawn in the system font
      (define (x . system-font-space->= . y)
        (let ([bdc (make-object bitmap-dc%)])
          (send bdc set-bitmap (make-object bitmap% 1 1 #t))
          (send bdc set-font (send the-font-list find-or-create-font
                                   12 'system 'normal 'normal))
          (let-values ([(wx _1 _2 _3) (send bdc get-text-extent x)]
                       [(wy _4 _5 _6) (send bdc get-text-extent y)])
            (wx . >= . wy))))

      ;; text-width : (isntanceof text%) -> exact-integer
      ;; calculates the width of widest line in the
      ;; editor. This only makes sense if auto-wrap
      ;; is turned off. Otherwise, you could just use
      ;; the admin's width.
      (define (text-width text)
	(let loop ([n (+ (send text last-line) 1)]
		   [current-max-width 0])
	  (cond
	    [(zero? n)
	     (+
	      10 ;; this should be some magic small constant (hopefully less than 10 on all platforms)
	      (floor (inexact->exact current-max-width)))]
	    [else (let* ([line-number (- n 1)]
			 [box (box 0.0)]
			 [eol-pos (send text line-end-position line-number)]
			 [eol-snip (send text find-snip eol-pos 'before)])
		    (when eol-snip
		      (send text get-snip-location eol-snip box #f #t))
		    (loop (- n 1)
			  (max current-max-width (unbox box))))])))
      
      ;; text-height : (is-a?/c text% -> exact-integer
      (define (text-height text)
        (let ([y-box (box 0)])
          (send text position-location 
                (send text last-position)
                #f
                y-box
                #f
                #f
                #t)
          (+ 10 ;; upper bound on some platform specific space I don't know how to get.
             (floor (inexact->exact (unbox y-box))))))

      (define teachpack-directory 
        (let ([lib-dir (build-path 
                        (collection-path "mzlib")
                        'up 'up "teachpack")])
          (if (directory-exists? lib-dir)
              lib-dir
              #f)))

      
                                                                      
                            ;;                          ;;            
  ;                          ;                           ;            
  ;                          ;                           ;            
 ;;;;;   ;;;   ;;;;    ;;;   ; ;;  ; ;;;   ;;;;    ;;;   ;  ;;   ;;;  
  ;     ;   ;      ;  ;   ;  ;;  ;  ;   ;      ;  ;   ;  ; ;    ;   ; 
  ;     ;;;;;   ;;;;  ;      ;   ;  ;   ;   ;;;;  ;      ;;      ;;;  
  ;     ;      ;   ;  ;      ;   ;  ;   ;  ;   ;  ;      ; ;        ; 
  ;   ; ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;  ;   ;   ; 
   ;;;   ;;;    ;;; ;  ;;;  ;;; ;;; ;;;;    ;;; ;  ;;;  ;;   ;;  ;;;  
                                    ;                                 
                                    ;                                 
                                   ;;;                                

      
      ;; add-new-teachpack : (instanceof frame%) -> void
      ;; querys the user for the name of a teachpack and adds it to the
      ;; current teachpacks. Uses the argument as the parent to the dialog
      (define (add-new-teachpack frame)
        (let ([lib-file
               (parameterize ([finder:dialog-parent-parameter frame])
                 (finder:get-file 
                  teachpack-directory
                  (string-constant select-a-teachpack)
                  ".*\\.(ss|scm)$"))])
          (when lib-file
            (let* ([interactions-text (send frame get-interactions-text)]
                   [tp-cache (send interactions-text get-user-teachpack-cache)]
                   [tp-filenames (drscheme:teachpack:teachpack-cache-filenames tp-cache)]
                   [new-item (normalize-path lib-file)])
              (cond
                [(member (normal-case-path new-item) (map normal-case-path tp-filenames))
                 (message-box (string-constant drscheme-teachpack-message-title)
                              (format (string-constant already-added-teachpack)
                                      new-item)
                              frame)]
                [else
                 (let ([new-teachpacks 
                        (drscheme:teachpack:new-teachpack-cache
                         (append tp-filenames (list new-item)))])
                   (send interactions-text set-user-teachpack-cache new-teachpacks)
                   (preferences:set 'drscheme:teachpacks new-teachpacks))]))
            (set! teachpack-directory (path-only lib-file)))))
      
      ;; clear-all-teachpacks : -> void
      ;; clears all of the teachpack settings
      (define (clear-all-teachpacks)
        (drscheme:teachpack:set-teachpack-cache-filenames!
         (preferences:get 'drscheme:teachpacks)
         null))

   ;             ;;;                             
                ;                                
                ;                                
 ;;;   ; ;;;   ;;;;;   ;;;           ;;;    ;;;  
   ;    ;;  ;   ;     ;   ;         ;   ;  ;   ; 
   ;    ;   ;   ;     ;   ;          ;;;    ;;;  
   ;    ;   ;   ;     ;   ;             ;      ; 
   ;    ;   ;   ;     ;   ;         ;   ;  ;   ; 
 ;;;;; ;;;  ;; ;;;;    ;;;    ;      ;;;    ;;;  
                                                 
                                                 
                                                 

                                                               
 ;;;                                                           
   ;                                                           
   ;                                                           
   ;    ;;;;  ; ;;;    ;;; ;;;  ;;  ;;;;    ;;; ;  ;;;    ;;;  
   ;        ;  ;;  ;  ;   ;  ;   ;      ;  ;   ;  ;   ;  ;   ; 
   ;     ;;;;  ;   ;  ;   ;  ;   ;   ;;;;  ;   ;  ;;;;;   ;;;  
   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;          ; 
   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
 ;;;;;;  ;;; ;;;;  ;;  ;;;;   ;;; ;  ;;; ;  ;;;;   ;;;    ;;;  
                          ;                    ;               
                          ;                    ;               
                       ;;;                  ;;;                

      
      (define (add-info-specified-languages)
        (for-each add-info-specified-language
                  (drscheme:init:all-toplevel-collections)))
      
      (define (add-info-specified-language collection-name)
        (let ([info-proc (get-info (list collection-name))])
          (when info-proc
            (let* ([lang-positions (info-proc 'drscheme-language-positions (lambda () null))]
                   [lang-modules (info-proc 'drscheme-language-modules (lambda () null))]
                   [numberss (info-proc 'drscheme-language-numbers 
                                        (lambda ()
                                          (map (lambda (lang-position)
                                                 (map (lambda (x) 0) lang-position))
                                               lang-positions)))]
                   [summaries (info-proc 'drscheme-language-one-line-summaries 
                                         (lambda ()
                                           (map (lambda (lang-position) "")
                                                lang-positions)))]
                   [urls      (info-proc 'drscheme-language-urls 
                                         (lambda ()
                                           (map (lambda (lang-position) "")
                                                lang-positions)))]
                   [reader-specs
                    (info-proc 'drscheme-language-readers
                               (lambda ()
                                 (map (lambda (lang-position) 'mzscheme)
                                      lang-positions)))])
              (cond
                [(and (list? lang-positions)
                      (andmap (lambda (lang-position numbers)
                                (and (list? lang-position)
                                     (pair? lang-position)
                                     (andmap string? lang-position)
                                     (list? numbers)
                                     (andmap number? numbers)
                                     (= (length numbers)
                                        (length lang-position))))
                              lang-positions
                              numberss)
                      (list? lang-modules)
                      (andmap (lambda (x)
                                (and (list? x)
                                     (andmap string? x)))
                              lang-modules)
                      (list? summaries)
                      (andmap string? summaries)

                      (list? urls)
                      (andmap string? urls)
                      
                      (list? reader-specs)
                      (andmap (lambda (x)
				;; approximation (no good test, really)
				;; since it depends on the value of a mz
				;; parameter to interpret the module spec
                                (or (symbol? x) (pair? x)))
                              reader-specs)
                      
                      (= (length lang-positions)
                         (length lang-modules)
                         (length summaries)
                         (length urls)
                         (length reader-specs)))
                 (for-each
                  (lambda (lang-module lang-position lang-numbers one-line-summary url reader-spec)
                    (let ([%
                           ((drscheme:language:get-default-mixin)
                            (drscheme:language:module-based-language->language-mixin
                             (drscheme:language:simple-module-based-language->module-based-language-mixin
                              drscheme:language:simple-module-based-language%)))]
                          [reader
                           (with-handlers ([not-break-exn?
                                            (lambda (x)
                                              (message-box (string-constant drscheme)
                                                           (if (exn? x)
                                                               (exn-message x)
                                                               (format "uncaught exception: ~s" x)))
                                              read-syntax)])
                             (contract
                              (opt-> (any?)
                                     (port? (list/p (and/f number? integer? exact? (>=/c 0))
                                                    (and/f number? integer? exact? (>=/c 0))
                                                    (and/f number? integer? exact? (>=/c 0))))
                                     (or/f syntax? eof-object?))
                              (dynamic-require reader-spec 'read-syntax)
                              (string->symbol (format "~s" lang-position))
                              'drscheme))])
                      (add-language (instantiate % ()
                                      (module `(lib ,@lang-module))
                                      (language-position lang-position)
                                      (language-numbers lang-numbers)
                                      (one-line-summary one-line-summary)
                                      (language-url url)
                                      (reader reader)))))
                  lang-modules
                  lang-positions
                  numberss
                  summaries
                  urls
                  reader-specs)]
                [else
                 (message-box
		  (string-constant drscheme)
		  (format
		   "The drscheme-language-position, drscheme-language-modules, drscheme-language-numbers, and drscheme-language-readers specifications aren't correct. Expected (listof (cons string (listof string))), (listof (listof string)), (listof (listof number)), (listof string), (listof string), and (listof module-spec) respectively, where the lengths of the outer lists are the same. Got ~e, ~e, ~e, ~e, ~e, and ~e"
		  lang-positions
		  lang-modules
		  numberss
		  summaries
                  urls
		  reader-specs))])))))
      
      

                                                        
;;               ;    ;;;                    ;          
 ;                      ;     ;                         
 ;                      ;     ;                         
 ;;;;  ;;  ;;  ;;;      ;    ;;;;;         ;;;   ; ;;;  
 ;   ;  ;   ;    ;      ;     ;              ;    ;;  ; 
 ;   ;  ;   ;    ;      ;     ;              ;    ;   ; 
 ;   ;  ;   ;    ;      ;     ;              ;    ;   ; 
 ;   ;  ;   ;    ;      ;     ;   ;          ;    ;   ; 
; ;;;    ;;; ; ;;;;;  ;;;;;;   ;;;         ;;;;; ;;;  ;;
                                                        
                                                        
                                                        

                                                               
 ;;;                                                           
   ;                                                           
   ;                                                           
   ;    ;;;;  ; ;;;    ;;; ;;;  ;;  ;;;;    ;;; ;  ;;;    ;;;  
   ;        ;  ;;  ;  ;   ;  ;   ;      ;  ;   ;  ;   ;  ;   ; 
   ;     ;;;;  ;   ;  ;   ;  ;   ;   ;;;;  ;   ;  ;;;;;   ;;;  
   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;          ; 
   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
 ;;;;;;  ;;; ;;;;  ;;  ;;;;   ;;; ;  ;;; ;  ;;;;   ;;;    ;;;  
                          ;                    ;               
                          ;                    ;               
                       ;;;                  ;;;                


      
      ;; add-expand-to-front-end : mixin
      ;; overrides front-end to make the language a language that expands its arguments
      (define (add-expand-to-front-end %)
        (class %
          (rename [super-front-end/complete-program front-end/complete-program]
                  [super-front-end/interaction front-end/interaction])
          (define/override (front-end/complete-program input settings)
            (wrap-front-end (super-front-end/complete-program input settings)))
          (define/override (front-end/interaction input settings)
            (wrap-front-end (super-front-end/interaction input settings)))
          (define/private (wrap-front-end thnk)
            (lambda ()
              (let ([res (thnk)])
                (cond
                  [(syntax? res) (with-syntax ([res res]
                                               [expand-top-level-with-compile-time-evals
                                                expand-top-level-with-compile-time-evals])
                                   #'(expand-top-level-with-compile-time-evals #'res))]
                  [(eof-object? res) res]
                  [else `(expand ',res)]))))
          (super-instantiate ())))
                                       
      
      ;; add-built-in-languages : -> void
      (define (add-built-in-languages)
        (let* ([extras-mixin
                (lambda (mred-launcher? one-line-summary)
                  (lambda (%)
                    (class %
                      (define/override (get-one-line-summary) one-line-summary)
                      (define/override (use-namespace-require/copy?) #t)
                      
                      (inherit get-module get-transformer-module get-init-code)
                      (define/override (create-executable setting parent program-filename)
                        (let ([executable-fn
			       (drscheme:language:put-executable
				parent
				program-filename
				mred-launcher?
				#t
				(if mred-launcher?
				    (string-constant save-a-mred-launcher)
				    (string-constant save-a-mzscheme-launcher)))])
                          (when executable-fn
                            (drscheme:language:create-module-based-launcher
                             program-filename
                             executable-fn
                             (get-module)
                             (get-transformer-module)
                             (get-init-code setting)
                             mred-launcher?
                             (use-namespace-require/copy?)))))
                      (super-instantiate ()))))]
               [make-simple
                (lambda (module position numbers mred-launcher? one-line-summary extra-mixin)
                  (let ([%
                         (extra-mixin
                          ((extras-mixin mred-launcher? one-line-summary)
                           ((drscheme:language:get-default-mixin)
                            (drscheme:language:module-based-language->language-mixin
                             (drscheme:language:simple-module-based-language->module-based-language-mixin
                              drscheme:language:simple-module-based-language%)))))])
                    (instantiate % ()
                      (module module)
                      (language-position position)
                      (language-numbers numbers))))])
          (add-language
           (make-simple '(lib "plt-mzscheme.ss" "lang") 
                        (list (string-constant plt)
                              (string-constant mzscheme-w/debug))
                        (list -10 1)
                        #f
                        (string-constant mzscheme-one-line-summary)
                        (lambda (x) x)))
          (add-language
           (make-simple '(lib "plt-mred.ss" "lang")
                        (list (string-constant plt)
                              (string-constant mred-w/debug))
                        (list -10 2)
                        #t
                        (string-constant mred-one-line-summary)
                        (lambda (x) x)))
          (add-language
           (make-simple '(lib "plt-pretty-big.ss" "lang")
                        (list (string-constant plt)
                              (string-constant pretty-big-scheme))
                        (list -10 3)
                        #t
                        (string-constant pretty-big-scheme-one-line-summary)
                        (lambda (x) x)))
          (add-language
           (make-simple '(lib "plt-mzscheme.ss" "lang")
                        (list (string-constant plt)
                              "Expander")
                        (list -10 4)
                        #t
                        "Expands, rather than evaluates expressions"
                        add-expand-to-front-end))
          (add-language
           (make-simple '(lib "r5rs.ss" "lang")
                        (list (string-constant r5rs-lang-name))
                        (list -1000)
                        #f
                        (string-constant r5rs-one-line-summary)
                        (lambda (x) x))))))))
