
(module app mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   (lib "mred.ss" "mred")
           (lib "browser.ss" "net")
           "drsig.ss"
           "../acks.ss"
           (lib "framework.ss" "framework")
           (lib "file.ss"))
  
  (provide app@)
  (define app@
    (unit/sig drscheme:app^
      (import [drscheme:unit : drscheme:unit^]
              [drscheme:frame : drscheme:frame^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^]
              [help-desk : drscheme:help-desk^]
              [drscheme:tools : drscheme:tools^])

      (define about-frame%
        (class100 (drscheme:frame:basics-mixin (frame:standard-menus-mixin frame:basic%)) (_main-text)
          (private-field [main-text _main-text])
          (private
            [edit-menu:do 
             (lambda (const)
	       (send main-text do-edit-operation const))])
          (override
            [file-menu:create-revert? (lambda () #f)]
            [file-menu:create-save? (lambda () #f)]
            [file-menu:create-save-as? (lambda () #f)]
            [file-menu:between-close-and-quit (lambda (x) (void))]
            [file-menu:between-print-and-close (lambda (x) (void))]
            [edit-menu:between-redo-and-cut (lambda (x) (void))]
            [edit-menu:between-select-all-and-find (lambda (x) (void))]
            [edit-menu:copy-callback (lambda (menu evt) (edit-menu:do 'copy))]
            [edit-menu:select-all-callback (lambda (menu evt) (edit-menu:do 'select-all))]
            [edit-menu:create-find? (lambda () #f)])
          (sequence
            (super-init (string-constant about-drscheme-frame-title)))))
      
      ;; check-new-version : -> void
      (define (check-new-version)
        (let ([this-version (version)]
              [last-version (preferences:get 'drscheme:last-version)]
              [last-language (preferences:get 'drscheme:last-language)])
          (when (or (not last-version)
                    (not last-language)
                    (not (equal? last-version this-version))
                    (not (equal? last-language (this-language))))
            (preferences:set 'drscheme:last-version this-version)
            (preferences:set 'drscheme:last-language (this-language))
            (show-wizard))))

      ;; show-welcome-dialog : -> void
      (define (show-v200-welcome-dialog)
        (let ([new-settings (drscheme:language-configuration:language-dialog
                             #t
                             (preferences:get
                              drscheme:language-configuration:settings-preferences-symbol)
                             #f)])
          (when new-settings
            (preferences:set
             drscheme:language-configuration:settings-preferences-symbol
             new-settings))))
      
      
      
      
;                                           
;                                           
;                                           
;             ;                           ; 
;                                         ; 
;                                         ; 
;  ;   ;   ;  ;   ;;;;;;  ;;;   ; ;;  ;;; ; 
;  ;   ;   ;  ;       ;  ;   ;  ;;   ;   ;; 
;   ; ; ; ;   ;      ;       ;  ;    ;    ; 
;   ; ; ; ;   ;     ;     ;;;;  ;    ;    ; 
;   ; ; ; ;   ;    ;     ;   ;  ;    ;    ; 
;    ;   ;    ;   ;      ;   ;  ;    ;   ;; 
;    ;   ;    ;   ;;;;;;  ;;;;; ;     ;;; ; 
;                                           
;                                           
;                                           

      
      (define (show-wizard)
        (define wizard-image-bitmap
          (make-object bitmap% (build-path (collection-path "icons") "wizard-image.jpg")))
        
        (define bkg-color (make-object color% 0 0 0))
        (define stupid-internal-define-syntax1
          (let ([bdc (make-object bitmap-dc% wizard-image-bitmap)])
            (send bdc get-pixel 0 0 bkg-color)
            (send bdc set-bitmap #f)))
        
        (define bkg-brush (send the-brush-list find-or-create-brush bkg-color 'solid))
        (define bkg-pen (send the-pen-list find-or-create-pen bkg-color 1 'solid))
      
        (define wizard-image-canvas%
          (class canvas%
            (inherit get-dc get-client-size)
            (define/override (on-paint)
              (let ([dc (get-dc)])
                (let-values ([(w h) (get-client-size)])
                  (send dc set-pen bkg-pen)
                  (send dc set-brush bkg-brush)
                  (send dc draw-rectangle 0 0 w h)
                  (send dc draw-bitmap
                        wizard-image-bitmap
                        0
                        (- h (send wizard-image-bitmap get-height))))))
            
            (super-instantiate ())
            (inherit stretchable-width min-width min-height)
            (stretchable-width #f)
            (min-width (send wizard-image-bitmap get-width))
            (min-height (send wizard-image-bitmap get-height))))  
        
        (define dlg (instantiate dialog% ()
                      (label (string-constant welcome-to-drscheme))))
        (define hp (make-object horizontal-panel% dlg))
        (define c (make-object wizard-image-canvas% hp))
        (define vp (make-object vertical-panel% hp))
        (define sp (make-object panel:single% vp))
        (define bp (instantiate horizontal-panel% ()
                     (parent vp)
                     (stretchable-height #f)
                     (alignment '(right center))))
        
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;                                                        ;;
        ;;                    State Machine                       ;;
        ;;                                                        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        
        ;; type state = (union 'natural-language 'programming-language)
        ;; state : state
        (define state 'natural-language)
        
        ;; set-state : state -> void
        ;; moves the state to `new-state'
        (define (set-state new-state)
          (set! state new-state)
          (cond
            [(first-state?) (send back-button enable #f)]
            [else (send back-button enable #t)])
          (cond
            [(last-state?) (send next-button set-label
				 (string-constant wizard-finish))]
            [else (send next-button set-label (string-constant wizard-next))])
          (case state
            [(programming-language) (send sp active-child programming-language-state-panel)]
            [(natural-language) (send sp active-child natural-language-state-panel)]))
        
        ;; next-state : -> void
        (define (next-state)
          (case state
            [(natural-language)
             (when (okay-to-leave-nl-state?)
               (set-state 'programming-language))]
            [(programming-language)
             (cond
               [(get-selected-language)
                (send dlg show #f)]
               [else
                (message-box (string-constant drscheme)
                             (string-constant please-select-a-language))])]))
        
        ;; prev-state : -> void
        ;; pre: state != 'natural-language
        (define (prev-state)
          (case state
            [(programming-language) (set-state 'natural-language)]
            [else (error 'next-state "no next state from: ~s" state)]))
        
        ;; first-state?, last-state? : -> boolean
        (define (first-state?) (eq? state 'natural-language))
        (define (last-state?) (eq? state 'programming-language))
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;                                                        ;;
        ;;                 State 1 GUI                            ;;
        ;;                                                        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        
        (define natural-language-state-panel (make-object vertical-panel% sp))
        
        (define nl-space-above (make-object horizontal-panel% natural-language-state-panel))
        
        (define nl-welcome-panel (instantiate horizontal-panel% ()
                                   (parent natural-language-state-panel)
                                   (stretchable-height #f)
                                   (alignment '(center center))))
        
        (define stupid-internal-define-syntax2
          (begin
            (send nl-welcome-panel set-label-font
                  (send the-font-list find-or-create-font 24 'default 'normal 'normal #f))
            '(send nl-welcome-panel set-control-font
                  (send the-font-list find-or-create-font 24 'default 'normal 'normal #f))))
        
        (define nl-welcome-msg (instantiate message%  ()
                                 (label (string-constant welcome-to-drscheme))
                                 (parent nl-welcome-panel)))
        (define nl-lang-msg (instantiate message%  ()
                                 (label (format (string-constant version/language)
                                                (version)
                                                (this-language)))
                                 (parent natural-language-state-panel)))
        
        (define nl-radio-box
          (instantiate radio-box% ()
            (label #f)
            (choices (string-constants interact-with-drscheme-in-language))
            (parent natural-language-state-panel)
            (callback (lambda (x y) (void)))))
        
        (define stupid-internal-define-syntax3
	  (let loop ([languages (all-languages)]
                     [n 0])
            (cond
              [(null? languages) (void)]
              [else (let ([language (car languages)])
		      (if (eq? (this-language) language)
			  (send nl-radio-box set-selection n)
                          (loop (cdr languages) (+ n 1))))])))

        (define nl-space-below (make-object horizontal-panel% natural-language-state-panel))
        
        ;; okay-to-leave-nl-state? : -> boolean
        ;; returns #t if next is okay to proceed, #f if not.
        (define (okay-to-leave-nl-state?)
          (let loop ([languages (all-languages)]
                     [n 0])
            (cond
              [(null? languages) (error 'wizard "lost language.2")]
              [else (let ([language (car languages)])
                      (if (= n (send nl-radio-box get-selection))
                          (if (eq? (this-language) language)
                              #t
                              (begin (switch-language-to dlg language) 
                                     #f))
                          (loop (cdr languages) (+ n 1))))])))
        
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;                                                        ;;
        ;;                     State 2 GUI                        ;;
        ;;                                                        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define programming-language-state-panel (instantiate vertical-panel% ()
                                                   (parent sp)
                                                   (alignment '(center center))))
        (define pl-main-panel (instantiate vertical-panel% ()
                                (parent programming-language-state-panel)
                                (stretchable-width #t)
                                (stretchable-height #t)))
        (define pl-choose-language-message
          (instantiate message% ()
            (parent pl-main-panel)
            (label (string-constant please-select-a-language))))
        (define language-config-panel (make-object vertical-panel% pl-main-panel))
        (define language-config-button-panel (instantiate horizontal-panel% ()
                                               (parent pl-main-panel)
                                               (stretchable-height #f)))
                                               
        (define-values (get-selected-language get-selected-language-settings)
          (drscheme:language-configuration:fill-language-dialog
           language-config-panel
           language-config-button-panel
           (preferences:get
            drscheme:language-configuration:settings-preferences-symbol)))
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;                                                        ;;
        ;;                         GO                             ;;
        ;;                                                        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define back-button (instantiate button% ()
                              (label (string-constant wizard-back))
                              (parent bp)
                              (callback (lambda (x y) (prev-state)))))
        (define next-button (instantiate button% ()
                              (label (if (< (string-length (string-constant wizard-next))
					    (string-length (string-constant wizard-finish)))
					 (string-constant wizard-finish)
					 (string-constant wizard-next)))
                              (parent bp)
                              (callback (lambda (x y) (next-state)))))
        
        (set-state 'natural-language)
        
        (send dlg center 'both)
        (send dlg show #t)
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;                                                        ;;
        ;;              Put in Wizard Choices                     ;;
        ;;                                                        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (preferences:set
         (drscheme:language-configuration:get-settings-preferences-symbol)
         (drscheme:language-configuration:make-language-settings
          (get-selected-language)
          (get-selected-language-settings))))

      
;                                                                   
;                                                                   
;                                                                   
;   ;                  ;                                            
;                                                                   
;                          ;               ;                        
;   ;   ; ;;;  ;     ; ;  ;;;;   ;;;      ;;;;   ;;;;   ;    ;  ; ;;
;   ;   ;;   ;  ;   ;  ;   ;    ;   ;      ;    ;    ;  ;    ;  ;;  
;   ;   ;    ;  ;   ;  ;   ;    ;   ;      ;    ;    ;  ;    ;  ;   
;   ;   ;    ;  ;   ;  ;   ;    ;;;;;      ;    ;    ;  ;    ;  ;   
;   ;   ;    ;   ; ;   ;   ;    ;          ;    ;    ;  ;    ;  ;   
;   ;   ;    ;   ; ;   ;   ;    ;          ;    ;    ;  ;   ;;  ;   
;   ;   ;    ;    ;    ;    ;;   ;;;;       ;;   ;;;;    ;;; ;  ;   
;                                                                   
;                                                                   
;                                                                   

      
      
      (define (same-widths items)
        (let ([max-width (apply max (map (lambda (x) (send x get-width)) items))])
          (for-each (lambda (x) (send x min-width max-width)) items)))
      
      (define (same-heights items)
        (let ([max-height (apply max (map (lambda (x) (send x get-height)) items))])
          (for-each (lambda (x) (send x min-height max-height)) items)))
      
      (define wrap-edit% 
        (class100-asi text:hide-caret/selection%
          (inherit begin-edit-sequence end-edit-sequence
                   get-max-width find-snip position-location)
          (rename [super-after-set-size-constraint after-set-size-constraint])
          (override
            [on-set-size-constraint
             (lambda ()
               (begin-edit-sequence)
               (let ([snip (find-snip 1 'after-or-none)])
                 (when (is-a? snip editor-snip%)
                   (send (send snip get-editor) begin-edit-sequence))))]
            [after-set-size-constraint
             (lambda ()
               (super-after-set-size-constraint)
               (let ([width (get-max-width)]
                     [snip (find-snip 1 'after-or-none)])
                 (when (is-a? snip editor-snip%)
                   (let ([b (box 0)])
                     (position-location 1 b #f #f #t)
                     (let ([new-width (- width 4 (unbox b))])
                       (when (> new-width 0)
                         (send snip resize new-width
                               17) ; smallest random number
                         (send snip set-max-height 'none))))
                   (send (send snip get-editor) end-edit-sequence)))
               (end-edit-sequence))])))
      
      (define (get-plt-bitmap)
        (make-object bitmap%
          (build-path (collection-path "icons")
                      (if (< (get-display-depth) 8)
                          "pltbw.gif"
                          "plt.gif"))))
      
      (define (make-release-notes-button button-panel)
        (make-object button% (string-constant release-notes) button-panel
          (lambda (a b)
            (help-desk:goto-release-notes))))
      
      (define tour-frame%
        (class (drscheme:frame:basics-mixin (frame:standard-menus-mixin frame:basic%))
	  (init-rest args)

          (override edit-menu:create-undo?
		    edit-menu:create-redo?
		    edit-menu:create-cut?
		    edit-menu:create-copy?
		    edit-menu:create-paste?
		    edit-menu:create-clear?
		    edit-menu:create-select-all?
		    edit-menu:between-select-all-and-find
		    edit-menu:between-find-and-preferences
		    edit-menu:between-redo-and-cut
		    file-menu:between-print-and-close)
          (define (edit-menu:create-undo?) #f)
          (define (edit-menu:create-redo?) #f)
          (define (edit-menu:create-cut?) #f)
          (define (edit-menu:create-copy?) #f)
          (define (edit-menu:create-paste?) #f)
          (define (edit-menu:create-clear?) #f)
          (define (edit-menu:create-select-all?) #f)
          (define (edit-menu:between-select-all-and-find x) (void))
          (define (edit-menu:between-find-and-preferences x) (void))
          (define (edit-menu:between-redo-and-cut x) (void))
          (define (file-menu:between-print-and-close x) (void))
          
          (apply super-make-object args)))
      
      (define (invite-tour)
        (let* ([f (make-object tour-frame% (format (string-constant welcome-to-something) 
                                                   (string-constant drscheme)))]
               [panel (send f get-area-container)]
               [top-hp (make-object horizontal-panel% panel)]
               [bottom-vp (make-object vertical-panel% panel)]
               [left-vp (make-object vertical-panel% top-hp)]
               [plt-bitmap (get-plt-bitmap)]
               [plt-icon (make-object message% (if (send plt-bitmap ok?)
                                                   plt-bitmap
                                                   "[plt]")
                           left-vp)]
               [outer-button-panel (make-object vertical-panel% top-hp)]
               [top-button-panel (make-object vertical-panel% outer-button-panel)]
               [bottom-button-panel (make-object vertical-panel% outer-button-panel)]
               [tour-button (make-object button% (string-constant take-a-tour) 
                              top-button-panel
                              (lambda (x y)
                                (help-desk:goto-tour))
                              '(border))]
               [release-notes-button (make-release-notes-button top-button-panel)]
               [close-button (make-object button% (string-constant close) bottom-button-panel
                               (lambda x
                                 (send f close)))]
               [messages-panel (make-object vertical-panel% left-vp)]
               
               [this-version (version)]
               [last-version (preferences:get 'drscheme:last-version)]
               [last-language (preferences:get 'drscheme:last-language)]
               [welcome-to-drs-msg (make-object message% (string-constant welcome-to-drscheme) messages-panel)]
               [this-version-message (make-object message%
                                       (format (string-constant version/language)
                                               this-version 
                                               (this-language))
                                       messages-panel)]
               [last-version-message
                (let ([msg (cond
                             [(and last-version
                                   last-language
                                   (not (equal? this-version last-version))
                                   (not (equal? (this-language) last-language)))
                              (format (string-constant parenthetical-last-version/language)
                                      last-version last-language)]
                             [(and last-language
                                   (not (equal? (this-language) last-language)))
                              (format (string-constant parenthetical-last-language)
                                      last-language)]
                             [(and last-version
                                   (not (equal? this-version last-version)))
                              (format (string-constant parenthetical-last-version)
                                      last-version)]
                             [else #f])])
                  (and msg (make-object message% msg messages-panel)))])
          (for-each (lambda (native-lang-string language)
                      (unless (equal? (this-language) language)
                        (instantiate button% ()
                          (label native-lang-string)
                          (parent bottom-vp)
                          (stretchable-width #t)
                          (callback (lambda (x1 x2) (switch-language-to f language))))))
                    (string-constants is-this-your-native-language)
                    (all-languages))
          (send bottom-vp stretchable-height #f)
          (send messages-panel stretchable-height #f)
          (send bottom-button-panel stretchable-height #f)
          (send top-button-panel set-alignment 'center 'center)
          (send bottom-button-panel set-alignment 'center 'center)
          (send messages-panel set-alignment 'center 'center)
          
          (send f reflow-container)
          (same-heights (list bottom-button-panel messages-panel))
          (same-widths (list tour-button release-notes-button close-button))
          
          (send tour-button focus)
          (send f show #t)))
      

      
      
;                                                              
;                                                              
;                                                              
;          ;                                     ;             
;          ;                                     ;             
;          ;                       ;             ;             
;    ;;;   ; ;;;    ;;;;   ;    ; ;;;;       ;;; ;  ; ;;  ;;;; 
;   ;   ;  ;;   ;  ;    ;  ;    ;  ;        ;   ;;  ;;   ;     
;       ;  ;    ;  ;    ;  ;    ;  ;        ;    ;  ;    ;     
;    ;;;;  ;    ;  ;    ;  ;    ;  ;        ;    ;  ;     ;;;  
;   ;   ;  ;    ;  ;    ;  ;    ;  ;        ;    ;  ;        ; 
;   ;   ;  ;;   ;  ;    ;  ;   ;;  ;        ;   ;;  ;        ; 
;    ;;;;; ; ;;;    ;;;;    ;;; ;   ;;       ;;; ;  ;    ;;;;  
;                                                              
;                                                              
;                                                              

      
      (define (about-drscheme)
        (let* ([e (make-object wrap-edit%)]
               [main-text (make-object wrap-edit%)]
               [plt-bitmap (get-plt-bitmap)]
               [plt-icon (if (send plt-bitmap ok?)
                             (make-object image-snip% plt-bitmap)
                             (let ([i (make-object string-snip%)]
                                   [label "[lambda]"])
                               (send i insert label (string-length label) 0)
                               i))]
               [editor-snip (make-object editor-snip% e #f)]
               [f (make-object about-frame% main-text)]
               [main-panel (send f get-area-container)]
               [editor-canvas (make-object editor-canvas% main-panel)]
               [button-panel (make-object horizontal-panel% main-panel)]
               [top (make-object style-delta% 'change-alignment 'top)]
               [d-usual (make-object style-delta% 'change-family 'decorative)]
               [d-dr (make-object style-delta%)]
               [d-http (make-object style-delta%)]
               
               [insert-url/external-browser
                (lambda (str url)
                  (send e change-style d-http)
                  (let* ([before (send e get-start-position)]
                         [_ (send e insert str)]
                         [after (send e get-start-position)])
                    (send e set-clickback before after 
                          (lambda args (send-url url))
                          d-http))
                  (send e change-style d-usual))])
          
          (send* d-http 
            (copy d-usual)
            (set-delta-foreground "BLUE")
            (set-delta 'change-underline #t))
          (send* d-usual 
            (set-delta-foreground "BLACK")
            (set-delta 'change-underline #f))
          
          (send* d-dr (copy d-usual) (set-delta 'change-bold))
          (send d-usual set-weight-on 'normal)
          (send* editor-canvas
            (set-editor main-text)
            (stretchable-width #t)
            (stretchable-height #t))
          
          ;; 50 is close enough to the space
          (if (send plt-bitmap ok?)
              (send* editor-canvas
                (min-width (floor (+ (* 5/2 (send plt-bitmap get-width)) 50)))
                (min-height (+ (send plt-bitmap get-height) 50)))
              (send* editor-canvas
                (min-width 500)
                (min-height 400)))
          
          (send* e 
            (change-style d-dr)
            (insert (format (string-constant welcome-to-drscheme-version/language) 
                            (version:version)
                            (this-language)))
            (change-style d-usual))
          
          (send e insert " by ")
          
          (insert-url/external-browser "PLT" "http://www.plt-scheme.org/")
          
          (send* e
            (insert ".")
            (insert #\newline)
            (insert (get-authors))
            (insert #\newline)
            (insert "For licensing information see "))
          
          (let ([copying.lib
                 (normalize-path
                  (build-path (collection-path "mzlib")
                              'up
                              'up
                              "notes"
                              "COPYING-LIB"))])
            (send e insert copying.lib))
          
          (send* e
            (insert ".")
            (insert #\newline)
            (insert #\newline)
            (insert "Based on:")
            (insert #\newline)
            (insert "  ")
            (insert (banner)))
          
          (when (or (eq? (system-type) 'macos)
                    (eq? (system-type) 'macosx))
            (send* e
              (insert "  The A List (c) 1997-2001 Kyle Hammond")
              (insert #\newline)))
          
          (let ([tools (drscheme:tools:get-successful-tools)])
            (unless (null? tools)
              (send* e
                (insert #\newline)
                (insert "Installed tools:")
                (insert #\newline))
              (for-each
               (lambda (successful-tool)
                 (let ([name (or (drscheme:tools:successful-tool-name successful-tool)
                                 (format "~s" (drscheme:tools:successful-tool-spec successful-tool)))]
                       [bm (drscheme:tools:successful-tool-bitmap successful-tool)]
                       [url (drscheme:tools:successful-tool-url successful-tool)])
                   (send e insert "  ")
                   (when bm
                     (send* e
                       (insert (make-object image-snip% bm))
                       (insert #\space)))
                   (cond
                     [url
                      (insert-url/external-browser name url)]
                     [else
                      (send e insert name)])
                   (send e insert #\newline)))
               tools)))
          
          (send e insert "\n")
          (send e insert (get-translating-acks))
          
          (send* e
            (auto-wrap #t)
            (set-autowrap-bitmap #f)
            (lock #t))
          (send* main-text 
            (set-autowrap-bitmap #f)
            (auto-wrap #t)
            (insert plt-icon)
            (insert editor-snip)
            (change-style top 0 2)
            (set-position 1)
            (hide-caret #t)
            (scroll-to-position 0)
            (lock #t))
          
          (let* ([tour-button (make-object button% (string-constant take-a-tour) button-panel
                                (lambda (x y)
                                  (help-desk:goto-tour)))]
                 [release-notes-button (make-release-notes-button button-panel)])
            (same-widths (list tour-button release-notes-button))
            (send tour-button focus))
          (send button-panel stretchable-height #f)
          (send button-panel set-alignment 'center 'center)
          (send f show #t)
          f))

      

;                                       
;                                       
;                                       
;                ;   ;   ;              
;                    ;                  
;           ;        ;       ;          
;   ;    ; ;;;;  ;   ;   ;  ;;;; ;     ;
;   ;    ;  ;    ;   ;   ;   ;    ;   ; 
;   ;    ;  ;    ;   ;   ;   ;    ;   ; 
;   ;    ;  ;    ;   ;   ;   ;     ;  ; 
;   ;    ;  ;    ;   ;   ;   ;     ; ;  
;   ;   ;;  ;    ;   ;   ;   ;      ;;  
;    ;;; ;   ;;  ;   ;   ;    ;;    ;   
;                                   ;   
;                                   ;   
;                                  ;    

      
      ;; switch-language-to : (is-a?/c top-level-window<%>) symbol -> void
      ;; doesn't return if the language changes
      (define (switch-language-to parent other-language)
        (define-values (other-are-you-sure other-cancel other-accept-and-quit)
          (let loop ([languages (all-languages)]
                     [are-you-sures (string-constants are-you-sure-you-want-to-switch-languages)]
                     [cancels (string-constants cancel)]
                     [accept-and-quits (if (eq? (system-type) 'windows)
                                           (string-constants accept-and-exit)
                                           (string-constants accept-and-quit))])
            (cond
              [(null? languages) (error 'app.ss ".1")]
              [(equal? other-language (car languages))
               (values (car are-you-sures)
                       (car cancels)
                       (car accept-and-quits))]
              [else (loop (cdr languages)
                          (cdr are-you-sures)
                          (cdr cancels)
                          (cdr accept-and-quits))])))
        (define dialog (make-object dialog% (string-constant drscheme) parent 400))
        (define (make-section are-you-sure cancel-label quit-label)
          (define text (make-object text:hide-caret/selection%))
          (define ec (instantiate editor-canvas% ()
                       (parent dialog)
                       (editor text)
                       (style '(no-hscroll))))
          (define bp (instantiate horizontal-panel% ()
                       (parent dialog)
                       (alignment '(right center))))
          (define-values (quit cancel)
            (gui-utils:ok/cancel-buttons
             bp
             (lambda (x y)
               (set! cancelled? #f)
               (send dialog show #f))
             (lambda (x y)
               (send dialog show #f))
             quit-label
             cancel-label))
          (send ec set-line-count 3)
          (send text auto-wrap #t)
          (send text set-autowrap-bitmap #f)
          (send text insert are-you-sure)
          (send text set-position 0 0))
        (define cancelled? #t)
        
        (make-section other-are-you-sure
                      other-cancel
                      other-accept-and-quit)
        
        (make-section (string-constant are-you-sure-you-want-to-switch-languages)
                      (string-constant cancel)
                      (if (eq? (system-type) 'windows)
                          (string-constant accept-and-exit)
                          (string-constant accept-and-quit)))
        
        (send dialog show #t)
        
        (unless cancelled?
          (let ([set-language? #t])
            (exit:insert-on-callback 
             (lambda ()
               (when set-language?
                 (set-language-pref other-language))))
            (exit:exit #t)
            (set! set-language? #f))))
      
      (define (add-important-urls-to-help-menu help-menu)
	(let* ([important-urls
                (instantiate menu% ()
                  (parent help-menu)
                  (label (string-constant web-materials)))]
               [add
                (lambda (name url)
                  (instantiate menu-item% ()
                    (label name)
                    (parent important-urls)
                    (callback
                     (lambda (x y)
                       (send-url url)))))])
          (add (string-constant drscheme-homepage) "http://www.drscheme.org/")
          (add (string-constant plt-homepage) "http://www.plt-scheme.org/")
          (add (string-constant teachscheme!-homepage) "http://www.teach-scheme.org/")
          (add (string-constant how-to-design-programs) "http://www.htdp.org/")
          (add (string-constant how-to-use-scheme) "http://www.htus.org/")))

      (define (add-language-items-to-help-menu help-menu)
        (let ([added-any? #f])
          (for-each (lambda (native-lang-string language)
                      (unless (equal? (this-language) language)
                        (unless added-any?
                          (make-object separator-menu-item% help-menu)
                          (set! added-any? #t))
                        (instantiate menu-item% ()
                          (label native-lang-string)
                          (parent help-menu)
                          (callback (lambda (x1 x2) (switch-language-to #f language))))))
                    (string-constants interact-with-drscheme-in-language)
                    (all-languages)))))))
