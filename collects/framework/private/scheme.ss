;; originally by Dan Grossman
;; 6/30/95

(module scheme mzscheme
  (require "collapsed-snipclass-helpers.ss"
           (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   "../macro.ss"
	   (lib "mred-sig.ss" "mred")
           (lib "mred.ss" "mred")
	   (lib "list.ss")
	   (lib "thread.ss")
           (lib "etc.ss"))
  
  (provide scheme@)
  
  (define scheme@
    (unit/sig framework:scheme^
      (import mred^
              [preferences : framework:preferences^]
              [match-cache : framework:match-cache^]
              [paren : framework:paren^] 
              [scheme-paren : framework:scheme-paren^]
              [icon : framework:icon^]
              [keymap : framework:keymap^]
              [text : framework:text^]
              [editor : framework:editor^]
              [frame : framework:frame^])
      
      (rename [-text% text%]
              [-text<%> text<%>])
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                                                                  ;;
      ;;                           Sexp Snip                              ;;
      ;;                                                                  ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (set-box/f! b v) (when (box? b) (set-box! b v)))

      (define sexp-snip<%>
        (interface ()
          get-saved-snips))

      (define sexp-snip%
        (class* snip% (sexp-snip<%> readable-snip<%>)
          (init-field left-bracket right-bracket saved-snips)
          (define/public (get-saved-snips) saved-snips)
          (field [sizing-text (format "~a   ~a" left-bracket right-bracket)])

          (define/public (read-one-special index file line col pos)
            (let ([text (make-object text:basic%)])
              (for-each
               (lambda (s) (send text insert (send s copy)
                                 (send text last-position)
                                 (send text last-position)))
               saved-snips)
              (values (datum->syntax-object
                       #f
                       (read (open-input-text-editor text))
                       (list file line col pos 1))
                      1
                      #t)))
          
          (rename [super-get-text get-text])
          (define/override get-text
            (opt-lambda (offset num [flattened? #f])
              (if flattened?
                  (apply string-append
                         (map (lambda (snip)
                                (send snip get-text 0 (send snip get-count) flattened?))
                              saved-snips))
                  (super-get-text offset num flattened?))))
              
          (define/override (copy)
            (instantiate sexp-snip% ()
              (left-bracket left-bracket)
              (right-bracket right-bracket)
              (saved-snips saved-snips)))
          
          (define/override (write stream-out)
            (send stream-out put (string left-bracket))
            (send stream-out put (string right-bracket))
            (send stream-out put (length saved-snips))
            (let loop ([snips saved-snips])
              (cond
                [(null? snips) (void)]
                [else
                 (let* ([snip (car snips)]
                        [snipclass (send snip get-snipclass)])
                   (send stream-out put (send snipclass get-classname))
                   (send snip write stream-out))
                 (loop (cdr snips))])))
          
          (define/override (draw dc x y left top right bottom dx dy draw-caret)
            (send dc draw-text sizing-text x y)
            (let-values ([(lpw lph lpa lpd) (send dc get-text-extent (string left-bracket))]
                         [(rpw rph rpa rpd) (send dc get-text-extent (string right-bracket))]
                         [(sw sh sa sd) (send dc get-text-extent sizing-text)])
              (let* ([dtw (- sw lpw rpw)]
                     [dot-start (+ x lpw)]
                     [dt1x (+ dot-start (* dtw 1/5))]
                     [dt2x (+ dot-start (* dtw 1/2))]
                     [dt3x (+ dot-start (* dtw 4/5))]
                     [dty (+ y (/ sh 2))])
                (send dc draw-rectangle dt1x dty 2 2)
                (send dc draw-rectangle dt2x dty 2 2)
                (send dc draw-rectangle dt3x dty 2 2))))

          (inherit get-style)
          (define/override (get-extent dc x y wb hb descentb spaceb lspaceb rspaceb)
            (let-values ([(w h d a) (send dc get-text-extent sizing-text (send (get-style) get-font))])
              (set-box/f! wb w)
              (set-box/f! hb h)
              (set-box/f! descentb d)
              (set-box/f! spaceb a)
              (set-box/f! lspaceb 0)
              (set-box/f! rspaceb 0)))
          (super-instantiate ())
          (inherit set-snipclass)
          (set-snipclass lib-snip-class)))

      (define sexp-snipclass% (make-sexp-snipclass% sexp-snip%))
      
      ;; old snips (from old versions of drscheme) use this snipclass
      (define lib-snip-class (make-object sexp-snipclass%))
      (send lib-snip-class set-classname (format "~s" '(lib "collapsed-snipclass.ss" "framework")))
      (send lib-snip-class set-version 0)
      (send (get-the-snip-class-list) add lib-snip-class)

      ;; new snips use this snipclass
      (define old-snip-class (make-object sexp-snipclass%))
      (send old-snip-class set-classname "drscheme:sexp-snip")
      (send old-snip-class set-version 0)
      (send (get-the-snip-class-list) add old-snip-class)

      (keymap:add-to-right-button-menu
       (let ([old (keymap:add-to-right-button-menu)])
         (lambda (menu text event)
           (old menu text event)
           (split/collapse-text menu text event)
	   (void))))
      
      ;; split/collapse-text : (instanceof menu%) (instanceof editor<%>) (instanceof mouse-event%) -> void
      (define (split/collapse-text menu text event)
        (when (is-a? text -text<%>)
          (let* ([on-it-box (box #f)]
                 [click-pos 
                  (call-with-values
                   (lambda ()
                     (send text dc-location-to-editor-location
                           (send event get-x)
                           (send event get-y)))
                   (lambda (x y)
                     (send text find-position x y #f on-it-box)))]
                 [snip (send text find-snip click-pos 'after)]
                 [char (send text get-character click-pos)]
                 [left? (memq char '(#\( #\{ #\[))]
                 [right? (memq char '(#\) #\} #\]))])
            (cond
              [(and snip (is-a? snip sexp-snip<%>))
               (make-expand-item text snip menu)]
              [(not (unbox on-it-box))
               ;; clicking in nowhere land, just ignore
               (void)]
              [(or left? right?)
               ;; clicking on left or right paren
               (let* ([pos (if left?
                               click-pos
                               (+ click-pos 1))]
                      [other-pos (if left?
                                     (send text get-forward-sexp pos)
                                     (send text get-backward-sexp pos))])
                 (when other-pos
                   (let ([left-pos (min pos other-pos)]
                         [right-pos (max pos other-pos)])
                     (make-collapse-item text left-pos right-pos menu))))]
              [else 
               ;; clicking on some other text -> collapse containing sexp
               (let ([up-sexp (send text find-up-sexp click-pos)])
                 (when up-sexp 
                   (let ([fwd (send text get-forward-sexp up-sexp)])
                     (make-collapse-item text up-sexp fwd menu))))]))))
      
      ;; make-expand-item : (instanceof text%) (instanceof sexp-snip<%>) (instanceof menu%) -> void
      (define (make-expand-item text snip menu)
        (instantiate separator-menu-item% ()
          (parent menu))
        (instantiate menu-item% ()
          (parent menu)
          (label (string-constant expand-sexp))
          (callback (lambda (item evt) (expand-from text snip)))))
      
      ;; expand-from : (instanceof text%) (instanceof sexp-snip<%>) -> void
      (define (expand-from text snip)
        (let ([snips (send snip get-saved-snips)])
          (send text begin-edit-sequence)
          (let ([pos (send text get-snip-position snip)])
            (send text delete pos (+ pos 1))
            (let loop ([snips (reverse snips)])
              (cond
                [(null? snips) (void)]
                [else (send text insert (car snips) pos pos)
                      (loop (cdr snips))])))
          (send text end-edit-sequence)))

      ;; make-collapse-item : (instanceof text%) number number (instanceof menu%) -> void
      ;; adds a collapse menu item to the menu
      (define (make-collapse-item text left-pos right-pos menu)
        (instantiate separator-menu-item% ()
          (parent menu))
        (instantiate menu-item% ()
          (parent menu)
          (label (string-constant collapse-sexp))
          (callback (lambda (item evt)
                      (collapse-from text left-pos right-pos)))))
      
      (define (collapse-from text left-pos right-pos)
        (let ([left-bracket (send text get-character left-pos)]
              [right-bracket (send text get-character (- right-pos 1))])
          (send text begin-edit-sequence)
          (send text split-snip left-pos)
          (send text split-snip right-pos)
          (let ([snips (let loop ([snip (send text find-snip left-pos 'after)])
                         (cond
                           [(not snip) null]
                           [((send text get-snip-position snip) . >= . right-pos)
                            null]
                           [else (cons (send snip copy) (loop (send snip next)))]))])
            (send text delete left-pos right-pos)
            (send text insert (instantiate sexp-snip% () 
                                (left-bracket left-bracket)
                                (right-bracket right-bracket)
                                (saved-snips snips))
                  left-pos left-pos)
            (send text end-edit-sequence)))) 
      

                                                                             
              ;;                                                             
               ;                                   ;                    ;    
               ;                                   ;                    ;    
  ;;;    ;;;   ; ;;    ;;;  ;;; ;    ;;;          ;;;;;   ;;;  ;;; ;;; ;;;;; 
 ;   ;  ;   ;  ;;  ;  ;   ;  ; ; ;  ;   ;    ;     ;     ;   ;   ; ;    ;    
  ;;;   ;      ;   ;  ;;;;;  ; ; ;  ;;;;;          ;     ;;;;;    ;     ;    
     ;  ;      ;   ;  ;      ; ; ;  ;              ;     ;       ; ;    ;    
 ;   ;  ;   ;  ;   ;  ;   ;  ; ; ;  ;   ;          ;   ; ;   ;  ;   ;   ;   ;
  ;;;    ;;;  ;;; ;;;  ;;;  ;; ; ;;  ;;;     ;      ;;;   ;;;  ;;   ;;   ;;; 
                                                                             
                                                                             
                                                                             
      
      (define-struct string/pos (string pos))
      
      (define -text<%>
        (interface ()
          highlight-parens
          get-limit
          balance-quotes
          balance-parens
          tabify-on-return?
          tabify
          tabify-selection
          tabify-all
          insert-return
          comment-out-selection
          uncomment-selection
          get-forward-sexp
          remove-sexp
          forward-sexp
          flash-forward-sexp
          get-backward-sexp
          flash-backward-sexp
          backward-sexp
          find-up-sexp
          up-sexp
          find-down-sexp
          down-sexp
          remove-parens-forward
          
          select-forward-sexp
          select-backward-sexp
          select-up-sexp
          select-down-sexp
          transpose-sexp
          mark-matching-parenthesis
          get-tab-size
          set-tab-size))
      
      (define init-wordbreak-map
        (lambda (map)
          (let ([v (send map get-map #\-)])
            (send map set-map 
                  #\-
                  '(line)))))
      (define wordbreak-map (make-object editor-wordbreak-map%))
      (define (get-wordbreak-map) wordbreak-map)
      (init-wordbreak-map wordbreak-map)
      
      (define style-list (make-object style-list%))
      (define (get-style-list) style-list)
      (define delta
        (let ([delta (make-object style-delta% 'change-normal)])
          (send delta set-delta 'change-family 'modern)
          delta))
      (let ([style (send style-list find-named-style "Standard")])
        (if style
            (send style set-delta delta)
            (send style-list new-named-style "Standard"
                  (send style-list find-or-create-style
                        (send style-list find-named-style "Basic")
                        delta))))
      
      (define match-color 
        (let ([gray-level
               ;; old gray-level 192
               (if (eq? (system-type) 'windows)
                   (* 3/4 256)
                   (- (* 7/8 256) 1))])
          (make-object color% gray-level gray-level gray-level)))
      (define mismatch-color (make-object color% "PINK"))
      
      (define matching-parenthesis-style 
        (let ([matching-parenthesis-delta (make-object style-delta% 'change-bold)])
          (send matching-parenthesis-delta set-delta-foreground "forest green")
          (send style-list new-named-style "Matching Parenthesis Style"
                (send style-list find-or-create-style
                      (send style-list find-named-style "Standard")
                      matching-parenthesis-delta))
          (send style-list find-named-style "Matching Parenthesis Style")))

      (define text-mixin 
        (mixin (text:basic<%> editor:keymap<%>) (-text<%>)
          (inherit begin-edit-sequence
                   delete
                   end-edit-sequence
                   local-edit-sequence?
                   find-string
                   get-character
                   get-keymap
                   get-text
                   get-start-position
                   get-end-position
                   flash-on
                   highlight-range
                   insert
                   kill
                   last-position
                   paragraph-start-position
                   paragraph-end-position
                   position-paragraph
                   set-keymap
                   set-load-overwrites-styles
                   set-position
                   set-wordbreak-map
                   set-tabs
                   set-style-list
                   set-styles-fixed
                   change-style)
          (rename [super-on-char on-char])
          
          (define (in-single-line-comment? position)
            (let ([para (position-paragraph position)])
              (ormap
               (lambda (comment-start)
                 (let loop ([f (find-string comment-start 'backward position)])
                   (cond
                     [(not f)
                      #f]
                     [(= (position-paragraph f) para)
                      (let ([f-1 (- f 2)]) ;; -1 to go back one, -1 to be before char
                        (cond
                          [(< f-1 0)
                           #t]
                          [(not (= (position-paragraph f-1) para))
                           #t]
                          [(not (char=? (get-character f-1) #\\ ))
                           #t]
                          [else 
                           (loop (find-string comment-start 'backward f-1))]))]
                     [else 
                      #f])))
               (scheme-paren:get-comments))))
          
          
          (rename [super-on-close on-close])
          (override on-close)
          (define (on-close)
            (remove-indents-callback)
            (remove-paren-callback)
            (super-on-close))
          
          (define remove-indents-callback
            (preferences:add-callback
             'framework:tabify
             (lambda (p value)
               (set! indents value))))
          (define indents (preferences:get 'framework:tabify))
          [define backward-cache (make-object match-cache:%)]
          [define forward-cache (make-object match-cache:%)]
          [define in-highlight-parens? #f]
          
          (inherit get-styles-fixed)
          (rename [super-on-focus on-focus]
                  [super-after-change-style after-change-style]
                  [super-after-edit-sequence after-edit-sequence]
                  [super-after-insert after-insert]
                  [super-after-delete after-delete]
                  [super-after-set-size-constraint after-set-size-constraint]
                  [super-after-set-position after-set-position])
          (inherit has-focus? find-snip split-snip)
          (override on-focus after-change-style after-edit-sequence
                    after-insert after-delete
                    after-set-size-constraint after-set-position)
          (define (on-focus on?)
            (super-on-focus on?)
            (highlight-parens (not on?)))
          (define (after-change-style start len)
            (unless (local-edit-sequence?)
              (unless (get-styles-fixed)
                (when (has-focus?)
                  (highlight-parens))))
            (super-after-change-style start len))
          (define (after-edit-sequence)
            (super-after-edit-sequence)
            (unless (local-edit-sequence?)
              (when (has-focus?)
                (unless in-highlight-parens?
                  (highlight-parens)))))
          (define (after-insert start size)
            (send backward-cache invalidate start)
            (send forward-cache forward-invalidate start size)
            (unless (local-edit-sequence?)
              (when (has-focus?)
                (highlight-parens)))
            (super-after-insert start size))
          (define (after-delete start size)
            (super-after-delete start size)
            (send backward-cache invalidate start)
            (send forward-cache forward-invalidate (+ start size) (- size))
            (unless (local-edit-sequence?)
              (when (has-focus?)
                (highlight-parens))))
          (define (after-set-size-constraint)
            (unless (local-edit-sequence?)
              (when (has-focus?)
                (highlight-parens)))
            (super-after-set-size-constraint))
          (define (after-set-position)
            (unless (local-edit-sequence?)
              (when (has-focus?)
                (highlight-parens)))
            (super-after-set-position))
          
          [define highlight-parens? (preferences:get 'framework:highlight-parens)]
          [define remove-paren-callback (preferences:add-callback
                                         'framework:highlight-parens 
                                         (lambda (p value)
                                           (set! highlight-parens? value)))]
          (define (find-enclosing-paren pos)
            (let loop ([pos pos])
              (let ([paren-pos 
                     (let loop ([pairs (scheme-paren:get-paren-pairs)]
                                [curr-max #f])
                       (cond
                         [(null? pairs) curr-max]
                         [else (let* ([pair (car pairs)]
                                      [fnd (find-string (car pair) 'backward pos 'eof #f)])
                                 (if (and fnd curr-max)
                                     (loop (cdr pairs)
                                           (max fnd curr-max))
                                     (loop (cdr pairs)
                                           (or fnd curr-max))))]))])
                (cond
                  [(not paren-pos) #f]
                  [else
                   (let ([semi-pos (find-string ";" 'backward paren-pos)])
                     (cond
                       [(or (not semi-pos)
                            (semi-pos . < . (paragraph-start-position (position-paragraph paren-pos))))
                        paren-pos]
                       [else (loop (- semi-pos 1))]))]))))
          
          [define clear-old-locations 'dummy]
          (set! clear-old-locations void)
          
          (define/public highlight-parens
            (opt-lambda ([just-clear? #f])
              (when highlight-parens?
                (set! in-highlight-parens? #t)
                (begin-edit-sequence)
                (clear-old-locations)
                (set! clear-old-locations void)
                (unless just-clear?
                  (let* ([here (get-start-position)]
                         [there (get-end-position)]
                         [slash?
                          (lambda (before after)
                            (and (>= before 0)
                                 (>= after 0)
                                 (let ([text (get-text before after)])
                                   (and (string? text)
                                        (>= (string-length text) 1)
                                        (char=? #\\ (string-ref text 0))))))]
                         [is-paren?
                          (lambda (f)
                            (lambda (char)
                              (ormap (lambda (x) (char=? char (string-ref (f x) 0)))
                                     (scheme-paren:get-paren-pairs))))]
                         [is-left-paren? (is-paren? car)]
                         [is-right-paren? (is-paren? cdr)])
                    (when (= here there)
                      
                      ;; before, after : (list number number boolean) 
                      ;;  numbers indicate the range to highlight
                      ;;  boolean indicates if it is an errorneous highlight
                      (let ([before
                             (cond
                               [(and (> here 0)
                                     (is-right-paren? (get-character (sub1 here)))
                                     (not (in-single-line-comment? here)))
                                (cond
                                  [(slash? (- here 2) (- here 1)) #f]
                                  [(scheme-paren:backward-match
                                    this here (get-limit here)
                                    backward-cache)
                                   =>
                                   (lambda (end-pos) 
                                     (list end-pos here #f))]
                                  [else (list (- here 1) here #t)])]
                               [else #f])]
                            [after
                             (cond
                               [(and (is-left-paren? (get-character here))
                                     (not (in-single-line-comment? here)))
                                (cond
                                  [(slash? (- here 1) here) #f]
                                  [(scheme-paren:forward-match
                                    this here (last-position)
                                    forward-cache)
                                   =>
                                   (lambda (end-pos)
                                     (list here end-pos #f))]
                                  [else (list here (+ here 1) #t)])]
                               [else #f])]
                            [handle-single
                             (lambda (single)
                               (let* ([left (first single)]
                                      [right (second single)]
                                      [error? (third single)]
                                      [off (highlight-range 
                                            left
                                            right
                                            (if error? mismatch-color match-color)
                                            (and (send (icon:get-paren-highlight-bitmap) ok?)
                                                 (icon:get-paren-highlight-bitmap))
                                            (= there here left))])
                                 (set! clear-old-locations
                                       (let ([old clear-old-locations])
                                         (lambda ()
                                           (old)
                                           (off))))))])
                        
                        (cond
                          [(and after before)
                           (handle-single after)
                           (handle-single before)]
                          [after (handle-single after)]
                          [before (handle-single before)]
                          [else (void)])))))
                (end-edit-sequence)
                (set! in-highlight-parens? #f))))
          
          (public get-limit balance-quotes balance-parens tabify-on-return? tabify tabify-selection
                  tabify-all insert-return calc-last-para comment-out-selection uncomment-selection
                  get-forward-sexp remove-sexp forward-sexp flash-forward-sexp get-backward-sexp
                  flash-backward-sexp backward-sexp find-up-sexp up-sexp find-down-sexp down-sexp
                  remove-parens-forward)
          (define (get-limit pos) 0)
          
          (inherit get-visible-position-range)
          (define (balance-quotes key)
            (let* ([char (send key get-key-code)]) ;; must be a character because of the mapping setup
              ;; this function is only bound to ascii-returning keys
              (insert char)
              (let* ([start-pos (get-start-position)]
                     [limit (get-limit start-pos)]
                     [match (scheme-paren:backward-match
                             this start-pos limit backward-cache)])
                (when match
                  (let ([start-b (box 0)]
                        [end-b (box 0)]
                        [to-flash-point (add1 match)])
                    (get-visible-position-range start-b end-b #f)
                    (when (<= (unbox start-b) to-flash-point (unbox end-b))
                      (flash-on match (add1 match))))))))
          
          (define (balance-parens key-event)
            (letrec ([char (send key-event get-key-code)] ;; must be a character. See above.
                     [here (get-start-position)]
                     [limit (get-limit here)]
                     [paren-match? (preferences:get 'framework:paren-match)]
                     [fixup-parens? (preferences:get 'framework:fixup-parens)]
                     [find-match
                      (lambda (pos)
                        (let loop ([parens (scheme-paren:get-paren-pairs)])
                          (cond
                            [(null? parens) #f]
                            [else (let* ([paren (car parens)]
                                         [left (car paren)]
                                         [right (cdr paren)])
                                    (if (string=? left (get-text pos (+ pos (string-length left))))
                                        right
                                        (loop (cdr parens))))])))])
              (cond
                [(in-single-line-comment? here)
                 (insert char)]
                [(and (not (= 0 here))
                      (char=? (string-ref (get-text (- here 1) here) 0) #\\))
                 (insert char)]
                [(or paren-match? fixup-parens?)
                 (let* ([end-pos (scheme-paren:backward-containing-sexp 
                                  this here limit
                                  backward-cache)])
                   (cond
                     [end-pos
                      (let* ([left-paren-pos (find-enclosing-paren end-pos)]
                             [match (and left-paren-pos
                                         (find-match left-paren-pos))])
                        (cond
                          [match
                              (insert (if fixup-parens? match char))
                            (when paren-match?
                              (flash-on
                               left-paren-pos
                               (+ left-paren-pos (string-length match))))]
                          [else
                           (insert char)]))]
                     [else (insert char)]))]
                [else (insert char)])
              #t))
          
          (define (tabify-on-return?) #t)
          (define tabify    
            (opt-lambda ([pos (get-start-position)])
              (let* ([last-pos (last-position)]
                     [para (position-paragraph pos)]
                     [okay (> para 0)]
                     [end (if okay (paragraph-start-position para) 0)]
                     [limit (get-limit pos)]
                     [contains 
                      (if okay
                          (scheme-paren:backward-containing-sexp 
                           this end limit backward-cache)
                          #f)]
                     [contain-para (and contains
                                        (position-paragraph contains))]
                     [last 
                      (if contains
                          (scheme-paren:backward-match this end limit backward-cache)
                          #f)]
                     [last-para (and last
                                     (position-paragraph last))])
                (letrec	
                    ([find-offset
                      (lambda (pos)
                        (let loop ([p pos][o 0])
                          (let ([c (get-character p)])
                            (cond
                              [(char=? c #\tab)
                               (loop (add1 p) (+ o (- 8 (modulo o 8))))]
                              [(char=? c #\newline)
                               (cons o p)]
                              [(char-whitespace? c)
                               (loop (add1 p) (add1 o))]
                              [else
                               (cons o p)]))))]
                     [visual-offset
                      (lambda (pos)
                        (let loop ([p (sub1 pos)])
                          (if (= p -1)
                              0
                              (let ([c (get-character p)])
                                (cond
                                  [(char=? c #\null) 0]
                                  [(char=? c #\tab)
                                   (let ([o (loop (sub1 p))])
                                     (+ o (- 8 (modulo o 8))))]
                                  [(char=? c #\newline) 0]
                                  [else (add1 (loop (sub1 p)))])))))]
                     [do-indent
                      (lambda (amt)
                        (let* ([pos-start end]
                               [curr-offset (find-offset pos-start)])
                          (unless (= amt (car curr-offset))
                            (delete pos-start (cdr curr-offset))
                            (insert
                             (make-string amt #\space)
                             pos-start))))]
                     [id-walker
                      (lambda (string)
                        (let ([last (string-length string)])
                          (let loop ([index 0])
                            (if (= index last)
                                last
                                (let ([current (string-ref string index)])
                                  (if (or (char-alphabetic? current)
                                          (char-numeric? current))
                                      (loop (add1 index))
                                      (case current
                                        [(#\# 
                                          #\+ #\- #\. #\* #\/ #\< #\= #\> #\! #\? #\:
                                          #\$ #\% #\_ #\& #\^ #\~)
                                         (loop (add1 index))]
                                        [else index])))))))]
                     [get-proc
                      (lambda ()
                        (let* ([text (get-text contains (paragraph-end-position contain-para))])
                          (hash-table-get indents
                                          (string->symbol (substring text 0 (id-walker text)))
                                          (lambda () 'other))))]
                     [procedure-indent
                      (lambda ()
                        (case (get-proc)
                          [(define) 1]
                          [(begin) 1]
                          [(lambda) 3]
                          [else 0]))]
                     [special-check
                      (lambda ()
                        (let* ([proc-name (get-proc)])
                          (or (eq? proc-name 'define)
                              (eq? proc-name 'lambda))))]
                     [indent-first-arg
                      (lambda (start)
                        (car (find-offset start)))])
                  (when (and okay
                             (not (char=? (get-character (sub1 end))
                                          #\newline)))
                    (insert #\newline (paragraph-start-position para)))
                  (cond
                    [(let ([real-start (cdr (find-offset end))]) 
                       (and (<= (+ 3 real-start) (last-position))
                            (string=? ";;;"
                                      (get-text real-start
                                                (+ 2 real-start)))))
                     (void)]
                    [(= para 0) (do-indent 0)]
                    [(not contains)
                     (do-indent 0)]
                    [(not last) ;; search backwards for the opening parenthesis, and use it to align this line
                     (let ([enclosing (find-enclosing-paren pos)])
                       (do-indent (if enclosing
                                      (+ (visual-offset enclosing) 1)
                                      0)))]
                    [(= contains last)
                     (do-indent (+ (visual-offset contains)
                                   (procedure-indent)))]
                    [(special-check)
                     (do-indent (add1 (visual-offset contains)))]
                    [(= contain-para last-para)
                     (let ([name-length 
                            (id-walker (get-text contains (paragraph-end-position contain-para)))])
                       (do-indent (+ (visual-offset contains)
                                     name-length
                                     (indent-first-arg (+ contains 
                                                          name-length)))))]
                    [else
                     (do-indent (indent-first-arg (paragraph-start-position last-para)))])))))
          
          (define tabify-selection
            (opt-lambda ([start-pos (get-start-position)]
                         [end-pos (get-end-position)])
              (let ([first-para (position-paragraph start-pos)]
                    [end-para (position-paragraph end-pos)])
                (with-handlers ([exn:break?
                                 (lambda (x) #t)])
                  (dynamic-wind
                   (lambda () 
                     (when (< first-para end-para)
                       (begin-busy-cursor))
                     (begin-edit-sequence))
                   (lambda ()
                     (let loop ([para first-para])
                       (when (<= para end-para)
                         (tabify (paragraph-start-position para))
                         (dynamic-enable-break (lambda () (break-enabled)))
                         (loop (add1 para))))
                     (when (and (>= (position-paragraph start-pos) end-para)
                                (<= (paren:skip-whitespace 
                                     this (get-start-position) 'backward)
                                    (paragraph-start-position first-para)))
                       (set-position 
                        (let loop ([new-pos (get-start-position)])
                          (if (let ([next (get-character new-pos)])
                                (and (char-whitespace? next)
                                     (not (char=? next #\newline))))
                              (loop (add1 new-pos))
                              new-pos)))))
                   (lambda ()
                     (end-edit-sequence)
                     (when (< first-para end-para)
                       (end-busy-cursor))))))))
          
          (define (tabify-all) (tabify-selection 0 (last-position)))
          (define (insert-return)
            (if (tabify-on-return?)
                (begin 
                  (begin-edit-sequence)
                  (insert #\newline)
                  (tabify (get-start-position))
                  (set-position 
                   (let loop ([new-pos (get-start-position)])
                     (if (let ([next (get-character new-pos)])
                           (and (char-whitespace? next)
                                (not (char=? next #\newline))))
                         (loop (add1 new-pos))
                         new-pos)))
                  (end-edit-sequence))
                (insert #\newline)))
          
          (define (calc-last-para last-pos)
            (let ([last-para (position-paragraph last-pos #t)])
              (if (and (> last-pos 0)
                       (> last-para 0))
                  (begin (split-snip last-pos)
                         (let ([snip (find-snip last-pos 'before)])
                           (if (member 'hard-newline (send snip get-flags))
                               (- last-para 1)
                               last-para)))
                  last-para)))
          
          (define comment-out-selection
            (opt-lambda ([start-pos (get-start-position)]
                         [end-pos (get-end-position)])
              (begin-edit-sequence)
              (let ([first-pos-is-first-para-pos?
                     (= (paragraph-start-position (position-paragraph start-pos))
                        start-pos)])
                (let* ([first-para (position-paragraph start-pos)]
                       [last-para (calc-last-para end-pos)])
                  (let para-loop ([curr-para first-para])
                    (if (<= curr-para last-para)
                        (let ([first-on-para (paragraph-start-position curr-para)])
                          (insert #\; first-on-para)
                          (para-loop (add1 curr-para))))))
                (when first-pos-is-first-para-pos?
                  (set-position
                   (paragraph-start-position (position-paragraph (get-start-position)))
                   (get-end-position))))
              (end-edit-sequence)
              #t))
          
          (define uncomment-selection
            (opt-lambda ([start-pos (get-start-position)]
                         [end-pos (get-end-position)])
              (begin-edit-sequence)
              (let* ([last-pos (last-position)]
                     [first-para (position-paragraph start-pos)]
                     [last-para (calc-last-para end-pos)])
                (let para-loop ([curr-para first-para])
                  (if (<= curr-para last-para)
                      (let ([first-on-para
                             (paren:skip-whitespace 
                              this 
                              (paragraph-start-position curr-para)
                              'forward)])
                        (when (and (< first-on-para last-pos)
                                   (char=? #\; (get-character first-on-para)))
                          (delete first-on-para (+ first-on-para 1)))
                        (para-loop (add1 curr-para))))))
              (end-edit-sequence)
              #t))
          
          [define get-forward-sexp
            (lambda (start-pos)
              (scheme-paren:forward-match 
               this start-pos
               (last-position)
               forward-cache))]
          [define remove-sexp
            (lambda (start-pos)
              (let ([end-pos (get-forward-sexp start-pos)])
                (if end-pos 
                    (kill 0 start-pos end-pos)
                    (bell)))
              #t)]
          [define forward-sexp
            (lambda (start-pos)
              (let ([end-pos (get-forward-sexp start-pos)])
                (if end-pos 
                    (set-position end-pos)
                    (bell))
                #t))]
          [define flash-forward-sexp
            (lambda (start-pos)
              (let ([end-pos (get-forward-sexp start-pos)])
                (if end-pos 
                    (flash-on end-pos (add1 end-pos))
                    (bell)) 
                #t))]	    
          [define get-backward-sexp
            (lambda (start-pos)
              (let* ([limit (get-limit start-pos)]
                     [end-pos
                      (scheme-paren:backward-match 
                       this start-pos limit backward-cache)]
                     [min-pos
                      (scheme-paren:backward-containing-sexp 
                       this start-pos limit backward-cache)]
                     [ans
                      (if (and end-pos 
                               (or (not min-pos)
                                   (>= end-pos min-pos)))
                          end-pos
                          #f)])
                ans))]
          [define flash-backward-sexp
            (lambda (start-pos)
              (let ([end-pos (get-backward-sexp start-pos)])
                (if end-pos
                    (flash-on end-pos (add1 end-pos))
                    (bell))
                #t))]
          [define backward-sexp
            (lambda (start-pos)
              (let ([end-pos (get-backward-sexp start-pos)])
                (if end-pos
                    (set-position end-pos)
                    (bell))
                #t))]
          [define find-up-sexp
            (lambda (start-pos)
              (let* ([exp-pos
                      (scheme-paren:backward-containing-sexp 
                       this start-pos
                       (get-limit start-pos) 
                       backward-cache)]
                     [paren-pos ;; find the closest open paren from this pair, behind exp-pos
                      (lambda (paren-pair)
                        (find-string
                         (car paren-pair)
                         'backward
                         exp-pos))])
                
                (if (and exp-pos (> exp-pos 0))
                    (let ([poss (let loop ([parens (scheme-paren:get-paren-pairs)])
                                  (cond
                                    [(null? parens) null]
                                    [else 
                                     (let ([pos (paren-pos (car parens))])
                                       (if pos
                                           (cons pos (loop (cdr parens)))
                                           (loop (cdr parens))))]))])
                      (if (null? poss) ;; all finds failed
                          #f
                          (- (apply max poss) 1))) ;; subtract one to move outside the paren
                    #f)))]
          [define up-sexp
            (lambda (start-pos)
              (let ([exp-pos (find-up-sexp start-pos)])
                (if exp-pos
                    (set-position exp-pos)
                    (bell))
                #t))]
          [define find-down-sexp
            (lambda (start-pos)
              (let ([last (last-position)])
                (let loop ([pos start-pos])
                  (let ([next-pos (scheme-paren:forward-match 
                                   this pos last
                                   forward-cache)])
                    (if (and next-pos (> next-pos pos))
                        (let ([back-pos
                               (scheme-paren:backward-containing-sexp 
                                this (sub1 next-pos) pos backward-cache)])
                          (if (and back-pos
                                   (> back-pos pos))
                              back-pos
                              (loop next-pos)))
                        #f)))))]
          [define down-sexp
            (lambda (start-pos)
              (let ([pos (find-down-sexp start-pos)])
                (if pos
                    (set-position pos)
                    (bell))
                #t))]
          [define remove-parens-forward
            (lambda (start-pos)
              (let* ([pos (paren:skip-whitespace this start-pos 'forward)]
                     [first-char (get-character pos)]
                     [paren? (or (char=? first-char #\( )
                                 (char=? first-char #\[ ))]
                     [closer (if paren? 
                                 (scheme-paren:forward-match 
                                  this pos (last-position)
                                  forward-cache))])
                (if (and paren? closer)
                    (begin (begin-edit-sequence)
                           (delete pos (add1 pos))
                           (delete (-  closer 2) (- closer 1))
                           (end-edit-sequence))
                    (bell))
                #t))]
          
          [define select-text
            (lambda (f forward?)
              (let* ([start-pos (get-start-position)]
                     [end-pos (get-end-position)])
                (let-values ([(new-start new-end)
                              (if forward?
                                  (values start-pos (f end-pos))
                                  (values (f start-pos) end-pos))])
                  (if (and new-start new-end) 
                      (set-position new-start new-end)
                      (bell))
                  #t)))]
          (public select-forward-sexp select-backward-sexp select-up-sexp select-down-sexp
                  transpose-sexp mark-matching-parenthesis)
          
          [define select-forward-sexp (lambda () (select-text (lambda (x) (get-forward-sexp x)) #t))]
          [define select-backward-sexp (lambda () (select-text (lambda (x) (get-backward-sexp x)) #f))]
          [define select-up-sexp (lambda () (select-text (lambda (x) (find-up-sexp x)) #f))]
          [define select-down-sexp (lambda () (select-text (lambda (x) (find-down-sexp x)) #t))]
          
          (define (mark-matching-parenthesis pos)
            (let ([open-parens (map car (scheme-paren:get-paren-pairs))]
                  [close-parens (map cdr (scheme-paren:get-paren-pairs))])
              (when (member (string (get-character pos)) open-parens)
                (let ([end (get-forward-sexp pos)])
                  (when (and end
                             (member (string (get-character (- end 1))) close-parens))
                    (let ([start-style (send (find-snip pos 'after) get-style)]
                          [end-style (send (find-snip end 'before) get-style)])
                      (cond
                        [(and (eq? matching-parenthesis-style start-style)
                              (eq? matching-parenthesis-style end-style))
                         (let ([standard-style (send style-list find-named-style "Standard")])
                           (change-style standard-style pos (+ pos 1))
                           (change-style standard-style (- end 1) end))]
                        [else
                         (change-style matching-parenthesis-style pos (+ pos 1))
                         (change-style matching-parenthesis-style (- end 1) end)])))))))
          
          [define transpose-sexp
            (lambda (pos)
              (let ([start-1 (get-backward-sexp pos)])
                (if (not start-1)
                    (bell)
                    (let ([end-1 (get-forward-sexp start-1)])
                      (if (not end-1)
                          (bell)
                          (let ([end-2 (get-forward-sexp end-1)])
                            (if (not end-2)
                                (bell)
                                (let ([start-2 (get-backward-sexp end-2)])
                                  (if (or (not start-2)
                                          (< start-2 end-1))
                                      (bell)
                                      (let ([text-1 
                                             (get-text start-1 end-1)]
                                            [text-2 
                                             (get-text start-2 end-2)])
                                        (begin-edit-sequence)
                                        (insert text-1 start-2 end-2)
                                        (insert text-2 start-1 end-1)
                                        (set-position end-2)
                                        (end-edit-sequence)))))))))))]
          [define tab-size 8]
          (public get-tab-size set-tab-size)
          [define get-tab-size (lambda () tab-size)]
          [define set-tab-size (lambda (s) (set! tab-size s))]
          
          (rename [super-get-keymaps get-keymaps])
          (override get-keymaps)
          [define get-keymaps
            (lambda ()
              (cons keymap (super-get-keymaps)))]
          
          (super-instantiate ())
          
          (highlight-parens #t)
          (set-load-overwrites-styles #f)
          (set-wordbreak-map wordbreak-map)
          (set-tabs null tab-size #f)
          (set-style-list style-list)
          (set-styles-fixed #t)))
      
      (define -text% (text-mixin text:info%))

      

                                                                                           
              ;;                                 ;;                                        
               ;                                  ;                                        
               ;                                  ;                                        
  ;;;    ;;;   ; ;;    ;;;  ;;; ;    ;;;          ;  ;;   ;;;  ;;; ;;;;;; ;   ;;;;  ; ;;;  
 ;   ;  ;   ;  ;;  ;  ;   ;  ; ; ;  ;   ;         ; ;    ;   ;  ;   ;  ; ; ;      ;  ;   ; 
  ;;;   ;      ;   ;  ;;;;;  ; ; ;  ;;;;;         ;;     ;;;;;  ;   ;  ; ; ;   ;;;;  ;   ; 
     ;  ;      ;   ;  ;      ; ; ;  ;             ; ;    ;       ; ;   ; ; ;  ;   ;  ;   ; 
 ;   ;  ;   ;  ;   ;  ;   ;  ; ; ;  ;   ;         ;  ;   ;   ;   ;;;   ; ; ;  ;   ;  ;   ; 
  ;;;    ;;;  ;;; ;;;  ;;;  ;; ; ;;  ;;;         ;;   ;;  ;;;     ;   ;; ; ;;  ;;; ; ;;;;  
                                                                  ;                  ;     
                                                                  ;                  ;     
                                                                ;;                  ;;;    
      (define setup-keymap
        (lambda (keymap)
          
          (let ([add-pos-function
                 (lambda (name call-method)
                   (send keymap add-function name
                         (lambda (edit event)
                           (call-method
                            edit
                            (send edit get-start-position)))))])
            (add-pos-function "remove-sexp" (lambda (e p) (send e remove-sexp p)))
            (add-pos-function "forward-sexp" (lambda (e p) (send e forward-sexp p)))
            (add-pos-function "backward-sexp" (lambda (e p) (send e backward-sexp p)))
            (add-pos-function "up-sexp" (lambda (e p) (send e up-sexp p)))
            (add-pos-function "down-sexp" (lambda (e p) (send e down-sexp p)))
            (add-pos-function "flash-backward-sexp" (lambda (e p) (send e flash-backward-sexp p)))
            (add-pos-function "flash-forward-sexp" (lambda (e p) (send e flash-forward-sexp p)))
            (add-pos-function "remove-parens-forward" (lambda (e p) (send e remove-parens-forward p)))
            (add-pos-function "transpose-sexp" (lambda (e p) (send e transpose-sexp p)))
            (add-pos-function "mark-matching-parenthesis"
                              (lambda (e p) (send e mark-matching-parenthesis p))))
          
          (let ([add-edit-function
                 (lambda (name call-method)
                   (send keymap add-function name
                         (lambda (edit event)
                           (call-method edit))))])
            (add-edit-function "select-forward-sexp" 
                               (lambda (x) (send x select-forward-sexp)))
            (add-edit-function "select-backward-sexp"  
                               (lambda (x) (send x select-backward-sexp)))
            (add-edit-function "select-down-sexp"  
                               (lambda (x) (send x select-down-sexp)))
            (add-edit-function "select-up-sexp"  
                               (lambda (x) (send x select-up-sexp)))
            (add-edit-function "tabify-at-caret"  
                               (lambda (x) (send x tabify-selection)))
            (add-edit-function "do-return"  
                               (lambda (x) (send x insert-return)))
            (add-edit-function "comment-out"  
                               (lambda (x) (send x comment-out-selection)))
            (add-edit-function "uncomment"  
                               (lambda (x) (send x uncomment-selection))))
          
          (send keymap add-function "balance-parens"
                (lambda (edit event)
                  (send edit balance-parens event)))
          (send keymap add-function "balance-quotes"
                (lambda (edit event)
                  (send edit balance-quotes event)))
          
          (send keymap map-function "TAB" "tabify-at-caret")
          
          (send keymap map-function "return" "do-return")
          (send keymap map-function "s:return" "do-return")
          (send keymap map-function "s:c:return" "do-return")
          (send keymap map-function "a:return" "do-return")
          (send keymap map-function "s:a:return" "do-return")
          (send keymap map-function "c:a:return" "do-return")
          (send keymap map-function "c:s:a:return" "do-return")
          (send keymap map-function "c:return" "do-return")
          (send keymap map-function "d:return" "do-return")
          
          (send keymap map-function ")" "balance-parens")
          (send keymap map-function "]" "balance-parens")
          (send keymap map-function "}" "balance-parens")
          (send keymap map-function "\"" "balance-quotes")
          (send keymap map-function "|" "balance-quotes")
          
          ;(send keymap map-function "c:up" "up-sexp") ;; paragraph
          ;(send keymap map-function "s:c:up" "select-up-sexp")
          
          ;(send keymap map-function "c:down" "down-sexp") ;; paragraph
          ;(send keymap map-function "s:c:down" "select-down-sexp")
          
          (let ([map-meta
                 (lambda (key func)
                   (keymap:send-map-function-meta keymap key func))]
                [map
                 (lambda (key func)
                   (send keymap map-function key func))])
            
            (map-meta "up" "up-sexp")
            (map-meta "c:u" "up-sexp")
            (map "a:up" "up-sexp")
            (map-meta "s:up" "select-up-sexp")
            (map "a:s:up" "select-up-sexp")
            (map-meta "s:c:u" "select-up-sexp")
            
            (map-meta "down" "down-sexp")
            (map "a:down" "down-sexp")
            (map-meta "c:down" "down-sexp")
            (map-meta "s:down" "select-down-sexp")
            (map "a:s:down" "select-down-sexp")
            (map-meta "s:c:down" "select-down-sexp")
            
            (map-meta "right" "forward-sexp")
            (map "a:right" "forward-sexp")
            (map-meta "s:right" "select-forward-sexp")
            (map "a:s:right" "select-forward-sexp")
            
            (map-meta "left" "backward-sexp")
            (map "a:left" "backward-sexp")
            (map-meta "s:left" "select-backward-sexp")
            (map "a:s:left" "select-backward-sexp")
            
            (map-meta "return" "do-return")
            (map-meta "s:return" "do-return")
            (map-meta "s:c:return" "do-return")
            (map-meta "a:return" "do-return")
            (map-meta "s:a:return" "do-return")
            (map-meta "c:a:return" "do-return")
            (map-meta "c:s:a:return" "do-return")
            (map-meta "c:return" "do-return")
            
            (map-meta "c:semicolon" "comment-out")
            (map-meta "c:=" "uncomment")
            (map-meta "c:k" "remove-sexp")
            
            (map-meta "c:f" "forward-sexp")
            (map-meta "s:c:f" "select-forward-sexp")
            
            (map-meta "c:b" "backward-sexp")
            (map-meta "s:c:b" "select-backward-sexp")
            
            (map-meta "c:p" "flash-backward-sexp")
            (map-meta "s:c:n" "flash-forward-sexp")
            
            (map-meta "c:space" "select-forward-sexp")
            (map-meta "c:t" "transpose-sexp")
            
            (map-meta "c:m" "mark-matching-parenthesis"))
          (send keymap map-function "c:c;c:b" "remove-parens-forward")))
      
      (define keymap (make-object keymap:aug-keymap%))
      (setup-keymap keymap)
      (define (get-keymap) keymap)
      
                                                                             
                        ;;;                                            ;;;   
                       ;                                                 ;   
                       ;                                                 ;   
; ;;;   ; ;;;   ;;;   ;;;;;   ;;;         ; ;;;   ;;;;  ; ;;;    ;;;     ;   
 ;   ;   ;     ;   ;   ;     ;   ;         ;   ;      ;  ;;  ;  ;   ;    ;   
 ;   ;   ;     ;;;;;   ;      ;;;          ;   ;   ;;;;  ;   ;  ;;;;;    ;   
 ;   ;   ;     ;       ;         ;         ;   ;  ;   ;  ;   ;  ;        ;   
 ;   ;   ;     ;   ;   ;     ;   ;         ;   ;  ;   ;  ;   ;  ;   ;    ;   
 ;;;;   ;;;;    ;;;   ;;;;    ;;;          ;;;;    ;;; ;;;;  ;;  ;;;   ;;;;;;
 ;                                         ;                                 
 ;                                         ;                                 
;;;                                       ;;;                                

      
      (define (add-preferences-panel)
        (preferences:add-panel
         (string-constant indenting-prefs-panel-label)
         (lambda (p)
           (let*-values
               ([(get-keywords)
                 (lambda (hash-table)
                   (letrec ([all-keywords (hash-table-map hash-table list)]
                            [pick-out (lambda (wanted in out)
                                        (cond
                                          [(null? in) (quicksort out string<=?)]
                                          [else (if (eq? wanted (cadr (car in))) 
                                                    (pick-out wanted (cdr in) (cons (symbol->string (car (car in))) out))
                                                    (pick-out wanted (cdr in) out))]))])
                     (values  (pick-out 'begin all-keywords null)
                              (pick-out 'define all-keywords null)
                              (pick-out 'lambda all-keywords null))))]
                [(begin-keywords define-keywords lambda-keywords)
                 (get-keywords (preferences:get 'framework:tabify))])
             (letrec ([add-button-callback
                       (lambda (keyword-type keyword-symbol list-box)
                         (lambda (button command)
                           (let ([new-one
                                  (keymap:call/text-keymap-initializer
                                   (lambda ()
                                     (get-text-from-user
                                      (format (string-constant enter-new-keyword) keyword-type)
                                      (format (string-constant x-keyword) keyword-type))))])
                             (when new-one
                               (let ([parsed (with-handlers ((exn:read? (lambda (x) #f)))
                                               (read (open-input-string new-one)))])
                                 (cond
                                   [(and (symbol? parsed)
                                         (hash-table-get (preferences:get 'framework:tabify)
                                                         parsed
                                                         (lambda () #f)))
                                    (message-box (string-constant error)
                                                 (format (string-constant already-used-keyword) parsed))]
                                   [(symbol? parsed)
                                    (let ([ht (preferences:get 'framework:tabify)])
                                      (hash-table-put! ht parsed keyword-symbol)
                                      (update-list-boxes ht))]
                                   [else (message-box 
                                          (string-constant error)
                                          (format (string-constant expected-a-symbol) new-one))]))))))]
                      [delete-callback
                       (lambda (list-box)
                         (lambda (button command)
                           (let* ([selections (send list-box get-selections)]
                                  [symbols (map (lambda (x) (string->symbol (send list-box get-string x))) selections)])
                             (for-each (lambda (x) (send list-box delete x)) (reverse selections))
                             (let ([ht (preferences:get 'framework:tabify)])
                               (for-each (lambda (x) (hash-table-remove! ht x)) symbols)))))]
                      [main-panel (make-object horizontal-panel% p)]
                      [make-column
                       (lambda (string symbol keywords)
                         (let* ([vert (make-object vertical-panel% main-panel)]
                                [_ (make-object message% (format (string-constant x-like-keywords) string) vert)]
                                [box (make-object list-box% #f keywords vert void '(multiple))]
                                [button-panel (make-object horizontal-panel% vert)]
                                [add-button (make-object button% (string-constant add-keyword)
                                              button-panel (add-button-callback string symbol box))]
                                [delete-button (make-object button% (string-constant remove-keyword)
                                                 button-panel (delete-callback box))])
                           (send* button-panel 
                             (set-alignment 'center 'center)
                             (stretchable-height #f))
                           (send add-button min-width (send delete-button get-width))
                           box))]
                      [begin-list-box (make-column "Begin" 'begin begin-keywords)]
                      [define-list-box (make-column "Define" 'define define-keywords)]
                      [lambda-list-box (make-column "Lambda" 'lambda lambda-keywords)]
                      [update-list-boxes
                       (lambda (hash-table)
                         (let-values ([(begin-keywords define-keywords lambda-keywords) (get-keywords hash-table)]
                                      [(reset) (lambda (list-box keywords)
                                                 (send list-box clear)
                                                 (for-each (lambda (x) (send list-box append x)) keywords))])
                           (reset begin-list-box begin-keywords)
                           (reset define-list-box define-keywords)
                           (reset lambda-list-box lambda-keywords)
                           #t))])
               (preferences:add-callback 'framework:tabify (lambda (p v) (update-list-boxes v)))
               main-panel))))))))
