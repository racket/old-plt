
(module tool mzscheme
  (require "private/xml-snip-helpers.ss"
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "tool.ss" "drscheme")
           (lib "xml.ss" "xml")
           (lib "string-constant.ss" "string-constants"))
  
  (provide tool@)
  
  (define orig (current-output-port))
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (printf . args) (apply fprintf orig args))
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      (preferences:set-default 'drscheme:xml-eliminate-whitespace #t boolean?)
      
      (define xml-box-color "forest green")
      (define scheme-splice-box-color "blue")
      (define scheme-box-color "purple")
      
      ;; get-bm : string -> (union (is-a?/c bitmap%) false?)
      (define (get-bm name)
        (let ([bm (make-object bitmap% (build-path (collection-path "icons") name))])
          (unless (send bm ok?)
            (error 'xml-box "bitmap ~a failed to load" name))
          bm))
      
      (define scheme-box-bm (get-bm "scheme-box.jpg"))
      (define scheme-splice-box-bm (get-bm "scheme-splice-box.jpg"))
      (define xml-box-bm (get-bm "xml-box.jpg"))
      (define xml-box-open-bm (get-bm "xml-box-open.jpg"))
      
      (define (make-string-snip obj)
        (let* ([str (format "~e" obj)]
               [sn (make-object string-snip% (string-length str))])
          (send sn insert str (string-length str) 0)
          sn))
      
      (define xml-snip%
        (class* decorated-editor-snip% (readable-snip<%> xml-snip<%>)
          (inherit get-editor)
          
          (init-field eliminate-whitespace-in-empty-tags?)
          
          (define/override (make-editor) (make-object (get-xml-text%)))
          (define/override (get-corner-bitmap) 
            (if eliminate-whitespace-in-empty-tags?
                xml-box-bm
                xml-box-open-bm))
          
          (define/override (get-menu)
            (let* ([menu (instantiate popup-menu% ()
                           (title (string-constant xml-tool-xml-box)))]
                   [leave-alone-item
                    (make-object checkable-menu-item%
                      (string-constant xml-tool-leave-whitespace-alone)
                      menu
                      (lambda (x y)
                        (set-eliminate-whitespace-in-empty-tags? #f)))]
                   [eliminate-item
                    (make-object checkable-menu-item%
                      (string-constant xml-tool-eliminate-whitespace-in-empty-tags)
                      menu
                      (lambda (x y)
                        (set-eliminate-whitespace-in-empty-tags? #t)))])
              (send leave-alone-item check (not eliminate-whitespace-in-empty-tags?))
              (send eliminate-item check eliminate-whitespace-in-empty-tags?)
              menu))
          
          (inherit get-admin reset-min-sizes)
          (define/private (set-eliminate-whitespace-in-empty-tags? new)
            (unless (eq? eliminate-whitespace-in-empty-tags? new)
              (set! eliminate-whitespace-in-empty-tags? new)
              (preferences:set 'drscheme:xml-eliminate-whitespace new)
              (reset-min-sizes)
              (let ([admin (get-admin)])
                (when admin
                  (send admin resized this #t)))))
          
          (define/public (read-one-special index file line col pos)
            (xml-read-one-special eliminate-whitespace-in-empty-tags?
                                  translate-xml-exn-to-rep-exn
                                  this
                                  file
                                  line
                                  col
                                  pos))
          
          (define/override (write stream-out)
            (send stream-out put (if eliminate-whitespace-in-empty-tags?
                                     0
                                     1))
            (send (get-editor) write-to-file stream-out 0 'eof))
          (define/override (make-snip)
            (instantiate xml-snip% ()
              [eliminate-whitespace-in-empty-tags? eliminate-whitespace-in-empty-tags?]))
          
          (define/override (get-color) xml-box-color)
          
          (inherit show-border set-snipclass)
          (super-instantiate ())
          (show-border #t)
          (set-snipclass lib-xml-snipclass)))
      
      ; translate-xml-exn-to-rep-exn : editor -> exn -> alpha
      ; translates a xml exn to a drscheme:rep:make-exn:locs exn
      ; using `editor' as the location. raises the exn.
      (define (translate-xml-exn-to-rep-exn editor)
        (lambda (exn)
          (raise
           (drscheme:rep:make-exn:locs
            (exn-message exn)
            (exn-continuation-marks exn)
            (map (lambda (x) 
                   (let ([start (car x)]
                         [end (cadr x)])
                     (list editor (- start 1) (- end 1))))
                 (exn:xml-locs exn))))))
      
      (define xml-snipclass%
        (class decorated-editor-snipclass%
          (define/override (make-snip stream-in)
            (instantiate xml-snip% ()
              [eliminate-whitespace-in-empty-tags?
               (preferences:get 'drscheme:xml-eliminate-whitespace)]))
          (super-instantiate ())))
      
      ;; this snipclass is for old, saved files (no snip has it set)
      (define xml-snipclass (make-object xml-snipclass%))
      (send xml-snipclass set-version 1)
      (send xml-snipclass set-classname "drscheme:xml-snip")
      (send (get-the-snip-class-list) add xml-snipclass)
      
      ;; this snipclass overrides the actual one in (lib "xml-snipclass.ss" "xml")
      ;; as a full-fledged snipclass, for use in DrScheme.
      
      (define lib-xml-snipclass%
        (class snip-class%
          (define/override (read stream-in)
            (let* ([eliminate-whitespace-in-empty-tags? (zero? (send stream-in get-exact))]
                   [snip (instantiate xml-snip% ()
                           [eliminate-whitespace-in-empty-tags? eliminate-whitespace-in-empty-tags?])])
              (send (send snip get-editor) read-from-file stream-in)
              snip))
          (super-instantiate ())))
      
      (define lib-xml-snipclass (make-object lib-xml-snipclass%))
      (send lib-xml-snipclass set-version 1)
      (send lib-xml-snipclass set-classname (format "~s" '(lib "xml-snipclass.ss" "xml")))
      (send (get-the-snip-class-list) add lib-xml-snipclass)
      
      (define scheme-snip%
        (class* decorated-editor-snip% (scheme-snip<%> readable-snip<%>)
          (init-field splice?)
          (define/public (get-splice?) splice?)
          
          (define/override (get-corner-bitmap) 
            (if splice? 
                scheme-splice-box-bm
                scheme-box-bm))
          
          (define/override (get-menu)
            (let ([menu (instantiate popup-menu% ()
                          (title (if splice?
                                     (string-constant xml-tool-scheme-splice-box)
                                     (string-constant xml-tool-scheme-box))))])
              (instantiate menu-item% ()
                (label
                 (if splice?
                     (string-constant xml-tool-switch-to-scheme)
                     (string-constant xml-tool-switch-to-scheme-splice)))
                (parent menu)
                (callback (lambda (x y) (toggle-splice))))
              menu))
          
          (inherit get-admin reset-min-sizes)
          (define/private (toggle-splice)
            (let ([admin (get-admin)])
              (set! splice? (not splice?))
              (reset-min-sizes)
              (when admin
                (send admin resized this #t))))
          
          (inherit get-editor)
          
          (define/public (read-one-special index file line col pos)
            (scheme-read-one-special this file line col pos))
          
          (define/override (make-editor)
            (make-object ((drscheme:unit:get-program-editor-mixin)
                          (add-file-keymap-mixin
                           (scheme:text-mixin 
                            (editor:keymap-mixin text:basic%))))))
          
          (define/override (make-snip) 
            (instantiate scheme-snip% () (splice? splice?)))
          
          (rename [super-write write])
          (define/override (write stream-out)
            (send stream-out put (if splice? 0 1))
            (super-write stream-out))
          
          (inherit show-border set-snipclass)
          (define/override (get-color)
            (if splice?
                scheme-splice-box-color
                scheme-box-color))
          
          (super-instantiate ())
          (show-border #t)
          (set-snipclass lib-scheme-snipclass)))
      
      (define (add-file-keymap-mixin %)
        (class %
          (rename [super-get-keymaps get-keymaps])
          (define/override (get-keymaps)
            (cons (keymap:get-file) (super-get-keymaps)))
          (super-instantiate ())))
      
      (define scheme-snipclass%
        (class decorated-editor-snipclass%
          (define/override (make-snip stream-in)
            (let* ([splice? (zero? (send stream-in get-exact))])
              (instantiate scheme-snip% () (splice? splice?))))
          (super-instantiate ())))
      
      ;; this snipclass is for old, saved files (no snip has it set)
      (define scheme-snipclass (make-object scheme-snipclass%))
      (send scheme-snipclass set-version 2)
      (send scheme-snipclass set-classname "drscheme:scheme-snip")
      (send (get-the-snip-class-list) add scheme-snipclass)
      
      ;; this snipclass overrides the one in (lib "scheme-snipclass.ss" "xml")
      ;; as a full-fledged snipclass, for use in DrScheme.
      (define lib-scheme-snipclass (make-object scheme-snipclass%))
      (send lib-scheme-snipclass set-version 1)
      (send lib-scheme-snipclass set-classname (format "~s" '(lib "scheme-snipclass.ss" "xml")))
      (send (get-the-snip-class-list) add lib-scheme-snipclass)
      
      (define plain-text%
        (class text:keymap% 
          (init-field [delta (make-object style-delta%)])
          (inherit change-style copy-self-to)
          (rename [super-after-insert after-insert]
                  [super-on-insert on-insert])
          (inherit begin-edit-sequence end-edit-sequence)
          (define/override (copy-self)
            (let ([t (make-object plain-text% delta)])
              (copy-self-to t)
              t))
          (define/override (on-insert x y)
            (super-on-insert x y)
            (begin-edit-sequence))
          (define/override (after-insert x y)
            (super-after-insert x y)
            (change-style delta x (+ x y))
            (end-edit-sequence))
          
          (rename [super-get-keymaps get-keymaps])
          (define/override (get-keymaps)
            (cons (keymap:get-file) (super-get-keymaps)))
          
          (inherit set-styles-sticky)
          (super-instantiate ())
          (set-styles-sticky #f)))
      
      (define xml-keymap (make-object keymap%))
      (send xml-keymap add-function 
            "matching-xml" 
            (lambda (x e) 
              (when (is-a? x text%)
                (matching-xml x))))
      (send xml-keymap map-function ">" "matching-xml")
      
      ;; The Scheme style list's Standard size changes
      ;; according to the font size preference. So,
      ;; we create an XML style based on that stule
      ;; for the XML boxes, so they change size too.
      (let* ([style-list (scheme:get-style-list)]
             [style (send style-list find-named-style "XML")]) 
        (unless style
          (let ([xml-delta (make-object style-delta% 'change-family 'default)])
            (send style-list new-named-style "XML" 
                  (send style-list find-or-create-style 
                        (send style-list find-named-style "Standard")
                        xml-delta)))))

      (define xml-text-mixin
        (mixin (editor:keymap<%> (class->interface text%)) ()
          (rename [super-get-keymaps get-keymaps])
          (define/override (get-keymaps)
            (cons xml-keymap (super-get-keymaps)))
          
          (rename [super-after-insert after-insert]
                  [super-on-insert on-insert])
          (inherit begin-edit-sequence end-edit-sequence
                   change-style get-style-list)
          (define/override (on-insert start rng)
            (super-on-insert start rng)
            (begin-edit-sequence))
          (define/override (after-insert start rng)
            (super-after-insert start rng)
            (change-style (send (get-style-list) find-named-style "XML")
                          start 
                          (+ start rng))
            (end-edit-sequence))
          
          (super-instantiate ())
          
          (inherit set-style-list)
          (set-style-list (scheme:get-style-list))))
      
      (define get-xml-text%
	(let ([xml-text% #f])
	  (lambda ()
	    (unless xml-text%
	      (set! xml-text% ((drscheme:unit:get-program-editor-mixin)
			       (xml-text-mixin
				plain-text%))))
	    xml-text%)))

      ;; matching-xml : (is-a?/c text) -> void
      ;; inserts > and if there is an XML tag just
      ;; before the caret, inserts the corresponding
      ;; close XML tag after the caret.
      (define (matching-xml text)
        (send text begin-edit-sequence)
        (send text insert ">")
        (let* ([start (send text get-start-position)]
               [tagname (find-tag text start)])
          (when tagname
            (send text insert "</")
            (send text insert tagname)
            (send text insert ">")
            (send text set-position start)))
        (send text end-edit-sequence))
      
      ;; find-tag : (is-a?/c text%) number? -> (union false? string?)
      ;; finds the name of the XML tag just before `start' in `text'.
      ;; returns the tag name, with no trailing space of > or anything like that.
      (define (find-tag text start)
        ;; loop iterates backwards, searching for #\<
        ;; when it finds it, it gets the string starting
        ;; there, forwards to last-space (if there was a space)
        ;; or start-1.
        ;; If there is a #\/ or a #\> just after the #\<, return #f
        ;; (in that case, they are typing a close tag or closing an empty tag)
        ;; this technique gleaned from the spec at:
        ;;  http://www.w3.org/TR/2000/REC-xml-20001006
        (let loop ([pos (- start 2)]
                   [last-space #f])
          (cond
            [(< pos 0) #f]
            [else
             (let ([char (send text get-character pos)])
               (case char
                 [(#\>) #f]
                 [(#\<) 
                  (if (or (char=? (send text get-character (+ pos 1)) #\/)
                          (char=? (send text get-character (+ pos 1)) #\>))
                      #f
                      (send text get-text (+ pos 1) (or last-space (- start 1))))]
                 [(#\space #\return #\newline #\tab)
                  (loop (- pos 1) pos)]
                 [else (loop (- pos 1) last-space)]))])))
      
      (define (xml-box-frame-extension super%)
        (class super%
          (inherit get-editor get-special-menu get-edit-target-object)
          
          (super-instantiate ())
          
          (let* ([menu (get-special-menu)]
                 [find-insertion-point ;; -> (union #f editor<%>)
                  ;; returns the editor (if there is one) with the keyboard focus
                  (lambda ()
                    (let ([editor (get-edit-target-object)])
                      (and editor
                           (is-a? editor editor<%>)
                           (let loop ([editor editor])
                             (let ([focused (send editor get-focus-snip)])
                               (if (and focused
                                        (is-a? focused editor-snip%))
                                   (loop (send focused get-editor))
                                   editor))))))]
                 [insert-snip
                  (lambda (make-obj)
                    (let ([editor (find-insertion-point)])
                      (when editor
                        (let ([snip (make-obj)])
                          (send editor insert snip)
                          (send editor set-caret-owner snip 'display)))))]
                 [demand-callback ;; : menu-item% -> void
                  ;; enables the menu item when there is an editor available.
                  (lambda (item)
                    (send item enable (find-insertion-point)))])
            (instantiate menu:can-restore-menu-item% ()
              (label (string-constant xml-tool-insert-xml-box))
              (parent menu)
              (demand-callback demand-callback)
              (callback
               (lambda (menu evt)
                 (insert-snip
                  (lambda () 
                    (instantiate xml-snip% ()
                      [eliminate-whitespace-in-empty-tags?
                       (preferences:get 'drscheme:xml-eliminate-whitespace)]))))))
            (instantiate menu:can-restore-menu-item% ()
              (label (string-constant xml-tool-insert-scheme-box))
              (parent menu)
              (demand-callback demand-callback)
              (callback 
               (lambda (menu evt)
                 (insert-snip 
                  (lambda () (instantiate scheme-snip% () (splice? #f)))))))
            (instantiate menu:can-restore-menu-item% ()
              (label (string-constant xml-tool-insert-scheme-splice-box))
              (parent menu)
              (demand-callback demand-callback)
              (callback
               (lambda (menu evt)
                 (insert-snip
                  (lambda () (instantiate scheme-snip% () (splice? #t))))))))
          
          (frame:reorder-menus this)))
      
      (drscheme:get/extend:extend-unit-frame xml-box-frame-extension))))
