(module browser-extensions mzscheme
  (require (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "contract.ss")
           (lib "etc.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "url.ss" "net")
           (lib "browser.ss" "net")
           "standard-urls.ss"
           "cookie.ss"
           "docpos.ss")
  
  (provide make-help-desk-frame-mixin
           make-bug-report/help-desk-mixin)
  
  ;; where should the pref stuff really go?
  (install-help-browser-preference-panel)
  (preferences:set-default 'drscheme:help-desk:last-url-string "" string?)
  (preferences:set-default 'drscheme:help-desk:frame-width 350 number?)
  (preferences:set-default 'drscheme:help-desk:frame-height 400 number?)
  (preferences:set-default 'drscheme:help-desk:search-how 1 (lambda (x) (member x '(0 1 2))))
  (preferences:set-default 'drscheme:help-desk:search-where 1 (lambda (x) (member x '(0 1 2))))

  (preferences:set-default 'drscheme:help-desk:separate-browser #t boolean?)
  (preferences:set-default 'drscheme:help-desk:ask-about-external-urls #t boolean?)
  
  (preferences:add-to-warnings-checkbox-panel
   (lambda (panel)
     (let ([cb (instantiate check-box% ()
                 (label (string-constant plt:hd:use-homebrew-browser))
                 (parent panel)
                 (callback
                  (lambda (cb evt)
                    (preferences:set 'drscheme:help-desk:separate-browser
                                     (not (send cb get-value))))))])
       (preferences:add-callback 
        'drscheme:help-desk:separate-browser
        (lambda (p v) (send cb set-value (not v))))
       (void))))
  
  (define (make-help-desk-frame-mixin hd-cookie)
    (compose
     (make-catch-url-frame-mixin hd-cookie)
     (make-bug-report/help-desk-mixin hd-cookie)
     (make-help-desk-framework-mixin hd-cookie)
     frame:searchable-mixin
     frame:standard-menus-mixin
     (make-search-button-mixin hd-cookie)))
  
  (define (make-bug-report/help-desk-mixin hd-cookie)
    (mixin (frame:standard-menus<%>) ()
      (define/override (file-menu:create-open-recent?) #f)
      (define/override (help-menu:about-string)
        (string-constant plt:hd:about-help-desk))
      (define/override (help-menu:about-callback i e)
        (message-box (string-constant plt:hd:about-help-desk)
                     (format 
                      (string-constant plt:hd:help-desk-about-string)
                      (version:version))
                     this))
      (define/override (help-menu:create-about?) #t)
      (define/override (help-menu:after-about menu)
        (make-object menu-item% (string-constant plt:hd:help-on-help) menu
          (lambda (i e)
            (message-box
             (string-constant plt:hd:help-on-help)
             (string-constant plt:hd:help-on-help-details)
             this))))
      (super-instantiate ())))

  ;; redirect urls to outside pages to external browsers (depending on the preferences settings)
  ;; also catches links into documentation that isn't installed yet and sends that
  ;; to the missing manuals page.
  (define (make-catch-url-frame-mixin hd-cookie)
    (define (catch-url-hyper-panel-mixin %)
      (class %
        (rename [super-get-canvas% get-canvas%])
        (define/override (get-canvas%)
          (catch-url-canvas-mixin (super-get-canvas%)))
        (super-instantiate ())))
    
    (define (catch-url-canvas-mixin %)
      (class %
        (define/override (remap-url url)
          (let ([internal-url-test (hd-cookie-url-on-server-test hd-cookie)]
                [extract-url-path (hd-cookie-extract-url-path hd-cookie)])
            (cond
              [(internal-url-test url)
               =>
               (lambda (s)
                 (let ([external?
                        (cond
                          
                          ;; .plt files are always internal, no matter where from
                          ;; they will be caught elsewhere.
                          [(regexp-match #rx".plt$" s) #f]
                          
                          ;; files on download.plt-scheme.org in /doc are considered
                          ;; things that we should view in the browser itself.
                          [(is-download.plt-scheme.org/doc-url? s)
                           #f]
                          
                          [(preferences:get 'drscheme:help-desk:ask-about-external-urls)
                           (ask-user-about-separate-browser)]
                          [else
                           (preferences:get 'drscheme:help-desk:separate-browser)])])
                   (if external? 
                       (begin (send-url s) #f)
                       url)))]
              [(extract-url-path url)
               =>
               (lambda (s)
                 (let ([m (regexp-match #rx"^/doc/([^/]*)/" s)])
                   (if m
                       (let ([coll (cadr m)])
                         (cond
                           [(or (file-exists? (build-path (collection-path "doc") coll "index.htm"))
                                (not (assoc coll known-docs)))
                            url]
                           [else
                            (let ([doc-pr (assoc coll known-docs)]
                                  [url-str ((hd-cookie-url->string hd-cookie) url)])
                              (make-missing-manual-url hd-cookie coll (cdr doc-pr) url-str))]))
                       url)))]
              [else 
               url])))
        (super-instantiate ())))
    
    (lambda (%)
      (class %
        (rename [super-get-hyper-panel% get-hyper-panel%])
        (define/override (get-hyper-panel%)
          (catch-url-hyper-panel-mixin (super-get-hyper-panel%)))
        (super-instantiate ()))))

  (define (is-download.plt-scheme.org/doc-url? s)
    (let ([url (string->url s)])
      (and (equal? "download.plt-scheme.org" (url-host url))
           (string? (url-path url))
           (regexp-match #rx"^/doc" (url-path url)))))
  
  (define (ask-user-about-separate-browser)
    (define separate-default? (preferences:get 'drscheme:help-desk:separate-browser))
    (define dlg (instantiate dialog% ()
                  (label (string-constant help-desk))
                  (alignment '(left center))))
    (define hp (instantiate horizontal-panel% ()
                 (parent dlg)
                 (alignment '(center top))))
    (define icon (instantiate message% ()
                   (parent hp)
                   (label 'app)))
    (define txt (instantiate text% ()))
    (define ec-choice-panel (instantiate vertical-panel% ()
                              (parent hp)))
    (define ec (instantiate editor-canvas% ()
                 (parent ec-choice-panel)
                 (editor txt)
                 (min-width 400)
                 (min-height 150)))
    (define dont-ask-checkbox (instantiate check-box% ()
                                (label (string-constant dont-ask-again))
                                (parent ec-choice-panel)
                                (callback void)))
    (define button-panel (instantiate horizontal-panel% ()
                           (parent dlg)
                           (stretchable-height #f)
                           (alignment '(right center))))
    (define homebrew-button (instantiate button% ()
                              (label (string-constant plt:hd:homebrew-browser))
                              (parent button-panel)
                              (callback (lambda (x y) (homebrew-callback)))
                              (style (if separate-default?
                                         '()
                                         '(border)))))
    (define separate-button (instantiate button% ()
                              (label (string-constant plt:hd:separate-browser))
                              (parent button-panel)
                              (callback (lambda (x y) (separate-callback)))
                              (style (if separate-default?
                                         '(border)
                                         '()))))
    
    (define (homebrew-callback)
      (update-dont-ask)
      (set! answer #f)
      (preferences:set 'drscheme:help-desk:separate-browser #f)
      (send dlg show #f))
    (define (separate-callback)
      (update-dont-ask)
      (set! answer #t)
      (preferences:set 'drscheme:help-desk:separate-browser #t)
      (send dlg show #f))
    (define (update-dont-ask)
      (when (send dont-ask-checkbox get-value)
        (preferences:set 'drscheme:help-desk:ask-about-external-urls #f)))
    
    (define answer #f)
    
    (send txt insert (string-constant plt:hd:ask-about-separate-browser))
    (send txt set-autowrap-bitmap #f)
    (send txt set-position 0)
    (send txt lock #t)
    (send txt hide-caret #t)
    (send dlg reflow-container)
    (send txt auto-wrap #t)
    (send dlg center 'both)
    (send dlg show #t)
    answer)
  
  
  (define (make-help-desk-framework-mixin hd-cookie)
    (mixin (frame:searchable<%> frame:standard-menus<%>) ()
      (define/override (get-text-to-search)
        (send (send (send this get-hyper-panel) get-canvas) get-editor))
      
      (define/override (file-menu:create-new?) #t)
      (define/override (file-menu:new-callback x y)
        (visit-url-in-new-browser 
         hd-cookie
         (make-home-page-url
          (hd-cookie-port hd-cookie))))

      (define/override (file-menu:create-open-recent?) #f)
      
      (define/override (file-menu:create-open?) #f)
      (define/override (file-menu:create-print?) #t)
      
      (define/override (file-menu:print-callback x y)
        (let ([ed (send (send (send this get-hyper-panel) get-canvas) get-editor)])
          (and ed
               (send ed print))))
      
      (rename [super-file-menu:between-open-and-revert file-menu:between-open-and-revert])
      (define/override (file-menu:between-open-and-revert file-menu)
        (super-file-menu:between-open-and-revert file-menu)
        (instantiate menu:can-restore-menu-item% ()
          (parent file-menu)
          (callback (lambda (_1 _2) (open-url-callback)))
          (label (string-constant open-url...)))
        (instantiate menu:can-restore-menu-item% ()
          (parent file-menu)
          (label (string-constant reload))
          (callback (lambda (_1 _2) (send (send this get-hyper-panel) reload)))))
      
      (define/private (open-url-callback)
        (let ([url (get-url-from-user this)])
          (when url
            (let* ([hp (send this get-hyper-panel)]
                   [hc (send hp get-canvas)])
              (send hc goto-url url #f)))))
              
      (rename [super-on-size on-size])
      (define/override (on-size w h)
        (preferences:set 'drscheme:help-desk:frame-width w)
        (preferences:set 'drscheme:help-desk:frame-height h)
        (super-on-size w h))
      
      (super-instantiate ()
        (width (preferences:get 'drscheme:help-desk:frame-width))
        (height (preferences:get 'drscheme:help-desk:frame-height)))
      
      (frame:reorder-menus this)))
  
  (define (make-search-button-mixin hd-cookie)
    (mixin (frame:basic<%>) ()
      (field [search-panel #f])
      
      (rename [super-make-root-area-container make-root-area-container])
      (define/override (make-root-area-container class parent)
        (let* ([search-panel-parent (super-make-root-area-container vertical-panel% parent)]
               [main-panel (make-object class search-panel-parent)])
          (set! search-panel (instantiate vertical-panel% ()
                               (parent search-panel-parent)
                               (stretchable-height #f)))
          main-panel))
      
      (super-instantiate ()
        (label (string-constant help-desk)))
      
      (let ([hp (send this get-hyper-panel)])
        (send hp set-init-page (make-home-page-url (hd-cookie-port hd-cookie)))
        (send (send hp get-canvas) allow-tab-exit #t))
      
      (inherit get-menu-bar)
      (let ()
        (define search-menu (instantiate menu% ()
                              (label (string-constant plt:hd:search))
                              (parent (get-menu-bar))))
        (define search-menu-item (instantiate menu:can-restore-menu-item% ()
                                   (label (string-constant plt:hd:search))
                                   (parent search-menu)
                                   (shortcut #\e)
                                   (callback
                                    (lambda (x y) (search-callback #f)))))
        (define lucky-menu-item (instantiate menu:can-restore-menu-item% ()
                                  (label (string-constant plt:hd:feeling-lucky))
                                  (parent search-menu)
                                  (shortcut #\l)
                                  (callback
                                   (lambda (x y) (search-callback #t)))))
        (define field-panel (instantiate horizontal-panel% ()
                              (parent search-panel)))
        (define search-field (instantiate text-field% ()
                               (label (string-constant plt:hd:find-docs-for))
                               (callback (lambda (x y)
                                           (let ([on? (not (equal? "" (send search-field get-value)))])
                                             (send search-button enable on?)
                                             (send search-menu enable on?))))
                               (parent field-panel)))
        (define choices-panel (instantiate horizontal-panel% ()
                                (parent search-panel)
                                (alignment '(center center))))
        
        (define search-button (instantiate button% ()
                                (label (string-constant plt:hd:search))
                                (parent field-panel)
                                (callback (lambda (x y) (search-callback #f)))
                                (style '(border))))
        (define search-where (instantiate choice% ()
                               (label #f)
                               (parent choices-panel)
                               (selection (preferences:get 'drscheme:help-desk:search-where))
                               (choices
                                (list
                                 (string-constant plt:hd:search-for-keyword)
                                 (string-constant plt:hd:search-for-keyword-or-index)
                                 (string-constant plt:hd:search-for-keyword-or-index-or-text)))
                               (callback
                                (lambda (x y)
                                  (preferences:set 'drscheme:help-desk:search-where
                                                   (send search-where get-selection))))))
        (define search-how (instantiate choice% ()
                             (label #f)
                             (parent choices-panel)
                             (selection (preferences:get 'drscheme:help-desk:search-how))
                             (choices 
                              (list
                               (string-constant plt:hd:exact-match)
                               (string-constant plt:hd:containing-match)
                               (string-constant plt:hd:regexp-match)))
                             (callback
                              (lambda (x y)
                                (preferences:set 'drscheme:help-desk:search-how
                                                 (send search-how get-selection))))))
        (define grow-box-spacer (make-object grow-box-spacer-pane% choices-panel))
        (define (search-callback lucky?)
          (let ([url (make-results-url
                      (hd-cookie-port hd-cookie)
                      (send search-field get-value)
                      (case (send search-where get-selection)
                        [(0) "keyword"]
                        [(1) "keyword-index"]
                        [(2) "keyword-index-text"])
                      (case (send search-how get-selection)
                        [(0) "exact-match"]
                        [(1) "containing-match"]
                        [(2) "regexp-match"])
                      lucky?)])
            
            ;; have to use `send this' since I don't know yet
            ;; *which* (unit) instantiation of the hyper panel I might
            ;; be overriding and so I don't know the particular
            ;; interface. I do however, know the name of the method.....
            (send (send (send this get-hyper-panel) get-canvas) goto-url url #f)))
        
        (send search-button enable #f)
        (send search-menu enable #f)
        (send search-field focus))))
  
  (define/contract get-url-from-user
    ((union false? (is-a?/c top-level-window<%>)) . -> . (union false? string?))
    (lambda (parent)
      (define d (make-object dialog% (string-constant open-url) parent 500))
      (define t
        (keymap:call/text-keymap-initializer
         (lambda ()
           (make-object text-field% (string-constant url:) d
             (lambda (t e)
               (update-ok))))))
      (define p (make-object horizontal-panel% d))
      (define browse (make-object button% (string-constant browse...) p
                       (lambda (b e)
                         (let ([f (get-file)])
                           (when f
                             (send t set-value (string-append "file:" f))
                             (update-ok))))))
      (define spacer (make-object vertical-pane% p))
      (define result #f)
      (define (ok-callback b e)
        (let* ([s (send t get-value)]
               [done (lambda ()
                       ;; Might be called twice!
                       (preferences:set 'drscheme:help-desk:last-url-string s)
                       (send d show #f))])
          (with-handlers ([not-break-exn?
                           (lambda (x)
                             (message-box (string-constant bad-url) 
                                          (format (string-constant bad-url:this)
                                                  (exn-message x))
                                          d))])
            (let* ([removed-spaces (regexp-replace #rx"^[ \t]*" s "")]
                   [str (cond
                          [(regexp-match ":" removed-spaces) removed-spaces]
                          [(regexp-match #rx"^[a-zA-Z][a-zA-Z.]*($|/)" removed-spaces)
                           (string-append "http://" removed-spaces)]
                          [else
                           (string-append "file:" removed-spaces)])]

                   ;; just convert the string to test it out; don't use result...
                   [url (string->url str)])
              (set! result str)
              (done)))))
      (define cancel-callback (lambda (b e) (send d show #f)))
      (define-values (ok cancel)
        (gui-utils:ok/cancel-buttons
         p
         ok-callback
         cancel-callback))
      (define (update-ok)
        (send ok enable 
              (positive? (send (send t get-editor) 
                               last-position))))
      (define last-url-string (preferences:get 'drscheme:help-desk:last-url-string))
      (when last-url-string 
        (send t set-value last-url-string)
        (let ([text (send t get-editor)])
          (send text set-position 0 (send text last-position))))
      (send p set-alignment 'right 'center)
      (update-ok)
      (send d center)
      (send t focus)
      (send d show #t)
      result)))