(module server mzscheme 
  
  (require (lib "etc.ss")
	   (lib "class.ss")
           (lib "contract.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
	   (lib "configuration.ss" "web-server"))
  
  (require (lib "internal-server.ss" "web-server")
	   (lib "browser.ss" "net"))
  
  (require "server-config.ss"
           "standard-urls.ss")
  
  (provide/contract (start-help-server (mixin-contract . -> . hd-cookie?)))
  (provide hd-cookie->port
	   hd-cookie->exit-proc
           hd-cookie->browser-mixin
	   hd-cookie?)
  
  (define start-help-server
    (opt-lambda ([addl-browser-frame-mixin (lambda (x) x)])
      (internal-start-help-server addl-browser-frame-mixin)))
  
  (preferences:set-default 'drscheme:help-desk:frame-width 350 number?)
  (preferences:set-default 'drscheme:help-desk:frame-height 400 number?)
  (preferences:set-default 'drscheme:help-desk:search-how 1 (lambda (x) (member x '(0 1 2))))
  (preferences:set-default 'drscheme:help-desk:search-where 1 (lambda (x) (member x '(0 1 2))))
  
  ;; internal-start-help-desk-server : (mixin frame:basic<%>) -> hd-cookie
  (define (internal-start-help-server addl-browser-frame-mixin)
    (install-help-browser-preference-panel) ; in case we need prefs
    (let* ([configuration (build-developer-configuration (build-config-exp))]
           [hd-cookie (make-hd-cookie min-port #f #f addl-browser-frame-mixin)]
           [combined-browser-mixin
            (compose addl-browser-frame-mixin 
                     help-desk-framework-mixin
                     (make-generic-browser-frame-mixin hd-cookie))])
      (let-values ([(exit-proc browser-maker)
                    (serve configuration min-port #f combined-browser-mixin)])
        (set-hd-cookie-exit-proc! hd-cookie exit-proc)
        (set-hd-cookie-browser! hd-cookie browser-maker)
        hd-cookie)))
  
  (define help-desk-fill-in-mixin
    (mixin (frame:searchable<%>) ()
      (define/override (get-text-to-search)
        (send (send (send this get-hyper-panel) get-canvas) get-editor))
      (super-instantiate ())))
  
  (define help-desk-framework-mixin
    (compose help-desk-fill-in-mixin
             frame:searchable-mixin
             frame:standard-menus-mixin))
  
  (define (make-generic-browser-frame-mixin hd-cookie)
    (mixin (frame:basic<%>) ()
      (rename [super-on-size on-size])
      
      (field [search-panel #f])
      
      (rename [super-make-root-area-container make-root-area-container])
      (define/override (make-root-area-container class parent)
        (let* ([search-panel-parent (super-make-root-area-container vertical-panel% parent)]
               [main-panel (make-object class search-panel-parent)])
          (set! search-panel (instantiate vertical-panel% ()
                               (parent search-panel-parent)
                               (stretchable-height #f)))
          main-panel))
      
      (define/override (on-size w h)
        (preferences:set 'drscheme:help-desk:frame-width w)
        (preferences:set 'drscheme:help-desk:frame-height h)
        (super-on-size w h))
      
      (super-instantiate ()
        (width (preferences:get 'drscheme:help-desk:frame-width))
        (height (preferences:get 'drscheme:help-desk:frame-height))
        (label (string-constant help-desk)))
      
      (let ([hp (send this get-hyper-panel)])
        (send hp set-init-page (make-home-page-url (hd-cookie->port hd-cookie)))
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
                      (hd-cookie->port hd-cookie)
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
        (send search-menu enable #f))
      
      (frame:reorder-menus this))))
