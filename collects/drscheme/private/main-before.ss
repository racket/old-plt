
(module main-before mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
           "drsig.ss"
	   (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "unitsig.ss")
           (lib "class.ss")
           (prefix pretty-print: (lib "pretty.ss"))
           (prefix print-convert: (lib "pconvert.ss"))
	   (lib "include.ss")
           (lib "list.ss")
           (lib "file.ss")
           (lib "plt-installer.ss" "setup"))
  
  (provide main-before@)
  
  (define main-before@
    (unit/sig ()
      (import (drscheme:app : drscheme:app^)
              (drscheme:unit : drscheme:unit^)
              (drscheme:get/extend : drscheme:get/extend^)
              (drscheme:language-configuration : drscheme:language-configuration/internal^)
	      (drscheme:language : drscheme:language^)
              (drscheme:teachpack : drscheme:teachpack^)
              [drscheme:module-language : drscheme:module-language^])
      
      (finder:default-extension "scm")
      (application:current-app-name (string-constant drscheme))
      (version:add-spec 'd 7)
      
      (preferences:set-default 'drscheme:unit-window-size-percentage 1/2 
                               (lambda (x) (and (number? x) (<= 0 x 1))))
      
      (let ([frame-width 600]
            [frame-height 650]
            [window-trimming-upper-bound-width 20]
            [window-trimming-upper-bound-height 50])
        (let-values ([(w h) (get-display-size)])
          (set! frame-width (min frame-width (- w window-trimming-upper-bound-width)))
          (set! frame-height (min frame-height (- h window-trimming-upper-bound-height))))
        (preferences:set-default 'drscheme:unit-window-width frame-width number?)
        (preferences:set-default 'drscheme:unit-window-height frame-height number?))
      
      (preferences:set-default 'drscheme:backtrace-window-width 400 number?)
      (preferences:set-default 'drscheme:backtrace-window-height 300 number?)
      
      (preferences:set-default 
       'drscheme:keybindings-window-size
       (cons 200 400)
       (lambda (x) (and (pair? x)
                        (number? (car x))
                        (number? (cdr x)))))
      
      (preferences:set-default
       'drscheme:execute-warning-once
       #f
       (lambda (x)
         (or (eq? x #t)
             (not x))))
      
      (preferences:set-default
       'drscheme:teachpacks
       (drscheme:teachpack:new-teachpack-cache) 
       drscheme:teachpack:teachpack-cache?)
      (preferences:set-un/marshall
       'drscheme:teachpacks
       drscheme:teachpack:marshall-teachpack-cache
       drscheme:teachpack:unmarshall-teachpack-cache)
      
      (define get-fixed-faces
        (cond
          [(eq? (system-type) 'unix) 
           (lambda () (get-face-list))]
          [else
           (lambda ()
             (let* ([canvas (make-object canvas% (make-object frame% "bogus"))]
                    [dc (send canvas get-dc)]
                    [ans
                     (let loop ([faces (get-face-list)])
                       (cond
                         [(null? faces) null]
                         [else (let* ([face (car faces)]
                                      [font (make-object font% 12 face 'default 'normal 'normal #f)])
                                 (let*-values ([(wi _1 _2 _3) (send dc get-text-extent "i" font)]
                                               [(ww _1 _2 _3) (send dc get-text-extent "w" font)])
                                   (if (and (= ww wi) 
                                            (not (zero? ww)))
                                       (cons face (loop (cdr faces)))
                                       (loop (cdr faces)))))]))])
               (set! get-fixed-faces (lambda () ans))
               ans))]))
      
      (define get-all-faces
        (cond
          [(eq? (system-type) 'unix) #f]
          [else (lambda () (get-face-list))]))
      
      (define default-font-name (get-family-builtin-face 'modern))
      
      (preferences:set-default 'drscheme:font-name default-font-name string?)
      
      (preferences:set-default
       'drscheme:font-size
       (send (send (send (make-object text%) 
                         get-style-list)
                   basic-style)
             get-size)
       (lambda (x) (and (number? x) (exact? x) (= x (floor x)))))
      
      (define (set-font-size size)
        (let* ([scheme-standard (send (scheme:get-style-list)
                                      find-named-style "Standard")]
               [scheme-delta (make-object style-delta%)])
          (send scheme-standard get-delta scheme-delta)
          (send scheme-delta set-size-mult 0)
          (send scheme-delta set-size-add size)
          (send scheme-standard set-delta scheme-delta)))
      
      (define (set-font-name name)
        (let* ([scheme-standard (send (scheme:get-style-list)
                                      find-named-style "Standard")]
               [scheme-delta (make-object style-delta%)])
          (send scheme-standard get-delta scheme-delta)
          (send scheme-delta set-delta-face name)
          (send scheme-delta set-family 'modern)
          (send scheme-standard set-delta scheme-delta)))
      
      (set-font-size (preferences:get 'drscheme:font-size))
      (set-font-name (preferences:get 'drscheme:font-name))
      
      (preferences:add-callback
       'drscheme:font-size
       (lambda (p v)
         (set-font-size v)))
      
      (preferences:add-callback
       'drscheme:font-name
       (lambda (p v)
         (set-font-name v)))
      
      (unless (member (preferences:get 'drscheme:font-name)
                      (get-fixed-faces))
        (preferences:set 'drscheme:font-name default-font-name))
      
      (preferences:add-panel
       (string-constant font-prefs-panel-title)
       (lambda (panel)
         (let* ([main (make-object vertical-panel% panel)]
                [options-panel (make-object horizontal-panel% main)]
                [size (make-object slider% (string-constant font-size) 1 72 options-panel
                        (lambda (size evt)
                          (preferences:set 'drscheme:font-size (send size get-value)))
                        (preferences:get 'drscheme:font-size))]
                
                [font-name-control
                 (case (system-type)
                   [(windows macos macosx)
                    (let ([choice
                           (make-object choice% (string-constant font-name)
                             (get-fixed-faces)
                             options-panel
                             (lambda (font-name evt)
                               (preferences:set 
                                'drscheme:font-name
                                (send font-name get-string-selection))))])
                      (send choice set-string-selection (preferences:get 'drscheme:font-name))
                      choice)]
                   [(unix)
                    (make-object button%
                      (string-constant set-font)
                      options-panel
                      (lambda xxx
                        (let ([choice (get-choices-from-user
                                       (string-constant select-font-name)
                                       (string-constant select-font-name)
                                       (get-fixed-faces))])
                          (when choice
                            (preferences:set 
                             'drscheme:font-name 
                             (list-ref (get-fixed-faces) (car choice)))))))]
                   [else (error 'font-name-control "unknown system type: ~s~n" (system-type))])]
                
                [text (make-object text%)]
                [ex-panel (make-object horizontal-panel% main)]
                [msg (make-object message% (string-constant example-text) ex-panel)]
                [canvas (make-object editor-canvas% main text)]
                [update-text
                 (lambda (setting)
                   (send text begin-edit-sequence)
                   (send text lock #f)
                   (send text erase)
                   (send text insert 
                         (format
                          ";; howmany : list-of-numbers -> number~
                         \n;; to determine how many numbers are in `a-lon'~
                         \n(define (howmany a-lon)~
                         \n  (cond~
                         \n    [(empty? a-lon) 0]~
                         \n    [else (+ 1 (howmany (rest a-lon)))]))~
                         \n~
                         \n;; examples as tests~
                         \n(howmany empty)~
                         \n=~
                         \n0~
                         \n~
                         \n(howmany (cons 1 (cons 2 (cons 3 empty))))~
                         \n=~
                         \n3"))
                   (send text set-position 0 0)
                   (send text lock #t)
                   (send text end-edit-sequence))])
           
           (preferences:add-callback
            drscheme:language-configuration:settings-preferences-symbol
            (lambda (p v)
              (update-text v)))
           (update-text (preferences:get drscheme:language-configuration:settings-preferences-symbol))
           (send ex-panel set-alignment 'left 'center)
           (send ex-panel stretchable-height #f)
           (send canvas allow-tab-exit #t)
           (send options-panel stretchable-height #f)
           (send options-panel set-alignment 'center 'top)
           (send text set-style-list (scheme:get-style-list))
           (send text lock #t)
           main)))
      
      (scheme:add-preferences-panel)
      (preferences:add-general-panel)
      
      (preferences:add-panel
       (string-constant general-ii)
       (lambda (panel)
         (let* ([main (make-object vertical-panel% panel)]
                [right-align-in-main
                 (lambda (f)
                   (let ([hp (make-object horizontal-panel% main)])
                     (send hp stretchable-height #f)
                     (begin0 (f hp)
                             (make-object horizontal-panel% hp))))]
                [make-check-box
                 (lambda (pref-sym string)
                   (right-align-in-main
                    (lambda (p)
                      (let ([q (make-object check-box%
                                 string
                                 p
                                 (lambda (checkbox evt)
                                   (preferences:set 
                                    pref-sym 
                                    (send checkbox get-value))))])
                        (send q set-value (preferences:get pref-sym))))))])
           (make-check-box 'drscheme:execute-warning-once (string-constant only-warn-once))
           (make-object vertical-panel% main)
           main)))
      
      (let* ([use-copy-mixin
              (lambda (%)
                (class %
                  (define/override (use-namespace-require/copy?) #t)
                  (super-instantiate ())))]
             [make-simple
              (lambda (module position)
                (instantiate 
                    (use-copy-mixin
                     (drscheme:language:module-based-language->language-mixin
                      (drscheme:language:simple-module-based-language->module-based-language-mixin
                       drscheme:language:simple-module-based-language%)))
                  ()
                  (module module)
                  (language-position position)))])
	(drscheme:language-configuration:add-language
	 (make-simple '(lib "full-mred.ss" "lang")
                      (list (string-constant r5rs-like-languages)
                            (string-constant mred-w/debug))))
	(drscheme:language-configuration:add-language
	 (make-simple '(lib "full-mzscheme.ss" "lang") 
                      (list (string-constant r5rs-like-languages)
                            (string-constant mzscheme-w/debug))))
        (drscheme:language-configuration:add-language
	 (make-simple '(lib "r5rs.ss" "lang")
                      (list (string-constant r5rs-like-languages)
                            (string-constant r5rs-w/debug)))))
      
      (drscheme:module-language:add-module-language)
      (drscheme:language-configuration:add-info-specified-languages)
      
  ;; add a handler to open .plt files.
      (handler:insert-format-handler 
       "PLT Files"
       (lambda (filename)
         (and (equal? "plt" (filename-extension filename))
              (gui-utils:get-choice 
               (format (string-constant install-plt-file) filename)
               (string-constant install-plt-file/yes)
               (string-constant install-plt-file/no))))
       (lambda (filename)
         (run-installer filename))))))
