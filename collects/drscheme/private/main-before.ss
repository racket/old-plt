
(module main-before mzscheme
  (require (lib "unitsig.ss")
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
              (drscheme:language : drscheme:language/internal^)
	      (drscheme:language-tower : drscheme:language-tower^)
              (drscheme:teachpack : drscheme:teachpack^))
      
      (finder:default-extension "scm")
      (application:current-app-name "DrScheme")
      (version:add-spec 'd 105)
      
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
       'drscheme:enable-backtrace-in-teaching-levels
       #f
       boolean?)
      
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
      
      
      
      (include "various-programs.ss")
      
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
                                   (if (= ww wi)
                                       (cons face (loop (cdr faces)))
                                       (loop (cdr faces)))))]))])
               (set! get-fixed-faces (lambda () ans))
               ans))]))
      
      (define default-font-name (get-family-builtin-face 'modern))
      
      (preferences:set-default
       'drscheme:font-name
       default-font-name
       string?)
      
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
       "Font"
       (lambda (panel)
         (let* ([main (make-object vertical-panel% panel)]
                [options-panel (make-object horizontal-panel% main)]
                [size (make-object slider% "Font Size" 1 72 options-panel
                        (lambda (size evt)
                          (preferences:set 'drscheme:font-size (send size get-value)))
                        (preferences:get 'drscheme:font-size))]
                
                [font-name-control
                 (case (system-type)
                   [(windows macos)
                    (let ([choice
                           (make-object choice% "Font Name"
                             (get-fixed-faces)
                             options-panel
                             (lambda (font-name evt)
                               (preferences:set 
                                'drscheme:font-name
                                (send font-name get-string-selection))))])
                      (send choice set-string-selection
                            (preferences:get 'drscheme:font-name))
                      choice)]
                   [else
                    (make-object button%
                      "Set Font..."
                      options-panel
                      (lambda xxx
                        (let ([choice (get-choices-from-user
                                       "Select Font Name"
                                       "Select Font Name"
                                       (get-fixed-faces))])
                          (when choice
                            (preferences:set 
                             'drscheme:font-name 
                             (list-ref (get-fixed-faces) (car choice)))))))])]
                
                [text (make-object text%)]
                [ex-panel (make-object horizontal-panel% main)]
                [msg (make-object message% "Example Text:" ex-panel)]
                [canvas (make-object editor-canvas% main text)]
                [update-text
                 (lambda (setting)
                   (send text begin-edit-sequence)
                   (send text lock #f)
                   (send text erase)
                   (send text insert (format "<<language name>>"))
                   (send text set-position 0 0)
                   (send text lock #t)
                   (send text end-edit-sequence))])
           
           (preferences:add-callback
            drscheme:language:settings-preferences-symbol
            (lambda (p v)
              (update-text v)))
           (update-text (preferences:get drscheme:language:settings-preferences-symbol))
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
       "General II"
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
           (make-check-box 'drscheme:execute-warning-once
                           "Only warn once when executions and interactions are not synchronized")
           (make-check-box 'drscheme:enable-backtrace-in-teaching-levels
                           "Enable backtrace bug icon in teaching languages")
           (make-object vertical-panel% main)
           main)))
      
      (let ([make-simple
	     (lambda (lang ps)
	       (make-object drscheme:language-tower:module-based-language->language%
		 (make-object drscheme:language-tower:simple-module-based-language->module-based-language%
		   (make-object drscheme:language-tower:simple-module-based-language%
		     lang
		     ps))))])
	(drscheme:language:add-language
	 (make-simple '(lib "full-mred.ss" "lang") '("Full" "Graphical (MrEd)")))
	(drscheme:language:add-language
	 (make-simple 'mzscheme '("Full" "Textual (MzScheme)"))))
      
  ;; add a handler to open .plt files.
      (handler:insert-format-handler 
       "Projects"
       (lambda (filename)
         (and (equal? "plt" (filename-extension filename))
              (gui-utils:get-choice (format "Install ~a or open for editing?" filename)
                                    "Install" "Edit")))
       (lambda (filename)
         (run-installer filename))))))
