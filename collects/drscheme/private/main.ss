
(module main mzscheme
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
  
  (provide main@)
  (define argv (current-command-line-arguments))
  
  (define main@
    (unit/sig ()
      (import [drscheme:app : drscheme:app^]
              [drscheme:unit : drscheme:unit^]
              [drscheme:get/extend : drscheme:get/extend^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^]
	      [drscheme:language : drscheme:language^]
              [drscheme:teachpack : drscheme:teachpack^]
              [drscheme:module-language : drscheme:module-language^]
              [drscheme:tools : drscheme:tools^]
              [drscheme:debug : drscheme:debug^]
              [drscheme:frame : drscheme:frame^]
              [drscheme:font : drscheme:font^])

      (application-about-handler
       (lambda ()
         (drscheme:app:about-drscheme)))
      
      (finder:default-filters (cons '("Scheme (.scm)" "*.scm") (finder:default-filters)))
      (application:current-app-name (string-constant drscheme))
      ;(version:add-spec 'd 7)
      
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
      
      (preferences:set-default 'drscheme:profile-how-to-count 'time
                               (lambda (x)
                                 (memq x '(time number))))
      (preferences:set-default 'drscheme:profile:low-color
                               (make-object color% 150 255 150)
                               (lambda (x) (is-a? x color%)))
      (preferences:set-default 'drscheme:profile:high-color
                               (make-object color% 255 150 150)
                               (lambda (x) (is-a? x color%)))
      (preferences:set-default 'drscheme:profile:scale
                               'linear
                               (lambda (x) (memq x '(sqrt linear square))))
      (let ([marshall-color 
             (lambda (c)
               (list (send c red) (send c green) (send c blue)))]
            [unmarshall-color
             (lambda (l)
               (if (and (list? l) 
                        (= 3 (length l))
                        (andmap (lambda (x) (and number? (<= 0 x 255)))
                                l))
                   (make-object color% (car l) (cadr l) (caddr l))
                   (make-object color% 0 0 0)))])
        (preferences:set-un/marshall 
         'drscheme:profile:low-color
         marshall-color
         unmarshall-color)
        (preferences:set-un/marshall 
         'drscheme:profile:high-color
         marshall-color
         unmarshall-color))
      
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
      
      (preferences:set-default 'drscheme:font-name (drscheme:font:get-default-font-name) string?)
      
      (preferences:set-default
       'drscheme:font-size
       (send (send (send (make-object text%) 
                         get-style-list)
                   basic-style)
             get-size)
       (lambda (x) (and (number? x) (exact? x) (= x (floor x)))))
      
      (drscheme:font:setup-preferences)
      
      (scheme:add-preferences-panel)
      (preferences:add-editor-checkbox-panel)
      (preferences:add-warnings-checkbox-panel)
      (preferences:add-scheme-checkbox-panel)
      (preferences:add-to-warnings-checkbox-panel
       (lambda (warnings-panel)
         (let ([make-check-box
                (lambda (pref-sym string)
                  (let ([q (make-object check-box%
                             string
                             warnings-panel
                             (lambda (checkbox evt)
                               (preferences:set 
                                pref-sym 
                                (send checkbox get-value))))])
                    (send q set-value (preferences:get pref-sym))))])
           (make-check-box 'drscheme:execute-warning-once (string-constant only-warn-once)))))
      (drscheme:debug:add-prefs-panel)
      
      (handler:current-create-new-window
       (let ([drscheme-current-create-new-window
	      (lambda (filename)
		(drscheme:unit:open-drscheme-window filename))])
	 drscheme-current-create-new-window))

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
         (run-installer filename)
	 #f))
      
      (drscheme:tools:load/invoke-all-tools
       (lambda ()
         (void))
       (lambda ()
         (drscheme:language-configuration:add-built-in-languages)
         (drscheme:module-language:add-module-language)
         (drscheme:language-configuration:add-info-specified-languages)))
      
      ;; no more extension after this point
      (drscheme:get/extend:get-interactions-canvas)
      (drscheme:get/extend:get-definitions-canvas)
      (drscheme:get/extend:get-unit-frame)
      (drscheme:get/extend:get-interactions-text)
      (drscheme:get/extend:get-definitions-text)
      (drscheme:language-configuration:get-languages)
      
      ;; this default can only be set *after* the
      ;; languages have all be registered by tools
      (preferences:set-default
       drscheme:language-configuration:settings-preferences-symbol
       (drscheme:language-configuration:get-default-language-settings)
       drscheme:language-configuration:language-settings?)
      
      ;; if the unmarshaller returns #f, that will fail the
      ;; test for this preference, reverting back to the default.
      ;; In that case, the default is specified in the pref.ss file
      ;; of the default collection and may not be the default
      ;; specified below.
      (preferences:set-un/marshall
       drscheme:language-configuration:settings-preferences-symbol
       (lambda (x)
	 (let ([lang (drscheme:language-configuration:language-settings-language x)]
	       [settings (drscheme:language-configuration:language-settings-settings x)])
	   (list (send lang get-language-position)
		 (send lang marshall-settings settings))))
       (lambda (x)
	 (and (list? x)
	      (= 2 (length x))
	      (let* ([lang-position (first x)]
		     [marshalled-settings (second x)]
		     [lang (ormap
			    (lambda (x)
			      (and (equal? lang-position
					   (send x get-language-position))
				   x))
			    (drscheme:language-configuration:get-languages))])
		(and lang
		     (let ([settings (send lang unmarshall-settings marshalled-settings)])
		       (drscheme:language-configuration:make-language-settings
			lang
			(or settings (send lang default-settings)))))))))
      
      (handler:set-recent-items-frame-superclass
       (drscheme:frame:basics-mixin
        (frame:standard-menus-mixin
         frame:basic%)))
      
      ;;
      ;; Show expanded language dialog when version changes
      ;; 
      (preferences:set-default 'drscheme:last-version #f (lambda (x) (or (string? x) (not x))))
      (preferences:set-default 'drscheme:last-language #f (lambda (x) (or (symbol? x) (not x))))
      (drscheme:app:check-new-version)
      
      ;; the initial window doesn't set the 
      ;; unit object's state correctly, yet.
      (define (make-basic)
	(let* ([frame (drscheme:unit:open-drscheme-window)]
	       [interactions-edit (send frame get-interactions-text)]
	       [definitions-edit (send frame get-interactions-text)]
	       [filename (send definitions-edit get-filename)])
	  (unless filename
	    (send frame update-shown)
	    (send (send frame get-interactions-canvas) focus))
          (send frame show #t)))
      
      (define (remove-duplicates files)
        (let loop ([files files])
          (cond
            [(null? files) null]
            [else (if (member (car files) (cdr files))
                      (loop (cdr files))
                      (cons (car files) (loop (cdr files))))])))
      
      (define get-dropped-files (dynamic-require '(lib "splash.ss" "framework") 'get-dropped-files))
      (let* ([files-to-open (append (reverse (get-dropped-files))
				    (reverse (vector->list argv)))]
             [normalized/filtered
              (let loop ([files files-to-open])
                (cond
                  [(null? files) null]
                  [else (let ([file (car files)])
                          (if (file-exists? file)
                              (cons (normalize-path file) (loop (cdr files)))
                              (begin
                                (message-box
                                 (string-constant drscheme)
                                 (format (string-constant cannot-open-because-dne) file))
                                (loop (cdr files)))))]))]
             [no-dups (remove-duplicates normalized/filtered)]
	     [frames
	      (map (lambda (f) (handler:edit-file
				f
				(lambda () (drscheme:unit:open-drscheme-window f))))
		   no-dups)])
	(when (null? (filter (lambda (x) x) frames))
	  (make-basic))))))
