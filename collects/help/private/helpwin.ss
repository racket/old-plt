
(module helpwin mzscheme
  (require (lib "unitsig.ss")
           (lib "unit.ss")
           (lib "class100.ss")
           (lib "class.ss")
           
           (lib "string-constant.ss" "string-constants")
           
           "sig.ss"
           (lib "browser-sig.ss" "browser")
           (lib "mred-sig.ss" "mred")
           (lib "framework-sig.ss" "framework")
           (lib "plt-installer-sig.ss" "setup")
           
           "notthere.ss"
           "../startup-url.ss"
           (lib "getinfo.ss" "setup")
           (lib "list.ss")
           (lib "string.ss")
           (lib "file.ss")
           (lib "etc.ss")
           (lib "url.ss" "net"))
  
  (provide helpwin@)
  
  (define helpwin@
    (unit/sig help-window^
      (import search^
              browser^
              setup:plt-installer^
              mred^
              (framework : framework^)
              (frame-mixin))
      
      (define-values (screen-w screen-h) (get-display-size))
      (framework:preferences:set-default 'drscheme:help-desk:width
                                         600
                                         number?)
      (framework:preferences:set-default 'drscheme:help-desk:height
                                         (max 440 (min 800 (- screen-h 60)))
                                         number?)
      (define (set-font-size size)
        (let* ([standard (send hyper-style-list new-named-style "Standard"
                               (send hyper-style-list find-named-style "Basic"))]
               [delta (make-object style-delta%)])
          (when standard
            (send standard get-delta delta)
            (send delta set-size-mult 0)
            (send delta set-size-add size)
            (send standard set-delta delta))))
      
      (on-installer-run
       (let ([old (on-installer-run)])
         (lambda ()
           (doc-collections-changed)
           (old))))
      
      (define (get-icon size)
        (let* ([dir (collection-path "icons")]
               [icon (make-object bitmap% (build-path dir (format "help~a.xpm" size)))]
               [mask (make-object bitmap% (build-path dir (format "help~a.xbm" size)))])
          (if (and (send icon ok?)
                   (send mask ok?))
              (values icon mask)
              (values #f #f))))
      (define-values (icon16 mask16) (get-icon "16x16"))
      (define-values (icon32 mask32) (get-icon "32x32"))
      
      (framework:preferences:set-default 'drscheme:help-desk:last-url-string 
                                         #f
                                         (lambda (x) (or (not x) (string? x))))
      
      (define (open-url-from-user parent goto-url)
        (letrec ([d (make-object dialog% (string-constant open-url) parent 500)]
                 [t
                  (framework:keymap:call/text-keymap-initializer
                   (lambda ()
                     (make-object text-field% (string-constant url:) d
                       (lambda (t e)
                         (update-ok)))))]
                 [p (make-object horizontal-panel% d)]
                 [browse (make-object button% (string-constant browse...) p
                           (lambda (b e)
                             (let ([f (get-file)])
                               (when f
                                 (send t set-value (string-append "file:" f))
                                 (update-ok)))))]
                 [spacer (make-object vertical-pane% p)]
                 [ok (make-object button%
                       (string-constant ok) p
                       (lambda (b e)
                         (let* ([s (send t get-value)]
                                [done (lambda ()
                                        (framework:preferences:set 'drscheme:help-desk:last-url-string s)
                                        (send d show #f))])
                           (with-handlers ([not-break-exn?
                                            (lambda (x)
                                              (if (exn:file-saved-instead? x)
                                                  (done)
                                                  (unless (exn:cancelled? x)
                                                    (message-box (string-constant bad-url) 
                                                                 (format (string-constant bad-url:this)
                                                                         (exn-message x))
                                                                 d))))])
                             (let ([url (string->url
                                         (cond
                                           [(regexp-match ":" s) s]
                                           [(regexp-match "^[a-zA-Z][a-zA-Z.]*($|/)" s)
                                            (string-append "http://" s)]
                                           [else
                                            (string-append "file:" s)]))])
                               (goto-url url (lambda (will-display?) (done)))
                               (done)))))
                       '(border))]
                 [update-ok
                  (lambda ()
                    (send ok enable 
                          (positive? (send (send t get-editor) 
                                           last-position))))]
                 [cancel (make-object button% (string-constant cancel) p 
                           (lambda (b e) (send d show #f)))])
          (let ([last-url-string (framework:preferences:get 'drscheme:help-desk:last-url-string)])
            (when last-url-string 
              (send t set-value last-url-string)
              (let ([text (send t get-editor)])
                (send text set-position 0 (send text last-position)))))
          (send p set-alignment 'right 'center)
          (update-ok)
          (send d center)
          (send t focus)
          (send d show #t)))
      
      (define re:tools-notes (regexp "Tools"))
      (define re:mzlib-notes (regexp "MzLib=(.*)"))
      (define re:teach-notes (regexp "teach=(.*)"))
      
      (define go-unit
        (unit (import initial-url show-frame-mmediately? progress)
              (export)
              
              (define collecting-thread #f)
              
              (define results-editor% (class100 hyper-text% ()
                                                (inherit set-title)
                                                (sequence 
                                                  (super-init #f #f)
                                                  (set-title (string-constant search-results)))))
              
              (define (get-unique-title)
                (let ([frames (send (framework:group:get-the-frame-group) get-frames)])
                  (let loop ([n 2]
                             [name (string-constant help-desk)])
                    (if (ormap (lambda (f) (string=? name (send f get-label)))
                               frames)
                        (loop (add1 n) (format (string-constant help-desk-n) n))
                        name))))
              
              (define (help-desk-window-generic-mixin super%)
                (class100 super% args
                  (rename [super-on-size on-size])
                  (override
                    [on-size
                     (lambda (w h)
                       (framework:preferences:set 'drscheme:help-desk:width w)
                       (framework:preferences:set 'drscheme:help-desk:height h)
                       (super-on-size w h))])
                  (inherit get-edit-target-object)
                  (rename [super-on-subwindow-char on-subwindow-char])
                  (private
                    [search-for-help/mumble
                     (lambda (search-func)
                       (lambda (text type mode)
                         (send (send search-text get-editor) erase)
                         (search-func
                          text 
                          (case type
                            [(keyword) 0]
                            [(keyword+index) 1]
                            [(all) 2]
                            [else (raise-type-error 'search-for-help
                                                    "'keyword, 'keyword-index, or 'all"
                                                    type)])
                          (case mode
                            [(exact) 0]
                            [(contains) 1]
                            [(regexp) 2]
                            [else (raise-type-error 'search-for-help
                                                    "'exact, 'contains, or 'regexp"
                                                    mode)]))))])
                  (public
                    [search-for-help/lucky
                     (lambda (text type mode)
                       ((search-for-help/mumble do-lucky-search)
                        text type mode))]
                    [search-for-help
                     (lambda (text type mode)
                       ((search-for-help/mumble run-search)
                        text type mode))]
                    
                    [goto-url (opt-lambda (url [progress void]) (send results goto-url url #f progress))])
                  
                  (override 
                    [on-subwindow-char 
                     (lambda (w e)
                       (let ([pgup (lambda () (send (send results get-editor) move-position 'up #f 'page))]
                             [pgdn (lambda () (send (send results get-editor) move-position 'down #f 'page))]
                             [follow-link
                              (lambda ()
                                (let* ([text (send results get-editor)]
                                       [start (send text get-start-position)]
                                       [end (send text get-end-position)])
                                  (send text
                                        call-clickback
                                        start
                                        end)))])
                         (case (send e get-key-code)
                           [(prior) (pgup) #t]
                           [(#\rubout #\backspace)
                            (if (send results has-focus?)
                                (begin (pgup) #t)
                                (super-on-subwindow-char w e))]
                           [(next) (pgdn) #t]
                           [(#\return numpad-enter)
                            (if (send results has-focus?)
                                (begin (follow-link) #t)
                                (super-on-subwindow-char w e))]
                           [(#\space)
                            (if (send results has-focus?)
                                (begin (pgdn) #t)
                                (super-on-subwindow-char w e))]
                           [(left) (if (send e get-meta-down)
                                       (send html-panel rewind)
                                       (super-on-subwindow-char w e))]
                           [(right) (if (send e get-meta-down)
                                        (send html-panel forward)
                                        (super-on-subwindow-char w e))]
                           [else (super-on-subwindow-char w e)])))])
                  
                  (override
                    [get-text-to-search
                     (lambda ()
                       (send results get-editor))])
                  
                  ;; for conformance with the hyper-frame-mixin in the browser.
                  (public
                    [get-hyper-panel
                     (lambda ()
                       html-panel)])

                  (sequence (apply super-init args))))
              
              (define (help-desk-window-standalone-menus-mixin super%)
                (class100 super% args
                  (inherit get-edit-target-object goto-url)
                  (private
                    [edit-menu:do
                     (lambda (const)
                       (let ([edit (get-edit-target-object)])
                         (when (and edit (is-a? edit editor<%>))
                           (send edit do-edit-operation const))))])
                  
                  (override
                    [file-menu:new-string (lambda () (string-constant new-help-desk))]
                    [file-menu:new-callback (lambda (i e) (new-help-frame initial-url))]
                    [file-menu:create-new? (lambda () #t)]
                    
                    [file-menu:open-string (lambda () (string-constant open-url...))]
                    [file-menu:open-callback
                     (lambda (i e)
                       (open-url-from-user this (lambda (x progress) (goto-url x progress))))]
                    [file-menu:create-open? (lambda () #t)]
                    
                    [file-menu:print-callback 
                     (lambda (i e) 
                       (send (send results get-editor) print
                             #t
                             #t
                             (framework:preferences:get 'framework:print-output-mode)))]
                    [file-menu:create-print? (lambda () #t)]
                    
                    [file-menu:between-open-and-revert
                     (lambda (file-menu)
                       (make-object menu-item% (string-constant reload) file-menu 
                         (lambda xxx (send html-panel reload))
                         #\r))]
                    
                    [edit-menu:undo-callback (lambda (menu evt) (edit-menu:do 'undo))]
                    [edit-menu:create-undo? (lambda () #t)]
                    [edit-menu:redo-callback (lambda (menu evt) (edit-menu:do 'redo))]
                    [edit-menu:create-redo? (lambda () #t)]
                    [edit-menu:cut-callback (lambda (menu evt) (edit-menu:do 'cut))]
                    [edit-menu:create-cut? (lambda () #t)]
                    [edit-menu:clear-callback (lambda (menu evt) (edit-menu:do 'clear))]
                    [edit-menu:create-clear? (lambda () #t)]
                    [edit-menu:copy-callback (lambda (menu evt) (edit-menu:do 'copy))]
                    [edit-menu:create-copy? (lambda () #t)]
                    [edit-menu:paste-callback (lambda (menu evt) (edit-menu:do 'paste))]
                    [edit-menu:create-paste? (lambda () #t)]
                    [edit-menu:select-all-callback (lambda (menu evt) (edit-menu:do 'select-all))]
                    [edit-menu:create-select-all? (lambda () #t)])
                  (sequence (apply super-init args))))
              
              (define help-window-frame%
                (frame-mixin 
                 (help-desk-window-standalone-menus-mixin
                  (help-desk-window-generic-mixin
                   (framework:frame:searchable-mixin
                    (framework:frame:standard-menus-mixin
                     framework:frame:basic%))))))
              
              (define help-desk-frame
                (make-object help-window-frame%
                  (get-unique-title) #f
                  (framework:preferences:get 'drscheme:help-desk:width)
                  (framework:preferences:get 'drscheme:help-desk:height)))
              
              (when icon16
                (send help-desk-frame set-icon icon16 mask16 'small))
              (when icon32
                (send help-desk-frame set-icon icon32 mask32 'large))
              
              (define html-panel
                (make-object
                    (class100 hyper-panel% ()
                              (inherit get-canvas)
                              (rename [super-leaving-page leaving-page])
              
			      (public
                                [stop-search
                                 (lambda ()
                                   (set! cycle-key #f)
                                   (when collecting-thread
                                     (semaphore-wait break-sema)
                                     (break-thread collecting-thread)
                                     (semaphore-post break-sema)))])
                              (override
                                [on-url-click
                                 (lambda (k url)
					; Watch for clicks from docs to undownloaded docs
					; Try to see if this is a link to missing documentation.
					; Many things can go wrong; give up if anything does...
				   (let/ec escape
				     (with-handlers ([not-break-exn?
						      (lambda (x) void)])
				       (let ([start (send (send (get-canvas) get-editor) get-url)])
					 (when (or (not start)
						   (string=? "file" (url-scheme start)))
					   (let ([url (if (string? url)
							  (if start
							      (combine-url/relative start url)
							      (string->url url))
							  url)])
					     (when (string=? "file" (url-scheme url))
					       (when (not (file-exists? (url-path url)))
					; Try comparing the base-of-base-of-url to
					;  the collection directory. Many things can go wrong,
					;  in which case we let the normal error happen.
						 (let-values ([(doc-dir)
							       (normalize-path (collection-path "doc"))]
							      [(dest-dir dest-doc dest-file)
							       (let-values ([(base filename dir?)
									     (split-path
									      (simplify-path
									       (url-path url)))]) 
								 (let-values ([(base name dir?)
									       (split-path base)])
								   (values (normalize-path base)
									   name
									   filename)))])
						   (when (and (string=? doc-dir dest-dir)
							      (not (string=? dest-doc "help")))
					; Looks like the user needs to download some docs,
					;  rather than a generic file-not-found error.
					; Redirect to information about missing docs:
						     (escape (send (get-canvas) goto-url 
								   (build-notthere-page dest-doc dest-file (url->string url))
								   #f))))))))))
				     (k url)))]
                                [leaving-page
                                 (lambda (page new-page)
                                   (unless (is-a? (page->editor new-page) results-editor%)
                                     (stop-search))
                                   (super-leaving-page page new-page))]
                                [filter-notes
                                 (lambda (l url)
                                   (let ([toollibs
                                          (ormap
                                           (lambda (s)
                                             (let ([m (regexp-match re:tools-notes s)])
                                               (and m (cadr m))))
                                           l)])
                                     (if toollibs
                                         "This page describes functionality only available for tools"
                                         (let ([lib (ormap
                                                     (lambda (s)
                                                       (let ([m (regexp-match re:mzlib-notes s)])
                                                         (and m
                                                              (format
                                                               "Mz/Mr: load with (require (lib \"~a.ss\"))"
                                                               (cadr m)))))
                                                     l)])
                                           (if lib
                                               (string-append
                                                (if (member "Core" l)
                                                    ""
                                                    "Teaching Langages: not available   ")
                                                lib)
                                               (let ([drlibs
                                                      (filter
                                                       values
                                                       (map
                                                        (lambda (s)
                                                          (let ([m (regexp-match re:teach-notes s)])
                                                            (and m (cadr m))))
                                                        l))])
                                                 (if (pair? drlibs)
                                                     (format "Teachpack: select ~a using Language|Set Teachpack To... in DrScheme"
                                                             (let loop ([s (format "~s" (car drlibs))]
                                                                        [l (cdr drlibs)])
                                                               (if (null? l)
                                                                   s
                                                                   (loop (format "~a or ~s" s (car l)) (cdr l)))))
                                                     (with-handlers ([not-break-exn?
                                                                      (lambda (x) "")])
                                                       (if (url? url)
                                                           (let ([scheme (url-scheme url)])
                                                             (if (string=? scheme "file")
                                                                 (let ([path (url-path url)])
                                                                   (let-values ([(cll-path doc.txt _1) (split-path path)])
                                                                     (if (string=? doc.txt "doc.txt")
                                                                         (let-values ([(_1 collection-name _2) (split-path cll-path)])
                                                                           (if (string=? (normalize-path (collection-path collection-name))
                                                                                         (normalize-path cll-path))
                                                                               (let ([msg ((get-info (list collection-name)) 'help-desk-message (lambda () ""))])
                                                                                 (if (string? msg)
                                                                                     msg
                                                                                     ""))
                                                                               ""))
                                                                         "")))
                                                                 ""))
                                                           "")))))))))]
                                [on-navigate (lambda () (stop-search))])
                              (sequence (super-init #t (send help-desk-frame get-area-container))))))
              
              (send html-panel set-init-page startup-url)
              
              (define results (send html-panel get-canvas))
              
              (define top (make-object vertical-pane% (send help-desk-frame get-area-container)))
              (define search-text
                (framework:keymap:call/text-keymap-initializer
                 (lambda ()
                   (make-object text-field% (string-constant find-docs-for) top
                     (lambda (t e)
                       (let ([on? (positive? (send (send t get-editor) last-position))])
                         (send search-menu enable on?)
                         (send search enable on?)))))))
              (define search-pane (make-object horizontal-pane% top))
              
              (define (lucky-search-callback)
                (do-lucky-search
                 (send search-text get-value)
                 (send where get-selection)
                 (send exact get-selection)))
              (define (search-callback)
                (run-search
                 (send search-text get-value)
                 (send where get-selection)
                 (send exact get-selection)))
              (define search (make-object button% (string-constant search) search-pane 
                               (lambda (b e)
                                 (search-callback))
                               '(border)))
              
              (define where (make-object choice% #f (list (string-constant search-for-keyword)
                                                          (string-constant search-for-keyword-or-index)
                                                          (string-constant search-for-keyword-or-index-or-text))
                              search-pane void))
              (define exact (make-object choice% #f (list (string-constant exact-match)
                                                          (string-constant containing-match)
                                                          (string-constant regexp-match))
                              search-pane void))
              (define stop (make-object button% (string-constant stop) search-pane
                             (lambda (b e)
                               (break-thread collecting-thread))))
              
              (define (run-search text search-level exactness)
                (semaphore-wait break-sema) ; protects from too-early break
                (let ([e (send results get-editor)])
                  (send (send results get-parent) update-url-display (string-constant search-results))
                  (when (is-a? e results-editor%)
                    (send e lock #f)
                    (send e erase)
                    (send e lock #t)))
                (set! collecting-thread
                      (thread (lambda () (start-search text search-level exactness)))))
              
              (send where set-selection 1)
              (send exact set-selection 1)
              (send search enable #f)
              (send stop show #f)
              
              (send top stretchable-height #f)
              
              (send results allow-tab-exit #t)
              
              (send search-text focus)
              
              (define last-find-str #f)
              (define find-str
                (case-lambda
                 [() (find-str last-find-str)]
                 [(s)
                  (set! last-find-str s)
                  (if s
                      (let* ([e (send results get-editor)]
                             [sp (send e get-start-position)]
                             [ep (send e get-end-position)])
                        (let ([pos (send e find-string s 'forward ep 'eof #t #f)])
                          (if pos
                              (send e set-position pos (+ pos (string-length s)))
                              (let ([pos (send e find-string s 'forward 0 sp #t #f)])
                                (if pos
                                    (send e set-position pos (+ pos (string-length s)))
                                    (bell))))))
                      (bell))]))
              
              (define menubar (send help-desk-frame get-menu-bar))
              (define search-menu (make-object menu% (string-constant search) menubar))
              (send search-menu enable #f)
              (define regular-search-item (make-object menu-item% (string-constant search) search-menu (lambda (m e) (search-callback)) #\e))
              (define lucky-search-item (make-object menu-item% (string-constant feeling-lucky)
                                          search-menu (lambda (m e) (lucky-search-callback)) #\l))
              
              (framework:frame:reorder-menus help-desk-frame)
              
              (set-font-size (framework:preferences:get 'drscheme:font-size))
              
	      (if show-frame-mmediately?	
                  (send help-desk-frame show #t)
                  (send help-desk-frame on-close)) ; in case it's never shown
              
              (cond
                [(equal? initial-url startup-url)
                 (send html-panel goto-init-page)]
                [else
                 ; if the initial-url is bad, goto the default
                 ; initial page and re-raise the exception to
                 ; be reported to the user.
                 (with-handlers ([not-break-exn?
                                  (lambda (x)
                                    (send html-panel goto-init-page)
                                    (raise x))])
                   (send results goto-url initial-url #f (lambda (will-display?)
                                                           (unless (or show-frame-mmediately?
                                                                       (not will-display?))
                                                             (send help-desk-frame show #t))
                                                           (progress will-display?))))])
              
              (define searching-message-on? #f)
              (define (searching-done editor)
                (when searching-message-on?
                  (set! searching-message-on? #f)
                  (send editor erase)))
              
              (define cycle-key #f)
              (define break-sema (make-semaphore 1))
              
              (define choices-sema (make-semaphore 1))
              (define choices null)
              
              (define enbolden (make-object style-delta% 'change-bold))
              
              (define (find-start key name)
                (with-handlers ([not-break-exn? (lambda (x) #f)])
                  (let ([l (string-length key)])
                    (let loop ([n 0])
                      (if (string=? key (substring name n (+ n l)))
                          n
                          (loop (add1 n)))))))
              
              (define (add-choice key name title page label ckey)
                (semaphore-wait choices-sema)
                (set! choices (cons (list key name title page label) choices))
                (semaphore-post choices-sema)
                (queue-callback
                 (lambda ()
                   (when (eq? cycle-key ckey)
                     (semaphore-wait choices-sema)
                     (let ([l choices]
                           [editor (send results get-editor)])
                       (set! choices null)
                       (semaphore-post choices-sema)
                       (send editor lock #f)
                       (send editor begin-edit-sequence)
                       (searching-done editor)
                       (for-each
                        (lambda (i)
                          (let-values ([(key name title page label) (apply values i)])
                            (if key
                                (begin
                                  (send editor insert "  " (send editor last-position) 'same #f)
                                  (let ([start (send editor last-position)])
                                    (send editor insert name start 'same #f)
                                    (let ([end (send editor last-position)]
                                          [key-start (find-start key name)])
                                      (send editor insert (format " in ~s~n" title) end 'same #f)
                                      (send editor make-link-style start end)
                                      (when key-start
                                        (send editor change-style enbolden (+ key-start start) 
                                              (+ key-start start (string-length key))))
                                      (send editor set-clickback start end
                                            (lambda (edit start end)
                                              ; We don't catch errors explicitly because the
                                              ;  page is always documentation, an on-url-click
                                              ;  will catch it.
                                              (send html-panel on-url-click
                                                    (lambda (url)
                                                      (send results goto-url url #f))
                                                    (make-url
                                                     "file"
                                                     #f ; host
                                                     #f ; port
                                                     page
                                                     #f ; params
                                                     #f ; query
                                                     label)))))))
                                (begin
                                  (let ([pos (send editor last-position)])
                                    (send editor insert (format"~a~a:~n" label name) pos 'same #f)
                                    (send editor change-style (make-object style-delta% (car page) (cdr page))
                                          (+ pos (string-length label)) (- (send editor last-position) 2)))))))
                        (reverse l))
                       (send editor end-edit-sequence)
                       (send editor lock #t))))
                 #f))
              
              (define (add-doc-section name ckey)
                (add-choice #f name #f '(change-weight . bold) "In " ckey))
              
              (define (add-kind-section name ckey)
                '(add-choice #f name #f '(change-style . slant) " " ckey))
              
              (define (do-lucky-search given-find search-level exactness)
                (let ([regexp? (= 2 exactness)]
                      [exact? (= 0 exactness)]
                      [ckey (gensym)]
                      [found-something? #f])
                  (let/ec k
                    (do-search given-find
                               search-level
                               regexp?
                               exact?
                               ckey 
                               (lambda ()
                                 (k (void)))
                               void
                               void
                               (lambda (key name title page label ckey)
                                 (set! found-something? #t)
                                 (send html-panel on-url-click
                                       (lambda (url) (send results goto-url url #f))
                                       (make-url
                                        "file"
                                        #f ; host
                                        #f ; port
                                        page
                                        #f ; params
                                        #f ; query
                                        label))
                                 (send results focus)
                                 (k (void)))))
                  (unless found-something?
                    (message-box (string-constant help-desk)
                                 (format (string-constant nothing-found-for-search-key) given-find)
                                 help-desk-frame))))
              
              (define (start-search given-find search-level exactness)
                (let* ([editor (let ([e (send results get-editor)])
                                 (if (is-a? e results-editor%)
                                     e
                                     (let ([e (make-object results-editor%)])
                                       (send e lock #t)
                                       (send results set-page (editor->page e) #t)
                                       e)))]
                       [regexp? (= 2 exactness)]
                       [exact? (= 0 exactness)]
                       [maxxed-out? #f]
                       [ckey (gensym)]
                       [result #f])
                  (send editor lock #f)
                  (send editor insert (string-constant searching...))
                  (send editor lock #t)
                  (set! searching-message-on? #t)
                  (with-handlers ([exn:break?
                                   (lambda (x)
                                     (queue-callback
                                      (lambda ()
                                        (when (eq? cycle-key ckey)
                                          (send editor lock #f)
                                          (searching-done editor)
                                          (send editor insert
                                                (if maxxed-out?
                                                    (string-constant search-stopped)
                                                    (string-constant search-stopped-too-many-matches))
                                                (send editor last-position)
                                                'same
                                                #f)
                                          (send editor lock #t)))
                                      #f))])
                    (dynamic-wind
                     (lambda ()
                       (begin-busy-cursor)
                       (send search enable #f)
                       (send search-menu enable #f)
                       (send where enable #f)
                       (send exact enable #f)
                       (set! cycle-key ckey))
                     (lambda ()
                       (semaphore-post break-sema)
                       (send stop show #t)
                       (set! result
                             (do-search given-find
                                        search-level
                                        regexp?
                                        exact?
                                        ckey 
                                        (let ([search-thread (current-thread)])
                                          (lambda ()
                                            (set! maxxed-out? #t)
                                            (break-thread search-thread)))
                                        add-doc-section
                                        add-kind-section
                                        add-choice))
                       (semaphore-wait break-sema)) ; turn off breaks...
                     (lambda ()
                       (semaphore-post break-sema) ; breaks ok now because they have no effect
                       (send stop show #f)
                       (send search enable #t)
                       (send search-menu enable #t)
                       (send where enable #t)
                       (send exact enable #t)
                       (end-busy-cursor)))
                    
                    (queue-callback
                     (lambda ()
                       (when (eq? cycle-key ckey)
                         (when result
                           (send editor lock #f)
                           (searching-done editor)
                           (send editor insert result)
                           (send editor lock #t))))
                     #f))))
              
              ; Return the frame as the result
              help-desk-frame))
      
      (define new-help-frame
	(opt-lambda (initial-url [show-frame-mmediately? #t] [progress void])
          (invoke-unit go-unit initial-url show-frame-mmediately? progress)))
      
      (values new-help-frame open-url-from-user))))
