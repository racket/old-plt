(module hyper mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "class100.ss")
           "sig.ss"
           "../browser-sig.ss"
           (lib "file.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "etc.ss")
           (lib "url.ss" "net")
           (lib "head.ss" "net")
           (lib "mred-sig.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants")
           (lib "plt-installer-sig.ss" "setup"))
  
  (provide hyper@)
  
  (define hyper@
    (unit/sig browser^
      (import html^
              (bullet : bullet^)
              mred^
              setup:plt-installer^)
      
      (define bullet-size bullet:bullet-size)
      
      (define-struct (exn:file-saved-instead exn) (pathname))
      (define-struct (exn:cancelled exn) ())
      
      (define history-limit 20)
      
      (define-struct hyperlink (anchor-start 
                                anchor-end
                                url-string))
      
      (define-struct hypertag (name position))
      
      (define hyper-style-list (make-object style-list%))
      
      (define (same-page-url? a b)
	(or (eq? a b)
	    (and (url? a) (url? b)
		 ;; fragment can be different
		 (equal? (url-scheme a) (url-scheme b))
		 (equal? (url-host a) (url-host b))
		 (equal? (url-path a) (url-path b))
		 (equal? (url-params a) (url-params b))
		 (equal? (url-query a) (url-query b)))))
		 

      (define hyper-text-mixin
        (lambda (super%)
          (class100 super% (_url _top-level-window progress [post-data #f] . args)
            (inherit begin-edit-sequence end-edit-sequence lock erase clear-undos
                     change-style get-style-list set-style-list
                     set-modified auto-wrap get-view-size
                     find-snip get-snip-position set-clickback get-canvas
                     get-visible-position-range insert last-position hide-caret
                     get-end-position set-autowrap-bitmap)

            (private-field [url _url]
                           [top-level-window _top-level-window]
                           [post-string post-data]
                           [url-allows-evaling?
                            (cond
                              [(port? url) #f]
                              [(url? url)
                               (and (equal? "file" (url-scheme url))
                                    (with-handlers ([exn:i/o:filesystem? (lambda (x) #f)])
                                      (path-below?
                                       (normal-case-path (normalize-path (build-path (collection-path "mzlib") 
                                                                                     'up
                                                                                     'up)))
                                       (normal-case-path (normalize-path (url-path url))))))]
                              [else #f])])

            (rename [super-after-set-position after-set-position])
            
            (override
              [after-set-position
               (lambda ()
                 (unless (zero? (get-end-position))
                   (hide-caret #f))
                 (super-after-set-position))])
            (private-field
              [doc-notes null]
              [title #f]
              [htmling? #f]
              [hypertags-list (list (make-hypertag "top" 0))])
            
            (public
              [add-document-note
               (lambda (note)
                 (set! doc-notes (append doc-notes (list note))))]
              [get-document-notes
               (lambda () doc-notes)]
              
              [map-shift-style 
               (lambda (start end shift-style)
                 (let loop ([pos start])
                   (unless (>= pos end)
                     (let* ([curr-snip (find-snip pos 'after-or-none)]
                            [curr-snip-end (when curr-snip
                                             (+ (get-snip-position curr-snip)
                                                (send curr-snip get-count)))])
                       (when curr-snip
                         (change-style 
                          (send (get-style-list) find-or-create-join-style 
                                (send curr-snip get-style) shift-style)
                          pos (min curr-snip-end end))
                         (loop curr-snip-end))))))]
              [make-link-style       
               (lambda (start end)
                 (map-shift-style start end 
                                  (send (get-style-list) find-named-style "h-link-style")))]
              [get-url (lambda () (and (url? url) url))])
            (private
              [make-clickback-funct
               (opt-lambda (url-string [post-data #f])
                 (lambda (edit start end)
                   (on-url-click
                    (lambda (url-string post-data)
                      (with-handlers ([void (lambda (x)
                                              (unless (or (exn:break? x)
                                                          (exn:file-saved-instead? x)
                                                          (exn:cancelled? x))
                                                (message-box
                                                 (string-constant error)
                                                 (format (string-constant cannot-display-url) 
                                                         url-string
                                                         (if (exn? x)
                                                             (exn-message x)
                                                             x)))))])
                        (send (get-canvas) goto-url url-string (get-url) void post-data)))
                    url-string
                    post-data)))])
            (public
              [on-url-click (lambda (f x post-data) 
                              (let ([c (get-canvas)])
                                (if c
                                    (send c on-url-click f x post-data)
                                    (f x post-data))))]
              [get-title (lambda () (or title (and (url? url) (url->string url))))]
              [set-title (lambda (t) (set! title t))])
            (private-field
              [hyper-delta (make-object style-delta% 'change-underline #t)])
            (sequence
              (let ([mult (send hyper-delta get-foreground-mult)]
                    [add (send hyper-delta get-foreground-add)])
                (send mult set 0 0 0)
                (send add set 0 0 255)))
            
            (private
              [add-h-link-style
               (lambda ()
                 (let ([style-list (get-style-list)])
                   (send style-list replace-named-style  "h-link-style"
                         (send style-list find-or-create-style  
                               (send style-list basic-style)
                               hyper-delta))))])
            
            (public
              [add-tag 
               (lambda (name pos)
                 (for-each (lambda (tag) 
                             (when (string=? name (hypertag-name tag))
                               (remove-tag  name)))
                           hypertags-list)
                 (let ([new-tag (make-hypertag name pos)])
                   (set! hypertags-list
                         (let insert-loop ([tags-left hypertags-list])
                           (cond [(null? tags-left)(cons new-tag ())]
                                 [(> pos (hypertag-position (car tags-left)))
                                  (cons new-tag tags-left)]
                                 [else (cons (car tags-left)
                                             (insert-loop (cdr tags-left)))])))))]
              [find-tag
               (lambda (name)
                 (if (and (integer? name) (positive? name))
                     name
                     (and (string? name)
                          (ormap (lambda (x) 
				   (and (string=? name (hypertag-name x)) 
					(hypertag-position x)))
                                 hypertags-list))))]
              [remove-tag 
               (lambda (name)
                 (set! hypertags-list
                       (filter (lambda (x) (not (string=? name (hypertag-name x))))
                               hypertags-list)))]
              [add-link 
               (lambda (start end url-string)
                 (let* ([new-link (make-hyperlink start end url-string)])
                   (set-clickback start end (make-clickback-funct url-string))))]
              
              ;; remember the directory when the callback is added (during parsing)
              ;; and restore it during the evaluation of the callback.
              [add-scheme-callback
               (lambda (start end scheme-string)
                 (let ([dir (current-load-relative-directory)])
                   (set-clickback 
                    start end 
                    (lambda (edit start end)
                      (if url-allows-evaling?
                          (parameterize ([current-load-relative-directory dir])
                            (eval-scheme-string scheme-string))
                          (message-box (string-constant help-desk)
                                       "<A MZSCHEME= ...> disabled"))))))]
              [add-thunk-callback
               (lambda (start end thunk)
		 (set-clickback 
		  start end 
		  (lambda (edit start end)
		    (let-values ([(url post-data) (thunk)])
                      (when url
			((make-clickback-funct url post-data) edit start end))))))]

              [eval-scheme-string
               (lambda (s)
                 (let ([v (dynamic-wind
                           begin-busy-cursor
                           (lambda () (eval-string s))
                           end-busy-cursor)])
                   (when (string? v)
                     (send (get-canvas) goto-url (open-input-string v) (get-url)))))]
              
              [reload
               (opt-lambda ([progress void])
                 (define (read-from-port p mime-headers)
                   (let-values ([(wrapping-on?) #t])
                     (lock #f)
                     (dynamic-wind
                      (lambda ()
                        (begin-busy-cursor)
                        ; (send progress start)
                        (begin-edit-sequence #f))
                      (lambda () 
                        (set! htmling? #t)
                        (erase)
                        (clear-undos)
                        (let* ([mime-type (extract-field "content-type" mime-headers)]
                               [html? (and mime-type
                                           (regexp-match "text/html" mime-type))])
                          (cond
                            [(or (and mime-type (regexp-match "application/" mime-type))
                                 (and (url? url)
                                      (regexp-match "[.]plt$" (url-path url))
                                      ; document-not-found produces HTML:
                                      (not html?)))
                             ; Save the file
                             (progress #f)
                             (end-busy-cursor) ; turn off cursor for a moment...
                             (let* ([orig-name (and (url? url)
                                                    (let ([m (regexp-match "([^/]*)$" (url-path url))])
                                                      (and m (cadr m))))]
                                    [size (let ([s (extract-field "content-length" mime-headers)])
                                            (and s (let ([m (regexp-match
                                                             "[0-9]+"
                                                             s)])
                                                     (and m (string->number (car m))))))]
                                    [install?
                                     (and (and orig-name (regexp-match "[.]plt$" orig-name))
                                          (let ([d (make-object dialog% (string-constant install?))]
                                                [d? #f]
                                                [i? #f])
                                            (make-object message%
                                              (string-constant you-have-selected-an-installable-package)
                                              d)
                                            (make-object message% 
                                              (string-constant do-you-want-to-install-it?) d)
                                            (when size
                                              (make-object message%
                                                (format (string-constant paren-file-size) size) d))
                                            (let ([hp (make-object horizontal-panel% d)])
                                              (send hp set-alignment 'center 'center)
                                              (send (make-object button% 
                                                      (string-constant download-and-install)
                                                      hp
                                                      (lambda (b e)
                                                        (set! i? #t)
                                                        (send d show #f))
                                                      '(border))
                                                    focus)
                                              (make-object button% (string-constant download) hp
                                                (lambda (b e)
                                                  (set! d? #t)
                                                  (send d show #f)))
                                              (make-object button% (string-constant cancel) hp
                                                (lambda (b e)
                                                  (send d show #f))))
                                            (send d center)
                                            (send d show #t)
                                            (unless (or d? i?)
                                              
                                              ; turn the cursor back on before escaping
                                              (begin-busy-cursor)
                                              
                                              (raise (make-exn:cancelled
                                                      "Package Cancelled"
                                                      (current-continuation-marks))))
                                            i?))]
                                    [f (if install?
                                           (make-temporary-file "tmp~a.plt")
                                           (put-file 
                                            (if size
                                                (format 
                                                 (string-constant save-downloaded-file/size)
                                                 size)
                                                (string-constant save-downloaded-file))
                                            #f ; should be calling window!
                                            #f
                                            orig-name))])
                               (begin-busy-cursor) ; turn the cursor back on
                               (when f
                                 (let* ([d (make-object dialog% (string-constant downloading) top-level-window)]
                                        [message (make-object message% 
                                                   (string-constant downloading-file...)
                                                   d)]
                                        [gauge (if size
                                                   (make-object gauge% #f 100 d)
                                                   #f)]
                                        [exn #f]
                                        ; Semaphores to avoid race conditions:
                                        [wait-to-start (make-semaphore 0)]
                                        [wait-to-break (make-semaphore 0)]
                                        ; Thread to perform the download:
                                        [t (thread
                                            (lambda ()
                                              (semaphore-wait wait-to-start)
                                              (with-handlers ([void
                                                               (lambda (x)
                                                                 (when (not (exn:break? x))
                                                                   (set! exn x)))]
                                                              [void ; throw away break exceptions
                                                               void])
                                                (semaphore-post wait-to-break)
                                                (with-output-to-file f
                                                  (lambda ()
                                                    (let loop ([total 0])
                                                      (when gauge
                                                        (send gauge set-value 
                                                              (inexact->exact
                                                               (floor (* 100 (/ total size))))))
                                                      (let ([s (read-string 1024 p)])
                                                        (unless (eof-object? s)
                                                          (display s)
                                                          (loop (+ total (string-length s)))))))
                                                  'binary 'truncate))
                                              (send d show #f)))])
                                   (send d center)
                                   (make-object button% (string-constant &stop)
                                     d (lambda (b e)
                                         (semaphore-wait wait-to-break)
                                         (set! f #f)
                                         (send d show #f)
                                         (break-thread t)))
                                   ; Let thread run only after the dialog is shown
                                   (queue-callback (lambda () (semaphore-post wait-to-start)))
                                   (send d show #t)
                                   (when exn (raise exn)))
                                 (when (and f install?)
                                   (run-installer f)
                                   (delete-file f)))
                               (raise
                                (if f
                                    (make-exn:file-saved-instead
                                     (if install?
                                         (string-constant package-was-installed)
                                         (string-constant download-was-saved))
                                     (current-continuation-marks)
                                     f)
                                    (make-exn:cancelled "The download was cancelled."
                                                        (current-continuation-marks)))))]
                            [(or (port? url)
                                 (and (url? url)
                                      (regexp-match "[.]html?$" (url-path url)))
                                 html?)
                             ; HTML
                             (progress #t)
                             (let* ([d #f]
                                    [show-progress void]
                                    [e-text ""]
                                    [exn #f]
                                    [done? #f]
                                    ; Semaphores to avoid race conditions:
                                    [wait-to-continue (make-semaphore 0)]
                                    [wait-to-break (make-semaphore 0)]
                                    [wait-to-show (make-semaphore 1)]
                                    [directory
                                     (or (if (and (url? url)
                                                  (string=? "file" (url-scheme url)))
                                             (let ([path (url-path url)])
                                               (let-values ([(base name dir?) (split-path path)])
                                                 (if (string? base)
                                                     base
                                                     #f)))
                                             #f)
                                         (current-load-relative-directory))]
                                    ; Thread to perform the download:
                                    [t (parameterize ([break-enabled #f])
                                         (thread
                                          (lambda ()
                                            (with-handlers ([void (lambda (x) (set! exn x))])
                                              (parameterize ([break-enabled #t])
                                                (semaphore-post wait-to-break)
                                                (parameterize ([html-status-handler
                                                                (lambda (s) 
                                                                  (set! e-text s)
                                                                  (semaphore-wait wait-to-show)
                                                                  (show-progress s)
                                                                  (semaphore-post wait-to-show))]
                                                               [current-load-relative-directory directory]
                                                               [html-eval-ok url-allows-evaling?])
                                                  (html-convert p this))))
                                            (semaphore-wait wait-to-show)
                                            (set! done? #t)
                                            (when d
                                              (send d show #f))
                                            (semaphore-post wait-to-show)
                                            (semaphore-post wait-to-continue))))]
                                    [make-dialog
                                     (lambda ()
                                       (semaphore-wait wait-to-show)
                                       (unless done?
                                         (cond
                                           [(send top-level-window has-status-line?)
                                            (set! show-progress
                                                  (lambda (s)
                                                    (send top-level-window set-status-text s)))]
                                           [else
                                            (set! d (make-object dialog% (string-constant getting-page)
                                                      top-level-window 400))
                                            (let* ([c (make-object editor-canvas% d #f
                                                        '(no-hscroll no-vscroll))]
                                                   [progress-text (make-object text%)])
                                              (set! show-progress
                                                    (lambda (s)
                                                      (send progress-text erase)
                                                      (send progress-text insert s)))
                                              (send progress-text insert e-text)
                                              (send progress-text auto-wrap #t)
                                              (send c set-editor progress-text)
                                              (send c set-line-count 3)
                                              (send c enable #f))
                                            (send (make-object button% (string-constant &stop) d
                                                    (lambda (b e)
                                                      (semaphore-wait wait-to-break)
                                                      (semaphore-post wait-to-break)
                                                      (break-thread t)))
                                                  focus)
                                            (send d center)
                                            (thread (lambda () (send d show #t)))
                                            (let loop () (sleep) (unless (send d is-shown?) (loop)))])
                                         (semaphore-post wait-to-show)))])
                               (thread (lambda ()
                                         (sleep 1)
                                         (unless done? (semaphore-post wait-to-continue))))
                               (semaphore-wait wait-to-continue)
                               (unless done?
                                 (make-dialog)
                                 (yield wait-to-continue))
                               (when exn (raise exn)))]
                            [else
                             ; Text
                             (progress #t)
                             (begin-edit-sequence)
                             (let loop ()
                               (let ([r (read-line p 'any)])
                                 (unless (eof-object? r)
                                   (insert r)
                                   (insert #\newline)
                                   (loop))))
                             (change-style (make-object style-delta% 'change-family 'modern)
                                           0 (last-position))
                             (set! wrapping-on? #f)
                             (end-edit-sequence)])))
                      (lambda ()
                        (end-edit-sequence)
                        (end-busy-cursor)
                        ; (send progress stop)
                        (set! htmling? #f)
                        (close-input-port p)))
                     (set-style-list hyper-style-list)
                     (set-modified #f)
                     (auto-wrap wrapping-on?)
                     (set-autowrap-bitmap #f)
                     (lock #t)))

                 (when url
                   (if (port? url)
                       (read-from-port url empty-header)
                       (let* ([busy? #t]
                              [stop-busy (lambda ()
                                           (when busy?
                                             (set! busy? #f)
                                             (end-busy-cursor)))])
                         (dynamic-wind
                          ;; Turn on the busy cursor:
                          begin-busy-cursor
                          (lambda ()
                            ;; Try to get mime info, but use get-pure-port if it fails.
                            (with-handlers ([(lambda (x)
                                               (and (not-break-exn? x)
                                                    busy?))
                                             (lambda (x) 
                                               (call/input-url 
                                                url
                                                (if post-string 
						    (lambda (u s) (post-pure-port u post-string s))
						    get-pure-port)
                                                (lambda (p)
                                                  (stop-busy)
                                                  (read-from-port p empty-header))
                                                null))])
                              (call/input-url 
                               url 
			       (if post-string 
				   (lambda (u s) (post-impure-port u post-string s))
				   get-impure-port)
                               (lambda (p)
                                 (let ([headers (purify-port p)])
                                   (stop-busy)
                                   (read-from-port p headers)))
                               null)))
                          stop-busy)))))])
            (sequence
              (apply super-init args)
              (add-h-link-style)
	      ;; load url, but the user might break:
	      (with-handlers ([exn:break? void])
                 (reload progress))))))
      
      (define hyper-text% (hyper-text-mixin text:keymap%))
      
      ;; path-below? : string[normalized-path] string[normalized-path] -> boolean
      ;; returns #t if subpath points to a place below top
      (define (path-below? top longer)
        (let loop ([top (explode-path top)]
                   [longer (explode-path longer)])
          (cond
            [(null? top) #t]
            [(null? longer) #f]
            [(equal? (car top) (car longer))
             (loop (cdr top) (cdr longer))]
            [else #f])))

      (keymap:add-to-right-button-menu/before
       (let ([old (keymap:add-to-right-button-menu/before)])
         (lambda (menu editor event)
           (when (is-a? editor hyper-text%)
             (let* ([panel (let ([canvas (send editor get-canvas)])
                             (and canvas
                                  (send (send canvas get-top-level-window) get-hyper-panel)))]
                    [back
                     (instantiate menu-item% ()
                       (label (string-append "< " (string-constant rewind-in-browser-history)))
                       (parent menu)
                       (callback
                        (lambda (_1 _2)
                          (when panel
                            (send panel rewind)))))]
                    [forward
                     (instantiate menu-item% ()
                       (label (string-append (string-constant forward-in-browser-history) " >"))
                       (parent menu)
                       (callback
                        (lambda (_1 _2)
                          (when panel
                            (send panel forward)))))])
               (send back enable (send panel can-rewind?))
               (send forward enable (send panel can-forward?))
               (instantiate separator-menu-item% ()
                 (parent menu))))
           (old menu editor event))))
      
      (define (hyper-canvas-mixin super%)
        (class100 super% args
          (inherit get-editor set-editor refresh get-parent get-top-level-window)
          
          (public
            [get-editor% (lambda () hyper-text%)]
            [make-editor (lambda (url progress post-data) 
                           (make-object (get-editor%) url (get-top-level-window) progress post-data))]
            [current-page
             (lambda ()
               (let ([e (get-editor)])
                 (and e 
                      (let ([sbox (box 0)]
                            [ebox (box 0)])
                        (send e get-visible-position-range sbox ebox)
                        (list e (unbox sbox) (unbox ebox))))))]
            [on-url-click (lambda (k url post-data) (send (get-parent) on-url-click k url post-data))]
            [goto-url
             (opt-lambda (url relative [progress void] [post-data #f])
               (let* ([url (if (or (url? url) (port? url))
                               url
                               (if relative
                                   (combine-url/relative 
                                    relative
                                    url)
                                   (string->url url)))]
                      [e (let ([e-now (get-editor)])
			   (if (and e-now
				    (same-page-url? url (send e-now get-url)))
			       (begin
                                 (progress #t)
                                 e-now)
			       (make-editor url progress post-data)))]
                      [tag-pos (send e find-tag (and (url? url) (url-fragment url)))])
                 (unless (and tag-pos (positive? tag-pos))
                   (send e hide-caret #t))
                 (set-page (list e (or tag-pos 0) (send e last-position)) #t)
                 (when tag-pos (send e set-position tag-pos))
                 (send (get-parent) update-url-display
		       (format "~s"
			       (if (url? url)
				   (list (url->string url) (url-fragment url))
				   url)))))]
	    [after-set-page (lambda () (void))]
            [set-page
             (lambda (page notify?)
               (let ([e (car page)]
                     [spos (cadr page)]
                     [epos (caddr page)]
                     [curr (get-editor)]
                     [current (current-page)])
                 ; Pre-size the editor to avoid visible reflow
                 (when curr
                   (let ([wbox (box 0)])
                     (send curr get-view-size wbox (box 0))
                     (when (send e auto-wrap)
                       (send e set-max-width (unbox wbox)))))
                 (send e begin-edit-sequence)
                 (when notify?
                   (send (get-parent) leaving-page current (list e 0 0)))
                 (set-editor e (and current (zero? (cadr current)) (zero? spos)))
                 (send e scroll-to-position spos #f epos 'start)
                 (send e end-edit-sequence)
		 (after-set-page)
                 (when (or (positive? spos) (not current) (positive? (cadr current)))
                   (refresh))))])
          (sequence
            (apply super-init args))))
      
      (define hyper-canvas% (hyper-canvas-mixin editor-canvas%))
      
      (define info-canvas%
        (class100 canvas% (parent)
          (inherit min-client-height get-dc stretchable-height
                   enable refresh show)
          (private-field
            [text ""])
          (override
            [on-paint
             (lambda ()
               (let ([dc (get-dc)])
                 (send dc clear)
                 (send dc draw-text text 4 2)))])
          (public
            [erase-info (lambda ()
                          (unless (string=? text "")
                            (set! text "")
                            (let ([dc (get-dc)])
                              (send dc clear))))]
            [set-info (lambda (t)
                        (set! text t)
                        (if (string=? t "")
                            (show #f)
                            (let ([dc (get-dc)])
                              (send dc clear)
                              (show #t)
                              (refresh))))])
          (sequence 
            (super-init parent)
            (stretchable-height #f)
            (enable #f)
            (show #f)
            (let ([font (make-object font% 
                          (send (send parent get-label-font) get-point-size) 
                          'default 'normal 'normal)]
                  [dc (get-dc)])
              (send dc set-font font)
              (send dc set-text-foreground (make-object color% "FOREST GREEN"))
              (let-values ([(w h d a) (send dc get-text-extent "X" font)])
                (min-client-height (+ 4 (inexact->exact (ceiling h)))))))))
      
      (define (hyper-panel-mixin super%)
        (class100 super% (info-line? . args)
          (inherit reflow-container)
          (sequence (apply super-init args))
          
          (private-field
            [url-message ;; doesn't work for forwards and backwards in the history
             (and #f
                  (directory-exists? (build-path (collection-path "mzlib")
                                                 "CVS"))
                  (make-object message% "" this))])
          (sequence
            (when url-message
              (send url-message stretchable-width #t)))
          (public
            [update-url-display
             (lambda (str)
               (when url-message
                 (send url-message set-label str)))])
          
          
          (private
            [clear-info (lambda () 
                          (when info 
                            (send info erase-info)))]
            [update-info (lambda (page) 
                           (when (and info page)
                             (let ([notes (send (page->editor page) get-document-notes)])
                               (send info set-info
                                     (filter-notes notes (send (page->editor page) get-url))))))]
            [go (lambda (page)
                  (clear-info)
                  (send c set-page page #f)
                  (update-info page)
                  (update-buttons page)
                  (on-navigate))])
          (public
            [current-page
             (lambda ()
               (send c current-page))]
            [rewind 
             (lambda ()
               (unless (null? past)
                 (let ([page (car past)])
                   (set! future (cons (send c current-page) future))
                   (set! past (cdr past))
                   (go page))))]
            [forward
             (lambda ()
               (unless (null? future)
                 (let ([page (car future)])
                   (set! past (cons (send c current-page) past))
                   (set! future (cdr future))
                   (go page))))]
            [can-forward?
             (lambda ()
               (not (null? future)))]
            [can-rewind?
             (lambda ()
               (not (null? past)))]
            [get-canvas% (lambda () hyper-canvas%)]
            [make-canvas (lambda (f) (make-object (get-canvas%) f))]
            [make-control-bar-panel (lambda (f) (make-object horizontal-panel% f))])
          (private-field
            [past null]
            [future null] 
            
            
        ;; (union #f                             -- no init page
        ;;         string                        -- delayed init page
        ;;         url                           -- delayed init page
        ;;         (list editor number numer))   -- forced init page
            [init-page #f]
            
            
            [hp (make-control-bar-panel this)]
            [control-bar? (is-a? hp area-container<%>)]
            [back (and control-bar?
                       (make-object button%
                         (string-append "< " (string-constant rewind-in-browser-history))
                         hp
                         (lambda (b ev) 
                           (rewind))))]
            [forw (and control-bar?
                       (make-object button% 
                         (string-append (string-constant forward-in-browser-history) " >")
                         hp
                         (lambda (b ev) 
                           (forward))))])
          (private
            [home-callback
             (lambda () 
               (cond
                 [(not init-page) (void)]
                 [(or (url? init-page) (string? init-page))
                  
                  ; handle stopping when loading the home page
                  (with-handlers ([exn:break? 
                                   (lambda (x) (void))])
                    (send c goto-url init-page #f)
                    (set! init-page (send c current-page))
                    (update-buttons init-page))]
                 [else 
                  (send c set-page init-page #t)]))])
          (private-field
            [home (and control-bar?
                       (make-object button% (string-constant home) hp
                         (lambda (b ev)
                           (home-callback))))])
          (private
            [update-buttons
             (lambda (page)
               (unless init-page
                 (set! init-page page))
               (when control-bar?
                 (send back enable (pair? past))
                 (send forw enable (pair? future))
                 
                 (send home enable (and init-page
                                        (or (string? init-page)
                                            (url? init-page)
                                            (not (same-page? init-page page)))))
                 
                 (send choice clear)
                 (for-each
                  (lambda (p)
                    (send choice append 
                          (let ([s (send (car p) get-title)])
                            (or s (string-constant untitled)))))
                  (append (reverse future)
                          (if page (list page) null)
                          past))
                 (let ([c (send choice get-number)])
                   (unless (zero? c)
                     (send choice set-selection (length future))))))])
          (private-field
            [choice (and control-bar?
                         (make-object choice% #f null hp
                           (lambda (ch e)
                             (let* ([l (append (reverse past)
                                               (list (send c current-page))
                                               future)]
                                    [pos (- (send choice get-number) (send choice get-selection) 1)])
                               (let loop ([l l][pre null][pos pos])
                                 (cond
                                   [(zero? pos)
                                    (set! past pre)
                                    (set! future (cdr l))
                                    (go (car l))]
                                   [else (loop (cdr l)
                                               (cons (car l) pre)
                                               (sub1 pos))]))))))])
          (private-field
            [info (and info-line?
                       (make-object info-canvas% this))]
            [c (make-canvas this)])
          (public
            
        ;; set-init-page : (union string url) -> void
            [set-init-page
             (lambda (p)
               (set! init-page p))]
            [goto-init-page
             (lambda ()
               (home-callback))]
            
            ; [get-progress (lambda () progress)]
            [on-navigate (lambda () (void))]
            [filter-notes (lambda (l) (apply string-append l))]
            [get-canvas (lambda () c)]
            [on-url-click (lambda (k url post-data) (k url post-data))]
            
            [reload
             (lambda ()
               (let ([c (get-canvas)])
                 (and c 
                      (let ([e (send c get-editor)])
                        (and e
                             (send e reload))))))]
            
            [leaving-page (lambda (page new-page)
                            (set! future null)
                            (when page
                              (set! past (cons page past)))
                            (when (> (length past) history-limit)
                              (set! past
                                    (let loop ([l past])
                                      (if (null? (cdr l))
                                          null
                                          (cons (car l) (loop (cdr l)))))))
                            (clear-info)
                            (update-buttons new-page)
                            (update-info new-page))])
          (sequence
            (when control-bar?
              (send choice stretchable-width #t)
              (send hp stretchable-height #f))
            (update-buttons #f))))
      
      (define hyper-panel% (hyper-panel-mixin vertical-panel%))
      
      (define (hyper-frame-mixin super%)
        (class100 super% (start-url . args)
          (private-field
            [p #f])
          (public
            [get-hyper-panel% (lambda () hyper-panel%)]
            [get-hyper-panel (lambda () p)])
          (inherit show)
          (sequence 
            (apply super-init args)
            (set! p (make-object (get-hyper-panel%) #f this))
            (show #t)
            (send (send p get-canvas) goto-url start-url #f))))
      
      (define hyper-frame% (hyper-frame-mixin frame%))
      
      (define (editor->page e) (list e 0 0))
      (define (page->editor e) (car e))
      
      (define (same-page? a b)
        (eq? (car a) (car b)))
      
      (define (open-url file)
        (make-object hyper-frame% file (string-constant browser) #f 500 450)))))
