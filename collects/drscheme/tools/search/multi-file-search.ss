;; multi-file-search : -> void
;; opens a dialog to configure the search and initiates the search
(define (multi-file-search)
  (let ([search-info  (configure-search)])
    (when search-info
      (open-search-window search-info))))

;; search-type = (make-search-type string make-searcher (listof (cons string boolean)))
;; the param strings are the labels for checkboxes
;; the param booleans are the default values for the checkboxes
;; these are the available searches
(define-struct search-type (label make-searcher params))

;; make-searcher = ((listof boolean) string -> (union string searcher))
;; this returns the function that does the actual searching
;; or a string if the input string (or booleans) are wrong, somehow.
;; the string is an error message to present to the user.

;; searcher = (string (string int int int -> void) -> void)
;; this performs a single search.
;; the first argument is the filename to be searched
;; the second argument is called for each match.
;;     the arguments are: line-string line-number col-number match-length

;; search-info = (make-search-info searcher (-> (union #f string)))
;; this is the info from the user to do a particular search
;; do-single-search runs the search in a particular file
;; filenames is a thunk that returns the next filename, or #f
(define-struct search-info (searcher get-filenames))

;; search-types : (listof search-type)
(define search-types
  (list (make-search-type
         "String match (handles files with graphics)"
         (lambda (info search-string) (exact-match-searcher info search-string))
         (list (cons "Case sensitive" #f)))
        (make-search-type
         "Regular Expression (only raw text files)"
         (lambda (info search-string) (regexp-match-searcher info search-string))
         (list))))

;; preferences initialization
(preferences:set-default 'drscheme:multi-file-search:recur? #t boolean?)
(preferences:set-default 'drscheme:multi-file-search:filter? #t boolean?)
(preferences:set-default 'drscheme:multi-file-search:filter-string "ss" string?)
(preferences:set-default 'drscheme:multi-file-search:directory 
                         "Cupertino:robby:cvs:plt:collects:framework:" 
                         string?)
(preferences:set-default 'drscheme:multi-file-search:search-string "mixin" string?)
(preferences:set-default 'drscheme:multi-file-search:search-type
                         1
                         (lambda (x) 
                           (and (number? x)
                                (exact? x)
                                (integer? x)
                                (<= 0 x)
                                (< x (length search-types)))))

;; drscheme:mult-file-search:search-check-boxes : (listof (listof boolean))
(preferences:set-default 'drscheme:multi-file-search:search-check-boxes 
                         (map (lambda (x) (map cdr (search-type-params x)))
                              search-types)
                         (lambda (x) 
                           (and (list? x)
                                (andmap (lambda (x)
                                          (and (list? x)
                                               (andmap boolean? x)))
                                        x))))

(preferences:set-default 'drscheme:multi-file-search:percentages
                         '(1/3 2/3)
                         (lambda (x) (and (list? x)
                                          (= 2 (length x))
                                          (= 1 (apply + x)))))

(preferences:set-default 'drscheme:multi-file-search:frame-size '(300 . 400) 
                         (lambda (x) (and (cons? x)
                                          (number? (car x))
                                          (number? (cdr x)))))

;; open-search-window : search-info -> void
;; thread: eventspace main thread
;; opens a window and creates the thread that does the search
(define (open-search-window search-info)
  (define frame (make-object search-size-frame% "Search"))
  (define panel (make-object saved-vertical-resizable% frame))
  (define button-panel (make-object horizontal-panel% frame))
  (define stop-button (make-object button% "Stop Search" button-panel (lambda (x y) (stop-callback))))
  
  (define zoom-text (make-object text%))
  (define results-text (make-object results-text% zoom-text))
  (define results-ec (make-object editor-canvas% panel results-text))
  (define zoom-ec (make-object editor-canvas% panel zoom-text))
  
  (define (stop-callback)
    (break-thread thd)
    (send stop-button enable #f))
  
  (define lst null)
  (define callback-queued? #f)
  (define sema (make-semaphore 1))

  ;; -> void
  ;; thread : eventspace main thread
  (define (add-matches)
    (semaphore-wait sema)
    (let ([matches lst])
      (set! lst null)
      (semaphore-post sema)
      (send results-text begin-edit-sequence)
      (for-each
       (lambda (match)
         (let ([filename (car match)]
               [line-string (cadr match)]
               [line-number (caddr match)]
               [col-number (cadddr match)]
               [match-length (car (cddddr match))])
           (send results-text add-match
                 filename line-string line-number col-number match-length)))
       matches)
      (send results-text end-edit-sequence)))

  (define thd
    (thread
     (lambda ()
       (do-search search-info 
                  (lambda (filename line-string line-number col-number match-length)
                    (semaphore-wait sema)
                    (set! lst
                          (cons
                           (list
                            filename
                            line-string
                            line-number
                            col-number
                            match-length)
                           lst))
                    (unless callback-queued?
                      (queue-callback
                       (lambda ()
                         (add-matches))))
                    (semaphore-post sema)))
       (queue-callback
        (lambda ()
          (send stop-button enable #f))))))
  
  (send panel set-percentages (preferences:get 'drscheme:multi-file-search:percentages))
  (send button-panel set-alignment 'right 'center)
  (send button-panel stretchable-height #f)
  (send frame show #t))

;; do-search : search-info text -> void
;; thread: searching thread
;; called in a new thread that may be broken (to indicate a stop)
(define (do-search search-info add-match)
  (let ([searcher (search-info-searcher search-info)]
        [get-filenames (search-info-get-filenames search-info)])
    (let loop ()
      (let ([filename (get-filenames)])
        (when filename
          (searcher filename 
                    (lambda (line-string line-number col-number match-length)
                      (add-match
                       filename
                       line-string
                       line-number
                       col-number
                       match-length)))
          (loop))))))

;; results-text% : derived from text%
;; init args: zoom-text
;;   zoom-text : (instance-of text%)
;; public-methods:
;;   add-match : string int in tint int -> void
;;   adds a match to the text
(define results-text%
  (class text% (zoom-text)
    (inherit insert last-paragraph erase
             paragraph-start-position paragraph-end-position
             last-position change-style
             set-clickback)
    (private
      [filename-delta (make-object style-delta% 'change-bold)]
      [match-delta (let ([d (make-object style-delta%)])
                     (send d set-delta-foreground "forest green")
                     d)]
      [hilite-line-delta (make-object style-delta% 'change-style 'italic)]
      [unhilite-line-delta (make-object style-delta% 'change-style 'normal)]
      [widest-filename #f]
      [indent-all-lines
       ;; indent-all-lines : number -> void
       ;; inserts `offset' spaces to the beginning of each line,
       ;; except the last one. Must be at least one such line in the text.
       (lambda (offset)
         (let ([spaces (make-string offset #\space)])
           (let loop ([para (- (last-paragraph) 1)])
             (let ([para-start (paragraph-start-position para)])
               (insert spaces para-start para-start)
               (change-style filename-delta para-start (+ para-start offset)))
             (unless (zero? para)
               (loop (- para 1))))))]
      
      ;; match-shown? : boolean
      ;; indicates if a match has ever been shown.
      ;; if not, need to clean out the "searching" message
      ;; and show a match. Done in `add-match'
      [match-shown? #f]
      
      [old-line #f]
      [hilite-line
       (lambda (line)
         (when old-line
           (change-style unhilite-line-delta
                         (paragraph-start-position old-line)
                         (paragraph-end-position old-line)))
         (when line
           (change-style hilite-line-delta
                         (paragraph-start-position line)
                         (paragraph-end-position line)))
         (set! old-line line))])
    (public
      [add-match
       (lambda (filename line-string line-number col-number match-length)
         (let* ([this-match-number (last-paragraph)]
                [len (string-length filename)]
                [insertion-start #f]
                [show-this-match
                 (lambda ()
                   (set! match-shown? #t)
                   (send zoom-text begin-edit-sequence)
                   (send zoom-text lock #f)
                   (send zoom-text load-file filename)
                   (send zoom-text set-position (send zoom-text paragraph-start-position line-number))
                   (let ([start (+ (send zoom-text paragraph-start-position line-number)
                                   col-number)])
                     (send zoom-text change-style match-delta start (+ start match-length)))
                   (send zoom-text lock #t)
                   (send zoom-text set-caret-owner #f 'global)
                   (hilite-line this-match-number)
                   (send zoom-text end-edit-sequence))])
           (unless match-shown?
             (erase))
           (unless widest-filename
             (set! widest-filename len))
           (if (<= len widest-filename)
               (begin
                 (set! insertion-start (last-position))
                 (insert (make-string (- widest-filename len) #\space) 
                         (last-position) (last-position)))
               (begin
                 (indent-all-lines (- len widest-filename))
                 (set! insertion-start (last-position))
                 (set! widest-filename len)))
           (let ([filename-start (last-position)])
             (insert filename (last-position) (last-position))
             (insert ": " (last-position) (last-position))
             (change-style filename-delta insertion-start (last-position))
             (let ([line-start (last-position)])
               (insert line-string (last-position) (last-position))
               (change-style match-delta
                             (+ line-start col-number)
                             (+ line-start col-number match-length)))
             (set-clickback filename-start (last-position)
                            (lambda (_1 _2 _3)
                              (show-this-match)))
             (insert #\newline (last-position) (last-position))

             (unless match-shown?
               (show-this-match)))))])

    (inherit get-style-list set-styles-sticky)
    (sequence
      (super-init)
      (send zoom-text lock #t)
      (set-styles-sticky #f)
      (let ([delta (make-object style-delta% 'change-normal)]
            [style (send (get-style-list) find-named-style "Standard")])
        (send delta set-delta 'change-family 'modern)
        (send zoom-text set-style-list (get-style-list))
        (if style
            (send style set-delta delta)
            (send style-list new-named-style "Standard"
                  (send style-list find-or-create-style
                        (send style-list find-named-style "Basic")
                        delta))))
      (insert "Searching..."))))

;; this frame is just like a regular frame except that it
;; remembers the frame size in the preferences
;; thread: eventspace main thread
(define search-size-frame%
  (class frame% (name)
    (override
      [on-size
       (lambda (w h)
         (preferences:set 'drscheme:multi-file-search:frame-size (cons w h)))])
    (sequence
      (let ([size (preferences:get 'drscheme:multi-file-search:frame-size)])
        (super-init name #f (car size) (cdr size))))))


;; this vertical-resizable class just remembers the percentage between the
;; two panels
;; thread: eventspace main thread
(define saved-vertical-resizable%
  (class panel:vertical-resizable% args
    (inherit get-percentages)
    (rename [super-on-percentage-change on-percentage-change])
    (override
      [on-percentage-change
       (lambda ()
         (preferences:set 'drscheme:multi-file-search:percentages
                          (get-percentages))
         (super-on-percentage-change))])
    (sequence (apply super-init args))))

;; configure-search : -> (union #f search-info)
;; thread: eventspace main thread
;; configures the search
(define (configure-search)
  (define dialog (make-object dialog% "Configure search" #f 500 #f #f #f '(resize-border)))
  (define outer-files-panel (make-object vertical-panel% dialog '(border)))
  (define outer-method-panel (make-object vertical-panel% dialog '(border)))
  (define button-panel (make-object horizontal-panel% dialog))
  (define files-label (make-object message% "Files" outer-files-panel))
  (define files-inset-outer-panel (make-object horizontal-panel% outer-files-panel))
  (define files-inset-panel (make-object horizontal-panel% files-inset-outer-panel))
  (define files-panel (make-object vertical-panel% files-inset-outer-panel))
  (define method-label (make-object message% "Search" outer-method-panel))
  (define method-inset-outer-panel (make-object horizontal-panel% outer-method-panel))
  (define method-inset-panel (make-object horizontal-panel% method-inset-outer-panel))
  (define method-panel (make-object vertical-panel% method-inset-outer-panel))
    
  (define dir-panel (make-object horizontal-panel% files-panel))
  (define dir-field (make-object text-field% "Dir" dir-panel void))
  (define dir-button (make-object button% "Browse..." dir-panel (lambda (x y) (dir-button-callback))))

  (define recur-check-box (make-object check-box% "Recur over subdirectories" files-panel
                            (lambda (x y) (recur-check-box-callback))))

  (define filter-panel (make-object horizontal-panel% files-panel))
  (define filter-check-box (make-object check-box% "Regexp filename filter" filter-panel
                            (lambda (x y) (filter-check-box-callback))))
  (define filter-text-field (make-object text-field% #f filter-panel 
                              (lambda (x y) (filter-text-field-callback))))
  
  (define methods-choice (make-object choice% #f (map search-type-label search-types) method-panel 
                           (lambda (x y) (methods-choice-callback))))
  (define search-text-field (make-object text-field% "Search string" method-panel
                              (lambda (x y) (search-text-field-callback))))
  (define active-method-panel (make-object panel:single% method-panel))
  (define methods-check-boxess
    (map
     (lambda (search-type prefs-settings)
       (let ([p (make-object vertical-panel% active-method-panel)])
         (send p set-alignment 'left 'center)
         (map (lambda (flag-pair prefs-setting)
                (let ([cb (make-object check-box% 
                            (car flag-pair)
                            p
                            (lambda (evt chk) (method-callback chk)))])
                  (send cb set-value prefs-setting)
                  cb))
              (search-type-params search-type)
              prefs-settings)))
     search-types
     (preferences:get 'drscheme:multi-file-search:search-check-boxes)))
  
  (define ok-button (make-object button% "OK" button-panel
                      (lambda (x y) (ok-button-callback)) '(border)))
  (define cancel-button (make-object button% "Cancel" button-panel
                          (lambda (x y) (cancel-button-callback))))
  (define spacer (make-object grow-box-spacer-pane% button-panel))
  
  ;; initialized to a searcher during the ok button callback
  ;; so the user can be informed of an error before the dialog
  ;; closes.
  (define searcher #f)

  ;; initialized to a regexp if the user wants to filter filenames,
  ;; during the ok-button-callback, so errors can be signalled.
  (define filter #f)

  ;; title for message box that signals error messages
  (define message-box-title "DrScheme - Multi File Search")

  (define (ok-button-callback)
    (cond
      [(with-handlers ([exn:i/o:filesystem?
                        (lambda (x) #f)])
         (directory-exists? (send dir-field get-value)))
       (let ([_searcher
              ((search-type-make-searcher (list-ref search-types (send methods-choice get-selection)))
               (map (lambda (cb) (send cb get-value))
                    (send (send active-method-panel active-child) get-children))
               (send search-text-field get-value))])
         (if (string? _searcher)
             (message-box message-box-title _searcher dialog)
             (let ([regexp (with-handlers ([(lambda (x) #t)
                                            (lambda (exn) (exn-message exn))])
                             (and (send filter-check-box get-value)
                                  (regexp (send filter-text-field get-value))))])
               (if (string? regexp)
                   (message-box message-box-title regexp dialog)
                   (begin (set! searcher _searcher)
                          (set! filter regexp)
                          (set! ok? #t)
                          (send dialog show #f))))))]
      [else
       (message-box message-box-title
                    (format "\"~a\" is not a directory" (send dir-field get-value))
                    dialog)]))
  (define (cancel-button-callback)
    (send dialog show #f))
  
  (define (method-callback chk)
    (preferences:set
     'drscheme:multi-file-search:search-check-boxes
     (let loop ([methods-check-boxess methods-check-boxess])
       (cond
         [(null? methods-check-boxess) null]
         [else
          (cons
           (let loop ([methods-check-boxes (car methods-check-boxess)])
             (cond
               [(null? methods-check-boxes) null]
               [else (cons (send (car methods-check-boxes) get-values)
                           (loop (cdr methods-check-boxes)))])))]))))
  
  (define (filter-check-box-callback) 
    (preferences:set 'drscheme:multi-file-search:filter? (send filter-check-box get-value))
    (send filter-text-field enable (send filter-check-box get-value)))
  (define (filter-text-field-callback)
    (preferences:set 'drscheme:multi-file-search:filter-string (send filter-text-field get-value)))
 
  (define (recur-check-box-callback)
    (preferences:set 'drscheme:multi-file-search:recur? (send recur-check-box get-value)))
  (define (methods-choice-callback)
    (preferences:set 'drscheme:multi-file-search:search-type (send methods-choice get-selection)) 
    (send active-method-panel active-child
          (list-ref (send active-method-panel get-children)
                    (send methods-choice get-selection))))
  (define (search-text-field-callback)
    (preferences:set 'drscheme:multi-file-search:search-string (send search-text-field get-value)))
  (define (dir-callback)
    (preferences:set 'drscheme:multi-file-search:directory-string (send dir-field get-value)))
  (define (dir-button-callback) 
    (let ([d (get-directory)])
      (when (and d
                 (directory-exists? d))
        (preferences:set 'drscheme:multi-file-search:directory-string d)
        (send dir-field set-value d))))
  
  (define (get-files)
    (let ([dir (send dir-field get-value)])
      (and (directory-exists? dir)
           (if (send recur-check-box get-value)
               (build-recursive-file-list dir filter)
               (build-flat-file-list dir filter)))))

  (define ok? #f)
  
  (send button-panel set-alignment 'right 'center)
  (send dir-panel stretchable-height #f)
  (send outer-files-panel stretchable-height #f)
  (send outer-files-panel set-alignment 'left 'center)
  (send files-inset-panel min-width 20)
  (send files-inset-panel stretchable-width #f)
  (send files-panel set-alignment 'left 'center)

  (send recur-check-box set-value (preferences:get 'drscheme:multi-file-search:recur?))
  (send filter-check-box set-value (preferences:get 'drscheme:multi-file-search:filter?))
  (send search-text-field set-value (preferences:get 'drscheme:multi-file-search:search-string))
  (send filter-text-field set-value (preferences:get 'drscheme:multi-file-search:filter-string))
  (send dir-field set-value (preferences:get 'drscheme:multi-file-search:directory))
  
  (send outer-method-panel stretchable-height #f)
  (send outer-method-panel set-alignment 'left 'center)
  (send method-inset-panel min-width 20)
  (send method-inset-panel stretchable-width #f)
  (send method-panel set-alignment 'left 'center)
  (send filter-panel stretchable-height #f)
  
  (send search-text-field focus)
  (send dialog show #t)
  
  (and
   ok?
   (make-search-info
    searcher
    (get-files))))

;; build-recursive-file-list : string -> (-> (union string #f))
;; thread: first application: eventspace main thread, second applications: searching thread
(define (build-recursive-file-list dir filter)
  (letrec ([touched (make-hash-table)]
           [next-thunk (lambda () (process-dir dir (lambda () #f)))]
           [process-dir
            ; string[dirname] (listof string[filename]) -> (listof string[filename])
            (lambda (dir k)
              (let* ([key (string->symbol dir)]
                     [traversed? (hash-table-get touched key (lambda () #f))])
                (if traversed? 
                    (k)
                    (begin
                      (hash-table-put! touched key #t)
                      (process-dir-contents 
                       (map (lambda (x) (build-path dir x))
                            (directory-list dir))
                       k)))))]
           [process-dir-contents
            ; string[dirname] (listof string[filename]) -> (listof string[filename])
            (lambda (contents k)
              (cond
                [(null? contents) 
                 (k)]
                [else 
                 (let ([file/dir (car contents)])
                   (cond
                     [(and (file-exists? file/dir)
                           (or (not filter)
                               (regexp-match filter file/dir)))
                      (set! next-thunk
                            (lambda ()
                              (process-dir-contents (cdr contents) k)))
                      file/dir]
                     [(directory-exists? file/dir)
                      (process-dir-contents 
                       (cdr contents)
                       (lambda ()
                         (process-dir file/dir k)))]
                     [else (process-dir-contents (cdr contents) k)]))]))])
    (lambda () (next-thunk))))

;; build-flat-file-list : string -> (-> (union string #f))
;; thread: first application: eventspace main thread, second applications: searching thread
(define (build-flat-file-list dir)
  (let ([contents (map (lambda (x) (build-path dir x)) (directory-contents dir))])
    (lambda ()
      (if (null? contents)
          #f
          (begin0
            (car contents)
            (set! contents (cdr contents)))))))

;; exact-match-searcher : make-searcher
;; thread: searching thread
(define (exact-match-searcher params key)
  (let ([case-sensitive? (car params)])
    (lambda (filename add-entry)
      (let ([text (make-object text%)])
        (send text load-file filename)
        (let loop ([pos 0])
          (let ([found (send text find-string key 'forward pos 'eof #t case-sensitive?)])
            (when found
              (let* ([para (send text position-paragraph found)]
                     [para-start (send text paragraph-start-position para)]
                     [line-string (send text get-text para-start
                                        (send text paragraph-end-position para))]
                     [line-number para]
                     [col-number (- found para-start)]
                     [match-length (string-length key)])
                (add-entry line-string line-number col-number match-length)
                (loop (+ found 1))))))))))

;; regexp-match-searcher : make-searcher
;; thread: searching thread
(define (regexp-match-searcher parmas key)
  (let ([re:key (with-handlers ([(lambda (x) #t)
                                 (lambda (exn)
                                   (exn-message exn))])
                  (regexp key))])
    (if (string? re:key)
        re:key
        (lambda (filename add-entry)
          (call-with-input-file filename
            (lambda (port)
              (let loop ([line-number 0])
                (let ([line (read-line port)])
                  (cond
                    [(eof-object? line) (void)]
                    [else
                     (let ([match (regexp-match-positions re:key line)])
                       (when match
                         (let ([pos (car match)])
                           (add-entry line line-number 
                                      (car pos)
                                      (- (cdr pos) (car pos))))))
                     (loop (+ line-number 1))]))))
            'text)))))
  
;; -> string
;; stub for soon to come mred primitive
(define (get-directory) "")

(multi-file-search)
