(require-library "framework.ss" "framework")

;; search-results-frame : (union #f (instance frame%))
;; The search results frame. There is only ever one.
(define search-results-frame #f)

;; multi-file-search : -> void
;; opens a dialog to configure the search and initiates the search
(define (multi-file-search)
  (configure-search))

;; search-type = (make-search-type string make-searcher (listof (cons string boolean)))
;; the param strings are the labels for checkboxes
;; the param booleans are the default values for the checkboxes
;; these are the available searches
(define-struct search-type (label make-searcher params))

;; make-searcher = ((listof boolean) string -> searcher)
;; this returns the function that does the actual searching
;; it takes the search parameters and the search string

;; searcher = (string -> ...)
;; this performs a single search.
;; the input is the filename to be searched

;; search-info = (make-search-info searcher (-> (union #f string)))
;; this is the info from the user to do a particular search
;; do-single-search runs the search in a particular file
;; filenames is a thunk that returns the next filename, or #f
(define-struct search-info (do-single-search filenames))

;; search-types : (listof search-type)
(define search-types
  (list (make-search-type
         "String match (handles files with graphics)"
         (lambda (info search-string) (exact-match-searcher info search-string))
         (list (cons "Case sensitive" #f)))
        (make-search-type
         "Regular Expression (only raw text files)"
         (lambda (info search-string) (regexp-searcher info search-string))
         (list))))

;; preferences initialization
(preferences:set-default 'drscheme:multi-file-search:recur? #t boolean?)
(preferences:set-default 'drscheme:multi-file-search:filter? #f boolean?)
(preferences:set-default 'drscheme:multi-file-search:filter-string "" string?)
(preferences:set-default 'drscheme:multi-file-search:directory "" string?)
(preferences:set-default 'drscheme:multi-file-search:search-string "" string?)
(preferences:set-default 'drscheme:multi-file-search:search-type
                         0 
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

;; do-search : search-info -> void
;; opens a window and creates the thread that does the search
(define (do-search search-info)
  (define frame (make-object frame% "Search" #f 450))
  (define panel (make-object saved-vertical-resizable% frame))
  (define results-text (make-object text%))
  (define results-ec (make-object editor-canvas% panel results-text))
  (define zoom-text (make-object text%))
  (define zoom-ec (make-object editor-canvas% panel zoom-text))
  
  (send panel set-percentages (preferences:get 'drscheme:multi-file-search:percentages)))

;; configure-search : -> (union #f search-info)
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
  
  (define (ok-button-callback)
    (if (with-handlers ([exn:i/o:filesystem?
                         (lambda (x) #f)])
          (directory-exists? (send dir-field get-value)))
        (begin (set! ok? #t)
               (send dialog show #f))
        (message-box "DrScheme - Multi File Search"
                     (format "\"~a\" is not a directory" (send dir-field get-value)))))
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
    (preferences:set 'drscheme:multi-file-search:filter-string (send filter-text-field get-string)))
 
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
               (build-recursive-file-list dir)
               (build-flat-file-list dir)))))

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
    ((search-type-make-searcher (list-ref search-types (send methods-choice get-selection)))
     (map (lambda (cb) (send cb get-value))
          (send (send active-method-panel active-child) get-children))
     (send search-text-field get-value))
    (get-files))))

;; build-recursive-file-list : string -> (-> (union string #f))
(define (build-recursive-file-list dir)
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
                     [(file-exists? file/dir)
                      (begin
                        (set! next-thunk
                              (lambda ()
                                (process-dir-contents (cdr contents) k)))
                        file/dir)]
                     [(directory-exists? file/dir)
                      (process-dir-contents 
                       (cdr contents)
                       (lambda ()
                         (process-dir file/dir k)))]
                     [else (process-dir-contents (cdr contents) k)]))]))])
    (lambda () (next-thunk))))

;; build-flat-file-list : string -> (-> (union string #f))
(define (build-flat-file-list dir)
  (let ([contents (map (lambda (x) (build-path dir x)) (directory-contents dir))])
    (lambda ()
      (if (null? contents)
          #f
          (begin0
            (car contents)
            (set! contents (cdr contents)))))))

;; exact-match-searcher : make-searcher
(define (exact-match-searcher params key)
  (lambda (filename)
    (void)))

;; regexp-match-searcher : make-searcher
(define (regexp-match-searcher parmas key)
  (lambda (filename)
    (void)))
  
;; -> string
;; stub for soon to come mred primitive
(define (get-directory) "")

;(multi-file-search)