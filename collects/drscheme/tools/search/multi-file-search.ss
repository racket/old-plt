(require-library "framework.ss" "framework")

;; search-results-frame : (union #f (instance frame%))
;; The search results frame. There is only ever one.
(define search-results-frame #f)

;; multi-file-search : -> void
;; opens a dialog to configure the search and initiates the search
(define (multi-file-search)
  (configure-search))

;; search-type = (make-search-type string searcher (listof string))
;; searcher = ((listof boolean) -> ...)
(define-struct search-type (label searcher params))

;; search-types : (listof search-type)
(define search-types
  (list (make-search-type
         "String match (handles files with graphics)"
         (lambda (info) (exact-match-searcher info))
         (list "Case sensitive"))
        (make-search-type
         "Regular Expression (only raw text files)"
         (lambda (info) (regexp-searcher info))
         (list))))

;; preferences initialization
(preferences:set-default 'drscheme:multi-file-search:recur? #f boolean?)
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
(preferences:set-default 'drscheme:multi-file-search:search-check-boxes 
                         null
                         (lambda (x) (and (list? x) (andmap (lambda (x) (and (list? x) (andmap number? x))) x))))

;; configure-search : -> searcher
;; configures the search
(define (configure-search)
  (define dialog (make-object dialog% "Configure search" #f #f #f #f #f '(resize-border)))
  (define outer-files-panel (make-object vertical-panel% dialog '(border)))
  (define files-label (make-object message% "Files" outer-files-panel))
  (define files-inset-outer-panel (make-object horizontal-panel% outer-files-panel))
  (define files-inset-panel (make-object horizontal-panel% files-inset-outer-panel))
  (define files-panel (make-object vertical-panel% files-inset-outer-panel))
  (define outer-method-panel (make-object vertical-panel% dialog '(border)))
  (define method-label (make-object message% "Search Method" outer-method-panel))
  (define method-inset-outer-panel (make-object horizontal-panel% outer-method-panel))
  (define method-inset-panel (make-object horizontal-panel% method-inset-outer-panel))
  (define method-panel (make-object vertical-panel% method-inset-outer-panel))
  (define spacer (make-object grow-box-spacer-pane% dialog))
  
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
  
  (define (dir-button-callback) 
    (let ([d (get-directory)])
      (when (and d
                 (directory-exists? d))
        (send dir-field set-value d))))
  
  (define (filter-check-box-callback) 
    (preferences:set 'drscheme:multi-file-search:filter? (send filter-check-box get-value))
    (send filter-text-field enable (send filter-check-box get-value)))
  (define (filter-text-field-callback)
    (preferences:set 'drscheme:multi-file-search:filter-string (send filter-text-field get-string)))
 
  (define (recur-check-box-callback)
    (preferences:set 'drscheme:multi-file-search:recur? (send recur-check-box get-value)))
  (define methods-choice (make-object choice% #f (map search-type-label search-types) method-panel 
                           (lambda (x y) (methods-choice-callback))))
  (define search-string (make-object text-field% "Search string" method-panel void))
  (define active-method-panel (make-object panel:single% method-panel))
  (define methods-check-boxess
    (map
     (lambda (search-type)
       (let ([p (make-object vertical-panel% active-method-panel)])
         (send p set-alignment 'left 'center)
         (map (lambda (flag-name) (make-object check-box% flag-name p void))
              (search-type-params search-type))))
     search-types))
  
  (define (methods-choice-callback)
    (send active-method-panel active-child
          (list-ref (send active-method-panel get-children)
                    (send methods-choice get-selection))))

  (send dir-panel stretchable-height #f)
  (send outer-files-panel stretchable-height #f)
  (send outer-files-panel set-alignment 'left 'center)
  (send files-inset-panel min-width 20)
  (send files-inset-panel stretchable-width #f)
  (send files-panel set-alignment 'left 'center)

  (send recur-check-box set-value (preferences:get 'drscheme:multi-file-search:recur?))
  (send filter-check-box set-value (preferences:get 'drscheme:multi-file-search:filter?))
  
  (send outer-method-panel stretchable-height #f)
  (send outer-method-panel set-alignment 'left 'center)
  (send method-inset-panel min-width 20)
  (send method-inset-panel stretchable-width #f)
  (send method-panel set-alignment 'left 'center)
  (send filter-panel stretchable-height #f)
  (send dialog show #t))
  

(multi-file-search)