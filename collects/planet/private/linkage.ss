(module linkage mzscheme
  
  (require "planet-shared.ss"
           "../config.ss"
           (lib "file.ss"))

  (provide get-linkage add-linkage!)
  
  ; ==========================================================================================
  ; PHASE 1: LINKAGE
  ; The first check is to see if there is a valid linkage for the module.
  ; ==========================================================================================
  
  ;; NOTE :: right now we have a nasty situation with the linkage-table: it doesn't associate
  ;; keys to packages, which it seems it should. Instead it associates keys to the arguments
  ;; to the pkg-spec constructor; this is done to facilitate reading the data from disk but
  ;; causes ugliness in add-linkage! where we have the actual package but have to break it down
  ;; so the arguments needed to reconstitute it can be stored.
  
  
  ; LINKAGE-TABLE ::= hash-table[LINKAGE-KEY -> PKG-LOCATION]
  (define LT #f) 
  
  ; get-linkage-table : -> hash-table[LINKAGE-KEY -> PKG-LOCATION]
  (define (get-linkage-table)
    (unless (file-exists? (LINKAGE-FILE)) (with-output-to-file (LINKAGE-FILE) newline))
    (unless LT (set! LT (build-hash-table (with-input-from-file (LINKAGE-FILE) read-all))))
    LT)
  
  ; add-linkage! : (symbol | #f) FULL-PKG-SPEC PKG -> PKG
  ; unless the first argument is #f, associates the pair of the first two arguments 
  ; with the last in the linkage table. Returns the given package-location
  (define (add-linkage! module pkg-spec pkg)
    (when (and module (current-module-name-prefix))
      (let ((key (get-key module pkg-spec)))
        (hash-table-get 
         (get-linkage-table)
         key
         (lambda ()
           (let ((pkg-as-list (list (pkg-name pkg)
                                    (pkg-route pkg)
                                    (pkg-maj pkg)
                                    (pkg-min pkg)
                                    (pkg-path pkg))))
             (begin
               (hash-table-put! (get-linkage-table) key pkg-as-list)
               (with-output-to-file (LINKAGE-FILE)
                 (lambda () (write (list key pkg-as-list)))
                 'append)))))))
    pkg)
  
  ; get-linkage : symbol FULL-PKG-SPEC -> PKG | #f
  ; returns the already-linked module location, or #f if there is none
  (define (get-linkage module-specifier pkg-specifier)
    (let ((pkg-fields (hash-table-get
                       (get-linkage-table)
                       (get-key module-specifier pkg-specifier)
                       (lambda () #f))))
      (if pkg-fields (apply make-pkg pkg-fields) #f)))
  
  ; get-key : symbol FULL-PKG-SPEC -> LINKAGE-KEY
  ; produces a linkage key for the given pair.
  (define (get-key module-specifier pkg-spec)
    (list* (get-module-id module-specifier pkg-spec)
           (pkg-spec-name pkg-spec)
           (pkg-spec-maj pkg-spec)
           (pkg-spec-minor-lo pkg-spec)
           (pkg-spec-minor-hi pkg-spec)
           (pkg-spec-path pkg-spec)))
  
  ; get-module-id : TST FULL-PKG-SPEC -> LINKAGE-MODULE-KEY
  ; gets a unique identifier naming the module that produced the pkg-spec.
  ; (strategy due to Matthew)
  (define (get-module-id ms pkg-spec)
    (cond
      [(full-filename-identifier? ms) 
       (module-specifier->key ms)]
      [(and 
        (pkg-spec-stx pkg-spec) ;; <-- I don't know about this
        (syntax-original? (pkg-spec-stx pkg-spec))
        (path? (syntax-source (pkg-spec-stx pkg-spec))))
       (path->key (desuffix (syntax-source (pkg-spec-stx pkg-spec))))]
      [(and (symbol? ms) (current-load-relative-directory))
       (path->key (build-path 
                   (current-load-relative-directory)
                   (symbol->string ms)))]
      [else #f]))
  
  
  ;; ----------------------------------------
  ;; ALL THE BELOW CODE IN THIS SECTION NEEDS
  ;; MAJOR MODIFICATION FOR v299
  
  ; path? : tst -> bool
  (define path? string?)
  
  ; full-filename-identifier? : TST -> bool
  ; determines if the given value represents a fully-resolved module identifier
  (define (full-filename-identifier? ms) 
    (and (symbol? ms)
         (regexp-match "^\\,.*" (symbol->string ms))))
  
  ; module-specifier->key : symbol -> LINKAGE-MODULE-KEY
  (define (module-specifier->key ms)
    (string->symbol (substring (symbol->string ms) 1)))
  
  ; path->key : string -> LINKAGE-MODULE-KEY
  (define (path->key p) (string->symbol p))
  
  ; desuffix : string -> string
  ; removes the suffix from the given file
  (define (desuffix file)
    (let ((extension (filename-extension file)))
      (if extension
          (substring file 0 (- (string-length file) (+ (string-length extension) 1)))
          file))))
