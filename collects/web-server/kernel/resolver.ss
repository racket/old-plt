(module resolver mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net"))

  ;; ************************************************************
  ;; DATA DEFINITIONS

  ;; a logical-path is a structure
  ;; (make-logical-path (listof string) (listof string))
  (define-struct logical-path (directory-part file-part) (make-inspector))


  ;; A resource-pair is either
  ;; (make-static-resource-pair v-path r-path)
  ;; (make-dynamic-resource-pair v-parth r-path)
  ;; v-path is a logical-path in the virtual file system.
  ;; r-path is a logical-path in the real file system.
  (define-struct resource-pair (v-path r-path) (make-inspector))
  (define-struct (dynamic-pair resource-pair) () (make-inspector))
  (define-struct (static-pair resource-pair) () (make-inspector))

  (provide/contract
   [struct logical-path ([directory-part (listof string?)]
                         [file-part string?])]
   [struct resource-pair ([v-path logical-path?]
                          [r-path logical-path?])]
   [struct (dynamic-pair resource-pair) ([v-path logical-path?]
                                         [r-path logical-path?])]
   [struct (static-pair resource-pair) ([v-path logical-path?]
                                        [r-path logical-path?])]
   [resolve (logical-path? (listof resource-pair?) . -> . (union resource-pair?
                                                                 boolean?))]
   [url->logical-path (url? . -> . logical-path?)])

  ;; A resource-map is a (listof resource-pair)

  ;; ************************************************************
  ;; valid-resource-map?

  ;; valid-resource-map?: resource-map -> boolean
  ;; determine if a resource map makes sense
  (define (valid-resource-map? r-map)
    (and (r-map-is-function? r-map)
         (r-map-paths-same-type? r-map)
         (r-map-paths-valid? r-map)))

  ;; r-map-is-function?: resource-map -> boolean
  ;; determine if a resource-map maps has a unique image for every element in its range
  (define (r-map-is-function? r-map)
    (or (null? r-map)
        (and (andmap
              (lambda (r-p)
                (not (equal? (logical-path->list (resource-pair-v-path r-p))
                             (logical-path->list (resource-pair-v-path (car r-map))))))
              (cdr r-map))
             (r-map-is-function? (cdr r-map)))))

  ;; logical-path->list: logical-path -> (listof string)
  (define (logical-path->list l-path)
    (let ([f-p (logical-path-file-part l-path)]
          [d-p (logical-path-directory-part l-path)])
      (if (string=? "" f-p) d-p
          (append d-p (list f-p)))))

  ;; r-map-paths-same-type?: resource-map -> boolean
  ;; every pair of resource-pairs which specify a file and which
  ;; have the same r-path should be of the same type
  (define (r-map-paths-same-type? r-map)
    (or (null? r-map)
        (and (or (string=? "" (logical-path-file-part (resource-pair-r-path (car r-map))))
                 (andmap
                  (lambda (r-p)
                    (or (not (equal? (logical-path->list (resource-pair-r-path r-p))
                                     (logical-path->list (resource-pair-r-path (car r-map)))))
                        (if (dynamic-pair? r-p)
                            (dynamic-pair? (car r-map))
                            (static-pair? (car r-map)))))
                  (cdr r-map)))
             (r-map-paths-same-type? (cdr r-map)))))

  ;; r-map-paths-valid?: resource-map -> boolean
  ;; files map to files, directories map to directories or dynamic files
  (define (r-map-paths-valid? r-map)
    (let* ([directory?
            (lambda (lp)
              (string=? "" (logical-path-file-part lp)))]
           [file?
            (lambda (lp)
              (not (directory? lp)))])
      (andmap
       (lambda (r-p)
         (let ([v-path (resource-pair-v-path r-p)]
               [r-path (resource-pair-r-path r-p)])
           (if (file? v-path)
               (file? r-path)
               (or (directory? r-path)
                   (dynamic-pair? r-p)))))
       r-map)))

  ;; ************************************************************
  ;; resolve

  ;; resolve: logical-path resource-map -> (union resource-pair #f)
  ;; Find the real resource corresponding to the logical-path
  (define (resolve v-path r-map)
    (let-values ([(pref suff) (longest-prefix v-path r-map)])
      (cond
       [(dynamic-pair? pref)
        (make-dynamic-pair
         v-path
         (extend-real-path (resource-pair-r-path pref) suff))]
       [(static-pair? pref)
        (if (string=? "" (logical-path-file-part (resource-pair-r-path pref)))
            (make-static-pair
             v-path
             (extend-real-path (resource-pair-r-path pref) suff))
            ;; don't extend static files
            (and (null? (logical-path-directory-part suff))
                 (string=? "" (logical-path-file-part suff))
                 pref))]
       [else #f])))

  ;; extend-real-path: real-path logical-path -> real-path
  ;; extend the real-path with a suffix
  (define (extend-real-path r-path suff)
    (let ([r-file (logical-path-file-part r-path)])
      (if (and (null? (logical-path-directory-part suff))
               (string=? "" (logical-path-file-part suff)))
          r-path
          (make-logical-path
           (append (logical-path-directory-part r-path)
                   (if (string=? "" r-file)
                       '()
                       (list r-file))
                   (logical-path-directory-part suff))
           (logical-path-file-part suff)))))

  ;; longest-prefix: logical-path resource-map
  ;;                 -> (union (values #f #f) (values resource-pair logical-path)
  ;; Find the resource pair in the resource-map whose v-path contains the longest
  ;; prefix of the logical-path.
  (define (longest-prefix v-path r-map)
    (let loop ([pref #f] [suff #f] [r-map r-map])
      (cond
       [(null? r-map) (values pref suff)]
       [else
        (let* ([new-pref (car r-map)]
               [suff (suffix (resource-pair-v-path new-pref) v-path)])
          (if (and suff
                   (or (not pref)
                       (longer? new-pref pref)))
              (loop new-pref suff (cdr r-map))
              (loop pref suff (cdr r-map))))])))

  ;; suffix: logical-path logical-path -> (union logical-path #f)
  ;; if this is a prefix then return the suffix
  (define (suffix l-p v-path)
    (let ([l-p-dir (logical-path-directory-part l-p)]
          [v-p-dir (logical-path-directory-part v-path)])
      (cond
       [(and (null? l-p-dir)
             (null? v-p-dir))
        (cond
         [(string=? (logical-path-file-part l-p)
                    (logical-path-file-part v-path))
          (make-logical-path '() "")]
         [(string=? "" (logical-path-file-part l-p))
          v-path]
         [else #f])]
       [(null? v-p-dir) #f]
       [(null? l-p-dir)
        (and (string=? "" (logical-path-file-part l-p)) v-path)]
       [(string=? (car l-p-dir) (car v-p-dir))
        (suffix (make-logical-path (cdr l-p-dir)
                                   (logical-path-file-part l-p))
                (make-logical-path (cdr v-p-dir)
                                   (logical-path-file-part v-path)))]
       [else #f])))

  ;; longer?: resource-pair resource-pair -> boolean
  ;; is the logical-path in one pair longer than the one in the other
  (define (longer? r-p1 r-p2)
    (let ([len1 (length (logical-path-directory-part (resource-pair-v-path r-p1)))]
          [len2 (length (logical-path-directory-part (resource-pair-v-path r-p2)))])
      (or (> len1 len2)
          (and (= len1 len2)
               (not (string=? "" (logical-path-file-part (resource-pair-v-path
               r-p1))))))))

  ;; url->logical-path: url -> logical-path
  ;; create a logical-path from a url.
  ;; simplify the virtual path to remove as many dots as possible.
  (define (url->logical-path a-url)
    (let build-logical-path ([p (url-path a-url)])
      (cond
       [(null? p) (make-logical-path '() "")]
       [(path/param? (car p))
        (build-logical-path (cons (path/param-path (car p)) (cdr p)))]
       [(null? (cdr p))
        (let ([first (car p)])
          (cond
           [(string=? first ".") (make-logical-path '() "")]
           [(string=? first "..") (make-logical-path '("..") "")]
           [else
            (make-logical-path '() first)]))]
       [else
        (let ([first (car p)]
              [rest (build-logical-path (cdr p))])
          (cond
           [(or (string=? "" first)
                (string=? "." first)) rest]
           [(string=? ".." first)
            (make-logical-path
             (cons first (logical-path-directory-part rest))
             (logical-path-file-part rest))]
           [(null? (logical-path-directory-part rest))
            (make-logical-path
             (cons first (logical-path-directory-part rest))
             (logical-path-file-part rest))]
           [(string=? ".." (car (logical-path-directory-part rest)))
            (make-logical-path
             (cdr (logical-path-directory-part rest))
             (logical-path-file-part rest))]
           [else
            (make-logical-path
             (cons first (logical-path-directory-part rest))
             (logical-path-file-part rest))]))])))
  );module

