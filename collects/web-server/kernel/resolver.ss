(module resolver mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net"))

  (define myprint printf)

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
  (define-struct (servlet-library dynamic-pair) () (make-inspector))
  (define-struct (static-pair resource-pair) () (make-inspector))

  (provide/contract
   [struct logical-path ([directory-part (listof string?)]
                         [file-part string?])]
   [struct resource-pair ([v-path logical-path?]
                          [r-path logical-path?])]
   [struct (dynamic-pair resource-pair) ([v-path logical-path?]
                                         [r-path logical-path?])]
   [struct (servlet-library dynamic-pair) ([v-path logical-path?]
                                           [r-path logical-path?])]
   [struct (static-pair resource-pair) ([v-path logical-path?]
                                        [r-path logical-path?])]
   [apply-resource-map (((listof resource-pair?) logical-path?)
                        . ->* . ((union resource-pair? boolean?)
                                 (union logical-path? boolean?)))]
   [url->logical-path (url? . -> . logical-path?)]
   [build-servlet-path ((dynamic-pair? logical-path?)
                        . ->* . ((union path? boolean?)
                                 (union (listof string?) boolean?)
                                 (union (listof string?) boolean?)))]
   [spair (string? string? . -> . static-pair?)]
   [dpair (string? string? . -> . dynamic-pair?)]
   [slib (string? string? . -> . servlet-library?)]
   [string->logical-path (string? . -> . logical-path?)]
   )

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

  ;; **************************************************
  ;; resource map utilities

  ;; string->logical-path: string -> logical-path
  (define string->logical-path
    (let ([special-re (regexp "^([^/]*)$")]
          [file-re (regexp "^/([^/]*)$")]
          [dir-re (regexp "^/([^/]*)(/.*)")])
      (lambda (str)
        (cond
         [(regexp-match special-re str)
          => (lambda (a-match)
               (let ([a-file (cadr a-match)])
                 (make-logical-path '() a-file)))]
         [(regexp-match file-re str)
          => (lambda (a-match)
               (let ([a-file (cadr a-match)])
                 (make-logical-path '() a-file)))]
         [(regexp-match dir-re str)
          => (lambda (a-match)
               (let ([a-dir (cadr a-match)]
                     [rest (string->logical-path (caddr a-match))])
                 (make-logical-path
                  (if (string=? "" a-dir)
                      (logical-path-directory-part rest)
                      (cons a-dir (logical-path-directory-part rest)))
                  (logical-path-file-part rest))))]
         [else
          (error "wierd logical-path " str)]))))

    ;; dpair: string string -> dynamic-pair
  (define (dpair p1 p2)
    (make-dynamic-pair (string->logical-path p1)
                       (string->logical-path p2)))

  ;; spair: string string -> static-pair
  (define (spair p1 p2)
    (make-static-pair (string->logical-path p1)
                      (string->logical-path p2)))

  ;; slib: string string -> servlet-library
  (define (slib p1 p2)
    (make-servlet-library (string->logical-path p1)
                          (string->logical-path p2)))

  ;; ************************************************************
  ;; resolve

  ;; resolve: logical-path resource-map -> (union resource-pair #f)
  ;; Find the real resource corresponding to the logical-path
  ;(define (resolve v-path r-map)
;    (let-values ([(pref suff) (longest-prefix v-path r-map)])
;      (cond
;       [(dynamic-pair? pref)
;        (make-dynamic-pair
;         v-path
;         (extend-real-path (resource-pair-r-path pref) suff))]
;       [(static-pair? pref)
;        (if (string=? "" (logical-path-file-part (resource-pair-r-path pref)))
;            (make-static-pair
;             v-path
;             (extend-real-path (resource-pair-r-path pref) suff))
;            ;; don't extend static files
;            (and (null? (logical-path-directory-part suff))
;                 (string=? "" (logical-path-file-part suff))
;                 pref))]
;       [else #f])))

;  ;; extend-real-path: real-path logical-path -> real-path
;  ;; extend the real-path with a suffix
;  (define (extend-real-path r-path suff)
;    (let ([r-file (logical-path-file-part r-path)])
;      (if (and (null? (logical-path-directory-part suff))
;               (string=? "" (logical-path-file-part suff)))
;          r-path
;          (make-logical-path
;           (append (logical-path-directory-part r-path)
;                   (if (string=? "" r-file)
;                       '()
;                       (list r-file))
;                   (logical-path-directory-part suff))
;           (logical-path-file-part suff)))))

  ;; apply-resource-map: resource-map logical-path
  ;;                     -> (union (values #f #f) (values resource-pair logical-path)
  ;; The first return value is the resource pair in the resource-map
  ;; whose v-path contains the longest prefix of the logical-path.
  ;;
  ;; The second return value is a logical path representing the suffix of the
  ;; virtual path following the prefix.
  (define (apply-resource-map r-map v-path)
    (let loop ([pref #f] [suff #f] [r-map r-map])
      (cond
       [(null? r-map) (values pref suff)]
       [else
        (let* ([new-pref (car r-map)]
               [new-suff (suffix (resource-pair-v-path new-pref) v-path)])
          (if (and new-suff
                   (or (not pref)
                       (longer? new-pref pref)))
              (loop new-pref new-suff (cdr r-map))
              (loop pref suff (cdr r-map))))])))

  ;; suffix: logical-path logical-path -> (union logical-path #f)
  ;; if this is a prefix then return the suffix
  (define (suffix l-p v-path)
    (let ([null-path? (lambda (dir-part file-part)
                        (and (null? dir-part)
                             (string=? "" file-part)))])
      (let suffix ([lp-dir (logical-path-directory-part l-p)]
                   [lp-file (logical-path-file-part l-p)]
                   [v-path-dir (logical-path-directory-part v-path)]
                   [v-path-file (logical-path-file-part v-path)])
        (cond
         [(and (null-path? lp-dir lp-file)
               (null-path? v-path-dir v-path-file))
          (make-logical-path '() "")]
         [(null-path? v-path-dir v-path-file) #f]  ;; ran out of virtual-path parts
         [(null-path? lp-dir lp-file)              ;; ran out of logical-path parts
          (make-logical-path v-path-dir v-path-file)]

         ;; four combinations for "car"
         [(and (null? lp-dir) (null? v-path-dir))
          (if (string=? lp-file v-path-file)
              (make-logical-path '() "")
              #f)]
         [(null? lp-dir)
          (if (string=? lp-file (car v-path-dir))
              (make-logical-path (cdr v-path-dir) v-path-file)
              #f)]
         [(null? v-path-dir)
          (if (string=? (car lp-dir) v-path-file)
              (suffix (cdr lp-dir) lp-file v-path-dir "")
              #f)]
         [(string=? (car lp-dir) (car v-path-dir))
          (suffix (cdr lp-dir) lp-file (cdr v-path-dir) v-path-file)]
         [else #f]))))

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

  ;; build-servlet-path: dynamic-pair logical-path
  ;;                     -> (union path #f)
  ;;                        (union (listof string) #f)
  ;;                        (union (listof string #f)
  ;;
  ;; Move up in the directory tree until you find a file. The servlet-path
  ;; leads to this file.
  ;;
  ;; Keep track of which part of the virtual path maps to the servlet,
  ;; this becomes the path-prefix.
  ;;
  ;; The remaining part of the virtual-path becomes the path-suffix.
  ;;
  ;; All returned values are #f if no servlet is found.
  (define (build-servlet-path d-pair path-suffix)
    (myprint "build-servlet-path~n")
    (let* ([r-path (resource-pair-r-path d-pair)]
           [dir-part (logical-path-directory-part r-path)]
           [file-part (logical-path-file-part r-path)]
           [base-path (apply build-path
                             (if (string=? "" file-part)
                                 dir-part
                                 (append dir-part (list file-part))))]
           [path-prefix (logical-path->list (resource-pair-v-path d-pair))]
           [path-suffix (logical-path->list path-suffix)])
      (let build-it ([path-prefix path-prefix]
                     [path-suffix path-suffix]
                     [svt-path base-path])
        (cond
         [(file-exists? svt-path)
          (values svt-path path-prefix path-suffix)]
         [(null? path-suffix) (values #f #f #f)]
         [else
          (let ([next (car path-suffix)])
            (build-it (append path-prefix (list next))
                      (cdr path-suffix)
                      (build-path svt-path next)))]))))

  );module

