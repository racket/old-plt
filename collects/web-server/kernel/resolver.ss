;; ************************************************************
;; DATA DEFINITIONS

;; a logical-path is either
;; (make-virtual-path (listof string) string))
;; (make-real-path (listof string) string))
(define-struct logical-path (directory-part file-part) (make-inspector))
(define-struct (virtual-path logical-path) () (make-inspector))
(define-struct (real-path logical-path) () (make-inspector))

;; A resource-pair is either
;; (make-static-resource-pair v-path r-path)
;; (make-dynamic-resource-pair v-parth r-path)
;; v-path is a logical-path in the virtual file system.
;; r-path is a logical-path in the real file system.
(define-struct resource-pair (v-path r-path) (make-inspector))
(define-struct (dynamic-pair resource-pair) () (make-inspector))
(define-struct (static-pair resource-pair) () (make-inspector))

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

;; resolve: virtual-path resource-map -> (union resource-pair #f)
;; Find the real resource corresponding to the virtual-path
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
        (make-real-path
         (append (logical-path-directory-part r-path)
                 (if (string=? "" r-file)
                     '()
                     (list r-file))
                 (logical-path-directory-part suff))
         (logical-path-file-part suff)))))

;; longest-prefix: virtual-path resource-map 
;;                 -> (union (values #f #f) (values resource-pair logical-path)
;; Find the resource pair in the resource-map whose v-path contains the longest
;; prefix of the virtual-path.
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

;; suffix: logical-path virtual-path -> (union logical-path #f)
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
               (make-virtual-path (cdr v-p-dir)
                                  (logical-path-file-part v-path)))]
      [else #f])))

;; longer?: resource-pair resource-pair -> boolean
;; is the virtual-path in one pair longer than the one in the other
(define (longer? r-p1 r-p2)
  (let ([len1 (length (logical-path-directory-part (resource-pair-v-path r-p1)))]
        [len2 (length (logical-path-directory-part (resource-pair-v-path r-p2)))])
    (or (> len1 len2)
        (and (= len1 len2)
             (not (string=? "" (logical-path-file-part (resource-pair-v-path r-p1))))))))

;; ************************************************************
;; tests

;; string->logical-path: string ((listof string) string -> logical-path)-> logical-path
(define (string->logical-path str ctor)
  (let ([special-re (regexp "^([^/]*)$")]
        [file-re (regexp "^/([^/]*)$")]
        [dir-re (regexp "^/([^/]*)(/.*)")])
    (cond
      [(regexp-match special-re str)
       => (lambda (a-match)
            (let ([a-file (cadr a-match)])
              (ctor '() a-file)))]
      [(regexp-match file-re str)
       => (lambda (a-match)
            (let ([a-file (cadr a-match)])
              (ctor '() a-file)))]
      [(regexp-match dir-re str)
       => (lambda (a-match)
            (let ([a-dir (cadr a-match)]
                  [rest (string->logical-path (caddr a-match) ctor)])
              (ctor
               (if (string=? "" a-dir)
                   (logical-path-directory-part rest)
                   (cons a-dir (logical-path-directory-part rest)))
               (logical-path-file-part rest))))]
      [else
       (error "weird logical-path " str)])))

;; check-logical-path: logical-path (listof string) string -> boolean
(define (check-logical-path? lp dir-part file-part)
  (and (string=? file-part (logical-path-file-part lp))
       (equal? (logical-path->list lp)
               (if (string=? "" file-part)
                   dir-part
                   (append dir-part (list file-part))))))

;; suffix/strings: string string -> (union #f logical-path)
(define (suffix/strings pref of-this)
  (suffix (string->logical-path pref make-logical-path)
          (string->logical-path of-this make-virtual-path)))

;; logical-path=?: logical-path logical-path -> boolean
(define (logical-path=? lp1 lp2)
  (and (equal? (logical-path-directory-part lp1)
               (logical-path-directory-part lp2))
       (string=? (logical-path-file-part lp1)
                 (logical-path-file-part lp2))))

;; test-suffix: string string string -> boolean
(define (test-suffix pref of-this expected)
  (logical-path=? (suffix/strings pref of-this)
                  (string->logical-path expected make-logical-path)))

;; resolve/string: string resource-map -> (union resource-pair #f)
(define (resolve/string v-path-str r-map)
  (resolve (string->logical-path v-path-str make-virtual-path)
           r-map))

;; test-resolve: string resource-map resource-pair -> boolean
(define (test-resolve v-path-str r-map expected)
  (let ([res (resolve/string v-path-str r-map)])
    (if res (same-r-pair? res expected)
        (not expected))))

;; same-r-pair?: resource-pair resource-pair -> bollean
;; same type of resource pair with the same paths
(define (same-r-pair? rp1 rp2)
  (and (if (dynamic-pair? rp1)
           (dynamic-pair? rp2)
           (static-pair? rp2))
       (logical-path=? (resource-pair-v-path rp1)
                       (resource-pair-v-path rp2))
       (logical-path=? (resource-pair-r-path rp1)
                       (resource-pair-r-path rp2))))
       

(check-logical-path?
 (string->logical-path "foo" make-virtual-path)
 '() "foo")

(check-logical-path?
 (string->logical-path "/foo/" make-virtual-path)
 '("foo") "")

(check-logical-path?
 (string->logical-path "/" make-virtual-path)
 '() "")

(check-logical-path?
 (string->logical-path "///" make-virtual-path)
 '() "")

(check-logical-path?
 (string->logical-path "/foo/bar/baz" make-virtual-path)
 '("foo" "bar") "baz")

(check-logical-path?
 (string->logical-path "/foo//bar///baz" make-virtual-path)
 '("foo" "bar") "baz")

(check-logical-path?
 (string->logical-path "/foo/bar/baz/" make-virtual-path)
 '("foo" "bar" "baz") "")

;; dpair: string string -> dynamic-pair
(define (dpair p1 p2)
  (make-dynamic-pair (string->logical-path p1 make-virtual-path)
                     (string->logical-path p2 make-real-path)))

;; spair: string string -> static-pair
(define (spair p1 p2)
  (make-static-pair (string->logical-path p1 make-virtual-path)
                    (string->logical-path p2 make-real-path)))

(r-map-is-function? '())
(r-map-is-function?
 (list (dpair "foo" "bar")))
(r-map-is-function?
   (list (spair "foo" "bar")))
(r-map-is-function?
   (list (spair "foo" "bar")
         (dpair "boo" "baz")))
(not (r-map-is-function?
      (list (dpair "foo" "bar")
            (dpair "foo" "bar"))))
(not (r-map-is-function?
      (list (spair "foo" "bar")
            (spair "foo" "bar"))))
(not (r-map-is-function?
      (list (spair "/foo/bar" "baz")
            (dpair "/foo/bar" "bing"))))
(not (r-map-is-function?
      (list (spair "/foo/bar/" "/bing/bang")
            (spair "/foo/bar" "/bing/bat"))))
(not (r-map-is-function?
      (list (dpair "/foo" "bar")
            (dpair "/foo/bar" "bing")
            (spair "/baz/bing" "bing")
            (spair "foo" "bing"))))
(r-map-is-function?
 (list (dpair "foo" "bar")
       (spair "/foo/bar" "bar")
       (dpair "/foo/bar/baz" "bing")
       (dpair "/foo/bar/baz/boink" "go")))

(r-map-paths-same-type? '())
(r-map-paths-same-type?
 (list (dpair "/files/scripts/" "/foo/bar/")
       (spair "/files/htdocs/" "/foo/bar/")))
(not (r-map-paths-same-type?
      (list (dpair "/files/scripts/a-script" "/foo/bar")
            (spair "/files/htdocs/a-page" "/foo/bar"))))
(r-map-paths-same-type?
   (list (dpair "/files/scripts/" "/foo/bar/")
         (spair "/files/htdocs/" "/foo/bar/")
         (spair "/files/htdocs/a-page.html" "/foo/bar/a-file")
         (dpair "/files/htdocs/a-script.ss" "/foo/bar/a-script")))
(not (r-map-paths-same-type?
      (list (dpair "/" "foo")
            (spair "/" "foo"))))
(r-map-paths-same-type?
 (list (dpair "/" "foo")
       (spair "/" "bar")))

(r-map-paths-valid?
 (list (spair "virtual-file" "real-file")))
(r-map-paths-valid?
 (list (dpair "virtual-file" "real-file")))
(r-map-paths-valid?
 (list (spair "/virtual-dir/" "/real-dir/")))
(r-map-paths-valid?
 (list (dpair "/virtual-dir/" "/real-dir/")))
(r-map-paths-valid?
   (list (dpair "/virtual-dir/" "real-file")))

(not (r-map-paths-valid?
      (list (spair "/virtual-dir/" "real-file"))))
(not (r-map-paths-valid?
      (list (spair "virtual-file" "/real-dir/"))))
(not (r-map-paths-valid?
      (list (dpair "virtual-file" "/real-dir/"))))

(longer? (spair "/foo" "/foo/bar/baz")
         (dpair "/" "/bing/bang"))
(longer? (spair "/foo/" "/")
         (spair "/" "/"))
(longer? (dpair "/foo/bar/baz" "foo")
         (dpair "/foo/bar/" ""))
(longer? (spair "/foo/bar/baz/bing" "foo")
         (spair "/foo/bar/baz/" ""))
(not (longer? (spair "/" "/foo/")
              (dpair "/foo/" "/bar")))
(not (longer? (spair "/foo" "/foo/bar")
              (spair "/foo/bar" "/")))

(test-suffix "/foo/" "/foo/bar" "/bar")
(test-suffix "/foo" "/foo" "")
(test-suffix "/foo/bar/bing/baz/" "/foo/bar/bing/baz/boing/" "/boing/")
(test-suffix "/" "foo" "foo")
(test-suffix "" "foo" "foo")
(test-suffix "/" "/foo/" "/foo/")
(test-suffix "/" "/foo/bar" "/foo/bar")
(not (suffix/strings "/foo" "/bar"))
(not (suffix/strings "/foo/bar" "/foo/baz"))
(not (suffix/strings "foo" "bar"))
(not (suffix/strings "/foo/" "/bar/"))

(not (resolve/string "foo" '()))
(not (resolve/string "/" '()))
(not (resolve/string "/foo" '()))
(not (resolve/string "/foo/bar" '()))
(not (resolve/string "/foo/bar/" '()))
(not (resolve/string "/foo/bar/baz"
                     (list (dpair "/foo/bing" "ting")
                           (spair "/foo/baz" "dong")
                           (dpair "/bar/baz" "ball"))))
(not (resolve/string "/foo/bar/baz"
                     (list (dpair "/foo/bing" "ting")
                           (spair "/foo/bar" "baz")
                           (dpair "/bar/baz" "ball"))))
(test-resolve "/foo"
              (list (spair "/foo" "bar"))
              (spair "/foo" "bar"))
(test-resolve "/"
              (list (spair "/" "bar"))
              (spair "/" "bar"))
(test-resolve "/foo/"
              (list (dpair "/foo/" "bar"))
              (dpair "/foo/" "bar"))
(test-resolve "/foo/bar"
              (list (dpair "/foo/bar" "bat"))
              (dpair "/foo/bar" "bat"))
(test-resolve "/foo/bar/bing/bang/bong/"
              (list (dpair "bar" "none")
                    (dpair "/foo/bar/" "/big/ben/"))
              (dpair "/foo/bar/bing/bang/bong/"
                     "/big/ben/bing/bang/bong/"))
(test-resolve "/foo/bar/bing/bang/bong/"
              (list (dpair "/foo/bar/" "/big/ben"))
              (dpair "/foo/bar/bing/bang/bong/"
                     "/big/ben/bing/bang/bong/"))

