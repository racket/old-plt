(module test-resolver mzscheme
  (require (lib "url.ss" "net")
           "the-real-test-harness.ss"
           "resolver.ss")

  (require/expose "resolver.ss" (logical-path->list suffix r-map-is-function?
                                                    r-map-paths-same-type? r-map-paths-valid? longer?))


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
    (test (format "lp = ~s    dir-part = ~s   file-part = ~s"
                  lp dir-part file-part)
          (lambda ()
            (and (string=? file-part (logical-path-file-part lp))
                 (equal? (logical-path->list lp)
                         (if (string=? "" file-part)
                             dir-part
                             (append dir-part (list file-part))))))))

  ;; suffix/strings: string string -> (union #f logical-path)
  ;; call suffix after convertin strings to logical-paths
  (define (suffix/strings pref of-this)
    (suffix (string->logical-path pref make-logical-path)
            (string->logical-path of-this make-logical-path)))

  ;; logical-path=?: logical-path logical-path -> boolean
  (define (logical-path=? lp1 lp2)
    (and (equal? (logical-path-directory-part lp1)
                 (logical-path-directory-part lp2))
         (string=? (logical-path-file-part lp1)
                   (logical-path-file-part lp2))))

  ;; test-suffix: string string string -> boolean
  (define (test-suffix pref of-this expected)
    (test (format "test-suffix: pref = ~s   of-this = ~s   expected = ~s"
                  pref of-this expected)
          (lambda ()
            (logical-path=? (suffix/strings pref of-this)
                            (string->logical-path expected make-logical-path)))))

  ;; resolve/string: string resource-map -> (union resource-pair #f)
  (define (resolve/string v-path-str r-map)
    (resolve (string->logical-path v-path-str make-logical-path)
             r-map))

  ;; test-resolve: string resource-map resource-pair -> boolean
  (define (test-resolve v-path-str r-map expected)
    (test (format "test-resolve: v-path-str = ~s   r-map = ~s expected = ~s"
                  v-path-str r-map expected)
          (lambda ()
            (let ([res (resolve/string v-path-str r-map)])
              (if res (same-r-pair? res expected)
                  (not expected))))))

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
   (string->logical-path "foo" make-logical-path)
   '() "foo")

  (check-logical-path?
   (string->logical-path "/foo/" make-logical-path)
   '("foo") "")

  (check-logical-path?
   (string->logical-path "/" make-logical-path)
   '() "")

  (check-logical-path?
   (string->logical-path "///" make-logical-path)
   '() "")

  (check-logical-path?
   (string->logical-path "/foo/bar/baz" make-logical-path)
   '("foo" "bar") "baz")

  (check-logical-path?
   (string->logical-path "/foo//bar///baz" make-logical-path)
   '("foo" "bar") "baz")

  (check-logical-path?
   (string->logical-path "/foo/bar/baz/" make-logical-path)
   '("foo" "bar" "baz") "")

  ;; dpair: string string -> dynamic-pair
  (define (dpair p1 p2)
    (make-dynamic-pair (string->logical-path p1 make-logical-path)
                       (string->logical-path p2 make-logical-path)))

  ;; spair: string string -> static-pair
  (define (spair p1 p2)
    (make-static-pair (string->logical-path p1 make-logical-path)
                      (string->logical-path p2 make-logical-path)))

  (test "tests for r-map-is-function?"
        (lambda ()
          (and
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
                  (dpair "/foo/bar/baz/boink" "go"))))))

  (test "r-map-paths-same-type?"
        (lambda ()
          (and
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
                  (spair "/" "bar"))))))

  (test "r-map-paths-valid?"
        (lambda ()
          (and
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
                 (list (dpair "virtual-file" "/real-dir/")))))))

  (test "longer?"
        (lambda ()
          (and
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
                         (spair "/foo/bar" "/"))))))

  (test-suffix "/foo/" "/foo/bar" "/bar")
  (test-suffix "/foo" "/foo" "")
  (test-suffix "/foo/bar/bing/baz/" "/foo/bar/bing/baz/boing/" "/boing/")
  (test-suffix "/" "foo" "foo")
  (test-suffix "" "foo" "foo")
  (test-suffix "/" "/foo/" "/foo/")
  (test-suffix "/" "/foo/bar" "/foo/bar")


  (test "more tests for suffix"
        (lambda ()
          (and
           (not (suffix/strings "/foo" "/bar"))
           (not (suffix/strings "/foo/bar" "/foo/baz"))
           (not (suffix/strings "foo" "bar"))
           (not (suffix/strings "/foo/" "/bar/")))))

  (test "some test for when a resource doesn't resolver"
        (lambda ()
          (and
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
                                      (dpair "/bar/baz" "ball")))))))
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

  (define test/lp
    (lambda (dir-part file-part vp-url)
      (test
       (format "test/lp url = ~a" (url->string vp-url))
       (lambda ()
         (logical-path=? (make-logical-path dir-part file-part)
                         (url->logical-path vp-url))))))


  (define url1 (string->url "http://www.home.org"))
  (define url2 (string->url "http://www.home.org/"))
  (define url3 (string->url "http://www.home.org;param"))
  (define url4 (string->url "http://www.home.org/;param"))
  (define url5 (string->url "http://www.home.org/a"))
  (define url6 (string->url "http://www.home.org/a;param"))

  (test/lp '() ""  url1)
  (test/lp '() ""  url2)
  (test/lp '() ""  url3)
  (test/lp '() ""  url4)
  (test/lp '() "a"  url5)
  (test/lp '() "a"  url6)


  (define url7 (string->url ""))
  (define url8 (string->url "/"))
  (define url9 (string->url ";param"))
  (define url10 (string->url "/;param"))
  (define url11 (string->url "/a"))
  (define url12 (string->url "/a;param"))

  (test/lp '() ""  url7)
  (test/lp '() ""  url8)
  (test/lp '() ""  url9)
  (test/lp '() ""  url10)
  (test/lp '() "a"  url11)
  (test/lp '() "a"  url12)


  (define url13 (string->url "http://www.home.org/a/"))
  (define url14 (string->url "http://www.home.org/a/;param"))
  (define url15 (string->url "http://www.home.org/a/b;param"))
  (define url16 (string->url "a/"))
  (define url17 (string->url "a/;param"))
  (define url18 (string->url "a/b;param"))

  (test/lp '("a") ""  url13)
  (test/lp '("a") ""  url14)
  (test/lp '("a") "b"  url15)
  (test/lp '("a") ""  url16)
  (test/lp '("a") ""  url17)
  (test/lp '("a") "b"  url18)


  (define url18.5 (string->url ""))
  (define url19 (string->url ";"))
  (define url20 (string->url "/a/"))
  (define url21 (string->url "/a/;"))
  (define url22 (string->url "/a/;param"))

  (test/lp '() ""  url18.5)
  (test/lp '() ""  url19)
  (test/lp '("a") ""  url20)
  (test/lp '("a") ""  url21)
  (test/lp '("a") ""  url22)


  (define url23 (string->url "http://www.home.net/.."))
  (define url24 (string->url "http://www.home.net/a/.."))
  (define url25 (string->url "http://www.home.net/a/b/.."))
  (define url26 (string->url ".."))
  (define url27 (string->url "/.."))
  (define url28 (string->url "a/.."))
  (define url29 (string->url "/a/.."))
  (define url30 (string->url "/a/b/.."))

  (test/lp '("..") ""  url23)
  (test/lp '() ""  url24)
  (test/lp '("a") ""  url25)
  (test/lp '("..") ""  url26)
  (test/lp '("..") ""  url27)
  (test/lp '() ""  url28)
  (test/lp '() ""  url29)
  (test/lp '("a") ""  url30)


  (define url31 (string->url "http://www.home.net/1/2/3/4/.."))
  (define url32 (string->url "http://www.home.org/1/2/3/4/../../"))
  (define url33 (string->url "http://www.home.org/1/2/3/4/../../.."))
  (define url34 (string->url "http://www.home.org/1/2/3/4/../../../../"))
  (define url35 (string->url "http://www.home.org/1/2/3/4/../../../../.."))
  (define url36 (string->url "http://www.home.org/1/2/3/4/../../../../../../"))

  (test/lp '("1" "2" "3") ""  url31)
  (test/lp '("1" "2") ""  url32)
  (test/lp '("1") ""  url33)
  (test/lp '() ""  url34)
  (test/lp '("..") ""  url35)
  (test/lp '(".." "..") ""  url36)


  (define url37 (string->url "1/."))
  (define url38 (string->url "2/./."))
  (define url39 (string->url "3/././"))
  (define url40 (string->url "http://www.home.org/1/./.."))
  (define url41 (string->url "http://www.home.org/1/../."))
  (define url42 (string->url "http://www.home.org/1/./../."))

  (test/lp '("1") ""  url37)
  (test/lp '("2") ""  url38)
  (test/lp '("3") ""  url39)
  (test/lp '() ""  url40)
  (test/lp '() ""  url41)
  (test/lp '() ""  url42)


  (define url43 (string->url "http://www.home.org/1/2/3/../4/../../5"))
  (define url44 (string->url "http://www.home.org/1/2/3/../4/../../5/../../6"))
  (define url45 (string->url "http://www.home.org/1/2/3/../4/../../5/../../6/../.."))
  (define url46 (string->url "http://www.hme.org/1/2/3/../4/../../5/../../6/../../7/../"))
  (define url47 (string->url "http://www.h.org/1/2/3/../4/../../5/../../6/../../7/../.."))
  (define url48 (string->url "http://www.home.com/1/2/3/../../../a/b/c/d"))
  (define url49 (string->url "http://www.home.com/1/2/3/../../../../a/b/c/d"))
  (define url50 (string->url "http://www.home.net/1/2/3/../../../../a/b/./c/"))
  (define url51 (string->url "http://www.home.gov;paramfirst/then/a/path"))

  (test/lp '("1") "5"  url43)
  (test/lp '() "6"  url44)
  (test/lp '("..") ""  url45)
  (test/lp '("..") ""  url46)
  (test/lp '(".." "..") ""  url47)
  (test/lp '("a" "b" "c") "d"  url48)
  (test/lp '(".." "a" "b" "c") "d"  url49)
  (test/lp '(".." "a" "b" "c") ""  url50)
  (test/lp '("then" "a") "path"  url51)

  );module
