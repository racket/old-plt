(module test-resolver mzscheme
  (require (lib "url.ss" "net")
           "the-real-test-harness.ss"
           "resolver.ss")

  (require/expose "resolver.ss" (logical-path->list suffix r-map-is-function?
                                                    r-map-paths-same-type? r-map-paths-valid? longer?))

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
  ;; call suffix after converting strings to logical-paths
  (define (suffix/strings pref of-this)
    (suffix (string->logical-path pref)
            (string->logical-path of-this)))

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
            (let ([sffx (suffix/strings pref of-this)])
              (logical-path=? sffx
                              (string->logical-path expected))))))

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
   (string->logical-path "foo")
   '() "foo")

  (check-logical-path?
   (string->logical-path "/foo/")
   '("foo") "")

  (check-logical-path?
   (string->logical-path "/")
   '() "")

  (check-logical-path?
   (string->logical-path "///")
   '() "")

  (check-logical-path?
   (string->logical-path "/foo/bar/baz")
   '("foo" "bar") "baz")

  (check-logical-path?
   (string->logical-path "/foo//bar///baz")
   '("foo" "bar") "baz")

  (check-logical-path?
   (string->logical-path "/foo/bar/baz/")
   '("foo" "bar" "baz") "")



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
  (test-suffix "/foo/bar/bing" "/foo/bar/bing/bang" "/bang")

  (test "more tests for suffix"
        (lambda ()
          (and
           (not (suffix/strings "/foo" "/bar"))
           (not (suffix/strings "/foo/bar" "/foo/baz"))
           (not (suffix/strings "foo" "bar"))
           (not (suffix/strings "/foo/" "/bar/")))))

  ;; test-apply-resource-map/not-in-map: string resource-map -> void
  ;; test how longext-prefix behaves when the resource is not in the resource
  ;; map
  (define (test-apply-resource-map/not-in-map str r-map)
    (test (format "test-apply-resource-map/not-in-map: str = ~s r-map = ~s" str r-map)
          (lambda ()
            (let-values ([(r-pair suff)
                          (apply-resource-map r-map (string->logical-path str))])
              (printf "r-pair = ~s   suff = ~s~n" r-pair suff)
              (and (not r-pair)
                   (not suff))))))

  (test-apply-resource-map/not-in-map "foo" '())
  (test-apply-resource-map/not-in-map "/" '())
  (test-apply-resource-map/not-in-map "/foo/bar" '())
  (test-apply-resource-map/not-in-map "/foo/bar/" '())
  (test-apply-resource-map/not-in-map "/foo/bar/baz"
                                      (list (dpair "/foo/bing" "ting")
                                            (spair "/foo/baz" "dong")
                                            (dpair "/bar/baz" "ball")))

  ;; test-apply-resource-map: string resource-map resource-pair string -> void
  ;; test apply-resource-map when the resource i
  (define (test-apply-resource-map str r-map expected-pair expected-suffix)
    (test (format (string-append "finding the longest prefix str = ~s   r-map = ~s"
                                 "expected-pair = ~s expected-suffix = ~s")
                  str r-map expected-pair expected-suffix)
          (lambda ()
            (let-values ([(r-pair suff)
                          (apply-resource-map r-map  (string->logical-path str))])
              (and (same-r-pair? r-pair expected-pair)
                   (logical-path=? suff (string->logical-path expected-suffix)))))))


  (test-apply-resource-map "/foo" (list (spair "/foo" "bar")) (spair "/foo" "bar")
                     "/")

  (test-apply-resource-map "/" (list (spair "/" "bar")) (spair "/" "bar")
                     "/")

  (test-apply-resource-map "/foo/" (list (dpair "/foo/" "bar")) (dpair "/foo/" "bar")
                     "/")

  (test-apply-resource-map "/foo/bar" (list (dpair "/foo/bar" "bat"))
                     (dpair "/foo/bar" "bat") "/")

  (test-apply-resource-map "/foo/bar/bing/bang/bong/"
                     (list (dpair "bar" "none")
                           (dpair "/foo/bar/" "/big/ben/"))
                     (dpair "/foo/bar/" "/big/ben/")
                     "/bing/bang/bong/")

  (test-apply-resource-map "/foo/bar/bing/bang/bong"
                           (list (dpair "/foo/bar/" "/big/ben"))
                           (dpair "/foo/bar/" "/big/ben")
                           "/bing/bang/bong")

  (test-apply-resource-map "/foo/bar/bing/bang/bong"
                           (list (spair "/foo/bar/" "/big/ben/")
                                 (spair "/foo/bar/bing/" "/big/ben/")
                                 (spair "/foo/bar/bing/bang/" "/big/ben/"))
                           (spair "/foo/bar/bing/bang/" "/big/ben/")
                           "/bong")

  (test-apply-resource-map "/foo/bar/bing/bang"
                           (list (dpair "/foo/bar" "/big/ben")
                                 (dpair "/foo/bar/bing" "/big/ben/bank"))
                           (dpair "/foo/bar/bing" "/big/ben/bank")
                           "/bang")

  (test-apply-resource-map "/foo/bar/bing/bang"
                           (list (spair "/foo/bar" "/big/ben")
                                 (spair "/foo/bar/bing" "/big/ben/bank"))
                           (spair "/foo/bar/bing" "/big/ben/bank")
                           "/bang")

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
