(module test-util mzscheme
  (require (lib "url.ss" "net")
           "test-harness.ss"
           "util.ss")

  (define (v-path=? p1 p2)
    (and (equal? (virtual-path-directory-part p1)
                 (virtual-path-directory-part p2))
         (equal? (virtual-path-file-part p1)
                 (virtual-path-file-part p2))))

  (define test/vp
    (lambda (dir-part file-part vp-url)
      (test
       (format "test/vp url = ~a" (url->string vp-url))
       (lambda ()
         (v-path=? (make-virtual-path dir-part file-part)
                   (url->virtual-path vp-url))))))


  (define url1 (string->url "http://www.home.org"))
  (define url2 (string->url "http://www.home.org/"))
  (define url3 (string->url "http://www.home.org;param"))
  (define url4 (string->url "http://www.home.org/;param"))
  (define url5 (string->url "http://www.home.org/a"))
  (define url6 (string->url "http://www.home.org/a;param"))

  (test/vp '() ""  url1)
  (test/vp '() ""  url2)
  (test/vp '() ""  url3)
  (test/vp '() ""  url4)
  (test/vp '() "a"  url5)
  (test/vp '() "a"  url6)


  (define url7 (string->url ""))
  (define url8 (string->url "/"))
  (define url9 (string->url ";param"))
  (define url10 (string->url "/;param"))
  (define url11 (string->url "/a"))
  (define url12 (string->url "/a;param"))

  (test/vp '() ""  url7)
  (test/vp '() ""  url8)
  (test/vp '() ""  url9)
  (test/vp '() ""  url10)
  (test/vp '() "a"  url11)
  (test/vp '() "a"  url12)


  (define url13 (string->url "http://www.home.org/a/"))
  (define url14 (string->url "http://www.home.org/a/;param"))
  (define url15 (string->url "http://www.home.org/a/b;param"))
  (define url16 (string->url "a/"))
  (define url17 (string->url "a/;param"))
  (define url18 (string->url "a/b;param"))

  (test/vp '("a") ""  url13)
  (test/vp '("a") ""  url14)
  (test/vp '("a") "b"  url15)
  (test/vp '("a") ""  url16)
  (test/vp '("a") ""  url17)
  (test/vp '("a") "b"  url18)


  (define url18.5 (string->url ""))
  (define url19 (string->url ";"))
  (define url20 (string->url "/a/"))
  (define url21 (string->url "/a/;"))
  (define url22 (string->url "/a/;param"))

  (test/vp '() ""  url18.5)
  (test/vp '() ""  url19)
  (test/vp '("a") ""  url20)
  (test/vp '("a") ""  url21)
  (test/vp '("a") ""  url22)


  (define url23 (string->url "http://www.home.net/.."))
  (define url24 (string->url "http://www.home.net/a/.."))
  (define url25 (string->url "http://www.home.net/a/b/.."))
  (define url26 (string->url ".."))
  (define url27 (string->url "/.."))
  (define url28 (string->url "a/.."))
  (define url29 (string->url "/a/.."))
  (define url30 (string->url "/a/b/.."))

  (test/vp '("..") ""  url23)
  (test/vp '() ""  url24)
  (test/vp '("a") ""  url25)
  (test/vp '("..") ""  url26)
  (test/vp '("..") ""  url27)
  (test/vp '() ""  url28)
  (test/vp '() ""  url29)
  (test/vp '("a") ""  url30)


  (define url31 (string->url "http://www.home.net/1/2/3/4/.."))
  (define url32 (string->url "http://www.home.org/1/2/3/4/../../"))
  (define url33 (string->url "http://www.home.org/1/2/3/4/../../.."))
  (define url34 (string->url "http://www.home.org/1/2/3/4/../../../../"))
  (define url35 (string->url "http://www.home.org/1/2/3/4/../../../../.."))
  (define url36 (string->url "http://www.home.org/1/2/3/4/../../../../../../"))

  (test/vp '("1" "2" "3") ""  url31)
  (test/vp '("1" "2") ""  url32)
  (test/vp '("1") ""  url33)
  (test/vp '() ""  url34)
  (test/vp '("..") ""  url35)
  (test/vp '(".." "..") ""  url36)


  (define url37 (string->url "1/."))
  (define url38 (string->url "2/./."))
  (define url39 (string->url "3/././"))
  (define url40 (string->url "http://www.home.org/1/./.."))
  (define url41 (string->url "http://www.home.org/1/../."))
  (define url42 (string->url "http://www.home.org/1/./../."))

  (test/vp '("1") ""  url37)
  (test/vp '("2") ""  url38)
  (test/vp '("3") ""  url39)
  (test/vp '() ""  url40)
  (test/vp '() ""  url41)
  (test/vp '() ""  url42)


  (define url43 (string->url "http://www.home.org/1/2/3/../4/../../5"))
  (define url44 (string->url "http://www.home.org/1/2/3/../4/../../5/../../6"))
  (define url45 (string->url "http://www.home.org/1/2/3/../4/../../5/../../6/../.."))
  (define url46 (string->url "http://www.hme.org/1/2/3/../4/../../5/../../6/../../7/../"))
  (define url47 (string->url "http://www.h.org/1/2/3/../4/../../5/../../6/../../7/../.."))
  (define url48 (string->url "http://www.home.com/1/2/3/../../../a/b/c/d"))
  (define url49 (string->url "http://www.home.com/1/2/3/../../../../a/b/c/d"))
  (define url50 (string->url "http://www.home.net/1/2/3/../../../../a/b/./c/"))
  (define url51 (string->url "http://www.home.gov;paramfirst/then/a/path"))

  (test/vp '("1") "5"  url43)
  (test/vp '() "6"  url44)
  (test/vp '("..") ""  url45)
  (test/vp '("..") ""  url46)
  (test/vp '(".." "..") ""  url47)
  (test/vp '("a" "b" "c") "d"  url48)
  (test/vp '(".." "a" "b" "c") "d"  url49)
  (test/vp '(".." "a" "b" "c") ""  url50)
  (test/vp '("then" "a") "path"  url51)
  )
