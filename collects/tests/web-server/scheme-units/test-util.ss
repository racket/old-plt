;; Tests for util.ss
(module test-util mzscheme
  (require (lib "test.ss" "schemeunit")
           (lib "util.ss" "web-server")
           )

  (provide test-util)

  (define test-util
    (make-test-suite
      "Test the Web server's utilities"

      ;; url-path->path
      (make-test-case
