;; Tests for util.ss
(module test-util mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1))
           (lib "util.ss" "web-server")
           )

  (provide test-util)

  (define test-util
    (make-test-suite
      "Test the Web server's utilities"

      ;; url-path->path
      (make-test-case
