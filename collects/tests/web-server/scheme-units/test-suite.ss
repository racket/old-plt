;; Run with:
;; mzscheme -mvf test-suite.ss
;; From collects/tests/web-server/scheme-units
(require (lib "text-ui.ss" "schemeunit")
         (lib "graphical-ui.ss" "schemeunit")
         "test-authentication.ss"
         "test-channel.ss"
         "test-serve-static-html.ss"
         "test-serve-static-jpeg.ss"
         "test-web-server.ss"
         "test-servlets.ss"
         "test-errors.ss")

(make-graphical-ui (list test-channel
 test-web-server
 test-serve-static-html
 test-serve-static-jpeg
 test-authentication
 test-errors
 test-servlets))
