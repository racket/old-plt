;; info.ss for mzcom collection

(module info (lib "infotab.ss" "setup")
  (define name "MzCOM")
  (define blurb
    (list
      "MzCOM is a COM class that makes Scheme available to any COM client."))
  (define release-version "200")
  (define release-iteration "1")
  (define install-collection "installer.ss"))

