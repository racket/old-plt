;; info.ss for mzcom collection

(module info (lib "infotab.ss" "setup")
  (define name "MzCOM")
  (define blurb
    (list
      "MzCOM is a COM class that makes Scheme available to any COM client."))
  (define release-version "206p1")
  (define release-iteration "0")
  (define install-collection "installer.ss"))
