(module info (lib "infotab.ss" "setup")
  (define name "SSL Driver")
  (define compile-omit-files '("mzssl.ss"))
  (define pre-install-collection "pre-installer.ss")
  (define blurb '("The SSL collection provides a driver for using the OpenSSL "
		  "secure connection library."))
  (define release-version "209")
  (define release-iteration "0"))
