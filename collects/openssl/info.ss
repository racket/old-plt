(module info (lib "infotab.ss" "setup")
  (define name "SSL Driver")
  (define compile-omit-files '("mzssl.ss" "mzmake.ss"))
  (define install-collection "installer.ss")
  (define blub '("The SSL collection provides a driver for using the OpenSSL "
		 "secure connection library.")))