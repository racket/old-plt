
(module pre-installer mzscheme
  (require (lib "setup-extension.ss" "make")
	   (lib "compile.ss" "dynext"))

  (define (pre-installer plthome)
    (pre-install plthome
		 (collection-path "openssl")
		 "mzssl.c" 
		 (build-path (collection-path "openssl") 
			     "openssl")
		 ;; header subdirs
		 (list "openssl")
		 ;; unix libs
		 (list "ssl" "crypto")
		 ;; windows libs
		 (list "libeay32" "ssleay32")
		 ;; unix extra libs (assume always there)
		 null
		 ;; Windows extra libs (assume always there)
		 (list "wsock32.lib")
		 ;; Extra depends:
		 (list "mzssl.ss")
		 ;; Last-chance k:
		 (lambda (k) (k))))

  (provide pre-installer))
