
(module pre-installer mzscheme
  (require (lib "setup-extension.ss" "make")
	   (lib "compile.ss" "dynext"))

  (define (pre-installer plthome)
    (define mach-id (string->symbol (system-library-subpath)))

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
		 ;; Last-chance k:
		 (lambda (k) (k))))

  (provide pre-installer))
