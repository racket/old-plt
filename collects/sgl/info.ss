(module info (lib "infotab.ss" "setup")
  (define name "sgl")
  (define pre-install-collection "makefile.ss")
  (define compile-omit-files (list))
  (define clean (list (build-path "gl-info.c") 
                      (build-path "compiled" "native" (system-library-subpath))
		      (build-path "compiled" "native" (system-library-subpath) "3m")
                      "compiled"
                      
                      )))
