; invoking this unit (with the two needed dynext units) causes a build
; of the starter-setup.so extension.  This extension is needed by 
; "install-starter-aliases.ss" to install aliases to mred and mzscheme
; in the appropriate launchers (gomz and gomr).

(unit/sig ()
  (import [c : dynext:compile^]
          [l : dynext:link^])
  
  (define (build-needed-extension)
    (let* ([launcher-path (collection-path "launcher")]
           [src-path (build-path launcher-path "startup-setup.c")]
           [obj-path (build-path launcher-path "startup-setup.o")]
           [shlib-path (build-path launcher-path "startup-setup.so")])
      (c:compile-extension (not (compiler:option:verbose))
                           src-path
                           obj-path
                           null)
      (l:link-extension (not (compiler:option:verbose))
                        (list obj-path)
                        shlib-path)))
  
  (build-needed-extension))

