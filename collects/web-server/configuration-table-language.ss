(module configuration-table-language mzscheme
  (require "util.ss"
           "configuration-table-structs.ss")
  (provide
   ; from mzscheme
   define provide build-path quote list cons null quasiquote unquote
   #%app #%datum #%module-begin #%top let*
   
   (all-from "configuration-table-structs.ss")
   
   web-server-collection build-path-maybe)
  
  (define web-server-collection (collection-path "web-server"))
  
  ; build-path-maybe : str str -> str
  (define (build-path-maybe base path)
    (if (absolute-path? path)
        path
        (build-path base path))))
