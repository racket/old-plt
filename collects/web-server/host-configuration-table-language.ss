(module host-configuration-table-language mzscheme
  (provide ;(all-from mzscheme)
   define provide build-path quote list cons null
   #%app #%datum #%module-begin #%top
   
   web-server-collection build-path-maybe
   )
  
  (define web-server-collection (collection-path "web-server"))
  
  ; build-path-maybe : str str -> str
  (define (build-path-maybe base path)
    (if (absolute-path? path)
        path
        (build-path base path))))
