(unit/sig help:get-info^
  (import framework^
          plt:basis^
          [drscheme:language : drscheme:language^]
          [drscheme:teachpack : drscheme:teachpack^])
  
  (define (get-teachpack-names)
    (drscheme:teachpack:teachpack-cache-filenames (preferences:get 'drscheme:teachpacks)))

  (define (get-language-level)
    (setting/unparse (preferences:get drscheme:language:settings-preferences-symbol))))
