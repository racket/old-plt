(unit/sig help:get-info^
  (import framework^
          plt:basis^
          [drscheme:language : drscheme:language^])
  
  (define (get-teachpack-names)
    (preferences:get 'drscheme:teachpack-file))

  (define (get-language-level)
    (setting/unparse (preferences:get drscheme:language:settings-preferences-symbol))))
