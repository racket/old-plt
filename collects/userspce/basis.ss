(module basis mzscheme
  (provide basis@)
  
  (require "prims.ss"
           "init-param.ss"
           "init-namespace.ss")
  
  (define basis@
    (compound-unit/sig
      (import [import : plt:basis-import^]
              [params : plt:userspace:params^]
              [zodiac : zodiac:system^]
              [zodiac:interface : drscheme:interface^]
              [aries : plt:aries^]
              [mzlib:print-convert : mzlib:print-convert^]
              [mzlib : mzlib:core^])
      (link
       [prims : plt:prims^ (prims@)]
       [init-params : plt:init-params^ (init-param@
                                        import
                                        init-namespace
                                        zodiac
                                        zodiac:interface
                                        aries
                                        mzlib:print-convert
                                        (mzlib pretty-print)
                                        (mzlib function)
                                        (mzlib thread))]
       [init-namespace : plt:init-namespace^ (init-namespace@
                                              import
                                              init-params
                                              prims
                                              params
                                              zodiac
                                              zodiac:interface
                                              (mzlib function))])
      (export
       (open init-params)
       (open init-namespace)))))

