(compound-unit/sig
  (import [import : plt:basis-import^]
	  [params : plt:userspace:params^]
	  [zodiac : zodiac:system^]
	  [zodiac:interface : drscheme:interface^]
	  [aries : plt:aries^]
	  [mzlib:print-convert : mzlib:print-convert^]
	  [mzlib : mzlib:core^])
  (link
   [prims : plt:prims^ ((require-relative-library "prims.ss"))]
   [init-params : plt:init-params^ ((require-relative-library "init-paramr.ss")
				    import
				    init-namespace
				    zodiac
				    zodiac:interface
				    aries
				    mzlib:print-convert
				    (mzlib pretty-print)
				    (mzlib function)
				    (mzlib thread))]
   [init-namespace : plt:init-namespace^ ((require-relative-library "init-namespacer.ss")
					  import
					  init-params
					  prims
                                          params
                                          zodiac
                                          zodiac:interface
					  (mzlib function))])
  (export
   (open init-params)
   (open init-namespace)))

