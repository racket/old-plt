
(compound-unit/sig
   (import)
   (link [pretty-print@ : mzlib:pretty-print^ ((reference-relative-library-unit/sig "prettyr.ss"))]
	 [file@ : mzlib:file^ ((reference-relative-library-unit/sig "filer.ss") string@ function@)]
	 [function@ : mzlib:function^ ((reference-relative-library-unit/sig "functior.ss"))]
	 [compat@ : mzlib:compat^ ((reference-relative-library-unit/sig "compatr.ss") function@)]
	 [string@ : mzlib:string^ ((reference-relative-library-unit/sig "stringr.ss"))]
	 [compile@ : mzlib:compile^ ((reference-relative-library-unit/sig "compiler.ss"))]
	 [thread@ : mzlib:thread^ ((reference-relative-library-unit/sig "threadr.ss"))])
   (export (unit pretty-print@)
	   (unit file@)
	   (unit function@)
	   (unit compat@)
	   (unit string@)
	   (unit compile@)
	   (unit thread@)))
