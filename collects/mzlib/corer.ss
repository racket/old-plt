
(compound-unit/sig
   (import)
   (link [pretty-print@ : mzlib:pretty-print^ ((require-library-unit/sig "prettyr.ss"))]
	 [file@ : mzlib:file^ ((require-library-unit/sig "filer.ss") string@ function@)]
	 [function@ : mzlib:function^ ((require-library-unit/sig "functior.ss"))]
	 [compat@ : mzlib:compat^ ((require-library-unit/sig "compatr.ss") function@)]
	 [string@ : mzlib:string^ ((require-library-unit/sig "stringr.ss"))]
	 [compile@ : mzlib:compile^ ((require-library-unit/sig "compiler.ss"))]
	 [thread@ : mzlib:thread^ ((require-library-unit/sig "threadr.ss"))])
   (export (unit pretty-print@)
	   (unit file@)
	   (unit function@)
	   (unit compat@)
	   (unit string@)
	   (unit compile@)
	   (unit thread@)))
