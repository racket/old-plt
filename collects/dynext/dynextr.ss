
(compound-unit/sig
 (import)
 (link [compile@ : dynext:compile^ ((require-relative-library-unit/sig "compiler.ss"))]
       [link@ : dynext:link^ ((require-relative-library-unit/sig "linkr.ss"))]
       [file@ : dynext:file^ ((require-relative-library-unit/sig "filer.ss"))])
 (export (unit compile@)
	 (unit link@)
	 (unit file@)))
