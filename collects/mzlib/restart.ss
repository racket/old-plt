
(require-library "cmdline.ss")
(require-library "restartu.ss")

(invoke-open-unit/sig mzlib:restart@ #f
		      mzlib:command-line^)
