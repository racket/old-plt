
(require-library "sig.ss" "guibuilder")
(require-library "functio.ss")
(require-library "pretty.ss")
(require-library "file.ss")

(invoke-open-unit/sig (require-library "gbr.ss" "guibuilder") 
		      mred 
		      mzlib:function^
		      mzlib:pretty-print^
		      mzlib:file^
		      (mred : mred^)
		      (wx : wx^))
