
(reference-library "sig.ss" "guibuilder")
(reference-library "functio.ss")
(reference-library "pretty.ss")
(reference-library "file.ss")

(invoke-open-unit/sig (require-library "gbr.ss" "guibuilder") 
		      mred 
		      mzlib:function^
		      mzlib:pretty-print^
		      mzlib:file^
		      (mred : mred^)
		      (wx : wx^))
