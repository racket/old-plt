(compound-unit/sig
  (import [M : mzlib:function^])
  (link [I : turtle:create-window^
	   ((unit/sig turtle:create-window^
	      (import)
	      (define (create-turtle-window % title width height)
		(make-object % title width height))))]
	[T : turtle^ ((reference-library-unit/sig "turtlmr.ss" "graphics")
		      M I)])
  (export (open T)))
   
