(require-relative-library "sigs.ss")

(define srpersist@
  (compound-unit/sig 
   (import)
   (link [srpersist : srpersist:srpersist^ 
		    ((unit->unit/sig 
		      (load-relative-extension "compiled/native/srpersist.dll")
		      ()
		      srpersist:srpersist^))])
   (export
    (open srpersist))))


