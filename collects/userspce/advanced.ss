(require-library "cores.ss")
(require-library "turtles.ss")

(compound-unit/sig
  (import)
  (link [core : mzlib:coreflat^ ((require-library "coreflatr.ss"))]
	[turtles : turtles^ ((require-library "turtler.ss") (core : mzlib:function^))])
  (export
   (open core)
   (open turtles)))
