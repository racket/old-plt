(require-library "cores.ss")
(require-library "turtles.ss")

(compound-unit/sig
  (import)
  (link [core : mzlib:core-flat^ ((require-library "coreflatr.ss"))]
	[turtles : turtles^ ((require-library "turtler.ss") (core : mzlib:function^))])
  (export
   (open core)
   (open turtles)))
