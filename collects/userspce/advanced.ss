(require-library "coreflats.ss")
(when (defined? 'mred^)
  (require-library "turtles.ss" "graphics"))

(if (defined? 'mred^)
    (compound-unit/sig
      (import)
      (link [core : mzlib:core-flat^ ((require-library "coreflatr.ss"))]
	    [turtles : turtle^ ((require-library "turtler.ss" "graphics") (core : mzlib:function^))])
      (export
       (open core)
       (open turtles)))
    (require-library "coreflatr.ss"))

