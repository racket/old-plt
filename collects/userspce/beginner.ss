;; (defined? 'mred^) is true in DrScheme, but false in DrScheme Jr

(require-library "cores.ss")
(compound-unit/sig (import)
  (link
   [core : mzlib:core^ ((require-library "corer.ss" "mzlib"))])
  (export
   (open (core function))
   (open (core pretty-print))
   (open (core file))
   (open (core string))
   (open (core compile))
   (open (core thread))))
