(require-library "errortrace.ss" "errortrace")

(require-library "sig.ss" "games" "loa")

(invoke-unit/sig
 (compound-unit/sig
  (import (mred : mred^))
  (link
   [core : mzlib:core^ ((require-library "corer.ss"))]
   [utils : loa:utils^ ((require-library "utils.ss" "games" "loa"))]
   [grid : loa:grid^ ((require-library "grid.ss" "games" "loa") (core function) mred utils)]
   [computer : loa:computer-player^ ((require-library "computer.ss" "games" "loa") loa (core function))]
   [loa : loa^ ((require-library "loa.ss" "games" "loa") (core function) mred computer grid)])
  (export))
 mred^)

(yield (make-semaphore))