(require-library "errortrace.ss" "errortrace")

(require-library "sig.ss" "games" "loa")

(invoke-unit/sig
 (compound-unit/sig
  (import (mred : mred^))
  (link
   [core : mzlib:core^ ((require-library "corer.ss"))]
   [utils : loa:utils^ ((require-library "utils.ss" "games" "loa"))]
   [grid : loa:grid^ ((require-library "grid.ss" "games" "loa") (core function) mred utils)]
   [loa : loa^ ((require-library "loa.ss" "games" "loa") (core function) mred grid)]
   [main : () ((require-library "main.ss" "games" "loa") mred loa utils)])
  (export))
 mred^)

(yield (make-semaphore))