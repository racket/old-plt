;; Avoids annoying prims in moreprims.ss
(module cmzscheme mzscheme
  (provide (all-from-except mzscheme
                            sin
                            cos
                            sqrt
                            abs
                            exp)))
