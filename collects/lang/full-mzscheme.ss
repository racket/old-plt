(module full-mzscheme mzscheme
  (define argv #())
  (define program (namespace-variable-binding 'program))
  (provide argv 
           program
           (all-from mzscheme)))
