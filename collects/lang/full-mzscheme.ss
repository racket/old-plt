(module full-mzscheme mzscheme
  (define argv #())
  (define program "mzscheme")
  (provide argv 
           program
           (all-from mzscheme)))
