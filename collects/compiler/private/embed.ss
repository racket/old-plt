
(module embed mzscheme
  (require (lib "embed.ss" "compiler"))
  (define mzc:make-embedding-executable make-embedding-executable)
  (provide mzc:make-embedding-executable))
