(module highlight-placeholder mzscheme
  
  (provide highlight-placeholder)
  
  ; highlight-placeholder : uninterned symbol
  (define highlight-placeholder (gensym "highlight-placeholder")))
