(module highlight-placeholder mzscheme
  
  (provide highlight-placeholder)
  
  ; highlight-placeholder : syntax-object
  
  ; we rely upon the fact that the highlight-placeholder is a syntax-object, so that
  ; syntax objects containing the highlight-placeholder still fit the data definition
  ; for syntax objects 
  
  (define highlight-placeholder (gensym "highlight-placeholder")))
