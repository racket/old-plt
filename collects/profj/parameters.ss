(module parameters mzscheme
  (provide (all-defined))
  
  ;Stores type info for known packages
  (define type-table (make-parameter null))

  ;Stores syntax-oddness for datum->syntax-object
  (define syntax-location (make-parameter #f))
  
  ;Stores asts of other packages
  (define packages (make-parameter null))
  
  ;Stores asts of other classes
  (define check-list (make-parameter null))
  
  ;Stores a symbol representing the class from which main should be called (#f for none)
  (define main (make-parameter #f))
  
  ;Stores a boolean indicating if compilation is directed at a file
  (define to-file (make-parameter #f))
  
  ) 
