(module parameters mzscheme
  (provide (all-defined))
  
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
  
  ;Stores an integer offset for interactions offset
  (define interactions-offset (make-parameter 0))
  
  ;Stores if we are the execution window executing
  (define execution? (make-parameter #f))
  
  ;Stores the error function to trigger for parsing
  (define determine-error (make-parameter (lambda () #t)))
  
  ;Stores a function to get the input port in a non-destructive maner
  (define input-port (make-parameter (lambda () void)))
  
  ;Stores a function ('a -> bool) that determines if the given object is an interactions box
  (define interactions-box-test (make-parameter (lambda (obj) #f)))
  
  ) 
