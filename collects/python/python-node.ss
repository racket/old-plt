(module python-node mzscheme
  (provide (all-defined))
  
    ;; python-node is a:
  ;; (make-python-node python-node (hash-table-of dict-member) bool)
  
  ;; a dict-member is one of:
  ;;  symbol python-node
  ;;  gensym scheme-value
  
  
  (define-struct python-node (type dict mutable?) (make-inspector))
  
  
  ;; these are hidden keys in built-in data types to hold their actual scheme values
  (define scheme-number-key (gensym 'number-key))
  (define scheme-string-key (gensym 'string-key))
  (define scheme-list-key (gensym 'list-key))
  (define scheme-procedure-key (gensym 'procedure-key))
  (define scheme-hash-table-key (gensym 'hash-table-key))

  
  (define (python-set-member! obj name value)
    (hash-table-put! (python-node-dict obj) name value))

  (define (python-new-object type)
    (make-python-node type (make-hash-table) #t))
  
  
  )