(module a-thread mzscheme

  (provide the-thread)

  (define the-thread (thread (lambda () (let loop () (loop)))))
  
  )
