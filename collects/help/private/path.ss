(module path mzscheme
  (require (lib "contract.ss"))
  (provide/contract
   [servlet-path? (path? . -> . boolean?)])

  (define (servlet-path? path)
    (regexp-match #rx#"^/servlets/" 
                  (path->bytes path))))

