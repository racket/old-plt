
(module embed mzscheme
  (require (lib "unitsig.ss")
           (lib "contract.ss"))
  
  (require "sig.ss")
  
  (require "embed-unit.ss"
	   "embed-sig.ss")
  
  (define-values/invoke-unit/sig compiler:embed^
                                 compiler:embed@
                                 #f)
  
  (provide/contract [make-embedding-executable
                     (opt-> (path-string?
                             any?
                             any?
                             (listof (list/c (union boolean? symbol?) any?))
                             (listof path-string?)
                             any?
                             (listof string?))
                            ((listof (cons/c symbol? any?))
                             any?
                             symbol?)
                            void?)])
  (provide write-module-bundle
           embedding-executable-is-directory?
           embedding-executable-put-file-extension+style+filters))

