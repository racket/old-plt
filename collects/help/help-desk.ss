(module help-desk mzscheme
  (require 
   
   "private/manuals.ss"
   "private/buginfo.ss"
   "private/sig.ss"
   "private/standard-urls.ss"
   
   (lib "mred-sig.ss" "mred")
   (lib "mred.ss" "mred")
   (lib "tcp-sig.ss" "net")
   (lib "plt-installer-sig.ss" "setup")
   (lib "plt-installer.ss" "setup")   
   (lib "unitsig.ss")
   
   "private/link.ss"
   "bug-report.ss" ;; this is require'd here to get the prefs defaults setup done early.
   (lib "contract.ss")
   (lib "mred.ss" "mred"))
  
  (provide help-desk-frame<%>)
  
  (define (goto-hd-location x) (error 'goto-hd-location "no"))
  
  (define-values/invoke-unit/sig gui^ help-desk@ #f setup:plt-installer^ mred^ net:tcp^)
  
  (provide/contract 
   (set-bug-report-info! any?)
   (find-doc-names (-> (listof (cons/c path? string?))))
   (goto-manual-link (string? string? . -> . any))
   
   (goto-hd-location (symbol? . -> . any))
   (new-help-desk (-> (is-a?/c help-desk-frame<%>)))
   (show-help-desk (-> any))
   (add-help-desk-mixin (-> mixin-contract void?))
   (search-for-docs (string?
                     search-type?
                     search-how?
                     any?
                     (listof path?) ;; manual names
                     . -> .
                     any))
   (find-help-desk-frame (-> (union false? (is-a?/c help-desk-frame<%>))))
   (search-for-docs/in-frame ((is-a?/c help-desk-frame<%>)
                              string?
                              search-type?
                              search-how?
                              any?
                              (listof path?) ;; manual names
                              . -> .
                              any))))
