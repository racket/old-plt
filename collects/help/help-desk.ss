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
  
  (provide/contract 
   (set-bug-report-info! any?)
   (find-doc-names (-> (listof (cons/p path? string?))))
   (goto-manual-link (string? string? . -> . any))
   
   (goto-hd-location (symbol? . -> . any))
   (new-help-desk (-> (is-a?/c frame%)))
   (show-help-desk (-> any))
   (search-for-docs (string?
                     search-type?
                     search-how?
                     any?
                     (listof path?) ;; manual names
                     . -> .
                     any)))
  
  (define (goto-hd-location x) (error 'goto-hd-location "no"))
  
  (define-values/invoke-unit/sig gui^ help-desk@ #f setup:plt-installer^ mred^ net:tcp^))
