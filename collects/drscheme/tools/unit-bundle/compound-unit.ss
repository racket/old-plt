#|

A compound unit is a class with this data:
  bundle-manager                          ; imports
  (list-of (make-link bundle-manager
                      bundle-manager
                      sexp))               ; links
  bundle-manager                           ; exports

It supports these operations:

  get-imports : (-> bundle-manager<%>)
  set-imports : (bundle-manager<%> -> void)
  get-exports : (-> bundle-manager<%>)
  set-exports : (bundle-manager<%> -> void)
  get-links : (-> (list-of (make-link bundle-manager bundle-manager sexp)))
  set-links : ((list-of (make-link bundle-manager bundle-manager sexp)) -> void)
  
  create-view : (compound-unit-pasteboard -> void)
     ;; puts the pasteboard into this pasteboard
  
|#

(unit/sig drscheme:bundle:compound-unit^
  (import mred^)
  
  (define compound-unit<%>
    (interface ()
      get-imports set-imports
      get-exports set-exports
      get-links set-links
      
      install-in-view))
  
  (define-struct make-link (import export exp))
  
  (define compound-unit%
    (class* object% (compound-unit<%>) (imports links exports)
      (public
        [get-imports (lambda () imports)]
        [set-imports (lambda (i) (set! imports i))]
        [get-exports (lambda () exports)]
        [set-exports (lambda (e) (set! exports e))]
        [get-links (lambda () links)]
        [set-links (lambda (l) (set! links l))])
      
      
      (public
        [install-in-view
         (lambda (pb)
           (void))])
      
      (sequence (super-init))))
  ;
  ;(define link-snip%
  ;  (class snip% (link)
  ;    (public
  ;      [
  ;
  (define compound-unit-pasteboard%
    (class pasteboard% (compound-unit)
      (sequence
        (super-init)))))