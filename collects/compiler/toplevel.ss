; toplevel.ss
; (c)1997 Sebastian Good
; routines for defining top-level abstractions

(unit/sig
 compiler:top-level^
 (import)
 
 ;;-------------------------------------------------------------
 ;; This contains information about a top-level block, either at
 ;; file level, or within a unit; typically a sequence of defines
 ;; but could be anything
 ;;
 (define-struct block (source local-vars global-vars used-vars captured-vars max-arity))
 (define make-empty-block (lambda () (make-block #f #f #f #f #f 0)))

 (define block:register-max-arity!
   (lambda (b n)
     (set-block-max-arity! b (max n (block-max-arity b))))))
