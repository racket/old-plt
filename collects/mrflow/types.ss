(module types mzscheme
  
  (provide (all-defined))
  
  (define-struct type () (make-inspector))
  
  ; (make-type-empty) is the same as (make-type-cst 'bottom) for now. The reason we
  ; *never* use (make-type-cst 'bottom) is because it would trigger the propagation of
  ; bottom everywhere, thus slowing down the analysis. There's two solutions to that:
  ; - not use initialize-label-set-for-value-source when using (make-type-cst 'bottom)
  ; - use a separate (make-type-empty), which is more correct anyway (note that there's
  ;   currently no way to define the type for a primitive that returns the symbol 'bottom
  ;   (or 'number, or 'null, etc...))
  (define-struct (type-empty type) () (make-inspector))
  
  (define-struct (type-cst type) (type) (make-inspector))
  (define-struct (type-cons type) (car cdr) (make-inspector))
  (define-struct (type-vector type) (element) (make-inspector))
  (define-struct (type-case-lambda type) (rest-arg?s req-args argss exps) (make-inspector))
  (define-struct (type-var type) (name reach handle) (make-inspector))
  (define-struct (type-union type) (elements) (make-inspector))
  (define-struct (type-rec type) (vars types body) (make-inspector))
  (define-struct (type-values type) (type) (make-inspector))
  (define-struct (type-flow-var type) (name) (make-inspector))
  (define-struct (type-promise type) (value) (make-inspector))
  (define-struct (type-scheme type) (flow-vars type^cs type) (make-inspector))
  
  ; note: we have to keep the type label around, because that's the only thing
  ; that allows us to differentiate structurally equivalent structure that have
  ; the same name (i.e. the only way to have subtyping work in the presence of generative
  ; structures). The reason for type-struct-type is because structure types are first
  ; class values in mzscheme. Also, by keeping the type-label around, we avoid the need
  ; to duplicate the type hierarchy all the way up to the root each time we compute the
  ; type of a structure.
  (define-struct (type-struct-value type) (type-label types) (make-inspector))
  (define-struct (type-struct-type type) (type-label) (make-inspector))
)
