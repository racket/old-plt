(unit/sig extra-xexpr^
  (import reader^ mzlib:function^)
  ;; Xexpr ::= String
  ;;        |  ProcessingInstruction
  ;;        |  (list* Symbol (listof Attribute-srep) (list Xexpr))
  ;; Attribute-srep ::= (list Symbol String)
  
  ;; assoc-sort : (listof (list Symbol a)) -> (listof (list Symbol a))
  (define (assoc-sort to-sort)
    (quicksort to-sort (bcompose string<? (compose symbol->string car))))
  
  ;; xml->xexpr : Content -> Xexpr
  (define (xml->xexpr x)
    (cond
      [(element? x)
       (list* (element-name x) 
              (assoc-sort (map attribute->srep (element-attributes x)))
              (map xml->xexpr (element-content x)))]
      [(pcdata? x) (pcdata-string x)]
      [else x]))
  
  ;; attribute->srep : Attribute -> Attribute-srep
  (define (attribute->srep a)
    (list (attribute-name a) (attribute-value a)))
  
  ;; srep->attribute : Attribute-srep -> Attribute
  (define (srep->attribute a)
    (make-attribute 'scheme 'scheme (car a) (cadr a)))
  
  ;; xexpr->xml : Xexpr -> Content
  (define (xexpr->xml x)
    (cond
      [(pair? x)
       (make-element 'scheme 'scheme (car x) (map srep->attribute (cadr x))
                     (map xexpr->xml (cddr x)))]
      [(string? x) (make-pcdata 'scheme 'scheme x)]
      [else x]))
  
  ;; bcompose : (a a -> c) (b -> a) -> (b b -> c)
  (define (bcompose f g)
    (lambda (x y) (f (g x) (g y)))))
