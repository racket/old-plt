(unit/sig extra-xexpr^
  (import xml-structs^ mzlib:function^)
  ;; Xexpr ::= String
  ;;        |  (list* Symbol (listof Attribute-srep) (listof Xexpr))
  ;;        |  (cons Symbol (listof Xexpr))
  ;;        |  Symbol
  ;;        |  Number
  ;;        |  Comment
  ;;        |  Processing-instruction
  ;; Attribute-srep ::= (list Symbol String)
  
  ;; sorting is no longer necessary, since xt3d uses xml->zxexpr, which sorts.
  
  ;; assoc-sort : (listof (list Symbol a)) -> (listof (list Symbol a))
  (define (assoc-sort to-sort)
    (quicksort to-sort (bcompose string<? (compose symbol->string car))))
  
  ;; xml->xexpr : Content -> Xexpr
  (define (xml->xexpr x)
    (cond
      [(element? x)
       (let ([body (map xml->xexpr (element-content x))]
             [atts (element-attributes x)])
         (cons (element-name x) 
               (if (null? atts)
                   body
                   (cons (assoc-sort (map attribute->srep atts))
                         body))))]
      [(pcdata? x) (pcdata-string x)]
      [(entity? x) (entity-text x)]
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
       (let ([f (lambda (atts body-sel)
                  (make-element 'scheme 'scheme (car x)
                                atts
                                (map xexpr->xml (body-sel x))))])
         (if (and (pair? (cdr x)) (or (null? (cadr x)) (and (pair? (cadr x)) (pair? (caadr x)))))
             (f (map srep->attribute (cadr x)) cddr)
             (f null cdr)))]
      [(string? x) (make-pcdata 'scheme 'scheme x)]
      [(or (symbol? x) (integer? x)) (make-entity x)]
      [else x]))
  
  ;; bcompose : (a a -> c) (b -> a) -> (b b -> c)
  (define (bcompose f g)
    (lambda (x y) (f (g x) (g y)))))
