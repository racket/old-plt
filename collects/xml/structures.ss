(unit/sig xml-structs^
  (import)
    
  ;; Document ::= (make-document Prolog Element (listof Processing-instruction))
  (define-struct document (prolog element misc))
  
  ;; Prolog ::= (make-prolog XMLDeclare (listof Processing-instruction) DocType (listof Processing-instruction))
  (define-struct prolog (xml before-dtd dtd before-element))
  
  ;; XMLDeclare ::= (make-xmlD String (U String #f) (U String #f))
  ;;             |  #f
  (define-struct xmlD (version encoding standalone))
  
  ;; DocType ::= (make-doctype String ExtID Dtd)
  ;;          |  #f
  (define-struct doctype (root ext-id dtd))
  
  ;; ExtID ::= (make-ext String String)
  ;;        |  #f
  (define-struct ext (location file-name))
  
  ;; Dtd ::= (make-int (listof Char))
  ;;      |  #f
  (define-struct int (content))
  
  ;; Location : (U Nat Symbol)
  ;; Source : (make-source Location Location)
  (define-struct source (start stop))
  
  ;; Element ::= (make-element Location Location Symbol (listof Attribute) (listof Content))
  (define-struct (element struct:source) (name attributes content))
  
  ;; Attribute ::= (make-attribute Location Location Symbol String)
  (define-struct (attribute struct:source) (name value))
  
  ;; Pcdata ::= (make-pcdata Location Location String)
  (define-struct (pcdata struct:source) (string))
  
  ;; Content ::= Pcdata  
  ;;          |  Element
  ;;          |  Entity
  ;;          |  Comment
  ;;          |  Processing-instruction
  
  ;; Entity ::= (make-entity (U Nat Symbol))
  (define-struct entity (text))
  
  ;; Processing-instruction ::= (make-pi Location Location String (list String))
  (define-struct (pi struct:source) (target-name instruction))
  
  ;; Comment ::= (make-comment String)
  (define-struct comment (text)))