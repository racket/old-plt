(require-library "core.ss")
(define-signature xml-lex^ 
  (port->fstream build-fstream
   (struct tokstream (token rest))
   (struct token ())
   (struct begin-tag (name contents start stop))
   (struct pi-tag (type contents start stop))
   (struct doc-tag (root contents start stop))
   (struct cdata-tag (contents start stop))
   (struct empty-tag (name contents start stop))
   (struct end-tag (contents start stop))
   (struct text (contents start stop))
   (struct entity-ref (contents start stop))
   (struct lex:error ())
   (struct internal ())
   build-tokstream))

(define-signature xml-structs^
  ((struct document (prolog element misc))
   (struct prolog (xml before-dtd dtd before-element))
   (struct xmlD (version encoding standalone))
   (struct doctype (root ext-id dtd))
   (struct int (content))
   (struct ext (location file-name))
   (struct element (name attributes content))
   (struct attribute (name value))
   (struct pi (target-name instruction))
   (struct source (start stop))
   (struct pcdata (string))
   (struct xml-read:error ())))

(define-signature writer^ (write-xml display-xml write-xml/content display-xml/content))
(define-signature reader^ ((open xml-structs^) read-xml))

(define-signature xexpr^ (xml->xexpr xexpr->xml))
(define-signature xml^ ((open reader^) (open writer^) (open xexpr^)))

