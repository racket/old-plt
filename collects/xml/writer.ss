(unit/sig writer^
  (import reader^ mzlib:function^)
  
  ;; gen-write/display-xml : (Nat Output-port -> Void) -> Content [Output-Port]-> Void
  (define (gen-write/display-xml dent)
    (lambda (c . out)
      (let ([port (cond
                    [(null? out) (current-output-port)]
                    [else (car out)])])
        (write-xml-content c 0 dent port))))
  
  ;; indent : Nat Output-port -> Void
  (define (indent n out)
    (newline out)
    (let loop ([n n])
      (unless (zero? n)
        (display #\space out)
        (loop (sub1 n)))))
  
  ;; write-xml/content : Content [Output-port] -> Void
  (define write-xml/content (gen-write/display-xml void))
  
  ;; display-xml/content : Content [Output-port] -> Void
  (define display-xml/content (gen-write/display-xml indent))
  
  ;; write-xml : Document [Output-port] -> Void
  (define (write-xml doc . out)
    ;; more-here - write out header junk
    (apply write-xml/content (document-element doc) out))
  
  ;; display-xml : Document [Output-port] -> Void
  (define (display-xml doc . out)
    ;; more-here - display the header
    (apply display-xml/content (document-element doc) out))
  
  ;; write-xml-content : Content Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define (write-xml-content el over dent out)
    ((cond
       [(element? el) write-xml-element]
       [(pcdata? el) write-xml-pcdata]
       [(pi? el) write-xml-pi]
       [else (error 'write-xml-content "received ~a" el)])
     el over dent out))
  
  ;; write-xml-element : Element Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define (write-xml-element el over dent out)
    (let ([start (lambda (f) (write-xml-base (format f (element-name el)) over dent out))]
          [content (element-content el)])
      (start "<~a")
      (for-each (lambda (att)
                  (fprintf out " ~s=~s" (attribute-name att)
                           (escape (attribute-value att) escape-attribute-table)))
                (element-attributes el))
      (if (null? content)
          (fprintf out "/>")
          (begin
            (fprintf out ">")
            (for-each (lambda (c) (write-xml-content c (incr over) dent out)) content)
            (start "</~a")
            (fprintf out ">")))))
  
  ;; write-xml-base : (U String Char Symbol) Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define (write-xml-base el over dent out)
    (dent over out)
    (display el out))
  
  ;; write-xml-pcdata : Pcdata Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define (write-xml-pcdata str over dent out)
    (write-xml-base (escape (pcdata-string str) escape-table) over dent out))
  
  ;; write-xml-pi : String Nat (Nat Output-Stream -> Void) Output-Stream -> Void
  (define write-xml-pi void) ;; more here
    
  (define escape-table
    (map (lambda (x y) (cons (regexp (symbol->string x)) y))
         '(< > &)
         '("\\&lt;" "\\&gt;" "\\&amp;")))
  
  (define escape-attribute-table
    (list* (cons (regexp "'") "\\&apos;") (cons (regexp "\"") "\\&quot;") escape-table))
  
  ;; escape : String -> String
  ;; more here - this could be much more efficient
  (define (escape x table)
    (foldr (lambda (esc str) (regexp-replace* (car esc) str (cdr esc)))
           x
           table))

  ;; incr : Nat -> Nat
  (define (incr n) (+ n 2)))
