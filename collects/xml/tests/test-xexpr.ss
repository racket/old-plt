;; Test the xexpr part of the XML collection.
(module test-xexpr mzscheme
  (require (lib "test.ss" "schemeunit")
           (lib "xml.ss" "xml")
           )

  (provide test-xexpr)

  (define test-xexpr
    (make-test-suite
      "Test the xexpr part of the XML collection."

      ;; xml->xexpr

      (make-test-case
        "xml->xexpr, given pcdata"
        (assert-equal?
          (xml->xexpr
            (make-pcdata 'scheme 'scheme "The PCDATA"))
          "The PCDATA"))

      (make-test-case
        "xml->xexpr, given element"
        (assert-equal?
          (xml->xexpr
            (make-element 'scheme 'scheme 'Name '() '()))
          '(Name ())))

      (make-test-case
        "xml->xexpr, given element with content"
        (assert-equal?
          (xml->xexpr
            (make-element 'scheme 'scheme 'Name '()
                          (list (make-pcdata 'scheme 'scheme "Blah"))))
          '(Name () "Blah")))

      (make-test-case
        "xml->xexpr, given element with attributes"
        (assert-equal?
          (xml->xexpr
            (make-element 'scheme 'scheme 'Name
                          (list (make-attribute 'scheme 'scheme 'foo "bar"))
                          '()))
          '(Name ((foo "bar")))))

      (make-test-case
        "xml->xexpr, given entity number"
        (assert-equal?
          (xml->xexpr (make-entity 'scheme 'scheme 140))
          140))

      (make-test-case
        "xml->xexpr, given entity name"
        (assert-equal?
          (xml->xexpr (make-entity 'scheme 'scheme 'nbsp))
          'nbsp))

      ;; Comments don't act as I'd expect
      (make-test-case
        "xml->xexpr, given comment"
        (assert-equal?
          (comment-text (xml->xexpr (make-comment "This is bad code")))
          "This is bad code"))

      (make-test-case
        "xml->xexpr, given pi"
        (assert-equal?
          (let ((p (xml->xexpr
                     (make-pi 'scheme 'scheme "XML" (list "version=1.0")))))
            (list (pi-target-name p) (pi-instruction p)))
          (list "XML" (list "version=1.0"))))

      ;; xexpr->xml

      ;; symbol -> make-entity
      (make-test-case
        "xexpr->xml, given a symbol"
        (assert-equal?
          (entity-text (xexpr->xml 'nbsp))
          'nbsp))

      ;; string -> make-pcdata
      (make-test-case
        "xexpr->xml, given a string"
        (assert-equal?
          (pcdata-string (xexpr->xml "String"))
          "String"))

      ;; integer -> make-entity
      (make-test-case
        "xexpr->xml, given an integer"
        (assert-equal?
          (entity-text (xexpr->xml 140))
          140))

      ;; comment -> make-comment
      (make-test-case
        "xexpr->xml, given a comment"
        (assert-equal?
          (comment-text (xexpr->xml (make-comment "Comment")))
          "Comment"))

      ;; pi -> make-pi
      (make-test-case
        "xexpr->xml, given a processing instruction"
        (assert-equal?
          (let ((p (xexpr->xml
                     (make-pi 'scheme 'scheme "XML" (list "version=1.0")))))
            (list (pi-target-name p) (pi-instruction p)))
          (list "XML" (list "version=1.0"))))

      ;; (name body ...) -> make-element
      (make-test-case
        "xexpr->xml, given an element with no attributes"
        (assert-equal?
          (let ((p (xexpr->xml '(name body1 body2))))
            (append
              (list (element-name p)
                    (element-attributes p))
              (map entity-text (element-content p))))
          (list 'name '() 'body1 'body2)))

      ;; (name attrs body ...) -> make-element
      (make-test-case
        "xexpr->xml, given an element with attributes"
        (assert-equal?
          (let ((p (xexpr->xml '(name ((foo "bar")) body1 body2))))
            (append
              (cons (element-name p)
                    (map (lambda (a)
                           (cons (attribute-name a)
                                 (attribute-value a)))
                         (element-attributes p)))
              (map entity-text (element-content p))))
          (list 'name (cons 'foo "bar") 'body1 'body2)))

      ;; xexpr->string
      (make-test-case
        "xexpr->string"
        (assert-equal?
          (xexpr->string '(name ((foo "bar") (baz "barney"))
                                (another-name "content")
                                "more content"))
          (string-append
            "<name foo=\"bar\" baz=\"barney\">"
            "<another-name>content</another-name>"
            "more content</name>")))

      ;; xexpr?
      (make-test-case
        "xexpr?, with a non-xexpr at the first level"
        (assert-false (xexpr? (lambda () 5))))

      (make-test-case
        "xexpr?, with a non-xexpr at a deep level"
        (assert-false
          (xexpr? `(html ((title "blah")) (a ((href ,(lambda () 5))) "5")))))

      (make-test-case
        "xexpr?, with an xexpr"
        (assert-true
          (xexpr? '(html ((title "blah")) (a ((href "n.html")) "n")))))

      ;; validate-xexpr
      (make-test-case
        "validate-xexpr, with a non-xexpr at the first level"
        (assert-equal?
          (with-handlers
            ((exn:invalid-xexpr?  (lambda (e) 17)))
            (validate-xexpr (lambda () 5)))
          17))

      (make-test-case
        "validate-xexpr, with a non-xexpr at a deep level"
        (assert-equal?
          (with-handlers
            ((exn:invalid-xexpr?  (lambda (e) 17)))
            (validate-xexpr `(html ((title "blah"))
                                   (a ((href ,(lambda () 5))) "5"))))
          17))

      (make-test-case
        "validate-xexpr, with a xexpr"
        (assert-true
          (validate-xexpr '(html ((title "blah")) (a ((href "n.html")) "n")))))

      ;; xexpr-attribute?
      (make-test-case
        "xexpr-attribute?"
        (assert-true
          (xexpr-attribute? '(foo "bar"))))

      ;; listof?
      ;;; Why is this provided from xexpr^ ?
      (make-test-case
        "listof?"
        (assert-true
          (listof? number? (list 1 2 3 4))))

      ))

  )
