(module search-util mzscheme

  (require (lib "string-constant.ss" "string-constants"))

  (provide 
    search-types 
    match-types
    kind-types)

  (define search-types
    `(("keyword" ,(string-constant search-for-keyword))
      ("keyword-index" ,(string-constant search-for-keyword-or-index) *) ; default
      ("keyword-index-text" ,(string-constant search-for-keyword-or-index-or-text))))

  (define match-types
    `(("exact-match" ,(string-constant exact-match))
      ("containing-match" ,(string-constant containing-match) *) ; default
      ("regexp-match" ,(string-constant regexp-match))))

  (define kind-types
    `(("index entries" html)
      ("keyword entries" text)
      ("text" text))))



