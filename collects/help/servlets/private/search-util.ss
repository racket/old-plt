(module search-util mzscheme

  (require (lib "string-constant.ss" "string-constants"))

  (provide 
    search-types 
    match-types
    kind-types)

  (define search-types
    `(("keyword" ,(string-constant plt:hd:search-for-keyword))
      ("keyword-index" ,(string-constant plt:hd:search-for-keyword-or-index) *) ; default
      ("keyword-index-text" ,(string-constant plt:hd:search-for-keyword-or-index-or-text))))

  (define match-types
    `(("exact-match" ,(string-constant plt:hd:exact-match))
      ("containing-match" ,(string-constant plt:hd:containing-match) *) ; default
      ("regexp-match" ,(string-constant plt:hd:regexp-match))))

  (define kind-types
    `(("index entries" html)
      ("keyword entries" text)
      ("text" text))))



