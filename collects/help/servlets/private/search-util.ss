(module search-util mzscheme

  (provide 
    search-types 
    match-types
    kind-types)

  (define search-types
    '(("keyword" "Keyword entry")
      ("keyword-index" "Keyword or index entry" *) ; default
      ("keyword-index-text" "Keyword, index entry, or text")))

  (define match-types
    '(("exact-match" "exact match")
      ("containing-match" "containing match" *) ; default
      ("regexp-match" "regexp match")))

  (define kind-types
    '(("index entries" html)
      ("keyword entries" text)
      ("text" text))))



