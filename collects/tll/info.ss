(module info (lib "infotab.ss" "setup")
  (define name "The Little LISPer")
  (define compile-omit-files '())
  (define tools
    (list (list "tool.ss")))
  (define tool-icons
    (list (list "tll.jpg" "icons")))

  (define drscheme-language-modules
    '(("tll.ss" "TLL")))
  (define drscheme-language-positions
    '(("Teaching Languages" "The Little LISPer")))
  (define drscheme-language-numbers
    '((-500 -1000)))
  (define drscheme-language-one-line-summaries
    '("Use this language level with The Little Schemer (nee LISPer)"))
  (define drscheme-language-urls
    '("http://www.ccs.neu.edu/home/matthias/BTLS/"))
  )
