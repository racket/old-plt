(module info (lib "infotab.ss" "setup")
  (require (lib "string-constant.ss" "string-constants"))
  (define name "The Little LISPer")
  (define compile-omit-files '())
  (define tools
    (list (list "tool.ss")))
  (define tool-icons
    (list (list "tll.jpg" "icons")))

  (define drscheme-language-modules
    '(("tll.ss" "TLL")))
  (define drscheme-language-positions
    `((,(string-constant teaching-languages) "The Little LISPer")))
  (define drscheme-language-numbers
    '((-500 -300)))
  (define drscheme-language-one-line-summaries
    '("Use with The Little Schemer (nee LISPer)"))
  (define drscheme-language-urls
    '("http://www.ccs.neu.edu/home/matthias/BTLS/"))
  )
#|
The
 drscheme-language-position : (listof (cons string (listof string)))
 drscheme-language-modules : (listof (listof string))
 drscheme-language-numbers : (listof (listof number))
 drscheme-language-readers : (listof string)
 (listof string)
 (listof module-spec)
specifications aren't
correct. Expected, ,
, , , and 
respectively, where the lengths of the outer lists are the same.

Got
 (("Teaching Languages" "The Little LISPer") 
  ("Teaching Languages" "The Little LISPer 2")),
 (("tll.ss" "TLL")
  ("tll.ss" "TLL 2")),
 ((-500 -1000 1)
  (-500 -1000 2)),
 ("Use with The Little Schemer (nee LISPer)" 
  "Use with The Little Schemer 8-10"),
 ("http://www.ccs.neu.edu/home/matthias/BTLS/"
  "http://www.ccs.neu.edu/home/matthias/BTLS/"), 
 (#f #f) 
|#
