(module docpos mzscheme
  (require (lib "list.ss"))

  (provide standard-html-doc-position 
           user-defined-doc-position
           set-doc-position!
	   reset-doc-positions!
	   known-docs)
  
  ;; Define an order on the standard docs.
  (define (standard-html-doc-position d)
    (case (string->symbol d)
  
      [(beginning) -20]
      [(beginning-abbr) -19]
      [(intermediate) -17]
      [(intermediate-lambda) -16]
      [(advanced) -15]

      [(help) 0]
      [(drscheme) 1]

      [(r5rs) 2]
      [(mzscheme) 3]
      [(mzlib) 4]
      [(misclib) 5]
      [(mred) 6]
      [(framework) 7]
      [(teach) 8]
      [(mzc) 10]
      [(tools) 30]
      [(insidemz) 50]
      [(tour) 90]

      [else 100]))

  (define user-doc-positions '())

  (define (set-doc-position! manual weight)
    (let ([man-sym (string->symbol manual)])
      (unless (assoc manual known-docs)
	      (error 
	       'set-doc-position! 
	       "Unknown manual \"~a\"" manual))
      (set! user-doc-positions
	    (cons (list man-sym weight)
		  (filter (lambda (x)
			    (not (eq? (car x) man-sym)))
			  user-doc-positions)))))

  (define (reset-doc-positions!)
    (set! user-doc-positions '()))

  (define (user-defined-doc-position manual)
    (let ([result (assoc (string->symbol manual) user-doc-positions)])
      (and result (cadr result))))

  ; known-docs: (listof (cons string[subdir-of-doc-dir] string[title]))
  (define known-docs
    '(("beginning" . "Beginning Student Language")
      ("beginning-abbr" . "Beginning Student with List Abbreviations Language")
      ("advanced" . "Advanced Student Language")
      ("insidemz" . "Inside PLT MzScheme")
      ("intermediate" . "Intermediate Student Language")
      ("intermediate-lambda" . "Intermediate Student with Lambda Language")
      ("drscheme" . "PLT DrScheme: Programming Environment Manual")
      ("r5rs" . "Revised^5 Report on the Algorithmic Language Scheme")
      ("mzscheme" . "PLT MzScheme: Language Manual")
      ("mzlib" . "PLT MzLib: Libraries Manual")
      ("mred" . "PLT MrEd: Graphical Toolbox Manual")
      ("framework" . "PLT Framework: GUI Application Framework")
      ("misclib" . "PLT Miscellaneous Libraries: Reference Manual")
      ("mzc" . "PLT mzc: MzScheme Compiler Manual")
      ("srfi" . "SRFI documents inside PLT")
      ("teachpack" . "Teachpacks for How to Design Programs")
      ("tools" . "PLT Tools: DrScheme Extension Manual")
      ("tour" . "A Brief Tour of DrScheme version 205")
      ("t-y-scheme" . "Teach Yourself Scheme in Fixnum Days")
      ("tex2page" . "TeX2page")))

  )
