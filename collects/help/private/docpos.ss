(module docpos mzscheme
  (require (lib "list.ss")
           (lib "contract.ss"))

  (provide/contract
   [standard-html-doc-position (string? . -> . number?)])
  
  (provide user-defined-doc-position
           set-doc-position!
	   reset-doc-positions!
	   known-docs)
  
  ;; Define an order on the standard docs.
  (define (standard-html-doc-position d)
    (if (string=? d "help")
        -1
        (let ([line (assoc d docs-and-positions)])
          (if line
              (caddr line)
              100))))

  (define user-doc-positions '())

  (define (set-doc-position! manual weight)
    (printf "set-doc-position! ~s ~s\n" manual weight)
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
  
  ;; (listof (list string string number))
  ;; the first string is the collection name
  ;; the second string is the title of the the manual
  ;; the number determines the sorting order for the manuals in the manuals page
  (define docs-and-positions
    `(("r5rs" "Revised^5 Report on the Algorithmic Language Scheme" -50)
      ("mzscheme" "PLT MzScheme: Language Manual" -49)
      ("mred" "PLT MrEd: Graphical Toolbox Manual" -48)
            
      ("beginning" "Beginning Student Language" -19)
      ("beginning-abbr" "Beginning Student with List Abbreviations Language" -18)
      ("intermediate" "Intermediate Student Language" -17)
      ("intermediate-lambda" "Intermediate Student with Lambda Language" -16)
      ("advanced" "Advanced Student Language" -15)
      ("teachpack" "Teachpacks for How to Design Programs" -16)
      
      ("profj-beginner" "ProfessorJ Beginner Language" -10)
      ("profj-intermediate" "ProfessorJ Intermediate Language" -9)
      ("profj-advanced" "ProfessorJ Advanced Language" -8)
      
      ("tour" ,(format "A Brief Tour of DrScheme version ~a" (version)) 0)
      ("drscheme" "PLT DrScheme: Programming Environment Manual" 1)
      
      ("srfi" "SRFI documents inside PLT" 3)
      
      ("mzlib" "PLT MzLib: Libraries Manual" 5)
      ("misclib" "PLT Miscellaneous Libraries: Reference Manual" 6)
      ("framework" "PLT Framework: GUI Application Framework" 8)

      ("mzc" "PLT mzc: MzScheme Compiler Manual" 10)
      
      ("tools" "PLT Tools: DrScheme Extension Manual" 30)
      ("insidemz" "Inside PLT MzScheme" 50)

      ("swindle"    "Swindle Manual"    60)
      ("plot"       "PLoT Manual"       61)
      ("web-server" "Web Server Manual" 62)

      ("t-y-scheme" "Teach Yourself Scheme in Fixnum Days" 100)
      ("tex2page" "TeX2page" 101)))
  
  ; known-docs: (listof (cons string[subdir-of-doc-dir] string[title]))
  (define known-docs (map (lambda (x) (cons (car x) (cadr x))) docs-and-positions)))
