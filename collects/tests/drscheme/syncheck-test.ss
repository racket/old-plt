
(module syncheck-test mzscheme
  
  (require "drscheme-test-util.ss"
           (lib "gui.ss" "tests" "utils")
           (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "text-string-style-desc.ss" "mrlib"))
  
  (provide run-test)
  
  ;; type str/ann = (list (union symbol string) symbol)
  ;; type test = (make-test string (listof str/ann))
  (define-struct test (input expected))
  
  ;; tests : (listof test)
  (define tests
    (list 
     (make-test "12345"
                '(("12345" constant)))
     (make-test "'abcdef"
                '(("'" keyword)
                  ("abcdef" constant)))
     (make-test "(define f 1)"
                '(("("      base)
                  ("define" keyword)
                  (" "      base)
                  ("f"      bound-variable)
                  (" "      base)
                  ("1"      constant)
                  (")"      base)))
     (make-test "(lambda (x) x)"
                '(("("      base)
                  ("lambda" keyword)
                  (" ("     base)
                  ("x"      bound-variable)
                  (") "     base)
                  ("x"      bound-variable)
                  (")"      base)))
     (make-test "(lambda x x)"
                '(("("      base)
                  ("lambda" keyword)
                  (" "      base)
                  ("x"      bound-variable)
                  (" "      base)
                  ("x"      bound-variable)
                  (")"      base)))
     (make-test "(lambda (x . y) x y)"
                '(("("      base)
                  ("lambda" keyword)
                  (" ("     base)
                  ("x"      bound-variable)
                  (" . "    base)
                  ("y"      bound-variable)
                  (") "     base)
                  ("x"      bound-variable)
                  (" "      base)
                  ("y"      bound-variable)
                  (")"      base)))
     
     (make-test "(case-lambda [(x) x])"
                '(("("           base)
                  ("case-lambda" keyword)
                  (" [("         base)
                  ("x"           bound-variable)
                  (") "          base)
                  ("x"           bound-variable)
                  ("])"          base)))
     (make-test "(if 1 2 3)"
                '(("("  base)
                  ("if" keyword)
                  (" "  base)
                  ("1"  constant)
                  (" "  base)
                  ("2"  constant)
                  (" "  base)
                  ("3"  constant)
                  (")"  base)))
     (make-test "(if 1 2)"
                '(("("  base)
                  ("if" keyword)
                  (" "  base)
                  ("1"  constant)
                  (" "  base)
                  ("2"  constant)
                  (")"  base)))
     (make-test "(begin 1 2)"
                '(("("     base)
                  ("begin" keyword)
                  (" "     base)
                  ("1"     constant)
                  (" "     base)
                  ("2"     constant)
                  (")"     base)))
     (make-test "(begin0 1 2)"
                '(("("      base)
                  ("begin0" keyword)
                  (" "      base)
                  ("1"      constant)
                  (" "      base)
                  ("2"      constant)
                  (")"      base)))
     (make-test "(let ([x x]) x)"
                '(("("   base)
                  ("let" keyword)
                  (" ([" base)
                  ("x"   bound-variable)
                  (" "   base)
                  ("x"   unbound-variable)
                  ("]) " base)
                  ("x"   bound-variable)
                  (")"   base)))
     (make-test "(letrec ([x x]) x)"
                '(("("      base)
                  ("letrec" keyword)
                  (" (["    base)
                  ("x"      bound-variable)
                  (" "      base)
                  ("x"      bound-variable)
                  ("]) "    base)
                  ("x"      bound-variable)
                  (")"      base)))
     (make-test "(set! x 1)"
                '(("("    base)
                  ("set!" keyword)
                  (" "    base)
                  ("x"    unbound-variable)
                  (" "    base)
                  ("1"    constant)
                  (")"    base)))
     (make-test "(let ([x 1]) (set! x 2))"
                '(("("    base)
                  ("let"  keyword)
                  (" (["  base)
                  ("x"    bound-variable)
                  (" "    base)
                  ("1"    constant)
                  ("]) (" base)
                  ("set!" keyword)
                  (" "    base)
                  ("x"    bound-variable)
                  (" "    base)
                  ("2"    constant)
                  ("))"   base)))
     (make-test "object%"
                '(("object%" bound-variable)))
     (make-test "unbound-id"
                '(("unbound-id" unbound-variable)))
     (make-test "(define bd 1) bd"
                '(("("       base)
                  ("define"  keyword)
                  (" "       base)
                  ("bd"      bound-variable)
                  (" "       base)
                  ("1"       constant)
                  (") "      base)
                  ("bd"      bound-variable)))
     (make-test "#'abc"
                '(("#'"  keyword)
                  ("abc" constant)))
     (make-test "(with-continuation-mark 1 2 3)"
                '(("("                      base)
                  ("with-continuation-mark" keyword)
                  (" "                      base)
                  ("1"                      constant)
                  (" "                      base)
                  ("2"                      constant)
                  (" "                      base)
                  ("3"                      constant)
                  (")"                      base)))
     (make-test "(f x)"
                '(("(" base)
                  ("f" unbound-variable)
                  (" " base)
                  ("x" unbound-variable)
                  (")" base)))
     (make-test "(define-syntax (f stx) (syntax 1))"
                '(("("             base)
                  ("define-syntax" keyword)
                  (" ("            base)
                  ("f"             keyword)
                  (" "             base)
                  ("stx"           bound-variable)
                  (") ("           base)
                  ("syntax"        keyword)
                  (" "             base)
                  ("1"             constant)
                  ("))"            base)))
     (make-test "(module m mzscheme)"
                '(("("        base)
                  ("module"   keyword)
                  (" m "      base)
                  ("mzscheme" unbound-variable)
                  (")"        base)))
     (make-test "(require-for-syntax mzscheme)"
                '(("("                  base)
                  ("require-for-syntax" keyword)
                  (" "          base)
                  ("mzscheme"   unbound-variable)
                  (")"          base)))
     (make-test "(require mzscheme)"
                '(("("          base)
                  ("require"    keyword)
                  (" "          base)
                  ("mzscheme"   unbound-variable)
                  (")"          base)))
     (make-test "(module m mzscheme (provide x) (define x 1))"
                '(("("             base)
                  ("module"        keyword)
                  (" m mzscheme (" base)
                  ("provide"       keyword)
                  (" "             base)
                  ("x"             bound-variable)
                  (") ("           base)
                  ("define"        keyword)
                  (" "             base)
                  ("x"             bound-variable)
                  (" "             base)
                  ("1"             constant)
                  ("))"            base)))
     
     (make-test "(module m mzscheme (+ 1 2))"
                '(("("             base)
                  ("module"        keyword)
                  (" m mzscheme (" base)
                  ("+"             bound-variable)
		  (" "             base)
		  ("1"             constant)
		  (" "             base)
		  ("2"             constant)
                  ("))"            base)))
     
     (make-test "(module m mzscheme (require (lib \"list.ss\")))"
                '(("("                 base)
                  ("module"            keyword)
                  (" m "               base)
                  ("mzscheme"          unbound-variable)
                  (" ("                base)
                  ("require"           keyword)
                  (" "                 base)
                  ("(lib \"list.ss\")" unbound-variable)
                  ("))"                base)))
     
     (make-test "(module m mzscheme (require-for-syntax (lib \"list.ss\")) (define-syntax s foldl))"
                '(("("                     base)
                  ("module"                keyword)
                  (" m mzscheme ("         base)
                  ("require-for-syntax"    keyword)
                  (" (lib \"list.ss\")) (" base)
                  ("define-syntax"         keyword)
                  (" "                     base)
                  ("s"                     keyword)
                  (" "                     base)
                  ("foldl"                 bound-variable)
                  ("))"                    base)))
     
     (make-test "(module m mzscheme (require-for-syntax (lib \"etc.ss\")) (define-syntax s (rec f 1)))"
                '(("("                     base)
                  ("module"                keyword)
                  (" m mzscheme ("         base)
                  ("require-for-syntax"    keyword)
                  (" (lib \"etc.ss\")) ("  base)
                  ("define-syntax"         keyword)
                  (" "                     base)
                  ("s"                     keyword)
                  (" ("                    base)
                  ("rec"                   keyword)
                  (" "                     base)
                  ("f"                     bound-variable)
                  (" "                     base)
                  ("1"                     constant)
                  (")))"                   base)))

     (make-test "(define-syntax s (lambda (stx) (syntax-case stx () (_ 123))))"
                '(("("             base)
                  ("define-syntax" keyword)
                  (" "             base)
                  ("s"             keyword)
                  (" ("            base)
                  ("lambda"        keyword)
                  (" ("            base)
                  ("stx"           bound-variable)
                  (") ("           base)
                  ("syntax-case"   keyword)
                  (" "             base)
                  ("stx"           bound-variable)
                  (" () (_ "       base)
                  ("123"           constant)
                  ("))))"          base)))

     (make-test "(require (lib \"list.ss\")) first"
                '(("("                    base)
                  ("require"              keyword)
                  (" (lib \"list.ss\")) " base)
                  ("first"                bound-variable)))
     
     (make-test "(require (lib \"etc.ss\")) (rec f 1)"
                '(("("                    base)
                  ("require"              keyword)
                  (" (lib \"etc.ss\")) (" base)
                  ("rec"                  keyword)
                  (" "                    base)
                  ("f"                    bound-variable)
                  (" "                    base)
                  ("1"                    constant)
                  (")"                    base)))
     
     (make-test "(define-struct s ()) (define-struct (t s) ())"
                '(("("             base)
                  ("define-struct" keyword)
                  (" "             base)
                  ("s"             keyword)
                  (" ()) ("        base)
                  ("define-struct" keyword)
                  (" ("            base)
                  ("t"             keyword)
                  (" s) ())"       base)))
     
     (make-test "`(1 ,x 2)"
                '(("`"        keyword)
                  ("("        base)
                  ("1"        constant)
                  (" ,"       base)
                  ("x"        unbound-variable)
                  (" "        base)
                  ("2"        constant)
                  (")"        base)))
     
     (make-test "#!"
                '(("#!" base)))
     
     (make-test "#!\n"
                '(("#!\n" base)))
     
     (make-test "#!\n1"
                '(("#!\n" base)
                  ("1"    constant)))
     
     (make-test "#!\n1\n1"
                '(("#!\n" base)
                  ("1"    constant)
                  ("\n"   base)
                  ("1"    constant)))
     
     (make-test "(module m mzscheme (lambda (x) x) (provide))"
                '(("("        base)
                  ("module"   keyword)
                  (" m "      base)
                  ("mzscheme" unbound-variable)
                  (" ("       base)
                  ("lambda"   keyword)
                  (" ("       base)
                  ("x"        bound-variable)
                  (") "       base)
                  ("x"        bound-variable)
                  (") ("      base)
                  ("provide"  keyword)
                  ("))"       base)))

     
     
                                                                             
                                          ;;             ;;;                 
                                           ;               ;                 
                                           ;               ;                 
 ;;;;   ; ;;;  ; ;;;   ;;;  ;;; ;;;        ;;;;    ;;;     ;     ;;;  ;;; ;;;
     ;   ;      ;     ;   ;  ;   ;         ;   ;  ;   ;    ;    ;   ;  ;   ; 
  ;;;;   ;      ;     ;   ;  ; ; ;         ;   ;  ;;;;;    ;    ;   ;  ; ; ; 
 ;   ;   ;      ;     ;   ;  ; ; ;         ;   ;  ;        ;    ;   ;  ; ; ; 
 ;   ;   ;      ;     ;   ;   ; ;          ;   ;  ;   ;    ;    ;   ;   ; ;  
  ;;; ; ;;;;   ;;;;    ;;;    ; ;         ; ;;;    ;;;   ;;;;;;  ;;;    ; ;  
                                                                             
                                                                             
                                                                             
     ;; the tests below should also be part of arrow-based test suite.


     (make-test "(let l () l l)"
                '(("("    base)
                  ("let"  keyword)
                  (" "    base)
                  ("l"    bound-variable)
                  (" () " base)
                  ("l"    bound-variable)
                  (" "    base)
                  ("l"    bound-variable)
                  (")"    base)))
     (make-test "(class object% this)"
                '(("("       base)
                  ("class"   keyword)
                  (" "       base)
                  ("object%" bound-variable)
                  (" "       base)
                  ("this"    keyword)
                  (")"       base)))
     
     (make-test "(module m mzscheme (require (lib \"list.ss\")) foldl)"
                '(("("                    base)
                  ("module"               keyword)
                  (" m "                  base)
                  ("mzscheme"             unbound-variable)
                  (" ("                   base)
                  ("require"              keyword)
                  (" (lib \"list.ss\")) " base)
                  ("foldl"                bound-variable)
                  (")"                    base)))
     (make-test "(module m (lib \"htdp-beginner.ss\" \"lang\") empty)"
                '(("("                                         base)
                  ("module"                                    keyword)
                  (" m (lib \"htdp-beginner.ss\" \"lang\") "   base)
                  ("empty"                                     bound-variable)
                  (")"                                         base)))
     (make-test "(module m mzscheme (require (prefix x: (lib \"list.ss\"))) x:foldl)"
                '(("("                                base)
                  ("module"                           keyword)
                  (" m "                              base)
                  ("mzscheme"                         unbound-variable)
                  (" ("                               base)
                  ("require"                          keyword)
                  (" (prefix x: (lib \"list.ss\"))) " base)
                  ("x:foldl"                          bound-variable)
                  (")"                                base)))
     (make-test "(module m mzscheme (require (lib \"etc.ss\")) (rec f 1))"
                '(("("                     base)
                  ("module"                keyword)
                  (" m "                   base)
                  ("mzscheme"              unbound-variable)
                  (" ("                    base)
                  ("require"               keyword)
                  (" (lib \"etc.ss\")) ("  base)
                  ("rec"                   keyword)
                  (" "                     base)
                  ("f"                     bound-variable)
                  (" "                     base)
                  ("1"                     constant)
                  ("))"                    base)))
     
     (make-test "(module m (lib \"htdp-intermediate.ss\" \"lang\") (local ((define x x)) x))"
                '(("("                                            base)
                  ("module"                                       keyword)
                  (" m (lib \"htdp-intermediate.ss\" \"lang\") (" base)
                  ("local"                                        keyword)
                  (" ((define "                                   base)
                  ("x"                                            bound-variable)
                  (" "                                            base)
                  ("x"                                            bound-variable)
                  (")) "                                          base)
                  ("x"                                            bound-variable)
                  ("))"                                           base)))
     
     ))
  
  (define (run-test)
    (set-language-level! (list "PLT" (regexp "Graphical")))
    (for-each run-one-test tests))
  
  (define (run-one-test test)
    (let ([drs (wait-for-drscheme-frame)]
          [input (test-input test)]
          [expected (test-expected test)])
      (clear-definitions drs)
      (type-in-definitions drs input)
      (test:button-push (send drs syncheck:get-button))
      (wait-for-computation drs)
      (let ([got (get-annotated-output drs)])
        (compare-output expected got input))))
  
  ;; compare-output 
  ;; should show first difference.
  (define (compare-output expected got input)
    (unless (equal? got expected)
      (printf "FAILED: ~s\n      expected: ~s\n           got: ~s\n"
              input expected got)))
  
  ;; get-annotate-output : drscheme-frame -> (listof str/ann)
  (define (get-annotated-output drs)
    (get-string/style-desc (send drs get-definitions-text))))

