#cs
(module scheme-lexer mzscheme
  
  (require (lib "lex.ss" "parser-tools"))
  
  (provide scheme-lexer)
   
  (define-lex-abbrevs
      
   [any-string (&)]
   [any-char (^)]
   [alphabetic (: (- "a" "z") (- "A" "Z"))]
   
   ;; For case insensitivity
   [a (: "a" "A")]
   [b (: "b" "B")]
   [c (: "c" "C")]
   [d (: "d" "D")]
   [e (: "e" "E")]
   [f (: "f" "F")]
   [g (: "g" "G")]
   [h (: "h" "H")]
   [i (: "i" "I")]
   [j (: "j" "J")]
   [k (: "k" "K")]
   [l (: "l" "L")]
   [m (: "m" "M")]
   [n (: "n" "N")]
   [o (: "o" "O")]
   [p (: "p" "P")]
   [q (: "q" "Q")]
   [r (: "r" "R")]
   [s (: "s" "S")]
   [t (: "t" "T")]
   [u (: "u" "U")]
   [v (: "v" "V")]
   [w (: "w" "W")]
   [x (: "x" "X")]
   [y (: "y" "Y")]
   [z (: "z" "Z")]

   [digit (- "0" "9")]
   [digit2 (: "0" "1")]
   [digit8 (- "0" "7")]
   [digit10 digit]
   [digit16 (: digit10 (- "a" "f") (- "A" "F"))]

   [scheme-whitespace (: #\newline #\return #\tab #\space #\vtab #\page)]
   [line-comment (@ ";" (* (^ #\newline)))]

   
   ;; What about char->integer constraint?
   #;[unicode  (: (@ (: "U" "u") digit16)
                (@ (: "U" "u") digit16 digit16)
                (@ (: "U" "u") digit16 digit16 digit16)
                (@ (: "U" "u") digit16 digit16 digit16 digit16)
                (@ "U" digit16 digit16 digit16 digit16 digit16)
                (@ "U" digit16 digit16 digit16 digit16 digit16 digit16)
                (@ "U" digit16 digit16 digit16 digit16 digit16 digit16 digit16)
                (@ "U" digit16 digit16 digit16 digit16 digit16 digit16 digit16 digit16))]

   [character (: (@ "#\\" any-char)
                 (@ "#\\" character-name)
                 (@ "#\\" (- "0" "3") digit8 digit8)
                 #;(@ "#\\" unicode))]
   
   [character-name (: (@ s p a c e)
                      (@ n e w l i n e)
                      (@ n u l) 
                      (@ n u l l)
                      (@ b a c k s p a c e)
                      (@ t a b)
                      (@ l i n e f e e d)
                      (@ v t a b)
                      (@ p a g e)
                      (@ r e t u r n)
                      (@ r u b o u t))]
   
   [bad-char (: (@ "#\\" alphabetic alphabetic (* alphabetic))
                (@ "#\\" (- "0" "3") digit8))]
       
   ;; What about byte string regexp strings
   [str (: (@ (? "#rx") "\"" (* (: string-element #;unicode)) "\"")
           byte-str)]
   [byte-str (@ (? "#rx") "#\"" (* string-element) "\"")]
   [string-element (: (^ "\"" "\\")
                      "\\\""
                      "\\\\"
                      "\\a"
                      "\\b"
                      "\\t"
                      "\\n"
                      "\\v"
                      "\\f"
                      "\\r"
                      "\\e"
                      (@ "\\" digit8)
                      (@ "\\" digit8 digit8)
                      (@ "\\" digit8 digit8 digit8)
                      (@ "\\x" digit16)
                      (@ "\\x" digit16 digit16)
                      (@ "\\" #\newline))]

   [bad-str (@ (? "#rx") (? "#") "\"" 
               (* (: (^ "\"" "\\")
                     (@ "\\" any-char)))
               (? (: "\\" "\"")))]
   [num2 (@ prefix2 complex2)]
   [complex2 (: real2
                (@ real2 "@" real2)
                (@ real2 "+" (: special-numbers ureal2) i)
                (@ real2 "-" (: special-numbers ureal2) i)
                (@ real2 "+" i)
                (@ real2 "-" i)
                (@ "+" (: special-numbers ureal2) i)
                (@ "-" (: special-numbers ureal2) i)
                (@ "+" i)
                (@ "-" i))]
   [real2 (: (@ sign ureal2)
             (@ (: "+" "-") special-numbers))]
   [decimal2 (: (@ uinteger2 suffix2)
                (@ "." (+ digit2) (* "#") suffix2)
                (@ (+ digit2) "." (* digit2) (* "#") suffix2)
                (@ (+ digit2) (+ "#") "." (* "#") suffix2))]
   [ureal2 (: uinteger2 (@ uinteger2 "/" uinteger2) decimal2)]
   [uinteger2 (@ (+ digit2) (* "#"))]
   [prefix2 (: (@ radix2 exactness)
               (@ exactness radix2))]   
   
   [num8 (@ prefix8 complex8)]
   [complex8 (: real8
                (@ real8 "@" real8)
                (@ real8 "+" (: special-numbers ureal8) i)
                (@ real8 "-" (: special-numbers ureal8) i)
                (@ real8 "+" i)
                (@ real8 "-" i)
                (@ "+" (: special-numbers ureal8) i)
                (@ "-" (: special-numbers ureal8) i)
                (@ "+" i)
                (@ "-" i))]
   [real8 (: (@ sign ureal8)
             (@ (: "+" "-") special-numbers))]
   [decimal8 (: (@ uinteger8 suffix8)
                (@ "." (+ digit8) (* "#") suffix8)
                (@ (+ digit8) "." (* digit8) (* "#") suffix8)
                (@ (+ digit8) (+ "#") "." (* "#") suffix8))]
   [ureal8 (: uinteger8 (@ uinteger8 "/" uinteger8) decimal8)]
   [uinteger8 (@ (+ digit8) (* "#"))]
   [prefix8 (: (@ radix8 exactness)
               (@ exactness radix8))]   
   
   [num10 (@ prefix10 complex10)]
   [complex10 (: real10
                 (@ real10 "@" real10)
                 (@ real10 "+" (: special-numbers ureal10) i)
                 (@ real10 "-" (: special-numbers ureal10) i)
                 (@ real10 "+" i)
                 (@ real10 "-" i)
                 (@ "+" (: special-numbers ureal10) i)
                 (@ "-" (: special-numbers ureal10) i)
                 (@ "+" i)
                 (@ "-" i))]
   [real10 (: (@ sign ureal10)
              (@ (: "+" "-") special-numbers))]
   [decimal10 (: (@ uinteger10 suffix10)
                 (@ "." (+ digit10) (* "#") suffix10)
                 (@ (+ digit10) "." (* digit10) (* "#") suffix10)
                 (@ (+ digit10) (+ "#") "." (* "#") suffix10))]
   [ureal10 (: uinteger10 (@ uinteger10 "/" uinteger10) decimal10)]
   [uinteger10 (@ (+ digit10) (* "#"))]
   [prefix10 (: (@ radix10 exactness)
                (@ exactness radix10))]   
   
   [num16 (@ prefix16 complex16)]
   [complex16 (: real16
                 (@ real16 "@" real16)
                 (@ real16 "+" (: special-numbers ureal16) i)
                 (@ real16 "-" (: special-numbers ureal16) i)
                 (@ real16 "+" i)
                 (@ real16 "-" i)
                 (@ "+" (: special-numbers ureal16) i)
                 (@ "-" (: special-numbers ureal16) i)
                 (@ "+" i)
                 (@ "-" i))]
   [real16 (: (@ sign ureal16)
              (@ (: "+" "-") special-numbers))]
   [decimal16 (: (@ uinteger16 suffix16)
                 (@ "." (+ digit16) (* "#") suffix16)
                 (@ (+ digit16) "." (* digit16) (* "#") suffix16)
                 (@ (+ digit16) (+ "#") "." (* "#") suffix16))]
   [ureal16 (: uinteger16 (@ uinteger16 "/" uinteger16) decimal16)]
   [uinteger16 (@ (+ digit16) (* "#"))]
   [prefix16 (: (@ radix16 exactness)
                (@ exactness radix16))]   
   
   [special-numbers (: (@ n a n ".0")(@ i n f ".0"))]
   
   [suffix2 (: "" (@ exponent-marker sign (+ digit2)))]
   [suffix8 (: "" (@ exponent-marker sign (+ digit8)))]
   [suffix10 (: "" (@ exponent-marker sign (+ digit10)))]
   [suffix16 (: "" (@ exponent-marker sign (+ digit16)))]
   [exponent-marker (: e s f d l)]
   [sign (: "" "+" "-")]
   [exactness (: "" "#i" "#e" "#I" "#E")]
   [radix2 (: "#b" "#B")]
   [radix8 (: "#o" "#O")]
   [radix10 (: "" "#d" "#D")]
   [radix16 (: "#x" "#X")]
   
   [script (@ "#!" (* (: (^ #\newline) (@ #\\ #\newline))))]

   [identifier-delims (: "\"" "," "'" "`" "(" ")" "[" "]" "{" "}" ";" scheme-whitespace)]
   [identifier-chars (^ (: identifier-delims "\\" "|"))]
   [identifier-escapes (: (@ "\\" any-char)
                          (@ "|" (* (^ "|")) "|"))]
   [identifier-start (: identifier-escapes
                        (^ (: identifier-delims "\\" "|" "#"))
                        "#%")]
   [identifier (@ identifier-start
                (* (: identifier-escapes identifier-chars)))]

   [bad-id-start (: identifier-escapes
                    (^ identifier-delims "\\" "|"))]
   [bad-id-escapes (: identifier-escapes
                      (@ "|" (* (^ "|"))))]
   [bad-id (: (@ bad-id-start
                 (* (: identifier-escapes identifier-chars))
                 (? (: "\\" bad-id-escapes)))
              "\\"
              bad-id-escapes)]
             
  
   [reader-command (: (@ "#" c s) (@ "#" c i))]
   [sharing (: (@ "#" uinteger10 "=")
               (@ "#" uinteger10 "#"))])
  
  (define (ret lexeme type paren start-pos end-pos)
    (values lexeme type paren (position-offset start-pos) (position-offset end-pos)))


  (define get-next-comment
    (lexer
     ["#|" (values 1 end-pos)]
     ["|#" (values -1 end-pos)]
     [(: "#" "|" (~ (@ any-string (: "|" "#") any-string))) (get-next-comment input-port)]
     [(eof) (values 'eof end-pos)]
     [(special)
      (get-next-comment input-port)]
     [(special-comment)
      (get-next-comment input-port)]
     [(special-error)
      (get-next-comment input-port)]))
  
  (define (read-nested-comment num-opens start-pos input)
    (let-values (((diff end) (get-next-comment input)))
      (cond
        ((eq? 'eof diff) (ret "" 'error #f start-pos end))
        (else
         (let ((next-num-opens (+ diff num-opens)))
           (cond
             ((= 0 next-num-opens) (ret "" 'comment #f start-pos end))
             (else (read-nested-comment next-num-opens start-pos input))))))))
  
  (define scheme-lexer
    (lexer
     [(+ scheme-whitespace) (ret lexeme 'white-space #f start-pos end-pos)]
     [(: "#t" "#f" "#T" "#F" num2 num8 num10 num16 character)
      (ret lexeme 'constant #f start-pos end-pos)]
     [str (ret lexeme 'string #f start-pos end-pos)]
     [(: "#;" line-comment) 
      (ret lexeme 'comment #f start-pos end-pos)]
     ["#|" (read-nested-comment 1 start-pos input-port)]
     [(@ (: "" "#hash" "#hasheq" (@ "#" (* digit10))) "(")
      (values lexeme 'parenthesis '|(| (position-offset start-pos) (position-offset end-pos))]
     [(@ (: "" "#hash" "#hasheq" (@ "#" (* digit10))) "[")
      (values lexeme 'parenthesis '|[| (position-offset start-pos) (position-offset end-pos))]
     [(@ (: "" "#hash" "#hasheq" (@ "#" (* digit10))) "{")
      (values lexeme 'parenthesis '|{| (position-offset start-pos) (position-offset end-pos))]
     [(: ")" "]" "}")
      (values lexeme 'parenthesis (string->symbol lexeme) (position-offset start-pos) (position-offset end-pos))]
     [(: "'" "`" "#'" "#`" "#&")
      (values lexeme 'constant #f (position-offset start-pos) (position-offset end-pos))]
     [(: script sharing reader-command "." "," ",@" "#," "#,@")
      (values lexeme 'other #f (position-offset start-pos) (position-offset end-pos))]
     [identifier (values lexeme 'symbol #f (position-offset start-pos) (position-offset end-pos))]
     [(special)
      (ret "" 'no-color #f start-pos end-pos)]
     [(special-comment)
      (ret "" 'comment #f start-pos end-pos)]
     [(special-error)
      (ret "" 'no-color #f start-pos end-pos)]
     [(eof) (values lexeme 'eof #f #f #f)]
     [(& (: bad-char bad-str bad-id)
         (~ (@ (: reader-command sharing "#|" "#;" "#&" script) any-string)))
      (ret lexeme 'error #f start-pos end-pos)]
     [any-char (extend-error lexeme start-pos end-pos input-port)]))
  
  (define (extend-error lexeme start end in)
    (if (memq (peek-char-or-special in)
              `(special #\newline #\return #\tab #\space #\vtab
                 #\" #\, #\' #\` #\( #\) #\[ #\] #\{ #\} #\;
                 ,eof))
        (ret lexeme 'error #f start end)
        (let-values (((rest end-pos) (get-chunk in)))
          (ret (string-append lexeme rest) 'error #f start end-pos))))
  
  (define get-chunk
    (lexer
     ((+ (^ identifier-delims)) (values lexeme end-pos))))
  
  
  )
