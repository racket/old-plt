#cs
(module scheme-lexer mzscheme
  
  (require (lib "lex.ss" "parser-tools"))
  
  (provide scheme-lexer)
   
  (define-lex-abbrevs
   [any (- #\000 #\377)]
   [letter (: (- "a" "z") (- "A" "Z"))]
   [digit (- "0" "9")]
   [whitespace (: #\newline #\return #\tab #\space #\vtab)]
   [line-comment (@ ";" (* (^ #\newline)) (: #\newline (eof)))]
   [character (: (@ "#\\" any)
                 (@ "#\\" character-name)
                 (@ "#\\" (- "0" "3") (- "0" "7") (- "0" "7")))]
   [character-name (: "space" "newline" "nul" "null" "backspace" "tab" "linefeed"
                      "vtab" "page" "return" "rubout")]
   [bad-char (: "#\\" (^ identifier-delims) (+ (^ identifier-delims)))]
   [str (@ (: "" "#rx") "\"" (* string-element) "\"")]
   
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
   
   [bad-str (@ (: "" "#rx") "\"" 
             (* (: (^ "\"" "\\")
                   (@ "\\" (: (eof) any))))
             (? (: (eof) "\"")))]
   
   [num2 (@ prefix2 complex2)]
   [complex2 (: real2
                (@ real2 "@" real2)
                (@ real2 "+" (: special-numbers ureal2) "i")
                (@ real2 "-" (: special-numbers ureal2) "i")
                (@ real2 "+i")
                (@ real2 "-i")
                (@ "+" (: special-numbers ureal2) "i")
                (@ "-" (: special-numbers ureal2) "i")
                (@ "+i")
                (@ "-i"))]
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
                (@ real8 "+" (: special-numbers ureal8) "i")
                (@ real8 "-" (: special-numbers ureal8) "i")
                (@ real8 "+i")
                (@ real8 "-i")
                (@ "+" (: special-numbers ureal8) "i")
                (@ "-" (: special-numbers ureal8) "i")
                (@ "+i")
                (@ "-i"))]
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
                 (@ real10 "+" (: special-numbers ureal10) "i")
                 (@ real10 "-" (: special-numbers ureal10) "i")
                 (@ real10 "+i")
                 (@ real10 "-i")
                 (@ "+" (: special-numbers ureal10) "i")
                 (@ "-" (: special-numbers ureal10) "i")
                 (@ "+i")
                 (@ "-i"))]
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
                 (@ real16 "+" (: special-numbers ureal16) "i")
                 (@ real16 "-" (: special-numbers ureal16) "i")
                 (@ real16 "+i")
                 (@ real16 "-i")
                 (@ "+" (: special-numbers ureal16) "i")
                 (@ "-" (: special-numbers ureal16) "i")
                 (@ "+i")
                 (@ "-i"))]
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
   
   [special-numbers (: "nan.0" "inf.0")]
   
   [suffix2 (: "" (@ exponent-marker sign (* digit2)))]
   [suffix8 (: "" (@ exponent-marker sign (* digit8)))]
   [suffix10 (: "" (@ exponent-marker sign (* digit10)))]
   [suffix16 (: "" (@ exponent-marker sign (* digit16)))]
   [exponent-marker (: "e" "s" "f" "d" "l")]
   [sign (: "" "+" "-")]
   [exactness (: "" "#i" "#e")]
   [radix2 "#b"]
   [radix8 "#o"]
   [radix10 (: "" "#d")]
   [radix16 "#x"]
   [digit2 (: "0" "1")]
   [digit8 (- "0" "7")]
   [digit10 digit]
   [digit16 (: digit10 (- "a" "f") (- "A" "F"))]
   
   [script (@ "#!" (* (: (^ #\newline) (@ #\\ #\newline))))]
   
   [bad-num (@ (: "#o" "#b" "#x") (* (^ identifier-delims)))]
   
   
   [identifier-delims (: "\"" "," "'" "`" "(" ")" "[" "]" "{" "}" ";" whitespace)]
   [identifier-chars (^ (: identifier-delims "\\" "|"))]
   [identifier-escapes (: (@ "\\" any)
                          (@ "|" (* (^ "|")) "|"))]
   [identifier-start (: identifier-escapes
                        (^ (: identifier-delims "\\" "|" "#"))
                        "#%")]
   [identifier (@ identifier-start
                (* (: identifier-escapes identifier-chars)))]
   
   [bad-id-start (: bad-id-escapes
                    (^ (: identifier-delims "\\" "|" "#"))
                    "#%")]
   [bad-id-escapes (: identifier-escapes
                      (@ "\\" (eof))
                      (@ "|" (* (^ "|")) (eof)))]
   [bad-id (@ bad-id-start
            (* (: bad-id-escapes identifier-chars)))]
   
   [reader-command (: "#cs" "#ci" "#hash(" "#hasheq(")]
   [sharing (: (@ "#" uinteger10 "=")
               (@ "#" uinteger10 "#"))])
  
  (define (ret a b c)
    (values a #f (position-offset b) (position-offset c)))
  
  (define get-next-comment
    (lexer
     ["#|" (values 1 end-pos)]
     ["|#" (values -1 end-pos)]
     [(eof) (values 'eof end-pos)]
     [(: "|" "#" (+ (^ "|" "#"))) (get-next-comment input-port)]))
  
  (define (read-nested-comment num-opens start-pos input)
    (let-values (((diff end) (get-next-comment input)))
      (cond
        ((eq? 'eof diff) (ret 'error start-pos end))
        (else
         (let ((next-num-opens (+ diff num-opens)))
           (cond
             ((= 0 next-num-opens) (ret 'comment start-pos end))
             (else (read-nested-comment next-num-opens start-pos input))))))))
  
  (define scheme-lexer
    (lexer
     [(+ whitespace) (ret 'white-space start-pos end-pos)]
     [(: "#t" "#f" num2 num8 num10 num16 character)
      (ret 'literal start-pos end-pos)]
     [str (ret 'string start-pos end-pos)]
     [(: "#;" line-comment) (ret 'comment start-pos end-pos)]
     ["#|" (read-nested-comment 1 start-pos input-port)]
     [(: "(" ")") (values 'other (string->symbol lexeme) (position-offset start-pos) (position-offset end-pos))]
     [(: "(" ")" "[" "]" "{" "}"
         (@ "#" (* digit10) "(")
         "'" "`" "," ",@"
         "#'" "#`" "#," "#,@"
         "."  "#&"
         reader-command
         script
         sharing)
      (values 'other (string->symbol lexeme) (position-offset start-pos) (position-offset end-pos))]
     [identifier (values 'identifier lexeme (position-offset start-pos) (position-offset end-pos))]
     [(: bad-str bad-id bad-num bad-char)
      (ret 'error start-pos end-pos)]
     ["#" (extend-error start-pos end-pos input-port)]
     [any (ret 'error start-pos end-pos)]
     [(special)
      (ret 'white-space start-pos end-pos)]
     [(special-comment)
      (ret 'white-space start-pos end-pos)]
     [(special-error)
      (ret 'white-space start-pos end-pos)]
     [(eof) (values 'eof #f #f #f)]))
  
  (define (extend-error start end in)
    (if (memq (peek-char in)
              `(#\newline #\return #\tab #\space #\vtab
                 #\" #\, #\' #\` #\( #\) #\[ #\] #\{ #\} #\;
                 ,eof))
        (ret 'error start end)
        (ret 'error start (get-chunk in))))
  
  (define get-chunk
    (lexer
     ((+ (^ identifier-delims)) end-pos)))
  
  
  )
