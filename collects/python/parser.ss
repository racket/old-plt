#cs(module parser mzscheme
  (require (lib "lex.ss" "parser-tools")
           (lib "yacc.ss" "parser-tools")
           (lib "readerr.ss" "syntax")
           (lib "class.ss")
           (lib "contract.ss")
          ; (lib "mred.ss" "mred") ;; for text% in a contract.. :P
	   "compiler.ss"
           "compiler-expr.ss"
           "compiler-stmt.ss"
           "compiler-target.ss")
           
;  (provide build-ast) 
  (provide/contract
   (build-ast (input-port? . -> . (listof (is-a?/c ast-node%))))
   (build-ast-from-file (string? . -> . (listof (is-a?/c ast-node%)))))
  (provide build-ast-from-port)
;   (build-ast-from-port (input-port? (union (is-a?/c text%) string?) . -> . (listof (is-a?/c ast-node%)))))
  
  (define (build-ast ip)
    (port-count-lines! ip)
    (let ((get-token (make-lexer)))
      (p (lambda () 
           (let ((tok (get-token ip)))
             ;(printf "~a~n" tok)
             tok)))))
  
  (define (build-ast-from-file name)
    (with-input-from-file name
      (lambda ()
        (file-path name)
        (build-ast (current-input-port)))
      'text))
;    (let ((i (open-input-file name 'text)))
;      (file-path name)
;      (begin0
;        (build-ast i)
;        (close-input-port i))))

  (define (build-ast-from-port port name)
    (file-path name)
    (build-ast port))
  
  (define-lex-abbrevs
   (blank (: #\space #\tab #\page))
   (comment (@ #\# (* (^ #\newline))))
   (letter (: (- #\A #\Z) (- #\a #\z)))
   (digit (- #\0 #\9))
   (decimal-integer (: "0" 
                       (@ (- "1" "9") (* digit))))
   (oct-integer (@ "0" (+ (- "0" "7"))))
   (hex-integer (@ "0" (: #\x "X") (+ (: digit (- "A" "F") (- "a" "f")))))
   (float (: point-float exponent-float))
   (point-float (: (@ (? int-part) fraction)
                   (@ int-part ".")))
   (exponent-float (@ (: int-part point-float) exponent))
   (int-part (+ digit))
   (fraction (@ "." (+ digit)))
   (exponent (@ (: #\e "E") (? (: #\+ #\-)) (+ digit)))
   (string-literal (@ string-prefix (: short-string1
                                         short-string2
                                         long-string1
                                         long-string2)))
   (string-prefix (@ (? (: #\u #\U)) (? (: #\r #\R))))
   (short-string1 (@ "'" (* short-string-item1) "'"))
   (short-string2 (@ #\" (* short-string-item2) #\"))
   (long-string1 (@ "'''" (* (: long-string-item1
                                (@ #\' long-string-item1)
                                (@ "''" long-string-item1)))
                  "'''"))
   (long-string2 (@ "\"\"\"" (* (: long-string-item2
                                (@ #\" long-string-item2)
                                (@ "\"\"" long-string-item2)))
                  "\"\"\""))
   (short-string-item1 (: short-string-char1 escape-seq))
   (short-string-item2 (: short-string-char2 escape-seq))
   (long-string-item1 (: long-string-char1 escape-seq))
   (long-string-item2 (: long-string-char2 escape-seq))
   (short-string-char1 (^ #\\ #\newline #\'))
   (short-string-char2 (^ #\\ #\newline #\"))
   (long-string-char1 (^ #\\ #\'))
   (long-string-char2 (^ #\\ #\"))
   (escape-seq (@ #\\ (- #\000 #\377))))

  (define (translate-string str sp ep)
    (let-values (((raw? unicode? quote-length)
                  (let ((m (regexp-match "^(u|U)?(r|R)?('''|'|\"\"\"|\")" str)))
                    (values (cadr m) (caddr m) (string-length (cadddr m))))))
      (if unicode?
          (raise-read-error
           "Unicode strings are not supported"
           (file-path)
           (position-line sp)
           (position-col sp)
           (position-offset sp)
           (- (position-offset ep) (position-offset sp))))
      (let ((raw-str (substring str
                                (+ (if raw? 1 0) quote-length)
                                (- (string-length str) quote-length))))
        (cond
          (raw? raw-str)
          (else
           (let ((in (open-input-string raw-str))
                 (out (open-output-string)))
             (let loop ()
               (let ((m (regexp-match "\\\\\n|\\\\\\\\|\\\\'|\\\\\"|\\\\a|\\\\b|\\\\f|\\\\n|\\\\r|\\\\t|\\\\v|\\\\[0-3][0-7][0-7]|\\\\x([0-9]|[a-f]|[A-F])([0-9]|[a-f]|[A-F])"
                                      in 0 #f out)))
                 (cond
                   (m (display 
                       (cond
                         ((string=? (car m) "\\\n") "")
                         ((string=? (car m) "\\\\") #\\)
                         ((string=? (car m) "\\'") #\')
                         ((string=? (car m) "\\\"") #\")
                         ((string=? (car m) "\\a") #\007)
                         ((string=? (car m) "\\b") #\010)
                         ((string=? (car m) "\\f") #\014)
                         ((string=? (car m) "\\n") #\012)
                         ((string=? (car m) "\\r") #\015)
                         ((string=? (car m) "\\t") #\011)
                         ((string=? (car m) "\\v") #\013)
                         ((char=? #\x (string-ref (car m) 1))
                          (integer->char (string->number (substring (car m) 2 4) 16)))
                         (else
                          (integer->char (string->number (substring (car m) 1 4) 8))))
                       out)
                      (loop)))))
             (get-output-string out)))))))
    
  (define (make-lexer)
    (letrec (
             ;; Keep track of indentation levels for INDENT/DEDENT token generation
             (indent-stack '(0))
             
             ;; Keep track of nesting for implicit line joining
             (p-depth 0)
             (sq-depth 0)
             (c-depth 0)
             (reached-eof #f)
             (last-pos #f)
             (OPAREN (lambda () (set! p-depth (add1 p-depth)) '|(|))
             (CPAREN (lambda () (set! p-depth (sub1 p-depth)) '|)|))
             (OBRACKET (lambda () (set! sq-depth (add1 sq-depth)) '|[|))
             (CBRACKET (lambda () (set! sq-depth (sub1 sq-depth)) '|]|))
             (OBRACE (lambda () (set! c-depth (add1 c-depth)) '|{|))
             (CBRACE (lambda () (set! c-depth (sub1 c-depth)) '|}|))
             
             
             ;; true iff the last token returned was NEWLINE
             (line-start #t)

             ;; the number of DEDENT tokens left to return in a multiple DEDENT line start
             (num-dedents 0)

             ;; Read the spaces at the beginning of the line
             ;; lex-whitespace: input-port -> int
             (lex-whitespace
              (lambda (ip)
                (let loop ((num 0)
                           (count 0))
                  (let ((char (peek-string 1 num ip)))
                    (cond
                      ((eof-object? char)
                       (read-string num ip)
                       0)
                      ((string=? char "\t")
                       (loop (add1 num) (+ count 8 (- (modulo count 8)))))
                      ((string=? char " ")
                       (loop (add1 num) (add1 count)))
                      (else
                       (read-string num ip)
                       count))))))

             ;; The actual lexer
             ;; lex-token: input-port -> token
             (lex-token
              (lexer-src-pos

               ;; 2.1.5, 2.1.6
               ((@ #\newline (* (@ (* blank) (? comment) #\newline)))
                (cond
                  ((and (= 0 p-depth) (= 0 sq-depth) (= 0 c-depth))
                   (set! line-start #t)
                   (set! last-pos end-pos)
                   'NEWLINE)
                  (else
                   (return-without-pos (lex-token input-port)))))
               ;; 2.1.3
               (comment (return-without-pos (lex-token input-port)))
               ;; 2.1.4
               ((@ #\\ #\newline) (return-without-pos (lex-token input-port)))
               ;; 2.1.8
               ((+ blank) (return-without-pos (lex-token input-port)))
               
               ;;2.3.1
               ((: "and"       "del"       "for"       "is"        "raise"
                   "assert"    "elif"      "from"      "lambda"    "return"   
                   "break"     "else"      "global"    "not"       "try"      
                   "class"     "except"    "if"        "or"        "while"    
                   "continue"  "exec"      "import"    "pass"      "yield"    
                   "def"       "finally"   "in"        "print")
                (string->symbol lexeme))
               ;; 2.3
               ((@ (: letter "_") (* (: letter digit "_")))
                (token-NAME lexeme))
               
               
               ;; 2.4.1
               (string-literal (token-STRING (translate-string lexeme start-pos end-pos)))
               
               ;; 2.4.4
               (decimal-integer
                (token-NUMBER (string->number lexeme)))
               (oct-integer
                (token-NUMBER (string->number lexeme 8)))
               (hex-integer
                (token-NUMBER (string->number (substring lexeme 2 (string-length lexeme)) 16)))
               ((@ decimal-integer (: #\l "L"))
                (token-NUMBER 
                 (string->number (substring lexeme 0 (sub1 (string-length lexeme))))))
               ((@ oct-integer (: #\l "L"))
                (token-NUMBER 
                 (string->number (substring lexeme 0 (sub1 (string-length lexeme))) 8)))
               ((@ hex-integer (: #\l "L"))
                (token-NUMBER (string->number (substring lexeme 2 (sub1 (string-length lexeme)))
                                              16)))
               ;; 2.4.5
               (float
                (token-NUMBER (string->number lexeme)))
               
               ;; 2.4.6
               ((@ (: float int-part) (: #\j "J"))
                (token-NUMBER (* +i (string->number (substring lexeme 0 (sub1 (string-length lexeme)))))))
               
               ;; 2.5
               ((: #\+       #\-       #\*       "**"      #\/       "//"      #\%
                   "<<"      ">>"      #\&       #\|       #\^       #\~
                   #\<       #\>       "<="      ">="      "=="      "!="      "<>")
                (if (string=? "|" lexeme)
                    '\|
                    (string->symbol lexeme)))
               
               ;; 2.6
               ("|=" '\|=)
               ("(" (OPAREN))
               (")" (CPAREN))
               ("[" (OBRACKET))
               ("]" (CBRACKET))
               ("{" (OBRACE))
               ("}" (CBRACE))
               ((: #\, #\; #\. #\`
                   #\:       #\=       "+="      "-="      "*="      "/="      "//="     "%="
                   "&="      "^="      ">>="     "<<="     "**=")
                (string->symbol lexeme))

               ((eof)
                (cond
                  (reached-eof 'EOF)
                  (else
                   (set! reached-eof #t)
                   (set! line-start #t)
                   (set! last-pos end-pos)
                   'NEWLINE)))))
                 
             ;; Return the number of entries in l before n is reached and set the stack to
             ;; be the rest of l (after n)
             ;;count-dedent: int * int list -> int
             (count-dedent
              (lambda (n l)
                (cond
                  ((null? l) #f)
                  ((= n (car l)) 
                   (set! indent-stack l)
                   0)
                  ((< (car l) n)
                   (raise-read-error
                    "unmatching indentation"
                    (file-path)
                    (position-line last-pos)
                    (position-col last-pos)
                    (position-offset last-pos)
                    n))
                  (else
                   (add1 (count-dedent n (cdr l)))))))
             ;; get-token: input-port -> token
             (get-token
              (lambda (ip)
                (cond
                  ;; There are still DEDENTS to do
                  ((> num-dedents 0) 
                   (set! num-dedents (sub1 num-dedents))
                   (list 'DEDENT last-pos last-pos))
                  ;; We are at the start of a line
                  (line-start
                   (let ((indent-num (lex-whitespace ip)))
                     (cond
                       ;; Same indentation level as last line, find the first token on the line
                       ((= indent-num (car indent-stack))
                        (set! line-start #f)
                        (get-token ip))
                       ;; indented from last line
                       ((> indent-num (car indent-stack))
                        (set! line-start #f)
                        (set! indent-stack (cons indent-num indent-stack))
                        (list 'INDENT last-pos last-pos))
                       (else
                        (set! line-start #f)
                        (set! num-dedents (sub1 (count-dedent indent-num indent-stack)))
                        (list 'DEDENT last-pos last-pos)))))
                  (else
                   (lex-token ip))))))
      get-token))
  
  (define (err token-ok token-name token-value src-start src-end)
    (raise-read-error
     (if token-ok
         (format "parse error with token: ~a, value: ~a" token-name token-value)
         (format "unrecognized token: ~a, value: ~a" token-name token-value))
     (file-path)
     (position-line src-start)
     (position-col src-start)
     (position-offset src-start)
     (- (position-offset src-end) (position-offset src-start))))

  (define (is-subscription? x)
    (and (not (list? x)) (not (eq? '... x))))

  (define (is-simple-slice? x)
    (and (list? x) (= 2 (length x))))

(define (build-primary expr subs start)
    (cond
      ((null? subs) expr)
      (else
       (let ((obj
              (cond
                ((eq? 'att (caar subs))
                 (make-object attribute-ref% expr (cadar subs) start (caddar subs)))
                ((eq? 'call (caar subs))
                 (make-object call% expr (cadar subs) start (caddar subs)))
                ((eq? 'sub-single (caar subs))
                 (cond
                   ((is-subscription? (cadar subs))
                    (make-object subscription% expr (cadar subs) start (caddar subs)))
                   ((is-simple-slice? (cadar subs))
                    (make-object simple-slicing% 
                      expr (car (cadar subs)) (cadr (cadar subs)) start (caddar subs)))
                   (else
                    (send (make-object ast-node% start (caddar subs)) stx-err
                          "Extended slicing not supported"))))
                (else
                 (cond
                   ((andmap is-subscription? (cadar subs))
                    (make-object subscription% 
                      expr (make-object tuple% (cadar subs) start (caddar subs)) start (caddar subs)))
                   (else
                    (send (make-object ast-node% start (caddar subs)) stx-err
                          "Extended slicing not supported")))))))
         (build-primary obj (cdr subs) start)))))


  (define-tokens t (DEDENT INDENT NAME NEWLINE NUMBER STRING))
  (define-empty-tokens et
    (!= % %= & &= |(| |)| * ** **= *= + += |,| - -= |.| / // //= /= : |;|
     < << <<= <= <> = == > >= >> >>= |[| |]| ^ ^= |`| and assert break class 
     continue def del elif else except exec finally for from global if import
     in is lambda not or pass print raise return try while yield |{| \| \|= |}| ~ EOF))
  (define p
    (parser
     (start start); single_input)
     (end EOF)
     (error err)
     (tokens t et)
     (src-pos)
     ;;(debug "parser.output")
     (suppress)
     (grammar

      (ident ((NAME) (make-object identifier% $1 $1-start-pos $1-end-pos)))
      
      (start ((file_input) (reverse $1)))
      
;      (single_input
;       ((NEWLINE) #f)
;       ((simple_stmt) $1)
;       ((compound_stmt NEWLINE) $1))
      (file_input (() null)
                  ((file_input NEWLINE) $1)
                  ((file_input stmt) (append (reverse $2) $1)))
      (eval_input ((tuple_or_test) $1) 
                  ((eval_input NEWLINE) $1))
      (funcdef ((def ident parameters : suite)
                (make-object function-definition%
                  $2 (make-object parameters% $3 $3-start-pos $3-end-pos) $5 $1-start-pos $5-end-pos)))
      (parameters ((|(| |)|) null)
                  ((|(| varargslist |)|) $2))
      (varargslist ((** ident) `((dict ,$2)))
                   ((* ident) `((seq ,$2)))
                   ((* ident |,| ** ident) `((seq ,$2) (dict ,$5)))
                   ((fpdef |,|) `((pos ,$1)))
                   ((fpdef = test |,|) `((key ,$1 ,$3)))
                   ((fpdef) `((pos ,$1)))
                   ((fpdef = test) `((key ,$1 ,$3)))
                   ((fpdef |,| varargslist) (cons `(pos ,$1) $3))
                   ((fpdef = test |,| varargslist) (cons `(key ,$1 ,$3) $5)))
      (fpdef ((ident) $1)
             ((|(| fplist |)|) $2))
      (fplist ((fpdef) (list $1))
              ((fpdef |,|) (list $1))
              ((fpdef |,| fplist) (cons $1 $3)))
      (stmt ((simple_stmt) $1)
            ((compound_stmt) (list $1)))
      (simple_stmt
       ((small_stmt NEWLINE) (list $1))
       ((small_stmt |;| NEWLINE) (list $1))
       ((small_stmt |;| simple_stmt) (cons $1 $3)))
      (small_stmt
       ((expr_stmt) $1)
       ((print_stmt) $1)
       ((del_stmt) $1)
       ((pass_stmt) $1)
       ((flow_stmt) $1)
       ((import_stmt) $1)
       ((global_stmt)
        (make-object global% (reverse $1) $1-start-pos $1-end-pos))
       ((exec_stmt) $1)
       ((assert_stmt) $1))
      (expr_stmt ((test augassign tuple_or_test)
                  (make-object aug-assignment% $1 $2 $3 $1-start-pos $3-end-pos))
                 ((testlist_list_plus)
                  (if (null? (cdr $1)) 
                      (make-object expr-stmt% (car $1) $1-start-pos $1-end-pos)
                      (let ((x (reverse $1)))
                        (make-object assignment% 
                          (reverse (cdr x)) (car x) $1-start-pos $1-end-pos)))))
      (testlist_list_plus ((tuple_or_test) (list $1))
                          ((tuple_or_test = testlist_list_plus) (cons $1 $3)))
      (augassign ((+=) '+=)
                 ((-=) '-=)
                 ((*=) '*=)
                 ((/=) '/=)
                 ((%=) '%=)
                 ((&=) '&=)
                 ((\|=) '\|=)
                 ((^=) '^=)
                 ((<<=) '<<=)
                 ((>>=) '>>=)
                 ((**=) '**=)
                 ((//=) '//=))
      ;; For normal assignments, additional restrictions enforced by the interpreter
      (print_stmt ((print test_list)
                   (make-object print% #f $2 $1-start-pos $2-end-pos))
                  ((print >> test_list)
                   (make-object print% #t $3 $1-start-pos $3-end-pos)))
      (test_list (() null) 
                 ((test) (list $1))
                 ((test |,| test_list) (cons $1 $3)))
      (del_stmt ((del target_tuple_or_expr)
                 (make-object del% $2 $1-start-pos $2-end-pos)))
      (pass_stmt ((pass)
                  (make-object pass% $1-start-pos $1-end-pos)))
      (flow_stmt ((break_stmt) $1)
                 ((continue_stmt) $1)
                 ((return_stmt) $1)
                 ((raise_stmt) $1)
                 ((yield_stmt) $1))
      (break_stmt ((break)
                   (make-object break% $1-start-pos $1-end-pos)))
      (continue_stmt ((continue)
                      (make-object continue% $1-start-pos $1-end-pos)))
      (return_stmt ((return tuple_or_test)
                    (make-object return% $2 $1-start-pos $2-end-pos))
                   ((return)
                    (make-object return% #f $1-start-pos $1-end-pos)))
      (yield_stmt ((yield tuple_or_test)
                   (make-object yield% $2 $1-start-pos $2-end-pos)))
      (raise_stmt ((raise)
                   (make-object raise% #f #f #f $1-start-pos $1-end-pos))
                  ((raise test)
                   (make-object raise% $2 #f #f $1-start-pos $2-end-pos))
                  ((raise test |,| test)
                   (make-object raise% $2 $4 #f $1-start-pos $4-end-pos))
                  ((raise test |,| test |,| test)
                   (make-object raise% $2 $4 $6 $1-start-pos $6-end-pos)))
      (import_stmt ((import_stmt1)
                    (make-object import-module% (reverse $1) $1-start-pos $1-end-pos))
                   ((from dotted_name import *)
                    (make-object import-from% $2 '* $1-start-pos $1-end-pos))
                   ((from dotted_name import import_stmt2)
                    (make-object import-from% $2 $4 $1-start-pos $1-end-pos)))
      (import_stmt1 ((import dotted_as_name) (list $2))
                    ((import_stmt1 |,| dotted_as_name) (cons $3 $1)))
      (import_stmt2 ((import_as_name) (list $1))
                    ((import_as_name |,| import_stmt2) (cons $1 $3)))
      (import_as_name ((ident NAME ident) 
                       (if (string=? $2 "as")
                           (list $1 $3)
                           (send (make-object ast-node% $2-start-pos $2-end-pos) stx-err
                                 (format "Expected to be '~a as ~a'" $1 $3))))
                      ((ident) (list $1 #f)))
      (dotted_as_name ((dotted_name NAME ident)
                       (if (string=? $2 "as")
                           (list $1 $3)
                           (send (make-object ast-node% $2-start-pos $2-end-pos) stx-err
                                 (format "Expected to be '~a as ~a'" $1 $3))))
                      ((dotted_name) (list $1 #f)))
      (dotted_name ((ident) (list $1))
                   ((ident |.| dotted_name) (cons $1 $3)))
      (global_stmt ((global ident) (list $2))
                   ((global_stmt |,| ident) (cons $3 $1)))
      (exec_stmt ((exec expr)
                  (make-object exec% $2 #f #f $1-start-pos $2-end-pos))
                 ((exec expr in test)
                  (make-object exec% $2 $4 #f $1-start-pos $4-end-pos))
                 ((exec expr in test |,| test)
                  (make-object exec% $2 $4 $6 $1-start-pos $6-end-pos)))
      (assert_stmt ((assert test)
                    (make-object assert% $2 #f $1-start-pos $2-end-pos))
                   ((assert test |,| test)
                    (make-object assert% $2 $4 $1-start-pos $4-end-pos)))
      (compound_stmt ((if_stmt) $1)
                     ((while_stmt) $1)
                     ((for_stmt) $1)
                     ((try_stmt) $1)
                     ((funcdef) $1)
                     ((classdef) $1))
      (if_stmt ((if test : suite elif_list)
                (make-object if% (cons (list $2 $4) (reverse $5)) #f $1-start-pos $5-end-pos))
               ((if test : suite elif_list else : suite)
                (make-object if% (cons (list $2 $4) (reverse $5)) $8 $1-start-pos $8-end-pos)))
      (elif_list (() null) 
                 ((elif_list elif test : suite) (cons (list $3 $5) $1)))
      (while_stmt ((while test : suite)
                   (make-object while% $2 $4 #f $1-start-pos $4-end-pos))
                  ((while test : suite else : suite)
                   (make-object while% $2 $4 $7 $1-start-pos $7-end-pos)))
      (for_stmt ((for target_tuple_or_expr in tuple_or_test : suite)
                 (make-object for% $2 $4 $6 #f $1-start-pos $6-end-pos))
                ((for target_tuple_or_expr in tuple_or_test : suite else : suite)
                 (make-object for% $2 $4 $6 $9 $1-start-pos $9-end-pos)))
      (try_stmt ((try : suite except_clause_list_plus)
                 (make-object try-except% $3 (reverse $4) #f $1-start-pos $4-end-pos))
                ((try : suite except_clause_list_plus else : suite)
                 (make-object try-except% $3 (reverse $4) $7 $1-start-pos $7-end-pos))
                ((try : suite finally : suite)
                 (make-object try-finally% $3 $6 $1-start-pos $6-end-pos)))
      (except_clause_list_plus ((except_clause : suite) (list (append $1 (list $3))))
                               ((except_clause_list_plus except_clause : suite)
                                (cons (append $2 (list $4)) $1)))
      (except_clause ((except) (list #f #f))
                     ((except test) (list $2 #f))
                     ((except test |,| test) (list $2 $4)))
      (suite ((simple_stmt)
              (make-object suite% $1 $1-start-pos $1-end-pos))
             ((NEWLINE INDENT stmt_list_plus DEDENT)
              (make-object suite% (reverse $3) $1-start-pos $4-end-pos)))
      (stmt_list_plus ((stmt) $1)
                      ((stmt_list_plus stmt) (append $2 $1)))
      (test ((or_test) $1)
            ((lambdef) $1))
      (or_test ((and_test) $1)
               ((or_test or and_test) 
                (make-object binary% $1 'or $3 $1-start-pos $3-end-pos)))
      (and_test ((not_test) $1)
                ((and_test and not_test)
                 (make-object binary% $1 'and $3 $1-start-pos $3-end-pos)))
      (not_test ((not not_test)
                 (make-object unary% 'not $2 $1-start-pos $2-end-pos))
                ((comparison)
                 (if (> (length $1) 1)
                     (make-object comparison% (reverse $1) $1-start-pos $1-end-pos)
                     (car $1))))
      (comparison ((expr) (list $1))
                  ((comparison comp_op expr) (cons $3 (cons $2 $1))))
      (comp_op ((<) '<)
               ((>) '>)
               ((==) '==)
               ((>=) '>=)
               ((<=) '<=)
               ((<>) '<>)
               ((!=) '!=)
               ((in) 'in)
               ((not in) 'notin)
               ((is) 'is)
               ((is not) 'isnot))
      (expr ((xor_expr) $1)
            ((expr \| xor_expr) 
             (make-object binary% $1 '\| $3 $1-start-pos $3-end-pos)))
      (xor_expr ((and_expr) $1)
                ((xor_expr ^ and_expr)
                 (make-object binary% $1 '^ $3 $1-start-pos $3-end-pos)))
      (and_expr ((shift_expr) $1)
                ((and_expr & shift_expr)
                 (make-object binary% $1 '& $3 $1-start-pos $3-end-pos)))
      (shift_expr ((arith_expr) $1)
                  ((shift_expr << arith_expr)
                   (make-object binary% $1 '<< $3 $1-start-pos $3-end-pos))
                  ((shift_expr >> arith_expr)
                   (make-object binary% $1 '>> $3 $1-start-pos $3-end-pos)))
      (arith_expr ((term) $1)
                  ((arith_expr + term)
                   (make-object binary% $1 '+ $3 $1-start-pos $3-end-pos))
                  ((arith_expr - term)
                   (make-object binary% $1 '- $3 $1-start-pos $3-end-pos)))
      (term ((factor) $1)
            ((term * factor)
             (make-object binary% $1 '* $3 $1-start-pos $3-end-pos))
            ((term / factor)
             (make-object binary% $1 '/ $3 $1-start-pos $3-end-pos))
            ((term % factor)
             (make-object binary% $1 '% $3 $1-start-pos $3-end-pos))
            ((term // factor)
             (make-object binary% $1 '// $3 $1-start-pos $3-end-pos)))
      (factor ((+ factor)
               (make-object unary% '+ $2 $1-start-pos $2-end-pos))
              ((- factor)
               (make-object unary% '- $2 $1-start-pos $2-end-pos))
              ((~ factor)
               (make-object unary% '~ $2 $1-start-pos $2-end-pos))
              ((power) $1))
      (power ((atom trailer_list)
              (build-primary $1 $2 $1-start-pos))
             ((atom trailer_list ** factor)
              (let ((p (build-primary $1 $2 $1-start-pos)))
                (make-object binary% p '** $4 $1-start-pos $4-end-pos))))
      (trailer_list (() null)
                    ((trailer trailer_list) (cons $1 $2)))
      (atom ((|(| tuple_or_test |)|) $2)
            ((|[| listmaker |]|) $2)
            ((|{| dictmaker |}|)
             (make-object dictionary-display% $2 $1-start-pos $3-end-pos))
            ((|(| |)|)
             (make-object tuple% null $1-start-pos $2-end-pos))
            ((|[| |]|)
             (make-object list-display% null $1-start-pos $2-end-pos))
            ((|{| |}|)
             (make-object dictionary-display% null $1-start-pos $2-end-pos))
            ((|`| tuple_or_test |`|)
             (make-object string-conversion% $2 $1-start-pos $3-end-pos))
            ((ident) $1)
            ((NUMBER)
             (make-object literal% $1 $1-start-pos $1-end-pos))
            ((string_list_plus)
             (make-object literal% $1 $1-start-pos $1-end-pos)))
      (string_list_plus ((STRING) $1)
                        ((STRING string_list_plus) (string-append $1 $2)))
      (listmaker ((test list_for)
                  (make-object list-comprehension% $1 $2 $1-start-pos $2-end-pos))
                 ((testlist)
                  (make-object list-display% $1 $1-start-pos $1-end-pos))
                 ((test)
                  (make-object list-display% (list $1) $1-start-pos $1-end-pos)))
      (lambdef ((lambda varargslist : test)
                (make-object lambda% 
                  (make-object parameters% $2 $2-start-pos $2-end-pos) $4 $1-start-pos $4-end-pos))
               ((lambda : test)
                (make-object lambda%
                  (make-object parameters% null $2-start-pos $2-end-pos) $3 $1-start-pos $3-end-pos)))
      (trailer ((|(| |)|)  `(call ,null ,$2-end-pos))
               ((|(| arglist |)|) `(call ,$2 ,$3-end-pos))
               ((|[| subscriptlist |]|) `(sub-multi ,$2 ,$3-end-pos))
               ((|[| subscript |]|) `(sub-single ,$2 ,$3-end-pos))
               ((|.| ident) `(att ,$2 ,$2-end-pos)))
      (subscriptlist ((subscript |,|) (list $1))
                     ((subscript |,| subscript) (list $1 $3))
                     ((subscript |,| subscriptlist) (cons $1 $3)))
      (subscript ((:) (list #f #f))
                 ((: test) (list #f $2))
                 ((test :) (list $1 #f))
                 ((test : test) (list $1 $3))
                 ((: sliceop) (list #f #f $2))
                 ((: test sliceop) (list #f $2 $3))
                 ((test : sliceop) (list $1 #f $3))
                 ((test : test sliceop) (list $1 $3 $4))
                 ((test) $1)
                 ((|.| |.| |.|) '...))
      (sliceop ((:) #f)
               ((: test) $2))
      (exprlist ((expr |,|) (list $1))
                ((expr |,| expr) (list $1 $3))
                ((expr |,| exprlist) (cons $1 $3)))
      (tuple_or_test ((tuple) $1)
                     ((test) $1))
      (tuple ((testlist) (make-object tuple% $1 $1-start-pos $1-end-pos)))
      (testlist ((test |,|) (list $1))
                ((test |,| test) (list $1 $3))
                ((test |,| testlist) (cons $1 $3)))
      (target_tuple_or_expr ((target_tuple) $1)
                            ((expr) $1))
      (target_tuple ((exprlist) (make-object tuple% $1 $1-start-pos $1-end-pos)))
      (testlist_safe ((test) (list $1))
                     ((test |,| testlist) (cons $1 $3)))
      (dictmaker ((test : test) `((,$1 ,$3)))
                 ((test : test |,|) `((,$1 ,$3)))
                 ((test : test |,| dictmaker) (cons `(,$1 ,$3) $5)))
      (classdef ((class ident : suite)
                 (make-object class-definition% $2 null $4 $1-start-pos $4-end-pos))
                ((class ident |(| test |)| : suite)
                 (make-object class-definition% $2 (list $4) $7 $1-start-pos $7-end-pos))
                ((class ident |(| testlist |)| : suite)
                 (make-object class-definition% $2 $4 $7 $1-start-pos $7-end-pos)))
      (arglist ((** test) (list `(dict ,$2)))
               ((* test) (list `(seq ,$2)))
               ((* test |,| ** test) (list `(seq ,$2) `(dict ,$5)))
               ((argument) (list $1))
               ((argument |,|) (list $1))
               ((argument |,| arglist) (cons $1 $3)))
      (argument ((test) `(pos ,$1))
                ((ident = test) `(key ,$1 ,$3)))
      (list_iter ((list_for) $1)
                 ((list_if) $1))
      (list_for ((for target_tuple_or_expr in testlist_safe)
                 (let ((l (if (null? (cdr $4))
                              (car $4)
                              (make-object tuple% $4 $4-start-pos $4-end-pos))))
                   (make-object list-for% $2 l #f $1-start-pos $4-end-pos)))
                ((for target_tuple_or_expr in testlist_safe list_iter)
                 (let ((l (if (null? (cdr $4))
                              (car $4)
                              (make-object tuple% $4 $4-start-pos $4-end-pos))))
                   (make-object list-for% $2 l $5 $1-start-pos $5-end-pos))))
      (list_if ((if test)
                (make-object list-if% $2 #f $1-start-pos $2-end-pos))
               ((if test list_iter)
                (make-object list-if% $2 $3 $1-start-pos $3-end-pos)))))))

