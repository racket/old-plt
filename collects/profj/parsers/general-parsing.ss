#cs
(module general-parsing mzscheme
  
  (require (lib "lex.ss" "parser-tools")
           (lib "string.ss")
           (lib "list.ss"))
  (require "../ast.ss" "../parameters.ss" "lexer.ss")
  
  (provide (all-defined))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Methods used by all generated parsers
  (define-syntax (build-src stx)
    (syntax-case stx ()
      ((_ end)
       (syntax (build-src 1 end)))
      ((_ start end)
       (with-syntax ((start-pos (datum->syntax-object 
                                 (syntax end)
                                 (string->symbol 
                                  (format "$~a-start-pos"
                                          (syntax-object->datum (syntax start))))))
                     (end-pos (datum->syntax-object 
                               (syntax end)
                               (string->symbol 
                                (format "$~a-end-pos"
                                        (syntax-object->datum (syntax end)))))))
         (syntax
          (make-src (position-line start-pos)
                    (position-col start-pos)
                    (+ (position-offset start-pos) (interactions-offset))
                    (- (position-offset end-pos)
                       (position-offset start-pos))))))))
  
  (define (construct-method-header mods type-parms ret-type declarator throws)
    (make-method mods 
                 (make-type-spec (type-spec-name ret-type)
                                 (+ (type-spec-dim ret-type) (caddr declarator))
                                 (type-spec-src ret-type))
                 type-parms
                 (car declarator)
                 (cadr declarator)
                 throws
                 #f
                 #f
                 #f))
  
  (define (name->access n)
    (make-access #f
                 (name-src n)
                 (append (name-path n) (list (name-id n)))))
  
  (define (access->name a)
    (make-name (car (reverse (access-name a)))
               (cdr (access-name a))
               (expr-src a)))
  
  (define (build-name-call name args src-loc)
    (make-call #f src-loc 
               (if (null? (name-path name))
                   #f
                   (make-access #f 
                                (name-src name)
                                (name-path name)))
               (name-id name)
               args 
               #f))
  
  (define (build-field-decl mods type decl)
    (cond
      ((var-decl? decl)
       (make-var-decl (var-decl-name decl)
                      mods
                      (make-type-spec
                       (type-spec-name type)
                       (+ (type-spec-dim type) 
                          (type-spec-dim (var-decl-type decl)))
                       (type-spec-src type))
                      (var-decl-src decl)))
      ((var-init? decl)
       (make-var-init
        (build-field-decl mods type (var-init-var-decl decl))
        (var-init-init decl)
        (var-init-src decl)))))

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Token Accessors and Queries for error-messaging parsers
  
  (define-syntax define-sym-token?
    (syntax-rules ()
                  [(_ name check)
                   (define (name token) (and (symbol? token) (eq? token check)))]))
  
  (define-syntax define-token?
    (syntax-rules ()
                  [(_ name check)
                   (define (name token) (and (token? token) (eq? (token-name token) check)))]))

  ;get-token-name: (U symbol token) -> symbol
  (define (get-token-name token)
    (cond
      ((token? token) (token-name token))
      (else token)))
  
  ;Special
  (define-sym-token? eof? 'EOF)
  
  ;Modifiers
  (define-sym-token? abstract? 'abstract)
  (define-sym-token? native? 'native)
  (define-sym-token? private? 'private)
  (define-sym-token? protected? 'protected)
  (define-sym-token? public? 'public)
  (define-sym-token? static? 'static)
  (define-sym-token? strictfp? 'strictfp)
  (define-sym-token? transient? 'transient)
  (define-sym-token? volatile? 'volatile)
  (define-sym-token? final? 'final)
  (define (modifier-token? token)
    (and (symbol? token) (memq token `(abstract native private protected public static strictfp transient volatile))))
  
  ;Literals
  (define (literal-token? token)
    (or (empty-literal? token) (full-literal? token)))
  (define (empty-literal? token)
    (and (symbol? token) (memq token `(NULL_LIT TRUE_LIT FALSE_LIT))))
  (define (full-literal? token)
    (and (token? token) (memq (token-name token) `(STRING_LIT CHAR_LIT INTEGER_LIT LONG_LIT FLOAT_LIT DOUBLE_LIT))))
  
  ;Primitive types
  (define (prim-type? token)
    (and (symbol? token) (memq token `(boolean byte char double float int long short))))
  
  ;Operators
  (define (bin-operator? token)
    (memq (get-token-name token) `(PIPE OR > < == <= >= != && + - * / & ^ % << >> >>>)))
  (define (unary-end? token)
    (memq (get-token-name token) `(++ --)))
  (define (if-exp? token)
    (eq? (get-token-name token) '?))
  (define (teaching-unary-operator? token)
    (and (symbol? token) (memq token `(! ~))))
  (define (unary-operator? token)
    (and (symbol? token) (memq token `(! ~ ++ --))))
  (define (teaching-assignment-operator? token)
    (and (symbol? token) (eq? token '=)))
  (define (assignment-operator? token)
    (and (symbol? token) (memq token `(= += -= *= /= &= ^= %= <<= >>= >>>=))))
  
  ;Separators
  (define-token? o-paren? 'O_PAREN)
  (define-token? c-paren? 'C_PAREN)
  (define-token? o-brace? 'O_BRACE)
  (define-token? c-brace? 'C_BRACE)
  (define-token? o-bracket? 'O_BRACKET)
  (define-token? c-bracket? 'C_BRACKET)
  (define-token? star? '*)
  (define-token? semi-colon? 'SEMI_COLON)
  (define-token? colon? 'COLON)
  (define-token? dot? 'PERIOD)
  (define-token? comma? 'COMMA)
  
  (define (separator? tok)
    (or (open-separator? tok) (close-separator? tok)
        (memq (get-token-name tok) `(SEMI_COLON PERIOD COMMA))))
  (define (open-separator? tok)
    (memq (get-token-name tok) `(O_PAREN O_BRACE O_BRACKET)))
  (define (close-separator? tok)
    (memq (get-token-name tok) `(C_PAREN C_BRACE C_BRACKET)))
  
  ;top-level keywords
  (define-sym-token? package-token? 'package)
  (define-sym-token? import-token? 'import)
  
  ;Definition keywords
  (define-sym-token? class? 'class)
  (define-sym-token? extends? 'extends)
  (define-sym-token? implements? 'implements)
  (define-sym-token? interface? 'interface)
  
  ;Method keywords
  (define-sym-token? const? 'const)
  (define-sym-token? throws-token? 'throws)
  (define-sym-token? void-token? 'void)
  
  ;Statement keywords
  (define-sym-token? break-token? 'break)
  (define-sym-token? case-token? 'case)
  (define-sym-token? catch-token? 'catch)
  (define-sym-token? continue-token? 'continue)
  (define-sym-token? defualt? 'default)
  (define-sym-token? do-token? 'do)
  (define-sym-token? else? 'else)
  (define-sym-token? finally? 'finally)
  (define-sym-token? for-token? 'for)
  (define-sym-token? goto? 'goto)
  (define-sym-token? if-token? 'if)
  (define-sym-token? return-token? 'return)
  (define-sym-token? switch-token? 'switch)
  (define-sym-token? synchronized-token? 'synchronized)
  (define-sym-token? throw-token? 'throw)
  (define-sym-token? try-token? 'try)
  (define-sym-token? while-token? 'while)

  ;Expression tokens
  (define-sym-token? instanceof-token? 'instanceof)
  (define-sym-token? new-token? 'new)
  (define-sym-token? super? 'super)
  (define-sym-token? this? 'this)
  (define-sym-token? cond? '?)
  (define-token? id-token? 'IDENTIFIER)
  
  ;keyword? lex-token -> bool
  (define (keyword? t)
    (or (memq (get-token-name t) `(? this super new instanceof while try throw synchronized switch return if goto for finally
                                     else do default continue catch case break void throws const interface implements extends
                                     class import package))
        (assignment-operator? t)
        (prim-type? t)
        (modifier? t)))
  
  ;only looks for incorrect capitalization at this point, intend to add 1-off spelling errors for at least some keywords
  ;close-to-keyword? token (opt symbol )-> bool
  (define (close-to-keyword? t . args)
    (if (id-token? t)
        (let ((s (string-copy (token-value t))))
          (string-lowercase! s)
          (if (null? args)
              (or (keyword? (string->symbol s))
                  (member s all-words))
              (or (eq? (string->symbol s) (car args))
                  (member s (select-words (car args))))))
        #f))
  
  (define misspelled-list '((import "mport" "iport" "imort" "imprt" "impot" "impor" "improt")
                            (class "lass" "cass" "clss" "clas" "calss")
                            (abstract 
                             "bstract" "astract" "abtract" "absract" "abstact" "abstrct" "abstrat" "abstract" "abstarct" "abstracts")
                            (extends "xtends" "etends" "exends" "extnds" "exteds" "extens" "extneds" "extend")
                            (new "nw" "ne" "nwe")
                            (this "his" "tis" "ths" "thi" "tihs" "thsi")
                            (if "fi")
                            (else "lse" "ese" "els" "eles")
                            (return "eturn" "rturn" "reurn" "retrn" "retun" "retur" "reutrn" "retrun" "returns")
                            (true "rue" "tue" "tre" "tru" "ture" "treu")
                            (false "flse" "fase" "fale" "fals" "flase" "fasle")
                            (interface
                                "nterface" "iterface" "inerface" "intrface" "inteface" "interace" "interfce" "interfae" "intreface")
                            (implements 
                             "mplements" "iplements" "impements" "implments" "impleents" "implemnts" "implemets" "implemens"
                             "implement")
                            (void "oid" "vid" "voi" "viod")
                            (super "uper" "sper" "supr" "supe" "supper")
                            (public "ublic" "pblic" "pulic" "pubic" "publc" "publi" "pubilc")
                            (private "rivate" "pivate" "prvate" "priate" "privte" "privae" "privat" "pravite")
                            (package "ackage" "pckage" "pakage" "pacage" "packge" "packae" "packag")
                            (protected "rotected" "portected")
                            (final "inal" "fnal" "fial" "finl" "finale" "fianl")
                            ))

  (define (select-words key)
    (safe-car (filter (lambda (x) (eq? (car x) key)) misspelled-list)))
  (define (safe-car f)
    (if (null? f) null (car f)))
  
  (define all-words (filter string? (apply append misspelled-list)))
                                
  )