(module sexp-to-re mzscheme
  
  ;; the parse function that builds an ast.
  
  (require "structs.ss"
	   (lib "list.ss"))
  (provide parse)
  
  ;; The input should be a syntax object which conforms to:
  ;; re = char                       match the given character
  ;;    | (make-marker nat)          cannot match, a placeholder used in the 
  ;;                                   dfa algorithm
  ;;    | symbol                     match the sequence of chars in the symbol
  ;;    | string                     match its sequence of characters
  ;;    | (symbol)                   expand the lex abbreviation named symbol
  ;;    | (* re)                     match 0 or more re
  ;;    | (+ re)                     match 1 or more re
  ;;    | (? re)                     match 0 or 1 re
  ;;    | (: re ...1)                match one of the listed re
  ;;    | (@ re ...)                 match each re in succession
  ;;    | (- char char)              match any character between two (inclusive)
  ;;    | (^ char_or_range ...1)     match any character not listed
  ;; (the null concatenation `(@) means epsilon)
  
  
  ;; make-range : int * int -> char list
  ;; creates a list of all chars between i and j.  i <= j
  (define (make-range i j)
    (letrec ((make-range 
              (lambda (i j)
                (cond
                  ((= i j) (list (integer->char i)))
                  (else 
                   (cons (integer->char i) (make-range (add1 i) j)))))))
      (make-range i j)))
  
  ;; parse-assoc: ('a * 'a -> 'b) * 'a list -> 'b
  ;; like foldr, but l must have length at least 2, and the base case is
  ;; combine of the last 2 elements of l
  (define (parse-assoc combine l)
    (cond
      ((null? (cddr l)) (combine (car l) (cadr l)))
      (else
       (combine (car l) (parse-assoc combine (cdr l))))))
  
  ;; union : char list * char list -> char list
  ;; Combines 2 sorted, duplicate-free lists into 1, removing duplicates.
  (define (union l1 l2)
    (cond
      ((null? l2) l1)
      ((null? l1) l2)
      (else (let ((cl1 (car l1))
                  (cl2 (car l2)))
              (cond
                ((> (char->integer cl1) (char->integer cl2))
                 (cons cl2 (union l1 (cdr l2))))
                ((< (char->integer cl1) (char->integer cl2))
                 (cons cl1 (union (cdr l1) l2)))
                (else (union (cdr l1) l2)))))))
  
  ;; group-chars: re-ast list -> re-ast list
  ;; Takes all the (character containing) syms in l and combines them into 1 
  ;; element
  (define (group-chars l)
    (letrec (
             ;; group : re-ast list * char list * re-ast list -> re-ast list
             (group
              (lambda (l s acc)
                (cond
                  ((null? l) (cons (make-syms s -1) acc))
                  ((and (syms? (car l)) (list? (syms-chars (car l))))
                   (group (cdr l) (union (syms-chars (car l)) s) acc))
                  (else (group (cdr l) s (cons (car l) acc)))))))
      (group l null null)))
  
  (define (num-arg-err s expect given)
    (raise-syntax-error
     #f
     (format "expects ~a arguments, given ~a" expect given)
     s))
  
  ;; parse : syntax-object -> re-ast
  ;; checks for errors and generates the ast for s
  (define (parse s)
    (syntax-case s ()
      (obj
       (char? (syntax-object->datum (syntax obj)))
       (make-syms (list (syntax-object->datum (syntax obj))) -1))
      (obj
       (marker? (syntax-object->datum (syntax obj)))
       (make-syms (syntax-object->datum (syntax obj)) -1))
      (obj
       (or (string? (syntax-object->datum (syntax obj))) 
           (symbol? (syntax-object->datum (syntax obj))))
       (let* ((obj (syntax-object->datum (syntax obj)))
              (l (string->list 
                  (if (symbol? obj)
                      (symbol->string obj)
                      obj))))
         (cond
           ((= 1 (length l)) (make-syms l -1))
           (else (parse-assoc make-concat 
                              (map (lambda (x) (make-syms (list x) -1)) l))))))
      (()
       (printf "here~n")
       (raise-syntax-error
        'regular-expression
        "() is not valid"
        s))
      ((oper args ...)
       (let* ((op (syntax-object->datum (syntax oper)))
              (ar (syntax->list (syntax (args ...))))
              (num-args (length ar))
              (expand (syntax-local-value (syntax oper) (lambda () #f))))
         (cond
           ((and (eq? op 'eof) (= num-args 0))
            (make-syms eof -1))
           ((eq? op 'eof)
            (num-arg-err s 0 num-args))
           ((and (eq? op '*) (= 1 num-args))
            (make-kstar (parse (car ar))))
           ((eq? op '*)
            (num-arg-err s 1 num-args))
           ((and (eq? op '+) (= num-args 1))
            (let ((arg (parse (car ar))))
              (make-concat arg (make-kstar arg))))
           ((eq? op '+)
            (num-arg-err s 1 num-args))
           ((and (eq? op '?) (= num-args 1))
            (make-altern (make-epsilon) (parse (car ar))))
           ((eq? op '?)
            (num-arg-err s 1 num-args))
           ((and (eq? op ':) (> num-args 1))
            (let* ((parts (group-chars (map parse ar)))
                   (l (length parts)))
              (if (= 1 l)
                  (car parts)
                  (parse-assoc make-altern parts))))
           ((and (eq? op ':) (= num-args 1))
            (parse (car ar)))
           ((eq? op ':)
            (num-arg-err s "at least 1" num-args))
           ((and (eq? op '@) (+ 0 num-args))
            (make-epsilon))
           ((and (eq? op '@) (= 1 num-args))
            (parse (car ar)))
           ((and (eq? op '@))
            (parse-assoc make-concat (map parse ar)))
           ((and (eq? op '-) (= num-args 2))
            (let ((c1 (parse (car ar)))
                  (c2 (parse (cadr ar))))
              (if (and (syms? c1) (syms? c2)
                       (list? (syms-chars c1)) (list? (syms-chars c2))
                       (null? (cdr (syms-chars c1))) 
                       (null? (cdr (syms-chars c2))))
                  (let ((i1 (char->integer (car (syms-chars c1))))
                        (i2 (char->integer (car (syms-chars c2)))))
                    (if (<= i1 i2)
                        (make-syms (make-range i1 i2) -1)
                        (raise-syntax-error
                         #f
                         s
                         (format "first argument ~a does not preceed second argument ~a" c1 c2))))
                  ((eq? op '-)
                   (raise-syntax-error
                    #f
                    (format "operation expects single character arguments, given ~a and ~a"
                            (car ar) (cadr ar))
                    s)))))
           ((eq? op '-)
            (num-arg-err s 2 num-args))
           ((and (eq? op '^) (> num-args 0))
            (letrec ((res (map parse ar))
                     (v (make-vector 256 #t)))
              (if (not (andmap (lambda (x)
                                 (and (syms? x)
                                      (list? (syms-chars x))))
                               res))
                  (raise-syntax-error
                   #f
                   (format 
                    "expression expects single character or character range arguments, given ~a"
                    (syntax-object->datum ar)
                    s)))
              (for-each (lambda (sym)
                          (for-each (lambda (char)
                                      (vector-set! v (char->integer char) #f))
                                    (syms-chars sym)))
                        res)
              (make-syms
               (let loop ((i 0))
                 (cond
                   ((= i 256) null)
                   ((vector-ref v i)
                    (cons (integer->char i) (loop (add1 i))))
                   (else
                    (loop (add1 i)))))
               -1)))
           ((eq? op '^)
            (num-arg-err s "at least 1" num-args))
           ((and (lex-abbrev? expand))
            (if (= 0 num-args)
                (parse (lex-abbrev-abbrev expand))
                (num-arg-err s 0 num-args)))
           (else
            (raise-syntax-error
             #f
             "invalid operator or abbreviation"
             s)))))
      (_
       (raise-syntax-error
        #f
        s
        "invalid regular expression"))))
  )
        





