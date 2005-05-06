(module honu-typecheck-exp mzscheme

  (require (lib "struct.ss")
           (lib "contract.ss")
           (all-except (lib "list.ss" "srfi" "1") any))
  
  (require "../../ast.ss")
  (require "../../utils.ss")
  (require "../../tenv.ss")
  (require "honu-type-utils.ss")
  (require "../../read-error-with-stx.ss")

  ;; expects a symbol syntax, returns a type for that builtin
  (define (get-builtin-type stx)
    (case (printable-key stx)
      [(printStr)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-void-type stx)
        stx)]
      [(printLine)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-void-type stx)
        stx)]
      [(error)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-error-type stx)
        stx)]
      [(readChar)
       (honu-func-type-from-exp
        (list)
        (honu-char-type stx)
        stx)]
      [(readLine)
       (honu-func-type-from-exp
        (list)
        (honu-str-type stx)
        stx)]
      [(strToInt)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-int-type stx)
        stx)]
      [(strToFloat)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-float-type stx)
        stx)]
      [(intToStr)
       (honu-func-type-from-exp
        (list (honu-int-type stx))
        (honu-str-type stx)
        stx)]
      [(floatToStr)
       (honu-func-type-from-exp
        (list (honu-float-type stx))
        (honu-str-type stx)
        stx)]
      [(charToStr)
       (honu-func-type-from-exp
        (list (honu-char-type stx))
        (honu-str-type stx)
        stx)]
      [(strLen)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-int-type stx)
        stx)]
      [(substr)
       (honu-func-type-from-exp
        (list (honu-str-type stx)
              (honu-int-type stx)
              (honu-int-type stx))
        (honu-str-type stx)
        stx)]
      [(charAt)
       (honu-func-type-from-exp
        (list (honu-str-type stx)
              (honu-int-type stx))
        (honu-char-type stx)
        stx)]
      [else #f]))
  
  ;; honu-typecheck-exp : HPgm * Env * CEnv -> HExp -> HExp * Typ
  ;;
  ;; honu-typecheck-exp typechecks a honu expression given the honu
  ;; program in which it appears along with the current local
  ;; environment and class environment.
  ;;
  ;; Note that we curry the expression out to the right -- we do this
  ;; because for most expressions, nothing will change except for
  ;; running the typechecker recursively on subexpressions.
  ;;
  ;; We could likewise curry the program out to the left similarly
  ;; since the program will never change for a given program (haha),
  ;; but there would be better ways of handling that in either a
  ;; functional and imperative style such as either having a
  ;; global/parameterized "current-program" variable or by currying
  ;; out to the left and then writing one function that given a
  ;; program, applies the curried functions to it once, returning the
  ;; partially applied functions.  I should get things working before
  ;; getting that silly though.
;  (provide honu-typecheck-exp)
  (provide/contract [honu-typecheck-exp
                     (tenv?
                      any/c
                      any/c
                      . -> .
                      ((honu-exp?)
                       . ->* .
                       (honu-exp? honu-type?)))]) 
  (define (honu-typecheck-exp tenv env cenv)
    (define (f exp)
      (cond
       ;;            P |- t
       ;; ----------------------------
       ;; P, G, D |- null |=> null : t
       ;;
       ;; Since there's no easy way to do the above in this style of
       ;; typechecker, we'll create a "null" type that for every
       ;; type t such that P |- t, null <: t.
       ((honu-null? exp)
        (values exp (honu-null-type exp)))
       ;; P, G, D |- n |=> n : int
       ((honu-int? exp)
        (values exp (honu-int-type exp)))
       ;; P, G, D |- f |=> f : float
       ((honu-float? exp)
        (values exp (honu-float-type exp)))
       ;; P, G, D |- b |=> b : bool
       ((honu-bool? exp)
        (values exp (honu-bool-type exp)))
       ;; P, G, D |- s |=> s : str
       ((honu-str? exp)
        (values exp (honu-str-type exp)))
       ;; P, G, D |- c |=> c : char
       ((honu-char? exp)
        (values exp (honu-char-type exp)))
       [(honu-uprim? exp)
        (let-values (((e1 t1) (f (honu-uprim-body exp))))
          (case (honu-uprim-op exp)
            [(minus)
             (cond
               [(honu-type-equal? t1 (honu-int-type (honu-uprim-body exp)))
                (values (copy-struct honu-uprim exp
                          (honu-uprim-op-type t1)
                          (honu-uprim-body  e1))
                        (honu-int-type exp))]
               [(honu-type-equal? t1 (honu-float-type (honu-uprim-body exp)))
                (values (copy-struct honu-uprim exp
                          (honu-uprim-op-type t1)
                          (honu-uprim-body  e1))
                        (honu-float-type exp))]
               [else
                (raise-read-error-with-stx
                 "Unary minus takes an integer or floating point argument."
                 (honu-ast-src-stx (honu-uprim-body exp)))])]
            [(not)
             (if (honu-type-equal? t1 (honu-bool-type (honu-uprim-body exp)))
                 (values (copy-struct honu-uprim exp
                                (honu-uprim-op-type t1)
                                (honu-uprim-body  e1))
                              (honu-bool-type exp))
                 (raise-read-error-with-stx
                  "Unary not takes a boolean argument."
                  (honu-ast-src-stx (honu-uprim-body exp))))]
            [else (raise-read-error-with-stx
                   "Unknown unary primitive operation."
                   (honu-uprim-op-stx exp))]))]
       [(honu-prim? exp)
        (let-values (((e1 t1) (f (honu-prim-left  exp)))
                     ((e2 t2) (f (honu-prim-right exp))))
          (case (honu-prim-op exp)
            ;; +, -, *, /, and % are int * int -> int operators.
            ;;
            ;;  P, G, D |- e1 |=> e1' : int  P, G, D |- e2 |=> e2' : int
            ;;  --------------------------------------------------------
            ;;           P, G, D |- e1 op e2 |=> e1' op e2' : int
            [(plus)
             (cond
               [(and (honu-type-equal? t1 (honu-int-type (honu-prim-left exp)))
                     (honu-type-equal? t2 (honu-int-type (honu-prim-right exp))))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-int-type exp))]
               [(and (honu-type-equal? t1 (honu-float-type (honu-prim-left exp)))
                     (honu-type-equal? t2 (honu-float-type (honu-prim-right exp))))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-float-type exp))]
               [(and (honu-type-equal? t1 (honu-str-type (honu-prim-left exp)))
                     (honu-type-equal? t2 (honu-str-type (honu-prim-right exp))))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-str-type exp))]
               [else
                (raise-read-error-with-stx
                 "Types of operands do not match or are not of appropriate types."
                 (honu-ast-src-stx exp))])]
            [(minus times div)
             (cond
               [(and (honu-type-equal? t1 (honu-int-type (honu-prim-left exp)))
                     (honu-type-equal? t2 (honu-int-type (honu-prim-right exp))))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-int-type exp))]
               [(and (honu-type-equal? t1 (honu-float-type (honu-prim-left exp)))
                     (honu-type-equal? t2 (honu-float-type (honu-prim-right exp))))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-float-type exp))]
               [else
                (raise-read-error-with-stx
                 "Types of operands do not match or are not of appropriate types."
                 (honu-ast-src-stx exp))])]
            [(mod)
             (if (honu-type-equal? t1 (honu-int-type (honu-prim-left exp)))
                 (if (honu-type-equal? t2 (honu-int-type (honu-prim-right exp)))
                     (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-int-type exp))
                     (raise-read-error-with-stx
                      "Integer operator applied to non-integer operand."
                      (honu-ast-src-stx (honu-prim-right exp))))
                 (raise-read-error-with-stx
                  "Integer operator applied to non-integer operand."
                  (honu-ast-src-stx (honu-prim-left exp))))]
            [(lt le gt ge)
             (cond
               [(and (honu-type-equal? t1 (honu-int-type (honu-prim-left exp)))
                     (honu-type-equal? t2 (honu-int-type (honu-prim-right exp))))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-bool-type exp))]
               [(and (honu-type-equal? t1 (honu-float-type (honu-prim-left exp)))
                     (honu-type-equal? t2 (honu-float-type (honu-prim-right exp))))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-bool-type exp))]
               [(and (honu-type-equal? t1 (honu-str-type (honu-prim-left exp)))
                     (honu-type-equal? t2 (honu-str-type (honu-prim-right exp))))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-bool-type exp))]
               [(and (honu-type-equal? t1 (honu-char-type (honu-prim-left exp)))
                     (honu-type-equal? t2 (honu-char-type (honu-prim-right exp))))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-bool-type exp))]
               [else
                (raise-read-error-with-stx
                 "Types of operands do not match or are not of appropriate types."
                 (honu-ast-src-stx exp))])]
            ;; && and || are bool * bool -> bool operators.
            ;;
            ;;  P, G, D |- e1 |=> e1' : bool  P, G, D |- e2 |=> e2' : bool
            ;;  ----------------------------------------------------------
            ;;           P, G, D |- e1 op e2 |=> e1' op e2' : bool
            [(and or)
             (if (honu-type-equal? t1 (honu-bool-type (honu-prim-left exp)))
                 (if (honu-type-equal? t2 (honu-bool-type (honu-prim-right exp)))
                     (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-bool-type exp))
                     (raise-read-error-with-stx
                      "Boolean operator applied to non-boolean operand."
                      (honu-ast-src-stx (honu-prim-right exp))))
                 (raise-read-error-with-stx
                  "Boolean operator applied to non-boolean operand."
                  (honu-ast-src-stx (honu-prim-left exp))))]
            ;; For now we just have that the operands to an equality
            ;; operator can be of any type and that the types of the
            ;; operands do not need to be equal.  Might it be the
            ;; case that we want to check if we're comparing two
            ;; primitives and reject if they're not the same type?
            ;;
            ;; Yes, and so we do below.
            ;;
            ;; (old type rule)
            ;;  == is a 'a * 'b -> bool operator.
            ;;
            ;;  P, G, D |- e1 |=> e1' : t1    P, G, D |- e2 |=> e2' : t2
            ;;  --------------------------------------------------------
            ;;          P, G, D |- e1 == e2 |=> e1' == e2' : bool
            [(neq equal)
             (cond
               [(and (<:_P tenv t1 (honu-any-type (honu-prim-left exp)))
                     (<:_P tenv t2 (honu-any-type (honu-prim-right exp))))
                (values (copy-struct honu-prim exp
                          (honu-prim-op-type (honu-any-type (honu-prim-left exp)))
                          (honu-prim-left  e1)
                          (honu-prim-right e2))
                        (honu-bool-type exp))]
               [(honu-type-equal? t1 t2)
                (values (copy-struct honu-prim exp
                          (honu-prim-op-type t1)
                          (honu-prim-left  e1)
                          (honu-prim-right e2))
                        (honu-bool-type exp))]
               [else (raise-read-error-with-stx
                      "Attempt to check two unrelated types for (in)equality."
                      (honu-ast-src-stx exp))])]
            [(clseq)
             (cond
               [(not (<:_P tenv t1 (honu-any-type (honu-prim-left exp))))
                (raise-read-error-with-stx
                 "Expresion on left side of class equality is of primitive type."
                 (honu-ast-src-stx (honu-prim-left exp)))]
               [(not (<:_P tenv t2 (honu-any-type (honu-prim-right exp))))
                (raise-read-error-with-stx
                 "Expresion on right side of class equality is of primitive type."
                 (honu-ast-src-stx (honu-prim-right exp)))]
               [else (values (copy-struct honu-prim exp
                               (honu-prim-op-type (honu-any-type (honu-prim-left exp)))                             
                               (honu-prim-left e1)
                               (honu-prim-right e2))
                             (honu-bool-type exp))])]
            [else (raise-read-error-with-stx
                   "Unknown binary primitive operation."
                   (honu-prim-op-stx exp))]))]
       [(honu-lambda? exp)
        (let ((env (fold (lambda (n t e)
                           (extend-env e n t))
                         env 
                         (honu-lambda-arg-names exp)
                         (honu-lambda-arg-types exp))))
          (let-values (((e1 t1) ((honu-typecheck-exp tenv env cenv)
                                 (honu-lambda-body exp))))
            (values (copy-struct honu-lambda exp
                      (honu-lambda-body e1))
                    (honu-func-type-from-exp (honu-lambda-arg-types exp) t1 exp))))]
       [(honu-facc? exp)
        (if (eqv? (honu-facc-obj exp) 'my)
            ;;            D(fd) = t
            ;; ------------------------------
            ;; P, G, D |- my.fd |=> my.fd : t
            (if (cenv (honu-facc-field exp))
                (values exp (cenv (honu-facc-field exp)))
                (if (env 'this)
                    ;; We're inside a class or mixin, so this is just an invalid name.
                    ;; We do also have the extra case that if we're inside a method, this
                    ;; may have been an init field's name (which are not contained in the
                    ;; class environment passed to honu-typecheck-exp for method bodies).
                    (raise-read-error-with-stx
                     "No local field with this name or attempt to use init field in method."
                     (honu-facc-field exp))
                    (raise-read-error-with-stx
                     "Attempt to use static field access outside of class or mixin body."
                     (honu-ast-src-stx exp))))
            ;; P, G, D |- e |=> e' : t'  <fd, t> in t'
            ;; ---------------------------------------
            ;;     P, G, D |- e.fd |=> e'.fd : t
            (let-values (((e1 t1) (f (honu-facc-obj exp))))
              (let ((field-type (get-field-type tenv t1 (honu-facc-field exp))))
                (if field-type
                    (values (copy-struct honu-facc exp
                              ;; Make sure to elaborate the type
                              (honu-facc-elab t1)
                              (honu-facc-obj e1))
                            field-type)
                    (raise-read-error-with-stx
                     "Field not found in type of object."
                     (honu-facc-field exp))))))]
       [(honu-fassn? exp)
        ;; We will need this whichever branch we go down, so...
        (let-values (((e2 t2) (f (honu-fassn-rhs exp))))
          (if (eqv? (honu-fassn-obj exp) 'my)
            ;; D(fd) = t  P, G, D |- e |=> e' : t'  t' <: t
            ;; --------------------------------------------
            ;;  P, G, D |- my.fd = e |=> my.fd = e' : void
              (if (cenv (honu-fassn-field exp))
                  (if (<:_P tenv t2 (cenv (honu-fassn-field exp)))
                      (values (copy-struct honu-fassn exp
                                           (honu-fassn-rhs e2))
                              (honu-void-type exp))
                      (raise-read-error-with-stx
                       "Type being assigned to field does not match type of field."
                       (honu-ast-src-stx exp)))
                  (if (env 'this)
                      ;; We're inside a class or mixin, so this is just an invalid name.
                      ;; We do also have the extra case that if we're inside a method, this
                      ;; may have been an init field's name (which are not contained in the
                      ;; class environment passed to honu-typecheck-exp for method bodies).
                      (raise-read-error-with-stx
                       "No local field with this name or attempt to use init field in method."
                       (honu-fassn-field exp))
                      (raise-read-error-with-stx
                       "Attempt to use static field assignment outside of class or mixin body."
                       (honu-ast-src-stx exp))))
            ;; P, G, D |- e1 |=> e1' : t'        <fd, t> in t'
            ;; P, G, D |- e2 |=> e2' : t''            t'' <: t
            ;; -----------------------------------------------
            ;;  P, G, D |- e1.fd = e2 |=> e1'.fd = e2' : void
              (let-values (((e1 t1) (f (honu-facc-obj exp))))
                (let ((field-type (get-field-type tenv t1
                                                  (honu-facc-field exp))))
                  (if field-type
                      (if (<:_P tenv t2 field-type)
                          (values (copy-struct honu-fassn exp
                                    (honu-fassn-obj e1)
                                    ;; Make sure to elaborate the type
                                    (honu-fassn-elab t1)
                                    (honu-fassn-rhs e2))
                                  (honu-void-type exp))
                          (raise-read-error-with-stx
                           "Type being assigned to field does not match type of field."
                           (honu-ast-src-stx exp)))
                      (raise-read-error-with-stx
                       "Field not found in type of object."
                       (honu-fassn-field exp)))))))]
       [(honu-mcall? exp)
        ;; We need the arg elaborations and types no matter what, so...
        (let-values (((new-args new-types)
                      (map-two-values f (honu-mcall-args exp))))
          (if (eqv? (honu-mcall-obj exp) 'my)
              ;; D(md) = t_1 ... t_n -> t
              ;; P, G, D |- e_i |=> e_i' : t_i'   t_i' <: t_i  (NOT t_i <: t_i')
              ;; --------------------------------------------
              ;; P, G, D |- my.md(e_1, ..., e_n) |=>
              ;;            my'.md(e_1', ..., e_n') : t
              (if (cenv (honu-mcall-method exp))
                  (let ((method-type (cenv (honu-mcall-method exp))))
                    (if method-type
                        (let loop ((n 0)
                                   (dec-types (honu-func-type-args method-type))
                                   (calc-types new-types))
                          (cond
                            ((null? dec-types)
                             (if (null? calc-types)
                                 ;; We reached the end of both lists, so return
                                 ;; the new expression and the return type.
                                 (values (copy-struct honu-mcall exp
                                                      (honu-mcall-args new-args))
                                         (honu-func-type-return method-type))
                                 ;; calc-types isn't null, so too many arguments
                                 ;; were given in the mcall expression.
                                 (raise-read-error-with-stx
                                  "Too many arguments for method."
                                  (honu-ast-src-stx exp))))
                            ;; dec-types isn't null, so we have too few arguments.
                            ((null? calc-types)
                             (raise-read-error-with-stx
                              "Not enough arguments for method."
                              (honu-ast-src-stx exp)))
                            ;; t_i' <: t_i, so check the next one.
                            ((<:_P tenv (car calc-types) (car dec-types))
                             (loop (+ n 1) (cdr dec-types) (cdr calc-types)))
                            ;; t_i was _not_ <: t_i', so blame the appropriate
                            ;; expression.
                            (else
                             (raise-read-error-with-stx
                              "Argument type is not subtype of declared type."
                              (honu-ast-src-stx (list-ref (honu-mcall-args exp) n))))))
                        ;; method-type was #f, so it couldn't be found in D.
                        (raise-read-error-with-stx
                         "Method not found in current class or mixin."
                         (honu-mcall-method exp))))
                  (if (env 'this)
                      (raise-read-error-with-stx
                       "No local method with this name."
                       (honu-mcall-method exp))
                      (raise-read-error-with-stx
                       "Attempt to use static method call outside of class or mixin body."
                       (honu-ast-src-stx exp))))
              ;; P, G, D |- e |=> e' : t'  <md, t_1 ... t_n -> t> in t'
              ;; P, G, D |- e_i |=> e_i' : t_i'             t_i' <: t_i  (NOT t_i <: t_i')
              ;; ------------------------------------------------------
              ;; P, G, D |- e.md(e_1, ..., e_n) |=>
              ;;            e'.md(e_1', ..., e_n') : t
              (let-values (((e0 t0) (f (honu-mcall-obj exp))))
                (let ((method-type (get-method-type tenv t0
                                                    (honu-mcall-method exp))))
                  (if method-type
                      (let loop ((n 0)
                                 (dec-types (honu-func-type-args method-type))
                                 (calc-types new-types))
                        (cond
                         ((null? dec-types)
                          (if (null? calc-types)
                              ;; We reached the end of both lists, so return
                              ;; the new expression and the return type.
                              (values (copy-struct honu-mcall exp
                                        (honu-mcall-obj e0)
                                        (honu-mcall-elab t0)
                                        (honu-mcall-args new-args))
                                      (honu-func-type-return method-type))
                              ;; calc-types isn't null, so too many arguments
                              ;; were given in the mcall expression.
                              (raise-read-error-with-stx
                               "Too many arguments for method."
                               (honu-ast-src-stx exp))))
                         ;; dec-types isn't null, so we have too few arguments.
                         ((null? calc-types)
                          (raise-read-error-with-stx
                           "Not enough arguments for method."
                           (honu-ast-src-stx exp)))
                         ;; t_i' <: t_i, so check the next one.
                         ((<:_P tenv (car calc-types) (car dec-types))
                          (loop (+ n 1) (cdr dec-types) (cdr calc-types)))
                         ;; t_i was _not_ <: t_i', so blame the appropriate
                         ;; expression.
                         (else
                          (raise-read-error-with-stx
                           "Argument type is not subtype of declared type."
                           (honu-ast-src-stx (list-ref (honu-mcall-args exp) n))))))
                      ;; method-type was #f, so it couldn't be found in t1.
                      (raise-read-error-with-stx
                       "Method not found in type of object."
                       (honu-mcall-method exp)))))))]
       ;; P, G, D |- id |=> id : G(id)
       [(honu-var? exp)
        (cond
          [(env (honu-var-name exp))
           =>
           (lambda (t)
             (values exp t))]
          [(get-builtin-type (honu-var-name exp))
           =>
           (lambda (t)
             (values (copy-struct honu-var exp
                       (honu-var-builtin? #t))
                     t))]
          [else (raise-read-error-with-stx
                 "Variable not bound in local environment."
                 (honu-var-name exp))])]
       ;; E(id) = t   P, G, D |- e |=> e' : t'   t' <: t
       ;; ----------------------------------------------
       ;;      P, G, D |- id = e |=> id = e' : void
       [(honu-assn? exp)
        (let-values (((e1 t1) (f (honu-assn-rhs exp))))
          (let ((var-type (env (honu-assn-name exp))))
            (cond 
              [(not var-type)
               (raise-read-error-with-stx
                "Variable not bound in local environment."
                (honu-assn-name exp))]
              [(honu-type-equal? t1 var-type)
               (values (copy-struct honu-assn exp
                         (honu-assn-rhs e1))
                       (honu-void-type exp))]
              [else
               (raise-read-error-with-stx
                "RHS of assignment not the same type as the type of the variable."
                (honu-ast-src-stx (honu-assn-rhs exp)))])))]
       ;; We do not yet have functions in Honu, so raise an
       ;; appropriate exception.
       ;;
       ;; We need to allow error : str -> 'a and println : string -> void
       [(honu-call? exp)
        (cond
          [(env (honu-call-name exp))
           =>
           (lambda (t)
             (honu-typecheck-call tenv f exp t #f))]
          [(get-builtin-type (honu-call-name exp))
           =>
           (lambda (t)
             (honu-typecheck-call tenv f exp t #t))]
          [else 
           (raise-read-error-with-stx
            "Function not found!"
            (honu-call-name exp))])]
       ;; P, G, D |- this |=> this : G(this)
       [(honu-this? exp)
        (if (env #'this)
            (values exp (env #'this))
            (raise-read-error-with-stx
             "Use of this outside of a class or mixin body."
             (honu-ast-src-stx exp)))]
       ;; P, G, D |- e1 |=> e1' : t'       P |- t  
       ;; ---------------------------------------
       ;; P, G, D |- cast e1 t |=> cast e1' t : t
       ;;
       ;; Note that we don't check for primitive types (fuller
       ;; explanation under isa below), and also we don't do any
       ;; checking of how t' relates to t -- that's not the point
       ;; of a cast.  At runtime it will be checked that the object
       ;; that e1 results in is of a class that implements t.
       [(honu-cast? exp)
        (let-values (((e1 t1) (f (honu-cast-obj exp))))
          (if (honu-iface-type-in-tenv? tenv (honu-cast-type exp))
              (values (copy-struct honu-cast exp
                        (honu-cast-obj e1))
                      (honu-cast-type exp))
              (raise-read-error-with-stx
               "Attempt to cast to invalid type."
               (honu-ast-src-stx (honu-cast-type exp)))))]
       ;; P, G, D |- e1 |=> e1' : t'        P |- t
       ;; ----------------------------------------
       ;; P, G, D |- e1 isa t |=> e1' isa t : bool
       ;;
       ;; Note that we don't check to see if e1's type is a primitive
       ;; type and fail with an appropriate message if so.  How
       ;; primitive do we want to treat primitives?  Might they stay
       ;; "primitives", or might they eventually be changed into
       ;; classes?
       [(honu-isa? exp)
        (let-values (((e1 t1) (f (honu-isa-obj exp))))
          (if (honu-iface-type-in-tenv? tenv (honu-isa-type exp))
              (values (copy-struct honu-isa exp
                        (honu-isa-obj e1))
                      (honu-bool-type exp))
              (raise-read-error-with-stx
               "Attempt to check isa against invalid type."
               (honu-ast-src-stx (honu-isa-type exp)))))]
       ;; P, G, D |- e0 |=> e0' : bool  P, G, D |- e1 |=> e1' : t
       ;;               P, G, D |- e2 |=> e2' : t
       ;; -------------------------------------------------------
       ;;  P, G, D |- if e0  then e1  else e2  |=>
       ;;             if e0' then e1' else e2' : t
       ;;
       ;; We can make this a weaker rule by only requiring either
       ;;   a) t1 <: t2; or
       ;;   b) t2 <: t1
       ;; and returning the supertype as the type of the if expression.
       ;; Would this cause any problems (other than complicating the
       ;; type rule/code)?
       [(honu-if? exp)
        (let-values (((e0 t0) (f (honu-if-cond  exp)))
                     ((e1 t1) (f (honu-if-true  exp)))
                     ((e2 t2) (f (honu-if-false exp))))
          (cond
            [(not (honu-type-equal? t0 (honu-bool-type (honu-if-cond exp))))
             (raise-read-error-with-stx
              "Conditional expression of if must have bool type."
              (honu-ast-src-stx (honu-if-cond exp)))]
            [(<:_P tenv t1 t2)
             (values (copy-struct honu-if exp
                       (honu-if-cond  e0)
                       (honu-if-true  e1)
                       (honu-if-false e2))
                     t2)]
            [(<:_P tenv t2 t1)
             (values (copy-struct honu-if exp
                       (honu-if-cond  e0)
                       (honu-if-true  e1)
                       (honu-if-false e2))
                     t1)]
            [else
             (raise-read-error-with-stx
              "Branches of if expression are of unrelated types."
              (honu-ast-src-stx exp))]))]
       [(honu-while? exp)
        (let-values (((e1 t1) (f (honu-while-cond exp)))
                     ((e2 t2) (f (honu-while-body exp))))
          (if (honu-type-equal? t1 (honu-bool-type (honu-while-cond exp)))
              (values (copy-struct honu-while exp
                        (honu-while-cond e1)
                        (honu-while-body e2))
                      (honu-void-type exp))
              (raise-read-error-with-stx
               "Condition of while loop must be of boolean type"
               (honu-ast-src-stx (honu-while-cond exp)))))]
       ;; P, G, D |- e_i |=> e_i' : t_i                     c [= t
       ;; each init arg corresponding to id_i has type t_i' where
       ;; t_i <: t_i'
       ;; --------------------------------------------------------
       ;; P, G, D |- new c : t (id_1 = e_1, ..., id_n = e_n) |=>
       ;;            new c : t (id_1 = e_1', ..., id_n = e_n') : t
       [(honu-new? exp)
        (if (Implements_P tenv (honu-new-class exp) (honu-new-type exp))
            (let-values (((new-args new-types)
                          (map-two-values f (honu-new-arg-vals exp))))
              (let ((remainder (fold (lambda (n t i)
                                       (check-init-type-for-name tenv i n t))
                                     (get-init-names-and-types tenv (honu-new-class exp))
                                     (honu-new-arg-names exp)
                                     new-types)))
                (if (or (null? remainder)
                        (not (ormap tenv-init-optional? remainder))) ; checks to see if all optional
                    (values (copy-struct honu-new exp
                              (honu-new-arg-vals new-args))
                            (honu-new-type exp))
                    (raise-read-error-with-stx
                     "Too few initialization arguments in new expression."
                     (honu-ast-src-stx exp)))))
            (raise-read-error-with-stx
             "Class for new expression does not implement type in new expression."
             (honu-ast-src-stx exp)))]
       ;; P, G_i, D |- tid_i id_i = rhs_i |=> tid_i id_i = rhs_i', G_(i+1)
       ;; P, G_(m+1), D |- e_i |=> e_i' : t_i
       ;; ----------------------------------------------------------------
       ;; P, G_0, D |- { tid_0 id_0 = rhs_0; ...; tid_m id_m = rhs_m;
       ;;                e_0; ...; e_n; } |=>
       ;;              { tid_0 id_0 = rhs_0'; ...; tid_m id_m = rhs_m';
       ;;                e_0'; ...; e_n'; } : t_n
       [(honu-block? exp)
        (let*-values (((new-bind-f) (honu-typecheck-binding tenv cenv))
                      ((new-binds new-env)
                       (map-and-fold new-bind-f
                                     env (honu-block-binds exp)))
                      ((new-f) (honu-typecheck-exp tenv new-env cenv))
                      ((new-exps new-types)
                       (map-two-values new-f (honu-block-exps exp))))
          (values (copy-struct honu-block exp
                    (honu-block-binds new-binds)
                    (honu-block-exps  new-exps))
                  ;; Need the last expression's type.
                  (car (reverse new-types))))]
       ;;       P, G, D |- e |=> e' : t
       ;; -------------------------------------
       ;; P, G, D |- return e |=> return e' : t
       [(honu-return? exp)
        (if (honu-return-body exp)
            (let-values (((e1 t1) (f (honu-return-body exp))))
              (values (copy-struct honu-return exp
                        (honu-return-body e1))
                      t1))
            (values exp
                    (honu-void-type exp)))]
       [else
        (raise-read-error-with-stx
         "Unexpected type of Honu expression."
         (honu-ast-src-stx exp))]))
    f)

  ;;       P, G, D |- rhs |=> rhs' : t'    t' <: t
  ;; --------------------------------------------------
  ;; P, G, D |- t id = rhs |=> t id = rhs', G[id |-> t]
;  (provide honu-typecheck-binding)
  (provide/contract [honu-typecheck-binding
                     (tenv?
                      any/c
                      . -> .
                      ((honu-binding? any/c)
                       . ->* .
                       (honu-binding? any/c)))]) 
  (define (honu-typecheck-binding tenv cenv)
    (lambda (bind env)
      (let-values (((e1 t1) ((honu-typecheck-exp tenv env cenv)
                             (honu-binding-rhs bind))))
        (if (<:_P tenv t1 (honu-binding-type bind))
            (values (copy-struct honu-binding bind
                      (honu-binding-rhs e1))
                    (extend-env env
                                (honu-binding-name bind)
                                (honu-binding-type bind)))
            (raise-read-error-with-stx
             "Type for RHS of binding not subtype of declared type."
             (honu-ast-src-stx (honu-binding-rhs bind)))))))


  (define (honu-typecheck-call tenv f exp t builtin?)
    (let-values (((arg-exps arg-types)
                  (map-two-values (lambda (e) (f e))
                                  (honu-call-args exp))))
      (let loop ([formal-types (honu-func-type-args t)]
                 [actual-types arg-types])
        (cond
          [(null? formal-types)
           (if (not (null? actual-types))
               (let ([actuals-size (length actual-types)])
                 (raise-read-error-with-stx
                  (format "~a function got ~a more argument~a than expected"
                          (if builtin? "Built-in" "Declared")
                          actuals-size
                          (if (= actuals-size 1) "" "s"))
                  (honu-ast-src-stx exp)))
               (values (copy-struct honu-call exp
                          (honu-call-args arg-exps)
                          (honu-call-builtin? builtin?))
                       (honu-func-type-return t)))]
          [(null? actual-types)
           (let ([formals-size (length formal-types)])
             (raise-read-error-with-stx
              (format "~a function got ~a fewer argument~a than expected"
                      (if builtin? "Built-in" "Declared")
                      formals-size
                      (if (= formals-size 1) "" "s"))
              (honu-ast-src-stx exp)))]
          [(<:_P tenv (car actual-types) (car formal-types))
           (loop (cdr formal-types)
                 (cdr actual-types))]
          [else 
           (raise-read-error-with-stx
            (format "Types of the arguments do not match the ~a function"
                    (if builtin? "built-in" "declared"))
            (honu-ast-src-stx exp))]))))
  )
