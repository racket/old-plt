#cs
(module beginner-parse mzscheme
	(require "general-parse.ss"
		 (prefix ast: "../ast.ss"))

	(provide parse-beginner-ml-file parse-beginner-ml-port)

(define parse
  (parser
   (suppress)
   (tokens Keywords Labels Ops ConstantConstructors Literals Errors Others Precedences)
   (start <program>)
   (end EOF); SEMISEMI)
   (precs (right <prec_let>)                      ;; let ... in ...
	  (right <prec_type_def>)                 ;; = in type definitions
	  (right SEMI)                          ;; e1; e2 (sequence)
	  (right <prec_fun> <prec_match> <prec_try>)  ;; match ... with ...
	  (right <prec_list>)                     ;; e1; e2 (list, array, record)
	  (right <prec_if>)                       ;; if ... then ... else ...
	  (right COLONEQUAL LESSMINUS)          ;; assignments
	  (left AS)                             ;; as in patterns
	  (left BAR)                            ;; | in patterns
	  (left COMMA)                          ;; , in expressions, patterns, types
	  (right <prec_type_arrow>)               ;; -> in type expressions
	  (right OR BARBAR)                     ;; or
	  (right AMPERSAND AMPERAMPER)          ;; &
	  (left INFIXOP0 EQUAL LESS GREATER)    ;; = < > etc
	  (right INFIXOP1)                      ;; @ ^ etc
	  (right COLONCOLON)                    ;; ::
	  (left INFIXOP2 PLUS MINUS MINUSDOT)   ;; + -
	  (left INFIXOP3 STAR)                  ;; * /
	  (right INFIXOP4)                      ;; **
	  (right <prec_unary_minus>)              ;; - unary
	  (left <prec_appl>)                      ;; function application
	  (right <prec_constr_appl>)              ;; constructor application
	  (left SHARP)                          ;; method call 
	  (left DOT)                            ;; record access, array access
	  (right PREFIXOP))                     ;; ! 
   
;   (error (lambda (tok-ok name val start-pos end-pos)
;	    (set! parse-error #t)
;	    (pretty-print (list "Parse error near " tok-ok name (syntax-e val) (syntax-source val) (syntax-line val) (syntax-column val) (syntax-position val) (syntax-span val)))))
   (error (lambda (a b stx spos epos)
;	    (raise-read-error (format "parse error near ~a ~a ~a " a b (syntax-e stx))
	    (raise-read-error (format "Parse error near <~a:~a>" b (syntax-e stx))
			      (syntax-source stx)
			      (syntax-line stx)
			      (syntax-column stx)
			      (syntax-position stx)
			      (syntax-span stx))))
   (src-pos)
   (grammar
    (<program>
     [(<implementation>) $1]  ;; for implementation files
     ;[(<inteface>) $1]        ;; for interface files
     [(<toplevel_phrase>) $1] ;; for interactive use
     ;[(<use_file>) $1]      ;; for the #use directive
     )
    ;; Entry points
    (<implementation> 
     [(<structure>) $1])
    ;(<interface> [(<signature> EOF) $1])
    (<toplevel_phrase>
     [(<top_structure> SEMISEMI) $1]
     [(<seq_expr> SEMISEMI) $1])
;;     [(<toplevel_directive> SEMISEMI) $1])
    (<top_structure>
     [(<structure_item>) (list $1)]
     [(<structure_item> <top_structure>) (cons $1 $2)])
;;    (<structure>
;;     [(<structure_tail>) $1]
;;     [(<seq_expr> <structure_tail>)
    (<structure>
     [(<structure_tail>) $1]
     [(<seq_expr> <structure_tail>) (ast:make-structure_item (ast:make-pstr_eval (cons $1 $2) #f) (build-src 2))])

    (<structure_tail>
     [() null]
     [(SEMISEMI) null]
     [(SEMISEMI <seq_expr> <structure_tail>)
      (list (ast:make-structure_item (ast:make-pstr_eval (cons $2 $3) #f) (build-src 2 3)))]
     [(SEMISEMI <structure_item> <structure_tail>) (cons $2 $3)]
     [(<structure_item> <structure_tail>) (cons $1 $2)])

    (<structure_item>
     [(LET <rec_flag> <let_bindings>)
      (let ([bindings $3])
	(if (and (ast:pattern? (car bindings)) (ast:ppat_any? (ast:pattern-ppat_desc (car bindings))))
	    (ast:make-structure_item (ast:make-pstr_eval (cdr bindings) (if $2 (build-src 1 2) (build-src 1))) (build-src 3))
	    (ast:make-structure_item (ast:make-pstr_value $2 (reverse $3) (if $2 (build-src 1 2) (build-src 1))) (build-src 3))))]
;     [(EXTERNAL <val_ident_colon> <core_type> EQUAL <primitive_declaration>)
;      (ast:make-structure_item (ast:make-pstr_primitive $2 (ast:make-value_description $3 $5)) (build-src 5))]
;     [(OPEN <mod_longident>)
;      (ast:make-structure_item (ast:make-pstr_open $2))]
     ;[(CLASS <class_declarartions)
     ;[(CLASS TYPE <class_type_declarations>)
     ;[(INCLUDE <module_expr>
     )

    (<constrain>
     [(<core_type> EQUAL <core_type>) (cons (cons $1 $3) (build-src 3))])
    (<seq_expr>
     [(<expr>) (prec SEMI) $1])
;; Core Expressions


    (<labeled_simple_pattern>
     [(QUESTION LPAREN <label_let_pattern> <opt_default> RPAREN)
      (cons (cons (string-append "?" (car $3)) $4) (cdr $3))]
     [(QUESTION <label_var>)
      (cons (cons (string-append "?" (car $2)) null) (cdr $2))]
     [(OPTLABEL LPAREN <let_pattern> <opt_default> RPAREN)
      (cons (cons (string-append "?" $1) $4) $3)]
     [(OPTLABEL <pattern_var>)
      (cons (cons (string-append "?" $1) null) $2)]
     [(TILDE LPAREN <label_let_pattern> RPAREN)
      (cons (cons (car $3) null) (cdr $3))]
     [(TILDE <label_var>)
      (cons (cons (car $2) null) (cdr $2))]
     [(LABEL <simple_pattern>)
      (cons (cons $1 null) $2)]
     [(<simple_pattern>)
      (cons (cons "" null) $1)])

    (<pattern_var>
     [(LIDENT) (ast:make-pattern (ast:make-ppat_var $1) (build-src 1))])

    (<opt_default>
     [() null]
     [(EQUAL <seq_expr>) $2])

    (<label_let_pattern>
     [(<label_var>) $1]
     [(<label_var> COLON <core_type>)
      (let ([label-pattern $1])
	(cons (car label-pattern) (ast:make-pattern (ast:make-ppat_constraint (cdr label-pattern) $3))) (build-src 3))])

    (<label_var>
     [(LIDENT) (cons $1 (ast:make-pattern (ast:make-ppat_var $1)) (build-src 1))])

    (<let_pattern>
     [(<pattern>) $1]
     [(<pattern> COLON <core_type>) (ast:make-pattern (ast:make-ppat_constraint $1 $3) (build-src 3))])

    (<expr>
     [(<simple_expr>) $1]
     [(<simple_expr> <simple_labeled_expr_list>) (prec <prec_appl>)
      (ast:make-expression (ast:make-pexp_apply $1 (reverse $2)) (build-src 1 2))]
     [(LET <rec_flag> <let_bindings> IN <seq_expr>) (prec <prec_let>)
      (ast:make-expression (ast:make-pexp_let $2 (reverse $3) $5 (if $2 (build-src 1 2) (build-src 1)) (build-src 4 4)) (build-src 5))]
     ;[(LET MODULE UIDENT <module_binding> IN <seq_expr>) (prec <prec_let>)
     ; (ast:make-expression (ast:make-pexp_letmodule($3, $4, $6)))]
     [(FUNCTION <opt_bar> <match_cases>) (prec <prec_fun>)
      (ast:make-expression (ast:make-pexp_function "" null (reverse $3)) (build-src 3))]
     [(FUN <labeled_simple_pattern> <fun_def>) (prec <prec_fun>)
      (let ([pat $2])
	(ast:make-expression (ast:make-pexp_function (car (car pat)) (cdr (car pat)) (list (cons (cdr pat) $3))) (build-src 3)))]
     [(<constr_longident> <simple_expr>) (prec <prec_constr_appl>)
      (ast:make-expression (ast:make-pexp_construct $1 $2 #f) (build-src 2))]
     [(<name_tag> <simple_expr>) (prec <prec_constr_appl>)
      (ast:make-expression (ast:make-pexp_variant $1 $2) (build-src 2))]
     [(IF <seq_expr> THEN <expr> ELSE <expr>) (prec <prec_if>)
      (ast:make-expression (ast:make-pexp_ifthenelse $2 $4 $6 (build-src 1) (build-src 3 3) (build-src 5 5)) (build-src 6))]
     [(IF <seq_expr> THEN <expr>) (prec <prec_if>)
      (ast:make-expression (ast:make-pexp_ifthenelse $2 $4 null (build-src 1) (build-src 3 3) #f) (build-src 4))]
     [(<expr> INFIXOP0 <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident $2)) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> INFIXOP1 <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident $2)) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> INFIXOP2 <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident $2)) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> INFIXOP3 <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident $2)) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> INFIXOP4 <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident $2)) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> PLUS <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 "+" (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> MINUS <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 "-" (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> MINUSDOT <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 "-." (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> STAR <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 "*" (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> EQUAL <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 "=" (list #f
																	      (syntax-line $2)
																	      (syntax-column $2)
																	      (syntax-position $2)
																	      (syntax-span $2))))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> LESS <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 "<" (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> GREATER <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 ">" (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> OR <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 "or" (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> BARBAR <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 "||" (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> AMPERSAND <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 "&" (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> AMPERAMPER <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 "&&" (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<expr> COLONEQUAL <expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $2 ":=" (build-syn-list $2)))) (build-src 2 2)) (list (cons "" $1) (cons "" $3))) (build-src 1 3))]
     [(<subtractive> <expr>) (prec <prec_unary_minus>)
      (let ([type (ast:expression-pexp_desc $2)])
	(if (and (ast:pexp_constant? type) (number? (syntax-object->datum (ast:pexp_constant-const type))))
	    (ast:make-expression (ast:make-pexp_constant (datum->syntax-object $1 (- (syntax-object->datum (ast:pexp_constant-const type))) (build-syn-list $1 (position-offset $2-end-pos)))) (build-src 1 2))
	    (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident (datum->syntax-object $1 (string-append "~" (syntax-object->datum $1)) (build-syn-list $1 (position-offset $2-end-pos))))) (build-src 1)) (list (cons "" $2))) (build-src 1 2))))])
;;     [(<simple_expr> DOT LBRACE <expr> RBRACE LESSMINUS <expr>)
;;      (bigarray_set)]
    (<simple_expr>
     [(<val_longident>)
      (ast:make-expression (ast:make-pexp_ident $1) (build-src 1))]
     [(<constant>)
      (ast:make-expression (ast:make-pexp_constant $1) (build-src 1))]
     [(<constr_longident>)
      (ast:make-expression (ast:make-pexp_construct $1 null #f) (build-src 1))]
     [(<name_tag>)
      (ast:make-expression (ast:make-pexp_variant $1 null) (build-src 1))]
     [(LPAREN <seq_expr> RPAREN)
      $2]
;;     [(LPAREN <seq_expr> <error>)
;; Unclosed paren error
     [(LPAREN <seq_expr> <type_constraint> RPAREN)
      (let ([types $3])
	(ast:make-expression (ast:make-pexp_constraint $2 (car types) (cdr types)) (build-src 4)))]
     [(PREFIXOP <simple_expr>)
      (ast:make-expression (ast:make-pexp_apply (ast:make-expression (ast:make-pexp_ident (ast:make-lident $1)) (build-src 2 2)) (list (cons "" $2))) (build-src 1 2))])
;; Class related expression would go here

    (<simple_labeled_expr_list>
     [(<labeled_simple_expr>)
      (list $1)]
     [(<simple_labeled_expr_list> <labeled_simple_expr>)
      (cons $2 $1)])

    (<labeled_simple_expr>
     [(<simple_expr>)
      (cons "" $1)]
     [(<label_expr>)
      $1])

    (<label_expr>
     [(LABEL <simple_expr>)
      (cons $1 $2)]
     [(TILDE <label_ident>)
      $2]
     [(QUESTION <label_ident>)
      (cons (string-append "?" (car $2)) (cdr $2))]
     [(OPTLABEL <simple_expr>)
      (cons (string-append "?" $1) $2)])

    (<label_ident>
     [(LIDENT)
      (cons $1 (ast:make-expression (ast:make-pexp_ident (ast:make-lident $1)) (build-src 1)))])

    (<let_bindings>
     [(<let_binding>)
      (list $1)]
     [(<let_bindings> AND <let_binding>)
      (cons $3 $1)])

    (<let_binding>
     [(<val_ident> <fun_binding>)
      (cons (ast:make-pattern (ast:make-ppat_var $1) (build-src 1 2)) $2)]
     [(<pattern> EQUAL <seq_expr>) (prec <prec_let>)
      (cons $1 $3)])
    
    (<fun_binding>
     [(EQUAL <seq_expr>) (prec <prec_let>)
      $2]
     [(<type_constraint> EQUAL <seq_expr>) (prec <prec_let>)
      (let ([binding $1])
	(ast:make-expression (ast:make-pexp_constraint $3 (car binding) (cdr binding)) (build-src 3)))]
     [(<labeled_simple_pattern> <fun_binding>)
      (let ([lsp $1])
	(ast:make-expression (ast:make-pexp_function (caar lsp) (cdar lsp) (list (cons (cdr lsp) $2))) (build-src 2)))])
    
    (<match_cases>
      [(<pattern> <match_action>) (list (cons $1 $2))]
      [(<match_cases> BAR <pattern> <match_action>) ( cons (cons $3 $4) $1)])

    (<fun_def>
     [(<match_action>) $1]
     [(<labeled_simple_pattern> <fun_def>)
      (let ([pattern $1])
	(ast:make-expression (ast:make-pexp_function (car pattern) (cadr pattern) (list (cons (cddr pattern) $2))) (build-src 2)))])

    (<match_action>
     [(MINUSGREATER <seq_expr>) $2]
     [(WHEN <seq_expr> MINUSGREATER <seq_expr>) (ast:make-expression (ast:make-pexp_when $2 $4))])

    (<expr_comma_list>
     [(<expr_comma_list> COMMA <expr>)
      (cons $3 $1)]
     [(<expr> COMMA <expr>)
      (list $3 $1)])

    (<record_expr>
     [(<simple_expr> WITH <lbl_expr_list> <opt_semi>)
      (cons $1 (reverse $3))]
     [(<lbl_expr_list> <opt_semi>)
      (cons null (reverse $1))])

    (<lbl_expr_list>
     [(<label_longident> EQUAL <expr>) (prec <prec_list>)
      (list (cons $1 $3))]
     [(<lbl_expr_list> SEMI <label_longident> EQUAL <expr>) (prec <prec_list>)
      (cons (cons $3 $5) $1)])
    
    (<expr_semi_list>
     [(<expr>) (prec <prec_list>) 
      (list $1)]
     [(<expr_semi_list> SEMI <expr>) (prec <prec_list>) 
      (cons $3 $1)])

    (<type_constraint>
     [(COLON <core_type>) (cons $2 null)]
     [(COLON <core_type> COLONGREATER <core_type>) (cons $2 $4)]
     [(COLONGREATER <core_type>) (cons null $2)]
     ;; Errors when COLON error and COLONGREATER error
     )

;; Patterns

    (<pattern>
     [(<simple_pattern>)
      $1])

    (<simple_pattern>
     [(<val_ident>)
      (ast:make-pattern (ast:make-ppat_var $1) (build-src 1))]
;     [(UNDERSCORE)
;      (ast:make-pattern (ast:make-ppat_any null) (build-src 1))]
;     [(<signed_constant>)
;      (ast:make-pattern (ast:make-ppat_constant $1) (build-src 1))]
;;     [(CHAR DOTDOT CHAR)
;;      (let ([order (if (char-locale>? $1 $3)
;;		       (cons $1 $3)
;;		       (cons $3 $1))])
;;	(if (char=? $1 $3)
;;	    (ast:make-pattern (ast:make-ppat_constant $1) (build-src 1))
;;	    (ast:make-pattern (ast:make-ppat_or (ast:make-pattern (ast:make-ppat_constant (cdr order))) (ast:make-
;; Silly recursion. How do I do it and still take advantage of the source?
;     [(<constr_longident>)
;      (ast:make-pattern (ast:make-ppat_construct $1 null #f) (build-src 1))]
     [(LPAREN <pattern> COLON <core_type> RPAREN)
;woodoo
      (ast:make-pattern (ast:make-ppat_constraint $2 $4) (build-src 5))]
;;     [(LPAREN <pattern> COLON <core_type> error)
;; Unclosed error
     )

;; Primitive declarations

    (<primitive_declaration>
     [(STRING) (list $1)]
     [(STRING <primitive_declaration>) (cons $1 $2)])

;; Type declarations

    (<core_type>
     [(<core_type2>) $1]
     [(<core_type2> AS QUOTE <ident>) (ast:make-core_type (ast:make-ptyp_alias $1 $4) (build-src 4))])

    (<core_type2>
     [(<simple_core_type_or_tuple>) $1]
     [(QUESTION LIDENT COLON <core_type2> MINUSGREATER <core_type2>) (prec <prec_type_arrow>)
      (ast:make-core_type (ast:make-ptyp_arrow (string-append "?" $2) (ast:make-core_type (ast:make-ptyp_constr (ast:make-lident "option") (list $4)) (ast:core_type-src $4)) $6) (build-src 6))]
     [(OPTLABEL <core_type2> MINUSGREATER <core_type2>) (prec <prec_type_arrow>)
      (ast:make-core_type (ast:make-ptyp_arrow (string-append "?" $1) (ast:make-core_type (ast:make-ptyp_constr (ast:make-lident "option") (list $2)) (ast:core_type-src $2)) $4) (build-src 4))]
     [(LIDENT COLON <core_type2> MINUSGREATER <core_type2>) (prec <prec_type_arrow>)
      (ast:make-core_type (ast:make-ptyp_arrow $1 $3 $5) (build-src 4))]
     [(<core_type2> MINUSGREATER <core_type2>) (prec <prec_type_arrow>)
      (ast:make-core_type (ast:make-ptyp_arrow "" $1 $3) (build-src 3))])

    (<simple_core_type_or_tuple>
     [(<simple_core_type>) $1])

    (<core_type_comma_list>
     [(<core_type>) (list $1)]
     [(<core_type_comma_list> COMMA <core_type>) (cons $3 $1)])

    (<core_type_list>
     [(<simple_core_type>) (list $1)]
     [(<core_type_list> STAR <simple_core_type>) (cons $3 $1)])

    (<simple_core_type>
     [(<simple_core_type2>) $1]
     [(LPAREN <core_type_comma_list> RPAREN)
      (let ([ctcl $2])
	(if (and (list? ctcl) (= (length ctcl) 1))
	    (car ctcl)
;; this is an error case
	    ))])

    (<simple_core_type2>
     [(QUOTE <ident>)
      (ast:make-core_type (ast:make-ptyp_var $2) (build-src 2))]
     [(UNDERSCORE)
      (ast:make-core_type (ast:make-ptyp_any null) (build-src 1))]
     [(<type_longident>)
      (ast:make-core_type (ast:make-ptyp_constr $1 null) (build-src 1))]
     [(<simple_core_type2> <type_longident>) (prec <prec_constr_appl>)
      (ast:make-core_type (ast:make-ptyp_constr $2 (list $1)) (build-src 2))]
     [(LPAREN <core_type_comma_list> RPAREN <type_longident>) (prec <prec_constr_appl>)
      (ast:make-core_type (ast:make-ptyp_constr $4 (reverse $2)) (build-src 4))]
;;     [(LESS <meth_list> GREATER)
;;      (ast:make-core_type (ast:make-ptyp_object $2) (build-src 1))]
;; And more object-related stuff
     )

;; Constants

    (<constant>
     [(INT) $1]
     [(CHAR) $1]
     [(STRING) $1]
     [(FLOAT) $1])

    (<signed_constant>
     [(<constant>) $1]
     [(MINUS INT) (- $2)]
     [(<subtractive> FLOAT) (- $2)])

;; Identifiers and long identifiers

    (<ident>
     [(UIDENT) $1]
     [(LIDENT) $1])

    (<val_ident>
     [(LIDENT) $1]
     [(LPAREN <operator> RPAREN) $2])

    (<val_ident_colon>
     [(LIDENT COLON) $1]
     [(LPAREN <operator> RPAREN COLON) $2]
     [(LABEL) $1])

    (<operator>
     [(PREFIXOP) $1]
     [(INFIXOP0) $1]
     [(INFIXOP1) $1]
     [(INFIXOP2) $1]
     [(INFIXOP3) $1]
     [(INFIXOP4) $1]
     [(PLUS) (datum->syntax-object $1 "+" (build-syn-list $1))]
     [(MINUS) (datum->syntax-object $1 "-" (build-syn-list $1))]
     [(MINUSDOT) (datum->syntax-object $1 "-." (build-syn-list $1))]
     [(STAR) (datum->syntax-object $1 "*" (build-syn-list $1))]
     [(EQUAL) (datum->syntax-object $1 "=" (build-syn-list $1))]
     [(LESS) (datum->syntax-object $1 "<" (build-syn-list $1))]
     [(GREATER) (datum->syntax-object $1 ">" (build-syn-list $1))]
     [(OR) (datum->syntax-object $1 "or" (build-syn-list $1))]
     [(BARBAR) (datum->syntax-object $1 "||" (build-syn-list $1))]
     [(AMPERSAND) (datum->syntax-object $1 "&" (build-syn-list $1))]
     [(AMPERAMPER) (datum->syntax-object $1 "&&" (build-syn-list $1))]
     [(COLONEQUAL) (datum->syntax-object $1 ":=" (build-syn-list $1))])

    (<constr_ident>
     [(UIDENT) $1]
;;     [(LBRACKET RBRACKET) "[]"] Commented out
     [(LPAREN RPAREN) (datum->syntax-object $1 "()" (build-syn-list $1 (position-offset $2-end-pos)))]
     [(COLONCOLON) (datum->syntax-object $1 "::" (build-syn-list $1))]
     [(FALSE) (datum->syntax-object $1 "false" (build-syn-list $1))]
     [(TRUE) (datum->syntax-object $1 "true" (build-syn-list $1))])

    (<val_longident>
     [(<val_ident>) (ast:make-lident $1)]
     [(<mod_longident> DOT <val_ident>) (ast:make-ldot $1 $3)])

    (<constr_longident>
     [(<mod_longident>) $1]
     [(LBRACKET RBRACKET) (ast:make-lident (datum->syntax-object $1 "[]" (build-syn-list $1 (position-offset $2-end-pos))))]
     [(LPAREN RPAREN) (ast:make-lident (datum->syntax-object $1 "()" (build-syn-list $1 (position-offset $2-end-pos))))]
     [(FALSE) (ast:make-lident (datum->syntax-object $1 "false" (build-syn-list $1)))]
     [(TRUE) (ast:make-lident (datum->syntax-object $1 "true" (build-syn-list $1)))])

    (<label_longident>
     [(LIDENT) (ast:make-lident $1)]
     [(<mod_longident> DOT LIDENT) (ast:make-ldot $1 $3)])

    (<type_longident>
     [(LIDENT) (ast:make-lident $1)]
     [(<mod_ext_longident> DOT LIDENT) (ast:make-ldot $1 $3)])

    (<mod_longident>
     [(UIDENT) (ast:make-lident $1)]
     [(<mod_longident> DOT UIDENT) (ast:make-ldot $1 $3)])

    (<mod_ext_longident>
     [(UIDENT) (ast:make-lident $1)]
     [(<mod_ext_longident> DOT UIDENT) (ast:make-ldot $1 $3)]
     [(<mod_ext_longident> LPAREN <mod_ext_longident> RPAREN) (ast:make-lapply $1 $3)])
    
    (<mty_longident>
     [(<ident>) (ast:make-lident $1)]
     [(<mod_ext_longident> DOT <ident>) (ast:make-ldot $1 $3)])

    (<clty_longident>
     [(LIDENT) (ast:make-lident $1)]
     [(<mod_ext_longident> DOT LIDENT) (ast:make-ldot $1 $3)])

    (<class_longident>
     [(LIDENT) (ast:make-lident $1)]
     [(<mod_longident> DOT LIDENT) (ast:make-ldot $1 $3)])

;; Toplevel directives
     
;; None for now

;; Miscellaneous

    (<name_tag>
     [(BACKQUOTE <ident>) $2])

    (<rec_flag>
     [() #f]
     [(REC) #t])

    (<direction_flag>
     [(TO) #t]
     [(DOWNTO) #f])

    ;; Skip class-based flags

    (<mutable_flag>
     [() #f]
     [(MUTABLE) #t])

    (<opt_bar>
     [() null]
     [(BAR) null])
    
    (<opt_semi>
     [() null]
     [(SEMI) null])

    (<subtractive>
     [(MINUS) (datum->syntax-object $1 "-" (build-syn-list $1))]
     [(MINUSDOT) (datum->syntax-object $1 "-." (build-syn-list $1))]))))
  


  (define (parse-beginner-ml-port port file offset)
    (parse-ml-port port file offset parse))
  
  (define (parse-beginner-ml-file file)
    (parse-ml-file file parse))
)