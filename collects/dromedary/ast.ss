#cs
(module ast mzscheme

	;;Macro for structure definition and provision
	(define-syntax p-define-struct
	  (syntax-rules ()
			[(_ (name inherit) fields)
			 (begin
			   (provide (struct name fields))
			   (define-struct (name inherit) fields (make-inspector)))]
			[(_ name fields)
			 (begin
			   (provide (struct name fields))
			   (define-struct name fields (make-inspector)))]))

	;(make-src int int int)
	(p-define-struct src (line col pos span))

	;(make-ptop-def structure)
	(p-define-struct ptop_def (structure))

	;(make-stucture list)
	(p-define-struct structure (structure_items))

	;(make-structure_item structure_item_desc src)
	(p-define-struct structure_item (pstr_desc pstr_src))

	;structure_item_desc => eval
	;                     | value
	;                     | primitive
	;                     | type
	;                     | exception
	;                     | exn_rebind
	;                     | module
	;                     | modtype
	;                     | open
	;                     | class
	;                     | class_type
	;                     | include

	;(make-pstr_eval expression)
	(p-define-struct pstr_eval (expr))
	;(make-pstr_value boolean list)
	(p-define-struct pstr_value (rec_flag pattern*expression-list))
	;(make-pstr_primitive string value_description)
	(p-define-struct pstr_primitive (name desc))
	;(make-pstr_type list)
	(p-define-struct pstr_type (string*type_declaration-list))
	;(make-pstr_exception string exception_declaration)
	(p-define-struct pstr_exception (name decl))
	;(make-pstr_exn_rebind string longident)
	(p-define-struct pstr_exn_rebind (name ident))
	;(make-pstr_module string 
  ;| pstr_module of string * module_expr
  ;| pstr_modtype of string * module_type
	;(make-pstr_open longident)
	(p-define-struct pstr_open (name))
	;(make-pstr_class list)
	(p-define-struct pstr_class (class_declaration-list))
	;(make-pstr_class_type list)
	(p-define-struct pstr_class_type (class_type_declaration-list))
  ;| pstr_include of module_expr

	;(make-value_description core_type string-list)
	(p-define-struct value_description (pval_type pval_prim))
	;(make-core_type core_type_desc src)
	(p-define-struct core_type (desc src))

	; core_type_desc => any
	;                 | var
	;                 | arrow
	;                 | tuple
	;                 | constr
	;                 | object
	;                 | class
	;                 | alias
	;                 | variant

	;(make-ptyp_any null)
	(p-define-struct ptyp_any (x))
	;(make-ptyp_var string)
	(p-define-struct ptyp_var (name))
	;(make-ptyp_arrow label core_type core_type)
	(p-define-struct ptyp_arrow (name fromtype totype))
	;(make-ptyp_tuple list)
	(p-define-struct ptyp_tuple (core_type-list))
	;(make-ptyp_constr longident list)
	(p-define-struct ptyp_constr (name core_type-list))
	;(make-ptyp_object list)
	(p-define-struct ptyp_object (core_field_type-list))
	;(make-ptyp_class longident list list)
	(p-define-struct ptyp_class (longident core_type-list label-list))
	;(make-ptyp_alias core_type string)
	(p-define-struct ptyp_alias (type name))
	;(make-ptyp_variant list boolean list-or-null)
	(p-define-struct ptyp_variant (row_field-list bool label-list))

        ;(make-core_field_type core_field_desc src)
	(p-define-struct core_field_type (pfield_desc src))

	; core_field_desc => string * core_type
	;                  | pfield_var
	
	;(make-pfield string core_type)
	(p-define-struct pfield (name type))
	;(make-pfield_var null)
	(p-define-struct pfield_var (x))
			 

	; row_field => label * bool * core_type-list
	;            | core_type
	;(make-Rtag label boolean list)
	(p-define-struct rtag (label bool core_type-list))
	;(make-Rinherit core_type)
	(p-define-struct rinherit (type))

	;(make-pattern pattern_desc src)
	(p-define-struct pattern (ppat_desc ppat_src))

	; pattern_desc => any
	;               | var
	;               | alias
	;               | constant
	;               | tuple
	;               | construct
	;               | variant
	;               | record
	;               | array
	;               | or
	;               | constraint
	;               | type

	;(make-ppat_any null)
	(p-define-struct ppat_any (x))
	;(make-ppat_var string)
	(p-define-struct ppat_var (name))
	;(make-ppat_alias pattern string)
	(p-define-struct ppat_alias (pat name))
	;(make-ppat_constant constant)
	(p-define-struct ppat_constant (const))
	;(make-ppat_tuple list)
	(p-define-struct ppat_tuple (pattern-list))
	;(make-ppat_construct longident pattern-or-null boolean)
	(p-define-struct ppat_construct (longident pat bool))
	;(make-ppat_variant label pattern-or-null)
	(p-define-struct ppat_variant (label pattern))
	;(make-ppat_record list)
	(p-define-struct ppat_record (longident*pattern-list))
	;(make-ppat_array list)
	(p-define-struct ppat_array (pattern list))
	;(make-ppat_or pattern pattern)
	(p-define-struct ppat_or (firstpattern secondpattern))
	;(make-ppat_constraint pattern core_type)
	(p-define-struct ppat_constraint (pat type))
	;(make-ppat_type longident)
	(p-define-struct ppat_type (name))


	 ;(make-sequence expression expression)
	(p-define-struct sequence (first rest))

	;(make-expression expression_desc src)
	(p-define-struct expression (pexp_desc pexp_src))

	; expression_desc => ident
	;                  | constant
	;                  | let
        ;                  | function
        ;                  | apply
	;                  | match
	;                  | try
        ;                  | tuple
	;                  | contruct
	;                  | variant
	;                  | record
	;                  | field
	;                  | setfield
	;                  | array
	;                  | ifthenelse
	;                  | sequence
	;                  | while
	;                  | for
	;                  | constraint
	;                  | when
	;                  | send
	;                  | new
	;                  | setinstvar
	;                  | override
	;                  | letmodule
	;                  | assert
	;                  | assertfalse
	
	;(make-pexp_ident longident)
	(p-define-struct pexp_ident (name))
	;(make-pexp_constant constant)
	(p-define-struct pexp_constant (const))
	;(make-pexp_let boolean list expression)
	(p-define-struct pexp_let (rec_flag pattern*expression-list expr))
	;(make-pexp_function label expression-or-null list)
	(p-define-struct pexp_function (label expr pattern*expression-list))
	;(make-pexp_apply expression list)
	(p-define-struct pexp_apply (expr label*expression-list))
	;(make-pexp_match expression list)
	(p-define-struct pexp_match (expr pattern*expression-list))
	;(make-pexp_try expression list)
	(p-define-struct pexp_try (expr pattern*expression-list))
	;(make-pexp_tuple list)
	(p-define-struct pexp_tuple (expression-list))
	;(make-pexp_construct longident expression-or-null boolean)
	(p-define-struct pexp_construct (name expr bool))
	;(make-pexp_variant label expression-or-null)
	(p-define-struct pexp_variant (lab expr))
	;(make-pexp_record list expression-or-null)
	(p-define-struct pexp_record (longident*expression-list expr))
	;(make-pexp_field expression longident)
	(p-define-struct pexp_field (expr longident))
	;(make-pexp_setfield expr longident expr)
	(p-define-struct pexp_setfield (expr1 longident expr2))
	;(make-pexp_array list)
	(p-define-struct pexp_array (expression-list))
	;(make-pexp_ifthenelse expression expression expression-or-null)
	(p-define-struct pexp_ifthenelse (test trueexpr falseexpr))
	;(make-pexp_sequence expression expression)
	(p-define-struct pexp_sequence (firstexpr restexpr))
	;(make-pexp_while expression expression)
	(p-define-struct pexp_while (testexpr bodyexpr))
	;(make-pexp_for string expression expression boolean expression)
	(p-define-struct pexp_for (var init test up body))
	;(make-pexp_constraint expression core_type-or-null core_type-or-null)
	(p-define-struct pexp_constraint (expr core_type1 core_type2))
	;(make-pexp_when expression expression)
	(p-define-struct pexp_when (test body))
	;(make-pexp_send expression string)
	(p-define-struct pexp_send (expr name))
	;(make-pexp_new longident)
	(p-define-struct pexp_new (name))
	;(make-pexp_setinstvar string expression)
	(p-define-struct pexp_setinstvar (name expr))
	;(make-pexp_override list)
	(p-define-struct pexp_override (string*expression-list))
;	  | pexp_letmodule of string - module_expr - expression
	;(make-pexp_assert expression)
	(p-define-struct pexp_assert (expr))
	;(make-pexp_assertfalse null)
	(p-define-struct pexp_assertfalse (null))

	; longident => lident of string
	;            | ldot of longident * string
        ;            | lapply of longident * longident
	(define (longident? l)
	  (or
	   (lident? l)
	   (ldot? l)
	   (lapply? l)))

	;(make-lident string)
	(p-define-struct lident (name))
	;(make-ldot longident string)
	(p-define-struct ldot (first name))
	;(make-lapply longident longident)
	(p-define-struct lapply (rand rator))

	;(make-type_declaration list list kind core_type-or-null list location)
	(p-define-struct type_declaration (params cstrs kind manifest variance loc))

	; type_kind => ptype_abstract
	;            | ptype_variant
	;            | ptype_record

	;(make-ptype_abstract null)
	(p-define-struct ptype_abstract (null))
	;(make-ptype_variant list)
	(p-define-struct ptype_variant (string*core_type-list-list))
	;(make-ptype_record list)
	(p-define-struct ptype_record (string*mutable_flag-core_type-list))

	; with_constraint => pwith_type
	;                  | pwith_module

	;(make-pwith_type type_declaration)
	(p-define-struct pwith_type (decl))
	;(make-pwith_module longident)
	(p-define-struct pwith_module (ident))

)