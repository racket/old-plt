#cs
(module general-parsing mzscheme
  
  (require (lib "lex.ss" "parser-tools"))
  
  (require "../ast.ss")
  (require "../parameters.ss")
  
  (provide (all-defined))

  ;;Definitions used by all parsers
  
  (define file-name (make-parameter null))
  
  ;;Methods used by all parsers
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
                 (make-block null #f)
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
)