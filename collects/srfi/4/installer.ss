(module installer mzscheme
  (require (lib "make.ss" "make")
           (lib "file.ss" "dynext")
           (lib "compile.ss" "dynext")
           (lib "link.ss" "dynext")
           (lib "file.ss")
           (lib "contract.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "setup-extension.ss" "make"))
  
  (provide pre-installer)
  
  (define compiler-include 
    (collection-path "compiler"))
  
  (define (delete/continue x)
    (with-handlers ([not-break-exn? void])
      (delete-file x)))
  
  (make-print-reasons #f)
  (make-print-checking #f)
  
  (define (type-name->file-stem type-name)
    (string-append "homo-" type-name "-vector-prims"))
  
  (define (type-name->glue-file-stem type-name)
    (string-append "homo-" type-name "-vector-glue"))
  
  (define (generate-group-header-file type-names source-dir)
    (call-with-output-file (build-path source-dir (string-append (type-name->file-stem "all") ".h"))
      (lambda (port)
        (for-each (lambda (type-name)
                    (fprintf port "#include \"~a.h\"\n" (type-name->file-stem type-name)))
                  type-names))
      'truncate))
  
  (define (generate-group-glue-file type-names source-dir)
    (call-with-output-file (build-path source-dir (string-append (type-name->glue-file-stem "all") ".c"))
      (lambda (port)
        (for-each (lambda (type-name)
                    (fprintf port "#include \"homo-~a-vector-glue.c\"\n" type-name))
                  type-names)
        (fprintf port "\nScheme_Object *set_all_vector_types(int argc, Scheme_Object **argv)\n{\n")       
        (for-each (lambda (type-name)
                    (fprintf port "  set_homo_~a_vector_type(argc, argv);\n" type-name))
                  type-names)
        (fprintf port "\n\n  return scheme_false;\n}\n"))
      'truncate))
  
  (define (make-make/proc-args plthome source-dir target-dir)
    (lambda (replace-spec)
      (let* ([name (type-name->file-stem (cadr replace-spec))]
             [dot-c (build-path source-dir (append-c-suffix name))]
             [glue-dot-c (build-path source-dir (append-c-suffix (type-name->glue-file-stem (cadr replace-spec))))]
             [dot-h (build-path source-dir (string-append name ".h"))]
             [dot-o (build-path source-dir (append-object-suffix name))]
             [shared-lib (build-path target-dir (append-extension-suffix name))])
        (printf "glue-dot-c name: ~v\n" glue-dot-c)
        (list
         (list (list shared-lib                                     ; shared-library
                    (list dot-c dot-h)
                    (lambda ()
                      (pre-install plthome
                                   (build-path (collection-path "srfi") "4")
                                   dot-c
                                   (build-path (collection-path "srfi") "4")
                                   `()
                                   `()
                                   `()
                                   `()
                                   `()
                                   `()
                                   (lambda (k) (k)))))
              (list dot-c                                           ; generated c file
                    (list "homo-vector-prims.c")
                    (lambda () 
                      (delete/continue dot-c)
                      (generate "homo-vector-prims.c" dot-c replace-spec)))
              (list glue-dot-c                                      ; generated c "glue" file
                    (list "homo-vector-glue.c")
                    (lambda ()
                      (delete/continue glue-dot-c)
                      (printf "generating glue-dot-c named: ~v\n" glue-dot-c)
                      (generate "homo-vector-glue.c" glue-dot-c replace-spec)))
              (list dot-h                                          ; generated header file
                    (list "homo-vector-prims.h")
                    (lambda ()
                      (delete/continue dot-h)
                      (generate "homo-vector-prims.h" dot-h replace-spec))))
         (vector shared-lib dot-c glue-dot-c)))))

  (define (generate input-file output-file trans)
    (let* ([in-str (call-with-input-file input-file
                     (lambda (in)
                       (read-string (expt 2 20) in)))]
           [caps-type-name (string-copy (cadr trans))]
           [_ (string-uppercase! caps-type-name)]
           [out-str (foldl (lambda (template-string instantiation-string accum)
                             (regexp-replace* template-string accum instantiation-string)) 
                           in-str
                           '("<caps-type-name>"
                             "<type>"
                             "<type-name>"
                             "<sreal-to-type>"
                             "<type-to-scheme>"
                             "<type-smallest>"
                             "<type-largest>"
                             "<enable-check>")
                           (cons caps-type-name
                                 trans))])
      (call-with-output-file output-file (lambda (x) (display out-str x)))))
  
  
  
  (define (pre-installer plthome)
    (let* ([trans-list 
            '(("double" "f64" "scheme_real_to_double" "scheme_make_double" "0" "0" "0") ; no bounds check performed on floating point numbers
              ("float" "f32" "scheme_real_to_double" "scheme_make_double" "0" "0" "0")
              ("int32_t" "s32" "scheme_get_int" "scheme_make_integer_value" "0" "0" "0") ; no bounds check performed on 32-bit integers
              ("int16_t" "s16" "scheme_get_int" "scheme_make_integer_value" "-0x8000" "0x7fff" "1")
              ("int8_t" "s8" "scheme_get_int" "scheme_make_integer_value" "-0x80" "0x7f" "1")
              ("uint32_t" "u32" "scheme_get_uint" "scheme_make_integer_value_from_unsigned" "0x0" "0xffffffff" "1")
              ("uint16_t" "u16" "scheme_get_uint" "scheme_make_integer_value_from_unsigned" "0x0" "0xffff" "1")
              ("uint8_t" "u8" "scheme_get_uint" "scheme_make_integer_value_from_unsigned" "0x0" "0xff" "1"))]
           [target-dir (build-path "compiled" "native" (system-library-subpath))]
           [source-dir (build-path "c-generation")]
           [specs/targets-list (map (make-make/proc-args plthome source-dir target-dir) trans-list)])

      (current-directory (build-path (collection-path "srfi") "4"))
      ; shouldn't be needed with call to pre-install: (make-directory* target-dir)
      (make-directory* source-dir)
      
      ; generate the group header file
      (generate-group-header-file (map cadr trans-list) source-dir)
      
      ; generate the group glue file
      (generate-group-glue-file (map cadr trans-list) source-dir)
      
      ; do the make for each sub-package
      (for-each (lambda (specs/targets) (apply make/proc specs/targets)) specs/targets-list))))
  
