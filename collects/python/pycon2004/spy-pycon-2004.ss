(module spy-pycon-2004 (lib "run.ss" "slideshow")
  (require (lib "etc.ss"))
  
  (define (file-here fname)
    (build-path (this-expression-source-directory)
                fname))
  
  (define (bitmap-here fname)
    (bitmap (file-here fname)))
 
  (define (std-bitmap fname . lib-path)
    (let ([fname (if (null? lib-path)
                     (build-path "mzlib" fname)
                     (apply build-path (append lib-path (list fname))))])
      (bitmap (ormap (lambda (s)
                       (let ([fname (build-path s fname)])
                         (and (file-exists? fname)
                              fname)))
                     (current-library-collection-paths)))))
  
  (define (py-kw kw)
    (bt kw))
  
  (define (spy-fn fn)
    (it fn))
  
  (define (py-code str)
    (it str))
  
  (define (cpy-sym name)
    (bt name))
  
  (define my-page-item (lambda args
                         (apply item 900 args)))
  
  (define my-page-subitem (lambda args
                           ; (apply item/bullet o-bullet 800 args)))
                            (apply subitem 850 args)))

  (define (my-page-item/bullet bullet . args)
    (apply item/bullet bullet 900 args))

  (define (code/w width . lines)
    (apply item/bullet (t " ") width
           (map (lambda (line)
                  (para width line))
                lines)))
  
  (define (code . lines) (apply code/w 900 lines))
  
  (define (code-append . picts)
    (apply para 900 picts))
  
  (define (code-sxp str)
    (colorize (t str) blue))
  
  (slide/title/center
   "From Python to PLT Scheme"
   (t "Daniel Silva, Philippe Meunier")
   (it "dsilva, meunier@ccs.neu.edu")
   (std-bitmap "PLT-206.png" "icons")
   (t "Northeastern University"))
  
  (slide/title
   "Motivation"
   (my-page-item "DrScheme is a good professional/educational PDE with nice tools")
   (my-page-item "Python is popular (many users, many libraries)")
   (my-page-item "Python should map easily to Scheme"
              (my-page-subitem "Features from FP languages")
              (my-page-subitem "Seemingly straightforward class system"))
   (my-page-item "Multiple implementations are good for a language"
              (my-page-subitem "Find bugs in the language, not the interpreter")
              (my-page-subitem "Different compilation techniques"))
   (my-page-item "We want DrScheme (with tools) + Python (with libraries)"))
  
  (slide/title
   "DrScheme background"
   (my-page-item "DrScheme is a PDE for languages compiled into the MzScheme language")
   (my-page-item "The MzScheme language is a Scheme dialect with:"
              (my-page-subitem "Namespaces")
              (my-page-subitem "Hash tables"))
   (my-page-item "DrScheme makes it easy to create cross-language development tools"
              (my-page-subitem "Simple tool (plugin) interface")
              (my-page-subitem "Access to program AST")))
  
  (slide/title "DrScheme tools: CheckSyntax"
               (my-page-item "Binding information and syntax correctness")
               (bitmap-here "check-syntax-sshot.png"))

  (slide/title "DrScheme tools: Test Coverage"
               (bitmap-here "test-coverage-sshot-fullwindow.png"))
  
  (slide/title "DrScheme tools: TestSuite"
               (bitmap-here "test-suite-sshot-fullwindow.png"))

  (slide/title "DrScheme tools: Profiler"
               (bitmap-here "profiler-sshot.png"))

  (slide/title "DrScheme tools: MrFlow"
               (bitmap-here "mrflow-sshot2.png"))
  
  (slide/title "DrScheme languages: ProfessorJ (Java)"
               (bitmap-here "profj-sshot2.png"))

  (slide/title "DrScheme languages: Dromedary (ML)"
               (bitmap-here "dromedary-sshot2.png"))
  
  (slide/title "DrScheme languages: Perl"
               (bitmap-here "perl-sshot3.png"))
#|               
   (my-page-item "DrScheme provides good tools"
              (my-page-item/bullet o-bullet "CheckSyntax: binding information and syntax correctness")
              (my-page-item/bullet o-bullet "Test coverage")
              (my-page-item/bullet o-bullet "Test suite")
              (my-page-item/bullet o-bullet "Profiler")
              (my-page-item/bullet o-bullet "Debugger"))
   (my-page-item "DrScheme makes it easy to create cross-language development tools"
              (my-page-item/bullet o-bullet "Simple tool (plugin) interface")
              (my-page-item/bullet o-bullet "Access to program AST")))
|#
               
  (slide/title "Spy:"
               (titlet "a Python to MzScheme compiler for DrScheme")
            
   (item 700 "Generates MzScheme source syntax objects"
              (subitem 600 "Syntax objects: code + source location + extra properties"))
   (item 700 "Provides a Python runtime system"))

  (slide/title/center "Code generation"
    (item 700 "Functions")
    (item 700 "Classes")
    (item 700 "Modules"))
  
  (slide/title/center "Code generation: Functions"
   (my-page-item "Definition"
     (my-page-subitem (cpy-sym "PyCode") "C object stores Scheme procedure")
     (my-page-subitem (cpy-sym "PyFunction") "C object created as in CPython")
     (my-page-subitem (py-kw "return") "statements become Scheme escapes"))
   (my-page-item "Application"
     (my-page-subitem "Always applied through " (spy-fn "py-apply"))
     (my-page-subitem "Tuple arguments unpacked inside function body")))
                                                                                
(slide/title/center "Code generation: Classes and objects"
  (my-page-item "All objects have an associated dictionary")
  (my-page-item "Inheritance information stored in dict" (py-code "(__bases__)"))
  (my-page-item "All attributes stored in dictionary"))
                                                                                
(slide/title/center "Code generation: Modules"
  (my-page-item "MzScheme modules are checked at compile-time:"
    (my-page-subitem "No cycles")
    (my-page-subitem "Statically loaded at the top level")
    (my-page-subitem "No inter-module assignment")
    )
  (my-page-item "Translated as MzScheme namespaces (top-level environments)"))

  (slide/title/center "Generating Syntax Objects"
     (code "#| Generate a while loop."
           "   `body` is an AST."
           "   `else` is either an AST or false."
           "|#"
           "(define/override (to-scheme)"
           " (->orig-so"
           "  (let ([normal-body"
           (code-append "        " (code-sxp "`(let/ec ,break-symbol"))
           (code-sxp "            (let ,continue-symbol ()")
           (code-sxp "              (py-if ,(send test to-scheme)")
           (code-append (code-sxp "                     (begin ") ",(send body to-scheme)")
           (code-append (code-sxp "                             (") ",continue-symbol" (code-sxp ")))))") "])")
           "    (if else"
           (code-append (code-sxp "        `(begin ") ",normal-body ,(send else to-scheme)" (code-sxp ")"))
           "                normal-body))))"))
  
  (slide/title "Syntax Objects: context"
       (code/w 1000
               ""
               ";; datum -> syntax-object"
               "(define/public (->orig-so datum)"
               "  (->lex-so datum (current-runtime-support-context)))"
               ""
               ";; datum context -> syntax-object"
               "(define/public (->lex-so datum context)"
               "  (datum->syntax-object context"
               "                                        datum"
               "                                        src-loc"
               "                                        stx-orig-prop))"))

  (slide/title "Syntax Objects: context: so what?"
        (code/w 1000
                "Generate code for an indentifier"
                ""
                "(define/override (to-scheme)"
                "  (->lex-so (get-symbol) (current-toplevel-context)))"
                ""
                "Back in the Python REPL..."
                ""
                "> x = 2"
                "> x"
                "2"
                "> car"
                "reference to undefined identifier: car"))

 (slide/title/center "Runtime system"
     (my-page-item "Minimal amount of Scheme code")
     (my-page-item "Hooks into CPython objects"))
 
 
 (slide/title/center "Runtime system: Scheme to Python"
     (code "// string -> PyString"
           "PyObject*"
           "make_py_string(int argc, Scheme_Object* argv[])"
           "{"
           "  Scheme_Object* str = argv[0];"
           "  return PyString_FromString(SCHEME_STR_VAL(str));"
           "}"))
 
 (slide/title/center "Runtime system: Python to Scheme"
      (code "// PyString -> string"
            "Scheme_Object*"
            "get_py_string(int argc, Scheme_Object* argv[])"
            "{"
            "  PyObject* obj = argv[0];"
            "  PyStringObject* pystr = (PyStringObject*) obj;"
            "  return pystr->ob_sval;"
            "}"))

; (slide/title/center "Runtime system: PyObject"
;       (code "typedef struct Spy_Scheme_Structure"
;             "{"
;             "  PyObject_HEAD"
;             "} PyObject;"
;             ))
 
 (slide/title/center "Runtime system: MzScheme structs"
        (code "struct Scheme_Structure"
              "{"
              "  Scheme_Type type;"
              "  MZ_HASH_KEY_EX"
              "  Scheme_Struct_Type* stype;"
              "  Scheme_Object* slots[1];"
              "};"
              ))
  
  (slide/title/center "Runtime system: python-node (v2)"
            (code "(define-struct python-node (type dict mutable?))"))
  
  (slide/title "Runtime system:"
              (titlet "python-node inside MzScheme (v2)")
        (my-page-item "Scheme:"
                      (code "(define-struct python-node (type dict mutable?))"))
        (my-page-item "C:"
                      (code "struct python_node"
                            "{"
                            "  Scheme_Type scheme_type;"
                            "  MZ_HASH_KEY_EX"
                            "  Scheme_Struct_Type* stype; // struct:python-node"
                            "  Scheme_Object* type; // slots[0]"
                            "  Scheme_Object* dict; // slots[1]"
                            "  Scheme_Object* is_mutable; // slots[2]"
                            "};"
                            )))

  (slide/title "Runtime system:"
              (titlet "python-node is PyObject (v2)")
        (my-page-item "Scheme:"
                      (code "(define-struct python-node (type dict mutable?))"))
        (my-page-item "pseudo-C:"
                      (code "typedef python_node PyObject;"
                            ""
                            "struct python_node"
                            "{"
                            "  Scheme_Type scheme_type;"
                            "  MZ_HASH_KEY_EX"
                            "  Scheme_Struct_Type* stype; // struct:python-node"
                            "  PyTypeObject* ob_type; // slots[0]"
                            "  python_node* dict; // slots[1]"
                            "  Scheme_Object* is_mutable; // slots[2]"
                            "};"
                            )))
  
  (slide/title "Runtime system:"
              (titlet "python-node inside MzScheme (v3)")
        (my-page-item "Scheme:"
                      (code "(define-struct python-node (type mutable?))"))
        (my-page-item "C:"
                      (code "struct python_node"
                            "{"
                            "  Scheme_Type scheme_type;"
                            "  MZ_HASH_KEY_EX"
                            "  Scheme_Struct_Type* stype; // struct:python-node"
                            "  python_node* ob_type; // slots[0]"
                            "  Scheme_Object* is_mutable; // slots[1]"
                            "};"
                            )))
 
 (slide/title "Runtime system:"
                     (titlet "python-node and PyObject_HEAD")
        (my-page-item "Scheme:"
                      (code "(define-struct python-node (type mutable?))"))
        (my-page-item "C:"
                      (code "#define PyObject_HEAD \\"
                            "    Scheme_Type scheme_type; \\"
                            "    MZ_HASH_KEY_EX \\"
                            "    void* struct_type; \\"
                            "    PYTYPEOBJECT* ob_type; \\"
                            "    Scheme_Object* is_mutable;")))
  
  (slide/title/center "Runtime system: PyObject"
                      (code
                            "typedef struct Spy_Scheme_Structure"
                            "{"
                            "  PyObject_HEAD"
                            "} PyObject;" ))
 
 
(slide/title "Status"
  (my-page-item "Spy project currently focusing on the runtime system"
    (my-page-subitem "Initially written in Scheme")
    (my-page-subitem "Porting system to calls to CPython's standard modules")
    )
  (my-page-item "C Extensions"
    (my-page-subitem "Currently implementing Python's C FFI/API")
    (my-page-subitem "Transparent source-level compatibility")
    ))
                                                                                
(slide/title/center "Tools: Check Syntax"
                    (bitmap-here "spy-check-syntax-oct-30-2003.png"))

(slide/title/center "Tools: Test coverage"
                    (bitmap-here "spy-expression-test-coverage-oct-30-2003.png"))

                                                                                
(slide/title/center "Tools: Some might work"
  (my-page-item "Test suite (looks promising, works with ProfJ)")
  (my-page-item "Profiler (should work as easily as Test Coverage)"))
                                                                                
(slide/title/center "Tools: Python cannot be analyzed by MrFlow"
  (my-page-item "No type-based flow information for primitives yet.")
  (my-page-item "Everything in Python behaves like a hash table.")
  (my-page-item "Analysis would need to internally simulate the hash tables."))
                                                                                
(slide/title/center "Future Work"
  (my-page-item "Complete the FFI")
  (my-page-item "Optimization"
    (my-page-subitem "Tail call translation: done! (so what?)")
    (my-page-subitem "Constant folding: next")
    (my-page-subitem "Common subexpression elimination: maybe")
    (my-page-subitem "Other peephole optimizations...")
    )
  (my-page-item "Special Python module: scheme"
    (my-page-subitem "From within Python:"
                     (code/w 700
                             "import scheme  # special Scheme module"
                             "scheme.x = 7   # define Scheme variable x"))))

(slide/title/center "Conclusion"
  (my-page-item "We have implemented the Python language")
  (my-page-item "We have the primitive Python types in Scheme")
  (my-page-item "We have begun embedding CPython's primitive types")
  (my-page-item "We have begun providing a source-compatible C API")
  (my-page-item "We provide access to DrScheme and its tools")
  (my-page-item "We provide Scheme access to Python"))

  (slide/title/center "Acknowledgements"
               (para 600 "Our many thanks to Matthias Felleisen, Scott Owens, and PLT Scheme"))
  
  (slide/title/center "Hacking"
     (my-page-item "1 year of spare time, most of Python done")
     (my-page-item "Easy to develop in DrScheme")
     (my-page-item "Easy to develop tools")
     (my-page-item "Easy optimizations on Spy ASTs")
     (my-page-item "Easy prototyping of new Python features")
     (my-page-item "Cross-language development")
     (my-page-item "Sharing libraries across languages")
     (my-page-item "Lure Schemers to Python!"))
  
(slide/title/center "Contact us"
               (bitmap-here "i-want-plt.png")
  (my-page-item "Information:"
    (my-page-subitem "http://spyweb.hopto.org"))
  (my-page-item "Volunteers:"
    (my-page-subitem "dsilva@ccs.neu.edu")))

  
  )