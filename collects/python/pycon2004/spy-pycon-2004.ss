(module spy-pycon-2004 (lib "run.ss" "slideshow")
  (require (lib "etc.ss"))
  
  (define (file-here fname)
    (build-path (this-expression-source-directory)
                fname))
  
  (define (bitmap-here fname)
    (bitmap (file-here fname)))
 
  (define (py-kw kw)
    (bt kw))
  
  (define (spy-fn fn)
    (it fn))
  
  (slide/title/center
   "From Python to PLT Scheme"
   (t "Daniel Silva, Philippe Meunier")
   (it "dsilva, meunier@ccs.neu.edu")
   (size-in-pixels (bitmap "/home/daniel/plt/collects/icons/PLT-206.png"))
   (t "Northeastern University"))
  
  (slide/title
   "Motivation"
   (page-item "DrScheme is a good professional/educational PDE with nice tools")
   (page-item "Python is popular (many users, many libraries)")
   (page-item "Python should map easily to Scheme")
   (page-item "Multiple implementations are good for a language")
   (page-item "We want DrScheme (with tools) + Python (with libraries)"))
  
  (slide/title
   "DrScheme background"
   (page-item "DrScheme is a PDE for languages compiled into the MzScheme language")
   (page-item "The MzScheme language is a Scheme language with:"
              (page-item/bullet o-bullet "Namespaces")
              (page-item/bullet o-bullet "Hash tables")))
  
  (slide/title "DrScheme tools: CheckSyntax"
               (page-item "Binding information and syntax correctness")
               (bitmap-here "check-syntax-sshot.png"))

  (slide/title "DrScheme tools: Test Coverage"
               (scale (bitmap-here "test-coverage-sshot.png") 0.8))
  
  (slide/title "DrScheme tools: TestSuite"
               (scale (bitmap-here "test-suite-sshot.png") 0.8))

  (slide/title "DrScheme tools: Profiler"
               (scale (bitmap-here "profiler-sshot.png") 0.8))

  
#|               
   (page-item "DrScheme provides good tools"
              (page-item/bullet o-bullet "CheckSyntax: binding information and syntax correctness")
              (page-item/bullet o-bullet "Test coverage")
              (page-item/bullet o-bullet "Test suite")
              (page-item/bullet o-bullet "Profiler")
              (page-item/bullet o-bullet "Debugger"))
   (page-item "DrScheme makes it easy to create cross-language development tools"
              (page-item/bullet o-bullet "Simple tool (plugin) interface")
              (page-item/bullet o-bullet "Access to program AST")))
|#
               
  (slide/title
   "Spy: a Python-->MzScheme compiler for DrScheme"
   (page-item "Generates MzScheme source syntax objects"
              (page-subitem "Syntax objects: code + source location + extra properties"))
   (page-item "Provides a Python runtime system"))

  
  (slide/title "Code generation: Functions"
   (page-item "Definition"
     (page-subitem (py-kw "return") "statements becomes Scheme escapes")
     (page-subitem "Keyword arguments stored in the function's dictionary"))
   (page-item "Application"
     (page-subitem "Always applied through " (spy-fn "py-call"))
     (page-subitem "Tuple arguments unpacked inside function body")))
                                                                                
(slide/title "Code generation: Classes and objects"
  (page-item "All objects have an associated dictionary")
  (page-item "Inheritance information stored in dict (__bases__)")
  (page-item "All attributes stored in dictionary"))
                                                                                
(slide/title "Code generation: Modules"
  (page-item "MzScheme modules are checked at compile-time:"
    (page-subitem "No cycles")
    (page-subitem "Statically loaded at the top level")
    (page-subitem "No inter-module assignment")
    )
  (page-item "Modeled with MzScheme namespaces (top-level environments)"))

  
(slide/title "Status"
  (page-item "Spy project currently updating its runtime system"
    (page-subitem "Initially written in Scheme")
    (page-subitem "Porting system to calls to CPython's standard modules")
    )
  (page-item "C Extensions"
    (page-subitem "Currently implementing Python's C FFI/API")
    (page-subitem "Transparent source-level compatibility")
    ))
                                                                                
(slide/title "Tools: Some automagically work"
  (page-item "Check Syntax")
  (page-item "Test coverage"))
                                                                                
(slide/title "Tools: Some might work"
  (page-item "Test suite (looks promising, awaiting ProfJ results)")
  (page-item "Profiler (should work as easily as Test Coverage)")
  (page-item "Debugger (PLT Debugger GUI upcoming)"))
                                                                                
(slide/title "Tools: Python cannot be analyzed by MrFlow"
  (page-item "No type-based flow information for primitives yet.")
  (page-item "Everything in Python behaves like a hash table.")
  (page-item "Analysis would need to internally simulate the hash tables."))
                                                                                
(slide/title "Future Work"
  (page-item "Complete the FFI")
  (page-item "Optimization"
    (page-subitem "Tail calls: done!")
    (page-subitem "Constant folding: next")
    (page-subitem "Common subexpression elimination: maybe")
    (page-subitem "Other peephole optimizations...")
    )
  (page-item "Special Python module: scheme"
    (page-subitem "From within Python:"
       "import scheme  # special Scheme module
       scheme.x = 7   # define Scheme variable x")))

(slide/title "Conclusion"
  (page-item "We have implemented the Python language (finished yield, exec, slices by PyCon 04)")
  (page-item "We have begun embedding CPython's primitive types")
  (page-item "We have begun providing a source-compatible C API")
  (page-item "We provide access to DrScheme and its tools"))

  (slide/title "Acknowledgements"
               (page-para "Our many thanks to Matthias Felleisen, Scott Owens, and PLT Scheme"))
  
(slide/title "Contact us"
  (page-item "Information:"
    (page-subitem "http://spyweb.hopto.org"))
  (page-item "Volunteers:"
    (page-subitem "dsilva@ccs.neu.edu")))

  
  )