
(module tool-contracts "tool-contract-language.ss"                       
                       
                     ; 
                     ; 
  ;;;  ;   ;   ;;;   ; 
 ;   ; ;   ;  ;   ;  ; 
 ;   ;  ; ;       ;  ; 
 ;;;;;  ; ;    ;;;;  ; 
 ;      ; ;   ;   ;  ; 
 ;      ;;    ;   ;  ; 
  ;;;;   ;     ;;;;; ; 
                       


(drscheme:eval:set-basic-parameters
 ((listof (is-a?/c snip-class%)) . -> . void?)
 (snipclasses)
 "sets the parameters that are shared between the repl's"
 "initialization and \\iscmprocedure{drscheme:eval:build-user-eventspace/custodian}"
 ""
 "Specifically, it sets these parameters:"
 "\\begin{itemize}"
"\\item \\rawscm{current-namespace} has been set to a newly"
"  created empty namespace. This namespace has the following modules "
"  copied (with \\MzLink{mz:namespace-utilities}{\\rawscm{namespace-attach-module}})"
"  from DrScheme's original namespace:"
"  \\begin{itemize}"
"  \\item \\rawscm{'mzscheme}"
"  \\item \\rawscm{'(lib \"mred.ss\" \"mred\")}"
"  \\end{itemize}"
""
 "\\item"
 "  \\MzLink{mz:p:read-curly-brace-as-paren}{\\rawscm{read-curly-brace-as-paren}}"
 "  is \\rawscm{\\#t},"
 "\\item"
 "  \\MzLink{mz:p:read-square-bracket-as-paren}{\\rawscm{read-square-bracket-as-paren}}"
 "  is \\rawscm{\\#t},"
 "\\item "
 "  \\MzLink{mz:p:break-enabled}{\\rawscm{break-enabled}}"
 "  is \\rawscm{\\#t}"
 "\\item "
 "  \\MzLink{mz:p:error-print-width}{\\rawscm{error-print-width}} is set to 250."
 "\\item"
 "@flink current-ps-setup"
 "is set to a newly created"
 "@link ps-setup"
 "object."
 "\\item The \\MzLink{mz:p:exit-handler}{\\rawscm{exit-handler}} is set to"
 "a parameter that kills the user's custodian."
 "\\item The snip-class-list, returned by"
 "@flink get-the-snip-class-list"
 "is initialized with all of the snipclasses in DrScheme's eventspace's snip-class-list."
 ""
 "\\end{itemize}")

(drscheme:eval:get-snip-classes
 (-> (listof (is-a?/c snip-class%)))
 ()
 "Returns a list of all of the snipclasses in the current eventspace")

(drscheme:eval:expand-program
 ((union port? drscheme:language:text/pos?)
  drscheme:language-configuration:language-settings?
  boolean?
  (-> void?)
  (-> void?)
  ((union eof-object? syntax? (cons/p string? any?))
   (-> any)
   . -> .
   any)
  . -> .
  void?)
 (input language-settings eval-compile-time-part? init kill-termination iter)

"Use this function to expand the contents of the definitions"
"window for use with external program processing tools."
""
"This function uses"
"@flink drscheme:eval:build-user-eventspace/custodian"
"to build the user's environment."
"The arguments \\var{language-settings}, \\var{init}, and"
"\\var{kill-termination} are passed to"
"@flink drscheme:eval:build-user-eventspace/custodian %"
"."
""
"The \\var{input} argument specifies the source of the program."
""
"The \\var{eval-compile-time-part?} argument indicates if"
"\\Mzhyperref{\rawscm{expand}}{mz:expansion}"
"is called or if"
"\\scheme|expand-top-level-with-compile-time-evals|"
"is called when the program is expanded."
"Roughly speaking, if your tool will evaluate each expression"
"itself by calling"
"\\Mzhyperref{\\rawscm{eval}}{mz:evalload}"
"then pass \\scheme{#f}. Otherwise, if your tool"
"just processes the expanded program, be sure to pass"
"\\scheme{#t}."
""
"This function calls"
"@ilink drscheme:language:language expand-program/complete-program"
"to expand the program."
""
"The first argument to \\var{iter} is the expanded program"
"(represented as syntax) or eof."
"The \\var{iter} argument is called for each expression in the"
"expanded program and once more with eof, unless an error is"
"raised during expansion."
"It is called from the user's thread."
"If an exception is raised during expansion of the"
"user's program, \\var{iter} is not called."
"Consider setting the exception-handler during \\var{init} to"
"handle this situation."
""
"The second argument to \\var{iter} is a thunk that"
"continues expanding the rest of the contents of the"
"definitions window. If the first argument to \\var{iter} was"
"eof, this argument is just the primitive"
"\\rawscm{void}."
""
"See also"
"@flink drscheme:eval:expand-program/multiple %"
".")

(drscheme:eval:expand-program/multiple
 (drscheme:language-configuration:language-settings?
  boolean?
  (-> void?)
  (-> void?)
  . -> .
  ((union port? drscheme:language:text/pos?)
   ((union eof-object? syntax? (cons/p string? any?))
    (-> any)
    . -> .
    any)
   boolean?
   . -> .
   void?))
 (language-settings eval-compile-time-part? init kill-termination)

 "This function is just like"
 "@flink drscheme:eval:expand-program"
 "except that it is curried and the second application"
 "can be used multiple times."
 "Use this function if you want to initialize the user's"
 "thread (and namespace, etc) once but have program text"
 "that comes from multiple sources."
 ""
 "The extra boolean argument to the result function"
 "determines if"
 "@ilink drscheme:language:language front-end/complete-program"
 "or"
 "@ilink drscheme:language:language front-end/interaction"
 "is called.")

(drscheme:eval:build-user-eventspace/custodian
 ((drscheme:language-configuration:language-settings?
   (-> void?)
   (-> void?))
  . ->* .
  (eventspace? custodian?))
 (language-settings init kill-termination)

"This function creates a custodian and an eventspace (on the"
"new custodian) to expand the user's program. It does not"
"kill this custodian, but it can safely be shutdown (with"
"\\MzLink{mz:custodians}{custodian-shutdown-all}) after the"
"expansion is finished."
""
"It initializes the"
"user's eventspace's main thread with several parameters:"
"\\begin{itemize}"
"\\item \\rawscm{current-custodian} is set to a new custodian."
"\\item"
"In addition, it calls"
"@flink drscheme:eval:set-basic-parameters %"
"."
"\\end{itemize}"
""
"The \\var{language-settings} argument is the current"
"language and its settings. See"
"@flink drscheme:language-configuration:make-language-settings"
"for details on that structure."
""
"If the program is associated with a DrScheme"
"frame, get the frame's language settings from the"
"@ilink drscheme:unit:definitions-text get-next-settings"
"method of "
"@ilink drscheme:unit:definitions-text %"
".  Also, the most recently chosen language in"
"the language dialog is saved via the framework's"
"preferences. Apply"
"@flink preferences:get"
"to"
"@flink drscheme:language-configuration:get-settings-preferences-symbol"
"for that \\var{language-settings}."
""
"The \\var{init} argument is called after the user's parameters"
"are all set, but before the program is run. It is called on"
"the user's thread. The"
"\\MzLink{mz:p:current-directory}{current-directory} and"
"\\MzLink{mz:p:current-load-relative-directory}{current-load-relative-directory}"
"parameters are not set, so if there are appropriate directories,"
"the \\var{init} argument is a good place to set them."
""
"The \\var{kill-termination} argument is called when the main thread of"
"the eventspace terminates, no matter if the custodian was"
"shutdown, or the thread was killed. This procedure is also"
"called when the thread terminates normally. This procedure is"
"called from a new, dedicated thread ({\\it i. e.}, not the thread"
"created to do the expansion, nor the thread that"
"\\rawscm{drscheme:eval:build-user-eventspace/custodian} was called from.)")


                                   
    ;;        ;;                   
     ;         ;                   
     ;         ;                   
  ;;;;   ;;;   ;;;;  ;;  ;;   ;;; ;
 ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
 ;   ;  ;;;;;  ;   ;  ;   ;  ;   ; 
 ;   ;  ;      ;   ;  ;   ;  ;   ; 
 ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
  ;;; ;  ;;;  ; ;;;    ;;; ;  ;;;; 
                                 ; 
                                 ; 
                              ;;;  


(drscheme:debug:make-debug-error-display-handler
 ((string? (union any? exn?) . -> . any)
  . -> .
  (string? (union any? exn?) . -> . any))

 (oedh)
 
"This function implements an error-display-handler in terms"
"of another error-display-handler."
""
"This function is designed to work in conjunction with"
"@flink drscheme:debug:make-debug-eval-handler %"
"."
""
"See also MzScheme's"
"MzLink{mz:p:error-display-handler}{error-display-handler}"
"parameter."
""
"If the current-error-port is the definitions window in"
"drscheme, this error handler inserts some debugging"
"annotations, calls \\var{oedh}, and then highlights the"
"source location of the runtime error.")

(drscheme:debug:make-debug-eval-handler
 ((any? . -> . any?)
  . -> .
  (any? . -> . any?))

 (odeh)

"This function implements an eval-handler in terms of another"
"eval-handler."
""
"This function is designed to work in conjunction with"
"@flink drscheme:debug:make-debug-error-display-handler %"
"."
""
"See also MzScheme's MzLink{mz:p:eval-handler}{eval-handler}"
"parameter. "
""
"The resulting eval-handler expands and annotates the input"
"expression and then passes it to the input eval-handler,"
"unless the input expression is already compiled, in which"
"case it just hands it directly to the input eval-handler.")

(drscheme:debug:hide-backtrace-window
  (-> void?)
  ()
  "Hides the backtrace window.")


(drscheme:debug:profiling-enabled
 (case-> (boolean? . -> . void?)
	 (-> boolean?))
 ((enabled?) ())
 "A parameter that controls if profiling information is recorded."
 ""
 "Defaults to \\scm{\\#f}."
 ""
 "Only applies if"
 "@flink drscheme:debug:make-debug-eval-handler"
 "has been added to the eval handler.")

(drscheme:debug:add-prefs-panel
 (-> void?)
 ()
 "Adds the profiling preferences panel.")

(drscheme:debug:open-and-highlight-in-file
 ((cons/p (union symbol? (is-a?/c editor<%>))
	  (cons/p number? number?))
  . -> .
  void?)
 (debug-info)
 "This function opens a DrScheme to display"
 "\\var{debug-info}. The first element in"
 "the cons indicates where the file is"
 "and the two number indicate a range of"
 "text to show."
 ""
 "See also"
 "@flink drscheme:debug:get-cm-key %"
 ".")

(drscheme:debug:show-backtrace-window
 (string?
  (listof (cons/p (union symbol? (is-a?/c editor<%>))
		  (cons/p number? number?)))
  . -> .
  void?)
 (error-message dis)
 "Shows the backtrace window you get when clicking on the bug in"
 "DrScheme's REPL."
 ""
 "The \\var{error-message} argument is the text of the error,"
 "and \\var{dis} is the debug information, extracted from the"
 "continuation mark in the exception record, using"
 "@flink drscheme:debug:get-cm-key %"
 ".")

(drscheme:debug:get-cm-key
 (-> any)
 ()
 "Returns a key used with \\scheme|contination-mark-set->list|."
 "The contination mark set attached to an exception record"
 "for the user's program may use this mark. If it does,"
 "each mark on the continuation is the same type as"
 "the input to"
 "@flink drscheme:debug:open-and-highlight-in-file %"
 ".")


                            
                 ;          
                            
                            
  ;;;  ; ;;;   ;;;   ; ;;;  
 ;   ;  ;;  ;    ;    ;   ; 
  ;;;   ;   ;    ;    ;   ; 
     ;  ;   ;    ;    ;   ; 
 ;   ;  ;   ;    ;    ;   ; 
  ;;;  ;;;  ;; ;;;;;  ;;;;  
                      ;     
                      ;     
                     ;;;    


(drscheme:number-snip:make-repeating-decimal-snip
 (number? boolean? . -> . (is-a?/c snip%))
 (num show-prefix?)

"Makes a number snip for DrScheme's REPL that is in the"
"decimal view state."
"The boolean indicates if a {\\tt \\#e} prefix appears"
"on the number."
""
"See also"
"@flink drscheme:number-snip:make-fraction-snip %"
".")

(drscheme:number-snip:make-fraction-snip
  (number? boolean? . -> . (is-a?/c snip%))
  (num show-prefix-in-decimal-view?)

"Makes a number snip for DrScheme's REPL that is in the"
"fraction view state."
"The boolean indicates if a {\\tt \\#e} prefix appears"
"on the number in the decimal state"
""
"See also"
"@flink drscheme:number-snip:make-repeating-decimal-snip %"
".")


                            
                 ;          
                       ;    
                       ;    
;;  ;; ; ;;;   ;;;    ;;;;; 
 ;   ;  ;;  ;    ;     ;    
 ;   ;  ;   ;    ;     ;    
 ;   ;  ;   ;    ;     ;    
 ;   ;  ;   ;    ;     ;   ;
  ;;; ;;;;  ;; ;;;;;    ;;; 
                            
                            
                            

(drscheme:unit:get-program-editor-mixin
 (-> ((subclass?/c text%) . -> . (subclass?/c text%)))
 ()
 "Returns a mixin that must be mixed in to any"
 "\\iscmclass{text} object that might contain"
 "program text (and thus can be in the source"
 "field of some syntax object)."
 ""
 "See also"
 "@flink drscheme:unit:add-to-program-editor-mixin %"
 ".")

(drscheme:unit:add-to-program-editor-mixin
 (((subclass?/c text%) . -> . (subclass?/c text%)) . -> . void?)
 (mixin)
 "\\phase{1}."
 ""
 "Adds \\var{mixin} to the result of"
 "@flink drscheme:unit:get-program-editor-mixin %"
 ".")
  
(drscheme:unit:open-drscheme-window
 (case->
  (-> (is-a?/c drscheme:unit:frame%))
  ((union string? false?) . -> . (is-a?/c drscheme:unit:frame%)))
 (() (filename))

"Opens a drscheme frame that displays \\var{filename},"
"or nothing if \\var{filename} is \\rawscm{\\#f} or not supplied.")

(drscheme:unit:make-bitmap
 (case->
  (string? . -> . ((is-a?/c area-container<%>) . -> . (union string? (is-a?/c bitmap%))))
  (string? string? . -> . ((is-a?/c area-container<%>) . -> . (union string? (is-a?/c bitmap%)))))

 ((button-name) (text filename))
 
"This function constructs a bitmap for a button label. It is"
"used for the buttons on the top row of DrScheme's frame."
""
"When one argument is supplied, this function"
"constructs a button from the image in the \\File{icons}"
"collection named by the \\var{button-name} with {\\tt .bmp}"
"added to the end of the name."
"The button's label is also \\var{button-name},"
"but with the first letter capitalized."
""
"When two arguments are supplied, constructs a button with"
"\\var{text} as the button's label and where \\var{filename}"
"specifies the full path to the bitmap"
""
"The \\iscmintf{area-container} argument is used to"
"find the font for the label"
""
"If the bitmap isn't found, this function returns a string"
"to be used as the button's label.")



                     
                     
                     
                     
 ; ;;;   ;;;  ; ;;;  
  ;     ;   ;  ;   ; 
  ;     ;;;;;  ;   ; 
  ;     ;      ;   ; 
  ;     ;   ;  ;   ; 
 ;;;;    ;;;   ;;;;  
               ;     
               ;     
              ;;;    


(drscheme:rep:get-error-ranges
 (-> (union false? (cons/p (list/p any? number? number?) (listof (list/p any? number? number?)))))
 ()
 "Returns the currently highlighted error range, or \\scheme|#f|"
 "if there is none.")

(drscheme:rep:reset-error-ranges
 (-> void?)
 ()
 "Clears the current error highlighting.")

(drscheme:rep:insert-error-in-text
 ((is-a?/c text%)
  (union false? (is-a?/c drscheme:rep:text<%>))
  string?
  exn?
  (union false? (and/f string? directory-exists?))
  . -> .
  void?)
 (text rep-text msg exn dir)
 "Formats and inserts the error message described by"
 "\\var{msg} and \\var{exn} into the text% object \\var{text}."
 ""
 "The \\var{rep-text} argument is used to trigger the actual"
 "highlighting."
 ""
 "The \\var{msg} and \\var{exn} arguments are expected to"
 "come from the"
 "\\MzLink{mz:p:error-print-source-location}{\\scheme|error-display-handler|},"
 "when the"
 "\\MzLink{mz:p:error-print-source-location}{\\scheme|error-print-source-location|}"
 "parameter is set to \\scheme|#f|."
 ""
 "The \\var{user-dir} argument is the current directory of"
 "the program where the error occurred. If it is a string,"
 "it is used to shorten the path the file where the error"
 "occurred."
 ""
 "See also"
 "@flink drscheme:rep:insert-error-in-text/highlight-errors %"
 ".")

(drscheme:rep:insert-error-in-text/highlight-errors
 ((is-a?/c text%)
  ((listof (list/p (is-a?/c text%) number? number?)) . -> . void?)
  string?
  exn?
  (union false? (and/f string? directory-exists?))
  . -> . 
  void?)
 (text highlight-errors msg exn dir)
 "Formats and inserts the error message described by"
 "\\var{msg} and \\var{exn} into the text% object \\var{text}."
 ""
 "The \\var{highlight-errors} argument is used to highlight the"
 "source location of the error."
 ""
 "The \\var{msg} and \\var{exn} arguments are expected to"
 "come from the"
 "\\MzLink{mz:p:error-print-source-location}{\\scheme|error-display-handler|},"
 "when the"
 "\\MzLink{mz:p:error-print-source-location}{\\scheme|error-print-source-location|}"
 "parameter is set to \\scheme|#f|."
 ""
 "The \\var{user-dir} argument is the current directory of"
 "the program where the error occurred. If it is a string,"
 "it is used to shorten the path the file where the error"
 "occurred."
 ""
 "See also"
 "@flink drscheme:rep:insert-error-in-text %"
 ".")
 
(drscheme:rep:exn:locs?
 (any? . -> . boolean?)
 (val)
 "Determines if \\var{val} is an exn:loc or not.")
(drscheme:rep:exn:locs-locs
 (drscheme:rep:exn:locs? . -> . (listof (list/p (is-a?/c text:basic<%>) number? number?)))
 (loc)
 "Extracts the loc field from the exn.")
(drscheme:rep:make-exn:locs
 (string?
  continuation-mark-set?
  (listof (list/p (is-a?/c text:basic<%>) number? number?))
  . -> .
  drscheme:rep:exn:locs?)
 (message continuation-mark-set locs)
 "Constructs an exn:loc."
 "These exceptions are handled specially by DrScheme's"
 "REPL. The source locations inside them are highlighted"
 "by the default exception handler.")

(drscheme:rep:get-drs-bindings-keymap
 (-> (is-a?/c keymap%))
 ()
 "Returns a keymap that bindings various DrScheme-specific"
 "keybindings. This keymap is used in the definitions"
 "and interactions window."
 ""
 "Defaultly binds C-x;o to a function that switches"
 "the focus between the definitions and interactions"
 "windows. Also binds f5 to Execute and f1 to Help Desk.")

(drscheme:rep:current-rep
  (-> (is-a?/c drscheme:rep:text%))
  ()

"This is a parameter whose value should not be set by tools."
"It is initialized to the repl that controls this evaluation"
"in the user's thread")

(drscheme:rep:which-number-snip
 (case->
  ((number?
    . -> .
    (symbols 'mixed-fraction 'mixed-fraction-e 'repeating-decimal 'repeating-decimal-e))
  . -> .
  void?)
 (->
  (number?
   . -> .
   (symbols 'mixed-fraction 'mixed-fraction-e 'repeating-decimal 'repeating-decimal-e))))
 ((which-number-snip) ())

"This function is called if"
"@flink drscheme:rep:use-number-snip"
"returns \\rawscm{\\#t} for some kind of snip."
"When that happens, this parameter determines what"
"kind of snip to use."
""
"The symbol \\rawscm{'mixed-fraction} indicates a mixed"
"fraction snip. The symbol \\rawscm{'repeating-decimal}"
"indicates a decimal expansion, possibly with an overbar on a"
"suffix of the decimal expansion indicating that suffix is"
"repeated forever. Either symbol suffixed with \\rawscm{-e} is"
"the same, except that an \\rawscm{\\#e} is prefixed to the"
"number when viewed in decimal notation.")

(drscheme:rep:use-number-snip
 (case->
  (-> (any? . -> . boolean?))
  ((any? . -> . boolean?) . -> . void?))
 (() (use-number-snip?))

"This is a parameter whose value is a predicate determines if"
"DrScheme uses a mixed fraction snip, a repeating decimal"
"snip, or a regular ASCII improper fraction for printing"
"numbers. "
""
"If the value of the parameter returns \\rawscm{\\#t}, a mixed"
"improper fraction snip is used. If it returns"
"\\rawscm{'repeating-decimal}, a repeating decimal snip is"
"used. If it returns \\rawscm{\\#f}, an ASCII improper fraction"
"is used."
""
"Its default value is:"
"\\begin{verbatim}"
"(lambda (x)"
"   (if (and (number? x)"
"            (exact? x)"
"            (real? x)"
"            (not (integer? x)))"
"       #t"
"       #f))"
"\\end{verbatim}"
""
"The value of this parameter must not return \\rawscm{\\#t}"
"more often than the above code, or else the snip"
"implementation will fail. It may, however, return"
"\\rawscm{\\#f} more often.")


                                                                      
                           ;                                       ;; 
                ;         ;                 ;                       ; 
                ;         ;                 ;                       ; 
  ;;; ;  ;;;   ;;;;;     ;    ;;;  ;;; ;;; ;;;;;   ;;;  ; ;;;    ;;;; 
 ;   ;  ;   ;   ;        ;   ;   ;   ; ;    ;     ;   ;  ;;  ;  ;   ; 
 ;   ;  ;;;;;   ;       ;    ;;;;;    ;     ;     ;;;;;  ;   ;  ;   ; 
 ;   ;  ;       ;       ;    ;       ; ;    ;     ;      ;   ;  ;   ; 
 ;   ;  ;   ;   ;   ;  ;     ;   ;  ;   ;   ;   ; ;   ;  ;   ;  ;   ; 
  ;;;;   ;;;     ;;;   ;      ;;;  ;;   ;;   ;;;   ;;;  ;;;  ;;  ;;; ;
     ;                ;                                               
     ;                                                                
  ;;;                                                                 


(drscheme:get/extend:extend-interactions-text
 (case->
  ((make-mixin-contract drscheme:rep:text<%>) . -> . void?)
  ((make-mixin-contract drscheme:rep:text<%>) boolean? . -> . void?))
 ((mixin) (mixin before?))

"This text is used in the bottom window of drscheme frames."
""
"The argument, \\var{before}, controls if the mixin is applied before or"
"after already installed mixins."
"If unsupplied, this is the same as supplying \\rawscm{\\#t}.")

(drscheme:get/extend:get-interactions-text
 (-> (implementation?/c drscheme:rep:text<%>))
 ()

"Once this function is called, "
"@flink drscheme:get/extend:extend-interactions-text "
"raises an error, disallowing any more extensions.")

(drscheme:get/extend:extend-definitions-text
 (case->
  ((make-mixin-contract drscheme:unit:definitions-text<%>) . -> . void?)
  ((make-mixin-contract drscheme:unit:definitions-text<%>) boolean? . -> . void?))
 ((mixin) (mixin before?))

"This text is used in the top window of drscheme frames."
""
"The argument, \\var{before}, controls if the mixin is applied before or"
"after already installed mixins."
"If unsupplied, this is the same as supplying \\rawscm{\\#f}.")

(drscheme:get/extend:get-definitions-text
 (-> (implementation?/c drscheme:unit:definitions-text<%>))
 ()

"Once this function is called, "
"@flink drscheme:get/extend:extend-definitions-text "
"raises an error, disallowing any more extensions.")

(drscheme:get/extend:extend-interactions-canvas
 (case->
  ((make-mixin-contract drscheme:unit:interactions-canvas%) . -> . void?)
  ((make-mixin-contract drscheme:unit:interactions-canvas%) boolean? . -> . void?))
 ((mixin) (mixin before?))

"This canvas is used in the bottom window of drscheme frames."
""
"The argument, \\var{before}, controls if the mixin is applied before or"
"after already installed mixins."
"If unsupplied, this is the same as supplying \\rawscm{\\#f}.")

(drscheme:get/extend:get-interactions-canvas
 (-> (subclass?/c drscheme:unit:interactions-canvas%))
 ()

"Once this function is called, "
"@flink drscheme:get/extend:extend-interactions-canvas"
"raises an error, disallowing any more extensions.")

(drscheme:get/extend:extend-definitions-canvas
 (case->
  ((make-mixin-contract drscheme:unit:definitions-canvas%) . -> . void?)
  ((make-mixin-contract drscheme:unit:definitions-canvas%) boolean? . -> . void?))
 ((mixin) (mixin before?))

"This canvas is used in the top window of drscheme frames."

"The argument, \\var{before}, controls if the mixin is applied before or"
"after already installed mixins."
"If unsupplied, this is the same as supplying \\rawscm{\\#f}.")

(drscheme:get/extend:get-definitions-canvas
 (-> (subclass?/c drscheme:unit:definitions-canvas%))
 ()

"Once this function is called, "
"@flink drscheme:get/extend:extend-definitions-canvas"
"raises an error, disallowing any more extensions.")

(drscheme:get/extend:extend-unit-frame
 (case->
  ((make-mixin-contract drscheme:unit:frame%) . -> . void?)
  ((make-mixin-contract drscheme:unit:frame%) boolean? . -> . void?))
 ((mixin) (mixin before?))

"This is the frame that implements the main drscheme window."
""
"The argument, \\var{before}, controls if the mixin is applied before or"
"after already installed mixins."
"If unsupplied, this is the same as supplying \\rawscm{\\#f}.")

(drscheme:get/extend:get-unit-frame
 (-> (subclass?/c drscheme:unit:frame%))
 ()

"Once this function is called, "
"@flink drscheme:get/extend:extend-unit-frame"
"raises an error, disallowing any more extensions.")



                                                        
 ;;;                                                    
   ;                                                    
   ;                                                    
   ;    ;;;;  ; ;;;    ;;; ;;;  ;;  ;;;;    ;;; ;  ;;;  
   ;        ;  ;;  ;  ;   ;  ;   ;      ;  ;   ;  ;   ; 
   ;     ;;;;  ;   ;  ;   ;  ;   ;   ;;;;  ;   ;  ;;;;; 
   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;     
   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
 ;;;;;;  ;;; ;;;;  ;;  ;;;;   ;;; ;  ;;; ;  ;;;;   ;;;  
                          ;                    ;        
                          ;                    ;        
                       ;;;                  ;;;         


                                                                                           
                        ;;;    ;                                         ;                 
                       ;                                         ;                         
                       ;                                         ;                         
  ;;;    ;;;  ; ;;;   ;;;;;  ;;;     ;;; ;;;  ;;  ; ;;;  ;;;;   ;;;;;  ;;;     ;;;  ; ;;;  
 ;   ;  ;   ;  ;;  ;   ;       ;    ;   ;  ;   ;   ;         ;   ;       ;    ;   ;  ;;  ; 
 ;      ;   ;  ;   ;   ;       ;    ;   ;  ;   ;   ;      ;;;;   ;       ;    ;   ;  ;   ; 
 ;      ;   ;  ;   ;   ;       ;    ;   ;  ;   ;   ;     ;   ;   ;       ;    ;   ;  ;   ; 
 ;   ;  ;   ;  ;   ;   ;       ;    ;   ;  ;   ;   ;     ;   ;   ;   ;   ;    ;   ;  ;   ; 
  ;;;    ;;;  ;;;  ;; ;;;;   ;;;;;   ;;;;   ;;; ; ;;;;    ;;; ;   ;;;  ;;;;;   ;;;  ;;;  ;;
                                        ;                                                  
                                        ;                                                  
                                     ;;;                                                   


(drscheme:language-configuration:add-language
 ((is-a?/c drscheme:language:language<%>)
  . -> .
  void?)
 (language)

"\\phase{2}"
""
"Adds \\var{language} to the languages offerend by DrScheme.")

(drscheme:language-configuration:get-settings-preferences-symbol
 (-> symbol?)
 ()
"Returns the symbol that is used to store the user's language"
"settings. Use as an argument to either"
"@flink preferences:get"
"or"
"@flink preferences:set %"
".")

(drscheme:language-configuration:make-language-settings
 ((implementation?/c drscheme:language:language<%>)
  any?
  . -> .
  drscheme:language-configuration:language-settings?)
 (language settings)

"This is the constructor for a record consisting of two"
"elements, a language and its settings. "
""
"The settings is a language-specific record that holds a"
"value describing a parameterization of the language."
""
"It has two selectors,"
"@flink drscheme:language-configuration:language-settings-language"
"and "
"@flink drscheme:language-configuration:language-settings-settings %"
", and a predicate,"
"@flink drscheme:language-configuration:language-settings?")

(drscheme:language-configuration:language-settings-settings
 (drscheme:language-configuration:language-settings?
  . -> .
  any?)
 (ls)
 "Extracts the settings field of a language-settings.")

(drscheme:language-configuration:language-settings-language
 (drscheme:language-configuration:language-settings?
  . -> .
  (is-a?/c drscheme:language:language<%>))
 (ls)

"Extracts the language field of a language-settings.")

(drscheme:language-configuration:language-settings?
 (any? . -> . boolean?)
 (val)

 "Determines if the argument is a langauge-settings or not.")

  (drscheme:language-configuration:language-dialog
   (opt->
    (boolean? drscheme:language-configuration:language-settings?)
    ((union false? (is-a?/c top-level-window<%>)))
    drscheme:language-configuration:language-settings?)
   ((show-welcome? language-settings-to-show)
    ((parent #t)))
   "Opens the language configuration dialog."
   "See also"
   "@flink drscheme:language-configuration:fill-language-dialog %"
   "."
   ""
   "The \\var{show-welcome?} argument determines if"
   "if a ``Welcome to DrScheme'' message and some"
   "natural language buttons are shown."
   ""
   "The \\var{language-settings-to-show} argument"
   "must be some default language settings that the dialog"
   "is initialized to."
   "If unsure of a default, the currently set language"
   "in the user's preferences can be obtained via:"
   "\\begin{schemedisplay}"
   "(preferences:get (drscheme:language-configuration:get-settings-preferences-symbol))"
   "\\end{schemedisplay}"
   ""
   "The \\var{parent} argument is used as the parent"
   "to the dialog.")
  
  (drscheme:language-configuration:fill-language-dialog
   ((is-a?/c vertical-panel%)
    (is-a?/c area-container<%>)
    drscheme:language-configuration:language-settings?
    . -> .
    drscheme:language-configuration:language-settings?)
   (panel button-panel language-setting)
   "This procedure accepts two parent panels and"
   "fills them with the contents of the language dialog."
   "It is used to include language configuration controls"
   "in some larger context in another dialog."
   ""
   "The \\var{panel} argument is the main panel where the"
   "language controls will be placed."
   "The function adds buttons to the \\var{button-panel}"
   "to revert a language to its default settings and to"
   "show the details of a language."
   ""
   "The \\var{language-setting} is the default"
   "language to show in the dialog.")
                                                        
 ;;;                                                    
   ;                                                    
   ;                                                    
   ;    ;;;;  ; ;;;    ;;; ;;;  ;;  ;;;;    ;;; ;  ;;;  
   ;        ;  ;;  ;  ;   ;  ;   ;      ;  ;   ;  ;   ; 
   ;     ;;;;  ;   ;  ;   ;  ;   ;   ;;;;  ;   ;  ;;;;; 
   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;     
   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
 ;;;;;;  ;;; ;;;;  ;;  ;;;;   ;;; ;  ;;; ;  ;;;;   ;;;  
                          ;                    ;        
                          ;                    ;        
                       ;;;                  ;;;         


(drscheme:language:extend-language-interface
 (interface?
  ((implementation?/c drscheme:language:language<%>) . ->d . (lambda (%) (subclass?/c %)))
  . -> .
  void?)
 (interface default-implementation)

"\\phase{1}"
""
"Each language added passed to"
"@flink drscheme:language-configuration:add-language"
"must implement \\var{interface}. "
""
"The \\var{default-implementation} is a mixin"
"that provides a default implementation of "
"\\var{interface}. Languages that are unaware of"
"the specifics of \\var{extension} use"
"\\var{default-implementation} via"
"@flink drscheme:language:get-default-mixin %"
".")

(drscheme:language:get-default-mixin
 (-> ((implementation?/c drscheme:language:language<%>) . ->d . (lambda (%) (subclass?/c %))))
 ()

"\\phase{2}"
""
"The result of this function is the composite of all of the "
"\\var{default-implementation} arguments passed"
"to"
"@flink drscheme:language:extend-language-interface %"
".")

(drscheme:language:get-language-extensions
 (-> (listof interface?))
 ()
 "\\phase{2}"
 ""
 "Returns a list of the interfaces passed to"
 "@flink drscheme:language:extend-language-interface %"
 ".")

(drscheme:language:put-executable
 ((is-a?/c top-level-window<%>) string? boolean? boolean? string? . -> . (union false? string?))
 (parent program-filename mred? launcher? title)
 "Calls the MrEd primitive"
 "@flink put-file"
 "with arguments appropriate for creating an executable"
 "from the file \\var{program-filename}. "
 ""
 "The arguments \\var{mred?} and \\var{launcher?} indicate"
 "what type of executable this should be (and the dialog"
 "may be slightly different on some platforms, depending"
 "on these arguments)."
 ""
 "The \\var{title} argument is used as the title to the primitive"
 "@flink put-file"
 "or"
 "@flink get-directory"
 "primitive.")

(drscheme:language:create-executable-gui
 ((union false? (is-a?/c top-level-window<%>))
  (union false? string?)
  (union (lambda (x) (eq? x #t)) (symbols 'launcher 'standalone))
  (union (lambda (x) (eq? x #t)) (symbols 'mzscheme 'mred))
  . -> .
  (union false?
	 (list/p (symbols 'no-show 'launcher 'stand-alone)
		 (symbols 'no-show 'mred 'mzscheme)
		 string?)))
 (parent program-name show-type? show-base?)
 "Opens a dialog to prompt the user about their choice of executable."
 "If \\var{show-type?} is \\scm{\\#t}, the user is prompted about"
 "a choice of executable: stand-alone, or launcher. If \\var{show-base?}"
 "is \\scm{\\#t}, the user is prompted about a choice of base"
 "binary: mzscheme or mred."
 ""
 "The \\var{program-name} argument is used to construct the default"
 "executable name in a platform-specific manner."
 ""
 "The \\var{parent} argument is used for the parent of the dialog."
 ""
 "The result of this function is \\scm{\\#f} if the user cancel's"
 "the dialog and a list of three items indicating what options"
 "they chose. If either \\var{show-type?} or \\var{show-base?}"
 "was \\scm{\\#f}, the corresponding result will be \\scm{'no-show},"
 "otherwise it will indicate the user's choice.")

(drscheme:language:create-module-based-stand-alone-executable 
 (string? string? any? any? any? boolean? boolean?
  . -> .
  void?)
 (program-filename
  executable-filename
  module-language-spec
  transformer-module-language-spec
  init-code
  gui?
  use-copy?)

"This procedure creates a stand-alone executable in the file"
"\\var{executable-filename} that runs the program"
"\\var{program-filename}. "
""
"The arguments"
"\\var{module-language-spec} and"
"\\var{transformer-module-language-spec} specify the "
"settings of the initial namespace, both the transformer"
"portion and the regular portion. "
""
"The \\var{init-code} argument is an s-expression representing"
"the code for a module. This module is expected to provide"
"the identifer \\rawscm{init-code}, bound to a procedure of no"
"arguments. That module is required and the \\scm{init-code}"
"procedure is executed to initialize language-specific"
"settings before the code in \\var{program-filename} runs."
""
"The \\var{gui?} argument indicates if a MrEd or MzScheme"
"stand-alone executable is created."
""
"The \\var{use-copy?} argument indicates if the initial"
"namespace should be populated with"
"\\rawscm{namespace-require/copy} or"
"\\rawscm{namespace-require}. ")

(drscheme:language:create-module-based-launcher
 (string? string? any? any? any? boolean? boolean?
  . -> .
  void?)
 (program-filename
  executable-filename
  module-language-spec
  transformer-module-language-spec
  init-code
  gui?
  use-copy?)

"This procedure is identical to "
"@flink drscheme:language:create-module-based-stand-alone-executable %"
", except that it creates a launcher instead of a"
"stand-alone executable.")

(drscheme:language:get-post-hash-bang-start
 ((is-a?/c text%) . -> . (>=/c 0))
 (text)
 "Returns the starting position of this text,"
 "skipping over \\#! if there is one. If there"
 "is no \\#!, returns 0.")

(drscheme:language:text/pos-text
 (drscheme:language:text/pos? . -> . (is-a?/c text%))
 (text/pos)

"Selects the \\iscmclass{text} from a text/pos.")

(drscheme:language:text/pos-start
 (drscheme:language:text/pos? . -> . number?)
 (text/pos)

"Selects the starting position from a text/pos.")

(drscheme:language:text/pos-end
 (drscheme:language:text/pos? . -> . number?)
 (text/pos)

"Selects the ending position from a text/pos.")

(drscheme:language:text/pos?
 (any? . -> . boolean?)
 (val)

"Returns \\rawscm{\\#t} if \\var{val} is a text/pos, and \\rawscm{\\#f}"
"otherwise.")

(drscheme:language:make-text/pos
 ((is-a?/c text%) number? number?
  . -> .
  drscheme:language:text/pos?)
 (text start end)

"Constructs a text/pos.")

(drscheme:language:simple-settings-case-sensitive 
 (drscheme:language:simple-settings? . -> . boolean?)
 (simple-settings)

"Extracts the case-sensitive setting from a simple-settings.")

(drscheme:language:simple-settings-printing-style
 (drscheme:language:simple-settings?
  . -> .
  (symbols 'constructor 'quasiquote 'write 'current-print))
 (simple-settings)

"Extracts the printing-style setting from a simple-settings.")

(drscheme:language:simple-settings-fraction-style
 (drscheme:language:simple-settings?
  . -> .
  (symbols 'mixed-fraction
	   'mixed-fraction-e
	   'repeating-decimal
	   'repeating-decimal-e))
 (simple-settings)

"Extracts the fraction-style setting from a simple-settings.")

(drscheme:language:simple-settings-show-sharing
 (drscheme:language:simple-settings?
  . -> .
  boolean?)
 (simple-settings)

"Extracts the show-sharing setting from a simple-settings.")

(drscheme:language:simple-settings-insert-newlines
 (drscheme:language:simple-settings?
  . -> .
  boolean?)
 (simple-settings)

"Extracts the insert-newline setting from a simple-settings.")

(drscheme:language:simple-settings-annotations
 (drscheme:language:simple-settings?
  . -> .
  (symbols 'none 'debug 'debug/profile))
 (simple-settings)

"Extracts the debugging setting from a simple-settings.")

(drscheme:language:simple-settings?
 (any? . -> . boolean?)
 (val)

"Determines if \\var{val} is a simple-settings.")

(drscheme:language:make-simple-settings
 (boolean?
  (symbols 'constructor 'quasiquote 'write 'current-print)
  (symbols 'mixed-fraction 'mixed-fraction-e 'repeating-decimal 'repeating-decimal-e)
  boolean?
  boolean?
  (symbols 'none 'debug 'debug/profile)
  . -> .
  drscheme:language:simple-settings?)
 (case-sensitive
  printing-style
  fraction-style
  show-sharing
  insert-newlines
  debugging)

"Constructs a simple settings.")

(drscheme:language:simple-settings->vector
 (drscheme:language:simple-settings? . -> . vector?)
 (simple-settings)

"Constructs a vector whose first index is the symbol"
"\\rawscm{'struct:simple-settings}"
"and the other elements are the fields of \\var{simple-settings}.")



                                                               
;;             ;;;                     ;;               ;;     
 ;               ;                      ;                ;     
 ;               ;                      ;                ;     
 ; ;;    ;;;     ;   ; ;;;           ;;;;   ;;;    ;;;   ;  ;; 
 ;;  ;  ;   ;    ;    ;   ;         ;   ;  ;   ;  ;   ;  ; ;   
 ;   ;  ;;;;;    ;    ;   ;  ;;;;;  ;   ;  ;;;;;   ;;;   ;;    
 ;   ;  ;        ;    ;   ;         ;   ;  ;          ;  ; ;   
 ;   ;  ;   ;    ;    ;   ;         ;   ;  ;   ;  ;   ;  ;  ;  
;;; ;;;  ;;;   ;;;;;; ;;;;           ;;; ;  ;;;    ;;;  ;;   ;;
                      ;                                        
                      ;                                        
                     ;;;                                       



(drscheme:help-desk:open-url
 (string? . -> . void?)
 (url)

"Opens \\var{url} in a new help desk window.")

(drscheme:help-desk:help-desk
 (case->
  (-> void?)
  (string? boolean? (symbols 'keyword 'keyword+index 'all) (symbols 'exact 'contains 'regexp)
   . -> .
   void?)
  (string? boolean? (symbols 'keyword 'keyword+index 'all) . -> . void?)
  (string? boolean? . -> . void?))
 (()
  (key lucky? type mode)
  (key lucky? type)
  (key lucky?))

"This function opens a help desk window, or brings an already open help"
"desk window to the front. If an argument is specified, that key is"
"searched for."
""
"If no arguments are supplied, this function"
"opens a help-desk window to the starting page, or just brings a"
"help-desk window to the front (without changing what page it is"
"viewing)."
""
"If any arguments are supplied, this function"
"opens a help-desk window and searches for \\var{key}, according to "
"\\var{lucky?}, \\var{type}, and \\var{mode}."
"If the third and fourth arguments are omitted, "
"they default to \\rawscm{'keyword+index} and \\rawscm{'exact},"
"respectively.")


                                                           
                                                           
                                                           
                         ;                           ;     
 ;                       ;                           ;     
;;;;  ;;;    ;;;    ;;;  ; ;;   ; ;;;    ;;;    ;;;  ;   ; 
 ;   ;   ;  ;   ;  ;     ;;  ;  ;;   ;  ;   ;  ;     ;  ;  
 ;   ;   ;      ;  ;     ;   ;  ;    ;      ;  ;     ; ;   
 ;   ;;;;;   ;;;;  ;     ;   ;  ;    ;   ;;;;  ;     ;;    
 ;   ;      ;   ;  ;     ;   ;  ;    ;  ;   ;  ;     ; ;   
 ;   ;      ;   ;  ;     ;   ;  ;;   ;  ;   ;  ;     ;  ;  
  ;;  ;;;;   ;;;;;  ;;;  ;   ;  ; ;;;    ;;;;;  ;;;  ;   ; 
                                ;                          
                                ;                          
                                                           

(drscheme:teachpack:install-teachpacks
 (drscheme:teachpack:teachpack-cache? . -> . void?)
 (teachpack-cache)
 "Installs the teachpack cache in the current namespace."
 "Passing \\scheme{'drscheme:teachpacks} to"
 "@flink preferences:get"
 "returns the user's currently selected TeachPacks.")

(drscheme:teachpack:teachpack-cache?
 (any? . -> . boolean?)
 (val)
 "Determines if \\var{val} is a teachpack"
 "cache or not.")

(drscheme:teachpack:teachpack-cache-filenames
 (drscheme:teachpack:teachpack-cache? . -> . (listof string?))
 (teachpack-cache)
 "Returns the list of filenames for the teachpacks"
 "in \\var{teachpack-cache}."
 ""
 "See also"
 "@flink drscheme:teachpack:install-teachpacks %"
 ".")
)