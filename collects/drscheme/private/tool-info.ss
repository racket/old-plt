(drscheme:debug:make-debug-error-display-handler
 ((string? (union any? exn?) . -> . void?)
  . -> .
  (string? (union any? exn?) . -> . void?))

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

(drscheme:snip:make-repeating-decimal-snip
 (number? boolean? . -> . (is-a?/c snip%))
 (num show-prefix?)

"Makes a number snip for DrScheme's REPL that is in the"
"decimal view state."
"The boolean indicates if a {\\tt \\#e} prefix appears"
"on the number."
""
"See also"
"@flink drscheme:snip:make-fraction-snip %"
".")

(drscheme:snip:make-fraction-snip
  (number? boolean? . -> . (is-a?/c snip%))
  (num show-prefix-in-decimal-view?)

"Makes a number snip for DrScheme's REPL that is in the"
"fraction view state."
"The boolean indicates if a {\\tt \\#e} prefix appears"
"on the number in the decimal state"
""
"See also"
"@flink drscheme:snip:make-repeating-decimal-snip %"
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
  (string? . -> . ((is-a?/c area-container<%>) . -> . (is-a?/c bitmap%)))
  (string? string? . -> . ((is-a?/c area-container<%>) . -> . (is-a?/c bitmap%))))

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
"find the font for the label")

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

(drscheme:frame:draw-button-label
 ((is-a?/c dc<%>) (union false? string?) (>/c 5) (>/c 5) boolean?
  . -> .
  void?)
 (dc label width height inverted)

"Draws a button label like the one for the \\gui{(define ...)}"
"and filename buttons in the top-left corner of the DrScheme"
"frame. Use this function to draw similar buttons. The basic"
"idea is to create a \\iscmclass{canvas} object whose"
"@link canvas on-paint"
"method is overridden to call this function.  The \\var{dc}"
"should be canvas's \\iscmintf{dc} object, the \\var{label}"
"should be the string to display on the button. "
"The \\var{width} and \\var{height} arguments should be the width and"
"height of the button and \\var{inverted?} should be"
"\\rawscm{\\#t} when the button is being depressed."
""
"See "
"@flink drscheme:frame:calc-button-min-sizes"
"for help calculating the min sizes of the button.")

(drscheme:frame:calc-button-min-sizes
 (->*
  ((is-a?/c dc<%>) string?)
  (number? number?))
 (dc label)

"Calculates the minimum width and height of a button label"
"(when drawn with"
"@flink drscheme:frame:draw-button-label %"
")."
"")

(drscheme:get/extend:extend-interactions-text
 (case->
  ((class? . ->d . (lambda (%) (subclass?/c %))) . -> . void?)
  ((class? . ->d . (lambda (%) (subclass?/c %))) boolean? . -> . void?))
 ((mixin) (mixin before?))

"The unextended class is \\iscmclass{drscheme:rep:text}. This text is used"
"in the bottom window of drscheme frames."
""
"The argument, \\var{before}, controls if the mixin is applied before or"
"after already installed mixins."
"If unsupplied, this is the same as supplying \\rawscm{\\#t}.")

(drscheme:get/extend:get-interactions-text
 (-> (subclass?/c drscheme:rep:text%))
 ()

"Once this function is called, "
"@flink drscheme:get/extend:extend-interactions-text "
"raises an error, disallowing any more extensions.")

(drscheme:get/extend:extend-definitions-text
 (case->
  ((class? . ->d . (lambda (%) (subclass?/c %))) . -> . void?)
  ((class? . ->d . (lambda (%) (subclass?/c %))) boolean? . -> . void?))
 ((mixin) (mixin before?))

"The unextended class is \\iscmclass{text:backup-autosave}. This text"
"is used in the top window of drscheme frames."
""
"The argument, \\var{before}, controls if the mixin is applied before or"
"after already installed mixins."
"If unsupplied, this is the same as supplying \\rawscm{\\#f}.")

(drscheme:get/extend:get-definitions-text
 (-> (subclass?/c text:backup-autosave%))
 ()

"Once this function is called, "
"@flink drscheme:get/extend:extend-definitions-text "
"raises an error, disallowing any more extensions.")

(drscheme:get/extend:extend-interactions-canvas
 (case->
  ((class? . ->d . (lambda (%) (subclass?/c %))) . -> . void?)
  ((class? . ->d . (lambda (%) (subclass?/c %))) boolean? . -> . void?))
 ((mixin) (mixin before?))

"The unextended class is \\iscmclass{canvas:wide-snip}. This canvas is used"
"in the bottom window of drscheme frames."
""
"The argument, \\var{before}, controls if the mixin is applied before or"
"after already installed mixins."
"If unsupplied, this is the same as supplying \\rawscm{\\#f}.")

(drscheme:get/extend:get-interactions-canvas
 (-> (subclass?/c canvas:wide-snip%))
 ()

"Once this function is called, "
"@flink drscheme:get/extend:extend-interactions-canvas"
"raises an error, disallowing any more extensions.")

(drscheme:get/extend:extend-definitions-canvas
 (case->
  ((class? . ->d . (lambda (%) (subclass?/c %))) . -> . void?)
  ((class? . ->d . (lambda (%) (subclass?/c %))) boolean? . -> . void?))
 ((mixin) (mixin before?))

"The unextended class is"
"\\iscmclass{drscheme:unit:definitions-canvas}. This canvas is used in"
"the top window of drscheme frames."

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
  ((class? . ->d . (lambda (%) (subclass?/c %))) . -> . void?)
  ((class? . ->d . (lambda (%) (subclass?/c %))) boolean? . -> . void?))
 ((mixin) (mixin before?))

"The unextended class is \\iscmclass{drscheme:unit:frame}. This is the"
"frame that implements the main drscheme window."
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

(drscheme:language:use-stand-alone-executable?
 ((union false? (is-a?/c frame%) (is-a?/c dialog%))
  . -> .
  boolean?)
 (parent)

"Prompts the user, with an explanatory dialog, asking if they"
"want to create a stand-alone executable or a launcher. See"
"also "
"@flink drscheme:language:create-module-based-stand-alone-executable "
"and"
"@flink drscheme:language:create-module-based-launcher %"
"."
""
"Uses \\var{parent} as the parent to the explanatory dialog.")

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

(drscheme:language:open-input-text
 ((is-a?/c text%) (>/c 0) (>/c 0)
  . -> .
  input-port?)
 (text start end)

"Returns a port that reads from the \\var{text}, starting"
"at position \\var{start} and ending at"
"position \\var{end}. "
""
"Any non-\\iscmclass{string-snip} snips in the text that"
"implement the"
"@ilink drscheme:snip:special"
"interface use the "
"@ilink drscheme:snip:special read-special"
"method to extract a syntax object. Then, that object is"
"returned from the \\rawscm{read-special} function of the"
"result port."
""
"If a non-\\iscmclass{string-snip} snip is encountered that"
"does not implement that interface, it is returned directly"
"from the \\rawscm{read-special} frunction for the resulting"
"port. Thus, it is treated like a constant in the program.")

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
  (symbols 'constructor 'quasiquote 'write))
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

(drscheme:language:simple-settings-debugging
 (drscheme:language:simple-settings?
  . -> .
  boolean?)
 (simple-settings)

"Extracts the debugging setting from a simple-settings.")

(drscheme:language:simple-settings?
 (any? . -> . boolean?)
 (val)

"Determines if \\var{val} is a simple-settings.")

(drscheme:language:make-simple-settings
 (boolean?
  (symbols 'constructor 'quasiquote 'write)
  (symbols 'mixed-fraction 'mixed-fraction-e 'repeating-decimal 'repeating-decimal-e)
  boolean?
  boolean?
  boolean?
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

(drscheme:help-desk:open-users-url
 ((union false? (is-a?/c frame%))
  . -> .
  void?)
 (frame)

"Queries the user for a URL and opens it in a new help desk window. The"
"\\var{frame} argument is used as a parent for the dialog box.")
