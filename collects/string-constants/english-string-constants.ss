(
 
 ;; general purpose
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Cancel")
 (untitled "Untitled")

 ;; misc
 (definitions-modified 
  "The definitions text has been modified in the file-system; please save or revert the definitions text.")
 (drscheme-internal-error "DrScheme Internal Error")

 ;; tools
 (invalid-tool-spec "The tool specification in collection ~a's info.ss file is invalid. Expected either a string or a non-empty list of strings, got: ~e")
 (error-loading-tool-title "DrScheme - Error loading tool ~s; ~s")
 (error-invoking-tool-title "Error invoking tool ~s;~s")
 (tool-tool-names-same-length
  "expected `tool-names' and `tools' to be lists of the same length, in info.ss file for ~s, got ~e and ~e")
 (tool-tool-icons-same-length
  "expected `tool-icons' and `tools' to be lists of the same length, in info.ss file for ~s, got ~e and ~e")
 (error-getting-info-tool
  "error loading info.ss file for ~s")
 
 ;; define popup menu
 (end-of-buffer-define "<< end of buffer >>")
 (sort-by-name "Sort by name")
 (sort-by-position "Sort by position in file")
 (no-definitions-found "<< no definitions found >>")
 
 ;; show menu (invariant: show-prefix and hide-prefix are the same length)
 (show-prefix "Show")
 (hide-prefix "Hide")

 ;; file menu
 (definitions "Definitions")
 (save-other "Save Other")
 (save-definitions-as-text "Save Definitions As Text...")
 (save-interactions "Save Interactions")
 (save-interactions-as "Save Interactions As...")
 (save-interactions-as-text "Save Interactions As Text...")
 (print-interactions "Print Interactions...")
 
 ;; edit-menu
 (split-menu-item-label "&Split")
 (collapse-menu-item-label "C&ollapse")

 ;; language menu
 (language-menu-name "&Language")

 ;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Execute")
 (execute-menu-item-help-string "Restart the program in the definitions window")
 (break-menu-item-label "Break")
 (break-menu-item-help-string "Break the current evaluation")
 (kill-menu-item-label "Kill")
 (kill-menu-item-help-string "Kill the current evaluation")
 (reindent-menu-item-label "&Reindent")
 (reindent-all-menu-item-label "Reindent &All")
 (comment-out-menu-item-label "&Comment Out")
 (uncomment-menu-item-label "&Uncomment")
 (hide-definitions-menu-item-label "Hide &Definitions")
 (hide-definitions-menu-item-help-string "Show/Hide the definitions window")
 (interactions-menu-item-label "Show &Interactions")
 (interactions-menu-item-help-string "Show/Hide the interactions window")

 ;; launcher
 (create-launcher-title "Create Launcher")
 (must-save-before-launcher "You must save your program before creating a launcher.")
 (save-a-launcher "Save a Launcher")

 ;; buttons
 (execute-button-label "Execute") 
 (save-button-label "Save")
 (break-button-label "Break")
 
 ;; search help desk popup menu
 (search-help-desk-for "Search in Help Desk for \"~a\"")
 (exact-lucky-search-help-desk-for "Exact lucky search in Help Desk for \"~a\"")
 
 ;; fraction dialog
 (enter-fraction "Enter Fraction")
 (whole-part "Whole Part")
 (numerator "Numerator")
 (denominator "Denominator")
 (invalid-number "Invalid number: must be an exact, real, non-integral number.")
 (insert-fraction-menu-item-label "Insert Fraction...")
 
 ;; TeachPack messages
 (select-a-teachpack "Select a TeachPack")
 (clear-teachpack "Clear ~a TeachPack")
 (teachpack-error-label "DrScheme - TeachPack error")
 (teachpack-dne/cant-read "The TeachPack file ~a does not exist or is not readable.")
 (teachpack-didnt-load "The TeachPack file ~a did not load properly.")
 (teachpack-error-invoke "The TeachPack file ~a raised an error when invoked.")
 (add-teachpack-menu-item-label "Add Teachpack...")
 (clear-all-teachpacks-menu-item-label "Clear All Teachpacks")
 (teachpack-not-only-one-import "The TeachPack unit/sig in ~a must have exactly one import.")
 
 ;; Language dialog
 (language-dialog-title "Configure Language")
 (case-sensitive-label "Case sensitive")
 (output-style-label "Output Style")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (sharing-printing-label "Show sharing in values")
 (use-pretty-printer-label "Insert newlines in printed values")
 (input-syntax "Input Syntax")
 (output-syntax "Output Syntax")
 (whole/fractional-exact-numbers-label "Print numbers as fractions")
 (booleans-as-true/false-label "Print booleans using true and false")
 (show-details-button-label "Show Details")
 (hide-details-button-label "Hide Details")
 (choose-language-menu-item-label "Choose Language...")
 (revert-to-language-defaults "Revert to Language Defaults")
 
 ;; repl stuff
 (evaluation-terminated "Evaluation Terminated")
 (evaluation-terminated-explanation
  "The evaluation thread is no longer running, so no evaluation can take place until the next execution.")
 (last-stack-frame "show the last stack frame")
 (more-stack-frames "show the ~a ~a stack frames")
 
 ;; welcoming message in repl
 (language "Language")
 (custom "custom")
 (teachpack "Teachpack")
 (welcome-to "Welcome to")
 (version "version")
 
 ;; kill evaluation dialog
 (kill-evaluation? "Do you want to kill the evaluation?")
 (just-break "Just Break")
 (kill "Kill")
 (kill? "Kill?")
 )
