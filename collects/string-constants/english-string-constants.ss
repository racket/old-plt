(

 ;;; when translating this constant, substitue name of actual langauge for `English'
 (is-this-your-native-language
  "Is English Your Native Language?")

 ;;; general purpose
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Cancel")
 (untitled "Untitled")
 (warning "Warning")
 (error "Error")
 (close "Close") ;; as in, close an open window
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Collect")
 (read-only "Read only")
 (read/write "Read/Write")
 (auto-extend-selection "Auto-extend Selection")
 (overwrite "Overwrite")
 (running "running")
 (not-running "not running")
 
 ;;; misc
 (welcome-to-something "Welcome to ~a")
 (goto-line "Goto line")
 (goto-line-invalid-number
  "~a is not a valid line number. It must be an integer between 1 and ~a")
 (goto-position "Goto Position")
 (no-full-name-since-not-saved
  "The file does not have a full name because it has not yet been saved.")
 (open-url "Open URL...")

 ;;; save file in particular format prompting.
 (save-as-plain-text "Save this file as plain text?")
 (save-in-drs-format "Save this file in drscheme-specific non-text format?")
 (yes "Yes")
 (no "No")
 
 ;;; preferences
 (preferences "Preferences")
 (preferences-category "Category")
 (saving-preferences "Saving Prefs")
 (error-unmarshalling "Error unmarshalling ~a preference")
 (error-saving-preferences "Error saving preferences: ~a")
 (error-reading-preferences "Error reading preferences")
 (found-bad-pref "found bad pref in file \"~a\"")
 (expected-list-of-length2 "expected a list of length 2")
 (general-prefs-panel-label "General")
 (highlight-parens "Highlight between matching parens")
 (fixup-parens "Correct parens")
 (flash-paren-match "Flash paren match")
 (auto-save-files "Auto-save files")
 (map-delete-to-backspace "Map delete to backspace")
 (verify-exit "Verify exit")
 (ask-before-changing-format "Ask before changing save format")
 (wrap-words-in-editor-buffers "Wrap words in editor buffers")
 (show-status-line "Show status-line")
 (count-from-one "Count line and column numbers from one") 
 (display-line-numbers "Display line numbers in buffer; not character offsets")
 (enable-keybindings-in-menus "Enable keybindings in menus")
 (automatically-to-ps "Automatically print to postscript file")
 (use-mdi "Use MDI Windows") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Use separate dialog for searching")
 (default-fonts "Default Fonts")
 
 ; should have entire alphabet
 (font-example-string "The quick brown fox jumped over the lazy dogs.") 

 (change-font-button-label "Change")
 (fonts "Fonts")

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Please choose a new \"~a\" font")

 (font-size-slider-label "Size")
 (restart-to-see-font-changes "Restart to see font changes")

 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Indenting")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Enter new ~a-like keyword:")
 (x-keyword "~a Keyword")
 (x-like-keywords "~a-like Keywords")

 (expected-a-symbol "expected a symbol, found: ~a")
 (already-used-keyword "\"~a\" is already a specially indented keyword")
 (add-keyword "Add")
 (remove-keyword "Remove")
 
 ;;; find/replace
 (find-and-replace "Find and Replace")
 (find "Find")
 (replace "Replace")
 (use-separate-dialog-for-searching "Use separate dialog for searching")
 (replace&find-again "Replace && Find Again") ;;; need double & to get a single &
 (replace-to-end "Replace to End")
 (forward "Forward")
 (backward "Backward")
 (hide "Hide")
 
 ;;;reverting a file
 (error-reverting "Error Reverting")
 (could-not-read "could not read \"~a\"")
 
 ;;; finder dialog
 (must-specify-a-filename "You must specify a file name")
 (file-does-not-exist "The file \"~a\" does not exist.")
 (ask-because-file-exists "The file \"~a\" already exists. Replace it?")
 (dne-or-cycle "The file \"~a\" contains a nonexistent directory or a cycle.")
 (get-file "Get file")
 (put-file "Put file")
 (full-pathname "Full pathname")
 (show-dot-files "Show files and directories that begin with a dot.")
 (up-directory-button-label "Up directory")
 (add-button-label "Add") ;;; for multi-file selection
 (add-all-button-label "Add all") ;;; for multi-file selection
 (remove-button-label "Remove") ;;; for multi-file selection
 (file-wrong-form "That filename does not have the right form.")
 (select-files "Select files")
 (select-file "Select a file")
 (dir-dne "That directory does not exist.")
 (file-dne "That file does not exist.")
 (empty-filename "The filename must have some letters in it.")
 (that-is-dir-name "That is a directory name.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrScheme's menus will appear
 ;;; in the wrong order.
 (file-menu "File")
 (edit-menu "Edit")
 (help-menu "Help")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label-windows "&File")
 (file-menu-label-other "F&ile")

 (new-info  "Open a new file")
 (new-menu-item-before "&New")
 (new-menu-item-after "")

 (open-info "Open a file from disk")
 (open-menu-item-before "&Open")
 (open-menu-item-after "...")

 (revert-info "Revert this file to the copy on disk")
 (revert-menu-item-before "&Revert")
 (revert-menu-item-after "")

 (save-info "Save this file to disk")
 (save-menu-item-before "&Save")
 (save-menu-item-after "")

 (save-as-info "Prompt for a filename and save this file to disk")
 (save-as-menu-item-before "Save")
 (save-as-menu-item-after " &As ...")

 (print-info "Send this file to a printer")
 (print-menu-item-before "&Print")
 (print-menu-item-after "...")

 (close-info "Close this file")
 (close-menu-item-before "&Close")
 (close-menu-item-after "")

 (quit-info "Close all windows")
 (quit-menu-item-before-windows "E&xit")
 (quit-menu-item-before-others "&Quit")
 (quit-menu-item-after "")
 
 (edit-menu-label "&Edit")
 
 (undo-info "Undo the most recent action")
 (undo-menu-item "&Undo")

 (redo-info "Undo the most recent undo")
 (redo-menu-item "&Redo")

 (cut-info "Move the selected items to the clipboard for later pasting")
 (cut-menu-item "Cu&t")

 (copy-info "Copy the selected items to the clipboard for later pasting")
 (copy-menu-item "&Copy")

 (paste-info "Paste the most recently copied or cut items, in place of the selected items")
 (paste-menu-item "&Paste")

 (clear-info "Erase the selected items without affecting the clipboard or pasting")
 (clear-menu-item-others "Clear")
 (clear-menu-item-windows "&Delete")

 (select-all-info "Select the entire document")
 (select-all-menu-item "Select A&ll")
 
 (find-info "Search for a string")
 (find-menu-item-before "Find")
 (find-menu-item-after "...")

 (find-again-info "Search for the same string as before")
 (find-again-menu-item-before "Find Again")
 (find-again-menu-item-after "")
 
 (replace-and-find-again-info "Replace the current text and search for the same string as before")
 (replace-and-find-again-menu-item-before "Replace && Find Again")
 (replace-and-find-again-menu-item-after "")

 (preferences-info "Configure your preferences")
 (preferences-menu-item-before "")
 (preferences-menu-item-after "Preferences...")

 (keybindings-info "Show the currently active keybindings")
 (keybindings-menu-item "Keybindings")
 (keybindings-frame-title "Keybindings")
 (keybindings-sort-by-name "Sort by Name")
 (keybindings-sort-by-key "Sort by Key")

 (insert-text-box-item "Insert Text Box")
 (insert-pb-box-item "Insert Pasteboard Box")
 (insert-image-item "Insert Image...")
 (wrap-text-item "Wrap Text")

 (windows-menu-label "&Windows")
 (show-menu-label "&Show")

 (help-menu-label "&Help")
 (about-info "Credits and details for this application")
 (about-menu-item-before "About ")
 (about-menu-item-after "...")
 
 ;;; exiting and quitting are you sure dialog
 ;;; (exit is used on windows, quit on macos. go figure)
 (exit-lc "exit")
 (exit-cap "Exit")
 (quit-lc "quit")
 (quit-cap "Quit")
 ;;; in are-you-sure-format, either exit or quit is filled in (from above)
 ;;; based on the platform drscheme is running on.
 (are-you-sure-format "Are you sure you want to ~a?")
 
 ;;; autosaving
 (error-autosaving "Error autosaving \"~a\".")
 (autosaving-turned-off "Autosaving is turned off\nuntil the file is saved.")
 
 ;;; file modified warning
 (file-has-been-modified
  "The file has beeen modified since it was last saved. Overwrite the modifications?")
 (overwrite-file-button-label "Overwrite")
 
 (definitions-modified 
  "The definitions text has been modified in the file-system; please save or revert the definitions text.")
 (drscheme-internal-error "DrScheme Internal Error")
 
 ;;; tools
 (invalid-tool-spec "The tool specification in collection ~a's info.ss file is invalid. Expected either a string or a non-empty list of strings, got: ~e")
 (error-loading-tool-title "DrScheme - Error loading tool ~s; ~s")
 (error-invoking-tool-title "Error invoking tool ~s;~s")
 (tool-tool-names-same-length
  "expected `tool-names' and `tools' to be lists of the same length, in info.ss file for ~s, got ~e and ~e")
 (tool-tool-icons-same-length
  "expected `tool-icons' and `tools' to be lists of the same length, in info.ss file for ~s, got ~e and ~e")
 (error-getting-info-tool
  "error loading info.ss file for ~s")
 
 ;;; define popup menu
 (end-of-buffer-define "<< end of buffer >>")
 (sort-by-name "Sort by name")
 (sort-by-position "Sort by position in file")
 (no-definitions-found "<< no definitions found >>")
 
 ;;; show menu (invariant: show-prefix and hide-prefix are the same length)
 (show-prefix "Show")
 (hide-prefix "Hide")
 
 ;;; file menu
 (definitions "Definitions")
 (save-other "Save Other")
 (save-definitions-as-text "Save Definitions As Text...")
 (save-interactions "Save Interactions")
 (save-interactions-as "Save Interactions As...")
 (save-interactions-as-text "Save Interactions As Text...")
 (print-interactions "Print Interactions...")
 
 ;;; edit-menu
 (split-menu-item-label "&Split")
 (collapse-menu-item-label "C&ollapse")
 
 ;;; language menu
 (language-menu-name "&Language")
 
 ;;; scheme-menu
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
 
 ;;; launcher
 (create-launcher-title "Create Launcher")
 (must-save-before-launcher "You must save your program before creating a launcher.")
 (save-a-launcher "Save a Launcher")
 
 ;;; buttons
 (execute-button-label "Execute") 
 (save-button-label "Save")
 (break-button-label "Break")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Search in Help Desk for \"~a\"")
 (exact-lucky-search-help-desk-for "Exact lucky search in Help Desk for \"~a\"")
 
 ;;; fraction dialog
 (enter-fraction "Enter Fraction")
 (whole-part "Whole Part")
 (numerator "Numerator")
 (denominator "Denominator")
 (invalid-number "Invalid number: must be an exact, real, non-integral number.")
 (insert-fraction-menu-item-label "Insert Fraction...")
 
 ;;; TeachPack messages
 (select-a-teachpack "Select a TeachPack")
 (clear-teachpack "Clear ~a TeachPack")
 (teachpack-error-label "DrScheme - TeachPack error")
 (teachpack-dne/cant-read "The TeachPack file ~a does not exist or is not readable.")
 (teachpack-didnt-load "The TeachPack file ~a did not load properly.")
 (teachpack-error-invoke "The TeachPack file ~a raised an error when invoked.")
 (add-teachpack-menu-item-label "Add Teachpack...")
 (clear-all-teachpacks-menu-item-label "Clear All Teachpacks")
 (teachpack-not-only-one-import "The TeachPack unit/sig in ~a must have exactly one import.")
 
 ;;; Language dialog
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
 
 ;;; repl stuff
 (evaluation-terminated "Evaluation Terminated")
 (evaluation-terminated-explanation
  "The evaluation thread is no longer running, so no evaluation can take place until the next execution.")
 (last-stack-frame "show the last stack frame")
 (more-stack-frames "show the ~a ~a stack frames")
 
 ;;; welcoming message in repl
 (language "Language")
 (custom "custom")
 (teachpack "Teachpack")
 (welcome-to "Welcome to")
 (version "version")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Do you want to kill the evaluation?")
 (just-break "Just Break")
 (kill "Kill")
 (kill? "Kill?")
 )
