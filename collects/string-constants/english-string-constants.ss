(

 ;;; when translating this constant, substitue name of actual langauge for `English'
 (is-this-your-native-language "Is English Your Native Language?")

 (are-you-sure-you-want-to-switch-languages
  "This will change the language of the GUI, which requires you to restart DrScheme. Are you sure?")

 ;; these two should probably be the same in all languages excepet English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "Accept and Quit")
 (accept-and-exit "Accept and Exit")
 
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Cancel")
 (untitled "Untitled")
 (untitled-n "Untitled ~a")
 (warning "Warning")
 (error "Error")
 (close "Close") ;; as in, close an open window
 (stop "Stop")   
 (&stop "&Stop") ;; for use in button and menu item labels, with short cut.

 ;;; important urls
 (web-materials "Related Web Sites") ;; menu item title
 (drscheme-homepage "DrScheme")
 (plt-homepage "PLT")
 (how-to-use-scheme "How to Use Scheme") ;; title of a book.
 (teachscheme!-homepage "TeachScheme!") ;; probably this should be a `word' in all languages

 ;;; bug report form
 (cancel-bug-report? "Cancel Bug Report?")
 (are-you-sure-cancel-bug-report?
  "Are you sure that you want to cancel sending this bug report?")
 (bug-report-form "Bug Report Form")
 (bug-report-field-name "Name")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Summary")
 (bug-report-field-severity "Severity")
 (bug-report-field-class "Class")
 (bug-report-field-priority "Priority")
 (bug-report-field-description "Description")
 (bug-report-field-reproduce1 "Steps to")
 (bug-report-field-reproduce2 "Reproduce")
 (bug-report-field-environment "Environment")
 (bug-report-field-tools "Tools")
 (bug-report-field-docs-installed "Docs Installed")
 (bug-report-field-language "Language")
 (bug-report-field-teachpacks "Teachpacks")
 (bug-report-field-collections "Collections")
 (bug-report-field-human-language "Human Language")
 (bug-report-field-version "Version")
 (bug-report-synthesized-information "Synthesized Information")  ;; dialog title
 (bug-report-show-synthesized-info "Show Synthesized Info")
 (bug-report-submit "Submit")
 (sending-bug-report "Sending Bug Report")
 (error-sending-bug-report "Error Sending Bug Report")
 (error-sending-bug-report-expln "An error occurred when sending this bug report. If your internet connection is otherwise working fine, please visit:\n\n    http://bugs.plt-scheme.org/\n\nand submit the bug via our online web-form. Sorry for the difficulties.\n\nThe error message is:\n~a")
 (bug-report-sent "Bug Report Sent")
 (bug-report-sent-detail "Thanks for the report. You should receive a confirmation email in the next 30 minutes. If you do not, send email to scheme@plt-scheme.org.")
 (illegal-bug-report "Illegal Bug Report")
 (pls-fill-in-field "Please fill in the \"~a\" field")
 (malformed-email-address "Malformed email address")
 (pls-fill-in-either-description-or-reproduce "Please fill in either the Description field or the Steps to Reproduce field.")

 ;;; check syntax
 (check-syntax "Check Syntax")
 (cs-italic "Italic")
 (cs-bold "Bold")
 (cs-underline "Underline")
 (cs-change-color "Change Color")
 (cs-tack/untack-arrow "Tack/Untack Arrow")
 (cs-jump "Jump")
 (cs-error-message "Error Message")
 (cs-open-file "Open ~a")
 (cs-rename-var "Rename ~a")
 (cs-rename-id "Rename Identifier")
 (cs-rename-var-to "Rename ~a to:")
 (cs-name-duplication-error "The new name you have chosen, ~s, conflicts with an already established name in this scope.")
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
 (read-only "Read only")
 (read/write "Read/Write")
 (auto-extend-selection "Auto-extend")
 (overwrite "Overwrite")
 (running "running")
 (not-running "not running")
 
 ;;; misc
 (welcome-to-something "Welcome to ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Welcome to DrScheme, version ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Welcome to DrScheme")
 (version/language "version ~a, ~a")

 (goto-line "Goto line")
 (goto-line-invalid-number
  "~a is not a valid line number. It must be an integer between 1 and ~a")
 (goto-position "Goto Position")
 (no-full-name-since-not-saved
  "The file does not have a full name because it has not yet been saved.")
 (cannot-open-because-dne "Cannot open ~a because it does not exist")
 (interactions-out-of-sync
  "WARNING: Interactions window is out of sync with the definitions window. Click Execute.")
 (file-is-not-saved "The file \"~a\" is not saved.")
 (save "Save")
 (please-choose-either "Please choose either \"~a\" or \"~a\"")
 (close-anyway "Close Anyway")

 (url "URL")
 (url: "URL:")
 (open-url... "Open URL...")
 (open-url "Open URL")
 (browse... "Browse...")
 (bad-url "Bad URL")
 (bad-url:this "Bad URL: ~a")
 
 ;; Help Desk
 (search-results "Search Results")
 (help-desk "Help Desk")
 (help-desk-n "Help Desk ~a")
 (about-help-desk "About Help Desk")
 (help-desk-about-string
  "Help Desk is a complete source of information about PLT software, including DrScheme, MzScheme, and MrEd.\n\nVersion ~a\nCopyright (c) 1995-2001 PLT")
 (help-on-help "Help on Help")
 (help-on-help-details "For help on using Help Desk, follow the `How to use Help Desk' link on Help Desk's home page. (To get to the home page if you're not already there, click the `Home' button at the top of the Help Desk window.)")
 (find-docs-for "Find docs for:")
 (search "Search")
 ; next 3 are popup menu choices at bottom of help desk window
 (search-for-keyword "for Keyword")
 (search-for-keyword-or-index "for Keyword or Index Entry")
 (search-for-keyword-or-index-or-text "for Keyword, Index Entry, or Text")
 (exact-match "exact match")
 (containing-match "containing match")
 (regexp-match "regexp match")
 (feeling-lucky "Feeling Lucky")
 (nothing-found-for-search-key "Nothing found for \"~a\".")
 (searching "Searching...")
 (search-stopped "(Search stopped.)")
 (search-stopped-too-many-matches "(Search stopped - found too many matches.)")
 (reload "Reload")
 (help "Help")
 (searching... "Searching...")
 (nothing-found-for-empty-search "Nothing found for the empty search")
 (nothing-found-for "Nothing found for ~a")
 (and "and")
 (error-finding-docs
  "Could not find documentation.\n\n~a")
 ; help desk htty proxy
 (http-proxy "HTTP Proxy")
 (proxy-direct-connection "Direct connection")
 (proxy-use-proxy "Use proxy:")
 (proxy-host "Host")
 (proxy-port "Port")
 (proxy-bad-host "Bad Proxy Host")

 ;; browser
 (rewind-in-browser-history "Rewind")
 (forward-in-browser-history "Forward")
 (home "Home")
 (browser "Browser")
 (cannot-display-url "Cannot display URL ~s: ~a")
 (install? "Install?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "You have selected an installable package.")
 (do-you-want-to-install-it? "Do you want to install it?")
 (paren-file-size "(The file is ~a bytes)")
 (download-and-install "Download && Install") ;; button label
 (download "Download") ;; button label
 (save-downloaded-file/size "Save downloaded file (~a bytes) as") ;; label for get-file dialog
 (save-downloaded-file "Save downloaded file as")  ;; label for get-file dialog
 (downloading "Downloading") ;; dialog title
 (downloading-file... "Downloading file...")
 (package-was-installed "The package was installed.")
 (download-was-saved "The downloaded file was saved.")
 (getting-page "Getting Page") ;; dialog title
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "Install ~a or open for editing?")
 (install-plt-file/yes "Install")
 (install-plt-file/no "Edit")
 
 ;;; about box
 (about-drscheme-frame-title "About DrScheme")
 (take-a-tour "Take a Tour!")
 (release-notes "Release Notes")
 (parenthetical-last-version "(previous version ~a)")
 (parenthetical-last-language "(previous language ~a)")
 (parenthetical-last-version/language "(previous version ~a, language ~a)")
 
 
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

 (font-prefs-panel-title "Font")
 (font-name "Font Name")
 (font-size "Font Size")
 (set-font "Set Font...")
 (select-font-name "Select Font Name")
 (example-text "Example Text:")
 (general-ii "General II")
 (only-warn-once "Only warn once when executions and interactions are not synchronized")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Waiting for the preferences lockfile...")
 (pref-lock-not-gone
  "The preferences lockfile:\n\n   ~a\n\nprevents the preferences from being saved. Ensure that no PLT software is running and delete this file.")
 (still-locked-exit-anyway? "The preferences were not saved sucessfully. Exit anyway?")
 
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
 (dock "Dock")
 (undock "Undock")
 (use-separate-dialog-for-searching "Use separate dialog for searching")
 (replace&find-again "Replace && Find Again") ;;; need double & to get a single &
 (replace-to-end "Replace to End")
 (forward "Forward")
 (backward "Backward")
 (hide "Hide")
 
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "Search in files...")
 (mfs-string-match/graphics "String match (handles files with graphics)")
 (mfs-regexp-match/no-graphics "Regular Expression (only raw text files)")
 (mfs-searching... "Searching...")
 (mfs-configure-search "Configure Search") ;; dialog title
 (mfs-files-section "Files")   ;; section in config dialog
 (mfs-search-section "Search") ;; section in config dialog
 (mfs-dir "Dir")
 (mfs-recur-over-subdirectories "Recur over subdirectories")
 (mfs-regexp-filename-filter "Regexp filename filter")
 (mfs-search-string "Search string")
 (mfs-drscheme-multi-file-search "DrScheme - Multi File Search") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" is not a directory")
 (mfs-open-file "Open File")
 (mfs-stop-search "Stop Search")
 (mfs-case-sensitive-label "Case sensitive")
 (mfs-no-matches-found "No matches found.")
 (mfs-search-interrupted "Search aborted.")
 
 ;;; reverting a file
 (error-reverting "DrScheme - Error Reverting")
 (could-not-read "could not read \"~a\"")
 (are-you-sure-revert
  "Are you sure that you want to revert this file? This change cannot be undone.")
 (are-you-sure-revert-title
  "Revert?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Error Saving") ;; title of error message dialog
 (error-saving-file/name "There was an error saving ~a.")

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
 (windows-menu "Windows")
 
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
 (new-menu-item "&New")

 (open-info "Open a file from disk")
 (open-menu-item "&Open...")

 (open-recent-info "A list of the recently opened files")
 (open-recent-menu-item "Open Recent")
 
 (revert-info "Revert this file to the copy on disk")
 (revert-menu-item "&Revert")

 (save-info "Save this file to disk")
 (save-menu-item "&Save")

 (save-as-info "Prompt for a filename and save this file to disk")
 (save-as-menu-item "Save &As...")

 (print-info "Send this file to a printer")
 (print-menu-item "&Print...")

 (close-info "Close this file")
 (close-menu-item "&Close")

 (quit-info "Close all windows")
 (quit-menu-item-windows "E&xit")
 (quit-menu-item-others "&Quit")
 
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
 (find-menu-item "Find...")

 (find-again-info "Search for the same string as before")
 (find-again-menu-item "Find Again")
 
 (replace-and-find-again-info "Replace the current text and search for the same string as before")
 (replace-and-find-again-menu-item "Replace && Find Again")

 (preferences-info "Configure your preferences")
 (preferences-menu-item "Preferences...")

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
 (bring-frame-to-front "Bring Frame to Front")       ;;; title of dialog
 (bring-frame-to-front... "Bring Frame to Front...") ;;; corresponding title of menu item

 (show-menu-label "&Show")
 (show-overview "Show Contour") 
 (hide-overview "Hide Contour")

 (help-menu-label "&Help")
 (about-info "Credits and details for this application")
 (about-menu-item "About...")
 (help-menu-check-for-updates "Check for Updates...")
 
 ;;; help-desk-specific menus
 (new-help-desk "New Help Desk")

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
 
 ;;; show menu
 (hide-definitions-menu-item-label "Hide &Definitions")
 (show-definitions-menu-item-label "Show &Definitions")
 (definitions-menu-item-help-string "Show/Hide the definitions window")
 (show-interactions-menu-item-label "Show &Interactions")
 (hide-interactions-menu-item-label "Hide &Interactions")
 (interactions-menu-item-help-string "Show/Hide the interactions window")
 
 ;;; file menu
 (save-definitions-as "Save Definitions As...")
 (save-definitions "Save Definitions")
 (print-definitions "Print Definitions...")
 (about-drscheme "About DrScheme")
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
 
 ;;; executables
 (create-executable-menu-item-label "Create Executable...")
 (create-executable-title "Create Executable")
 (must-save-before-executable "You must save your program before creating an executable.")
 (save-an-executable "Save an Executable")
 (definitions-not-saved "The definitions window has not been saved. The executable will use the latest saved version of the definitions window. Continue?")
 (inline-saved-program-in-executable?
  "Inline the saved program in the executable? If yes, you can copy the executable to another ~a computer but the executable will be quite large. If not, you cannot copy the executable to another computer, but it will be much smaller. Additionally, if not, the executable will load the latest version of the program.")
 (use-mred-binary?
  "Use the mred binary for this executable?\n\nIf yes, your program can use the (lib \"mred.ss\" \"mred\") library. If no, DrScheme will use mzscheme as the binary for this executable and you cannot use that library.\n\nIf unsure, choose yes.")
 
 ;;; buttons
 (execute-button-label "Execute") 
 (save-button-label "Save")
 (break-button-label "Break")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Search in Help Desk for \"~a\"")
 (exact-lucky-search-help-desk-for "Exact lucky search in Help Desk for \"~a\"")

 ;; collapse and expand popup menu items
 (collapse-sexp "Collapse sexpression")
 (expand-sexp "Expand sexpression")
 
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
 (drscheme-teachpack-message-title "DrScheme Teachpack")
 (already-added-teachpack "Already added ~a Teachpack")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Please select a language. Students in most introductory courses should use the default language.")
 (language-dialog-title "Configure Language")
 (case-sensitive-label "Case sensitive")
 (output-style-label "Output Style")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (sharing-printing-label "Show sharing in values")
 (use-pretty-printer-label "Insert newlines in printed values")
 (input-syntax "Input Syntax")
 (dynamic-properties "Dynamic Properties")
 (output-syntax "Output Syntax")
 (debugging "Debugging")
 (whole/fractional-exact-numbers-label "Print numbers as fractions")
 (booleans-as-true/false-label "Print booleans using true and false")
 (show-details-button-label "Show Details")
 (hide-details-button-label "Hide Details")
 (choose-language-menu-item-label "Choose Language...")
 (revert-to-language-defaults "Revert to Language Defaults")
 (language-docs-button-label "Language Docs")
 
 ;;; languages
 (beginning-student "Beginning Student")
 (beginning-one-line-summary "define, cond, structs, constants, and primitives")
 (beginning-student/abbrev "Beginning Student with List Abbreviations")
 (beginning/abbrev-one-line-summary "Beginner, with list style printing in the REPL")
 (intermediate-student "Intermediate Student")
 (intermediate-one-line-summary "Beginner plus lexical scope")
 (intermediate-student/lambda "Intermediate Student with lambda")
 (intermediate/lambda-one-line-summary "Intermediate plus higher-order functions")
 (advanced-student "Advanced Student")
 (advanced-one-line-summary "Intermediate plus lambda and mutation")
 (full-language "Full") ;; also in the HtDP languages section
 (htdp-full-one-line-summary "Advanced, plus PLT extensions and GUI library")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "R5RS-like")
 (mred-lang-name "Graphical without debugging (MrEd)")
 (mzscheme-lang-name "Textual without debugging (MzScheme)")
 (r5rs-lang-name "Standard (R5RS)")
 (r5rs-one-line-summary "R5RS, with no extra frills")
 (unknown-debug-frame "[unknown]")
 
 (module-language-one-line-summary "Language with module as the only construct")
 (bad-module-language-specs
  "The drscheme-language-position and drscheme-language-modules specifications aren't correct. Expected (listof (cons string (listof string))) and (listof (listof string)) respectively, where the lengths drscheme-language-position and drscheme-language-module lists are the same. Got ~e and ~e")
  
 ;;; debug language
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "~a's interactions") ;; filled with a filename
 (stack-frame-in-current-interactions "interactions")
 (stack-frame-in-current-definitions "definitions")
 (mzscheme-w/debug "Textual (MzScheme)")
 (mzscheme-one-line-summary "PLT Scheme without the GUI library")
 (mred-w/debug "Graphical (MrEd)")
 (mred-one-line-summary "PLT Scheme plus the GUI library")
 
 ;;; repl stuff
 (evaluation-terminated "Evaluation Terminated")
 (evaluation-terminated-explanation
  "The evaluation thread is no longer running, so no evaluation can take place until the next execution.")
 (last-stack-frame "show the last stack frame")
 (last-stack-frames "show the last ~a stack frames")
 (next-stack-frames "show the next ~a stack frames")
 
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

 ;;; version checker
 (vc-update-check "Update check")
 (vc-check-prompt "Check for PLT software updates over the Internet?")
 (vc-please-wait "Please wait")
 (vc-connecting-version-server "Connecting to PLT version server")
 (vc-network-timeout "Network timeout") 
 (vc-cannot-connect  "Can't connect to PLT version server")
 (vc-network-failure "Network failure")
 (vc-old-binaries "Installed binaries for DrScheme (or MzScheme) are not up-to-date")
 (vc-binary-information-format "Installed binary version: ~a (iteration ~a)")
 (vc-update-format "~a v.~a (iteration ~a) needs updating to v.~a (iteration ~a)")
 (vc-binary-name "Binary")
 (vc-updates-available "Updates are available at")
 (vc-latest-binary-information-format "Latest released version: ~a (iteration ~a)")
 (vc-update-dialog-title "PLT update status")
 (vc-need-update-string "One or more installed PLT software packages needs updating")
 (vc-no-update-string "All installed PLT software packages are up-to-date")
 
 ;; large semi colon letters
 (insert-large-letters... "Insert Large Letters...")
 (large-semicolon-letters "Large Semicolon Letters")
 (text-to-insert "Text to insert")
 )
