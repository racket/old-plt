(module dutch-string-constants "string-constant-lang.ss"

 ;;; when translating this constant, substitute name of actual langauge for `English'
 (is-this-your-native-language "Is uw moedertaal Nederlands?")

 (are-you-sure-you-want-to-switch-languages
  "Wisselen van taal vergt een herstart van DrScheme.  Weet u zeker dat u dit wilt?")

 ;; these two should probably be the same in all languages except English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "OK, sluit DrScheme maar af")
 (accept-and-exit "OK, sluit DrScheme maar af")
 
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Annuleren")
 (untitled "Naamloos")
 (untitled-n "Naamloos ~a")
 (warning "Waarschuwing")
 (error "Fout")
 (close "Sluiten") ;; as in, close an open window
 (stop "Stop")   
 (&stop "&Stop") ;; for use in button and menu item labels, with short cut.

 ;;; important urls
 (web-materials "Verwante Web Sites") ;; menu item title
 (drscheme-homepage "DrScheme")
 (plt-homepage "PLT")
 (how-to-use-scheme "How to Use Scheme") ;; title of a book.
 (teachscheme!-homepage "TeachScheme!") ;; probably this should be a `word' in all languages

 ;;; bug report form
 (cancel-bug-report? "Melden defect afbreken?")
 (are-you-sure-cancel-bug-report?
  "Weet u zeker dat u deze defectmelding NIET wilt sturen?")
 (bug-report-form "Defectmeldingsformulier")
 (bug-report-field-name "Naam")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Samenvatting")
 (bug-report-field-severity "Ernst")
 (bug-report-field-class "Aard")
 (bug-report-field-priority "Belang")
 (bug-report-field-description "Omschrijving")
 (bug-report-field-reproduce1 "Hoe te")
 (bug-report-field-reproduce2 "veroorzaken")
 (bug-report-field-environment "Omgeving")
 (bug-report-field-tools "Tools") ; <**> -- Where is this used?
 (bug-report-field-docs-installed "Geïnstalleerde documentatie") ; if allowed, add \n
 (bug-report-field-language "Programmeertaal")
 (bug-report-field-teachpacks "Teachpacks")
 (bug-report-field-collections "Collecties")
 (bug-report-field-human-language "Spreektaal")
 (bug-report-field-version "Versie")
 (bug-report-synthesized-information "Systeeminformatie")  ;; dialog title
 (bug-report-show-synthesized-info "Toon systeeminformatie")
 (bug-report-submit "Stuur")
 (sending-bug-report "Bezig de defectmelding te versturen")
 (error-sending-bug-report "Fout bij het versturen van de defectmelding")
 (error-sending-bug-report-expln "Het versturen van de defectmelding mislukte.  Mocht uw internetverbinding verder goed werken, ga dan naar:\n\n    http://bugs.plt-scheme.org/\n\nen verstuur de melding via het webformulier.  Sorry voor het ongemak.\n\nDe foutmelding is:\n~a")
 (bug-report-sent "defectmelding verzonden")
 (bug-report-sent-detail "Dank u voor de melding.  Als u niet binnen 30 minuten een bevestigingsmail ontvangt, mail dan naar scheme@plt-scheme.org.")
 (illegal-bug-report "Ongeldige defectmelding")
 (pls-fill-in-field "Gelieve het \"~a\"-veld in te vullen")
 (malformed-email-address "Onmogelijk emailadres")
 (pls-fill-in-either-description-or-reproduce 
  "Gelieve hetzij het omschrijvings-, hetzij het \"hoe te veroorzaken\"-veld in te vullen.")

 ;;; check syntax
 (check-syntax "Controleer Syntaxis")
 (cs-italic "Cursief")
 (cs-bold "Vet")
 (cs-underline "Onderstreept")
 (cs-change-color "Andere kleur")
 (cs-tack/untack-arrow "Pijlen vast/los")
 (cs-jump "Jump") ; <**>
 (cs-error-message "Foutmelding")
 (cs-open-file "Open ~a")
 (cs-rename-var "Hernoem ~a")
 (cs-rename-id "Hernoem Identifier") ;<**>
 (cs-rename-var-to "Hernoem ~a tot:")
 (cs-name-duplication-error "De nieuwgekozen naam ~s komt al voor in dit bereik")
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
 (read-only "Alleen lezen")
 (read/write "Lezen/Schrijven")
 (auto-extend-selection "Auto-extend") ; <**>
 (overwrite "Vervang")
 (running "Bezig")
 (not-running "Klaar")
 
 ;;; misc
 (welcome-to-something "Welkom bij ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Welkom bij DrScheme, versie ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Welkom bij DrScheme")
 (version/language "versie ~a, ~a")

 (goto-line "Ga naar regel")
 (goto-line-invalid-number
  "~a is geen geldig regelnummer. It must be an integer between 1 and ~a") ; <**>
 (goto-position "Goto Position") ; <**>
 (no-full-name-since-not-saved "Dit bestand is nog nooit opgeslagen, en heeft dus nog geen naam.")
 (cannot-open-because-dne "~a bestaat niet, en kan dus niet geopend worden.")
 (interactions-out-of-sync
  "WAARSCHUWING: Interactie- en definitievenster komen niet overeen.  Druk op Doen!.")
 (file-is-not-saved "Het bestand \"~a\" is niet opgeslagen.")
 (save "Opslaan")
 (please-choose-either "Gelieve te kiezen tussen \"~a\" en \"~a\"")
 (close-anyway "Toch sluiten")
 (clear-anyway "Toch wissen") ; <**>

 (url "URL")
 (url: "URL:")
 (open-url... "Open URL...")
 (open-url "Open URL")
 (browse... "Surf...")
 (bad-url "Bad URL") ; <**>
 (bad-url:this "Bad URL: ~a") ;<**>
 
 ;; Help Desk
 (search-results "Zoekresultaten")
 (help-desk "Hulpbron")
 (help-desk-n "Hulpbron ~a")
 (about-help-desk "Omtrent de hulpbron")
 (help-desk-about-string
  "De Hulpbron bevat complete informatie omtrent PLT programmatuur, waaronder DrScheme, MzScheme, en MrEd.\n\nVersie ~a\nAuteursrecht (c) 1995-2001 PLT")
 (help-on-help "Hulp voor hulp")
 (help-on-help-details "Voor hulp bij het gebruik van de Hulpbron, klik de link `Help Desk' de startpagina van de Hulpbron. (To get to the home page if you're not already there, click the `Home' button at the top of the Help Desk window.)")
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
 (manual-installed-date "(installed ~a)")

 ;; refreshing manuals
 (refreshing-manuals "Re-downloading Manuals")
 (refresh-downloading... "Downloading ~a...")
 (refresh-deleting... "Deleting old version of ~a...")
 (refresh-installing... "Installing new version of ~a...")

 ;; help desk htty proxy
 (http-proxy "HTTP Proxy")
 (proxy-direct-connection "Direct connection")
 (proxy-use-proxy "Use proxy:")
 (proxy-host "Host")
 (proxy-port "Port")
 (proxy-bad-host "Bad Proxy Host")

 ;; browser
 (rewind-in-browser-history "Terug")
 (forward-in-browser-history "Vooruit")
 (home "Start")
 (browser "Browser")
 (choose-browser "Choose a Browser")
 (no-browser "None")
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

 (install-plt-file-menu-item... "Install .plt File...")
 (install-plt-file-dialog-title "Install .plt File")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "File")
 (install-plt-filename "Filename:")
 (install-plt-url "URL:")
 
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
 (saving-preferences "Saving Prefs")
 (error-unmarshalling "Error unmarshalling ~a preference")
 (error-saving-preferences "Error saving preferences: ~a")
 (error-reading-preferences "Error reading preferences")
 (expected-list-of-length2 "expected a list of length 2")
 (scheme-prefs-panel-label "Scheme")
 (warnings-prefs-panel-label "Warnings")
 (editor-prefs-panel-label "Editing")
 (highlight-parens "Highlight between matching parens")
 (fixup-parens "Correct parens")
 (flash-paren-match "Flash paren match")
 (auto-save-files "Auto-save files")
 (backup-files "Backup files")
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
 (reuse-existing-frames "Reuse existing frames when opening new files")
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
 (hide "Sluiten")
 
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "Search in Files...")
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
 (file-menu "Bestand")
 (edit-menu "Bewerken")
 (help-menu "Hulp")
 (windows-menu "Vensters")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label-windows "&Bestand")
 (file-menu-label-other "B&estand")

 (new-info  "Open nieuw bestand")
 (new-menu-item "&Nieuw")
 (new-...-menu-item "&Nieuw...")

 (open-info "Open bestand van schijf")
 (open-menu-item "&Open...")
 (open-here-menu-item "&Open Hier...")

 (open-recent-info "Onlangs geopende bestanden")
 (open-recent-menu-item "Open opnieuw")
 
 (revert-info "Negeer de gemaakte wijzigingen, en herlaad het bestand")
 (revert-menu-item "&Herlaad")

 (save-info "Save this file to disk")
 (save-menu-item "O&pslaan")

 (save-as-info "Prompt for a filename and save this file to disk")
 (save-as-menu-item "Ops&laan als...")

 (print-info "Stuur dit bestand naar een printer")
 (print-menu-item "Af&drukken...")

 (close-info "Close this file")
 (close-menu-item "&Sluiten")

 (quit-info "Close all windows")
 (quit-menu-item-windows "&Afsluiten")
 (quit-menu-item-others "&Afsluiten")
 
 (edit-menu-label "Be&werken")
 
 (undo-info "Maak de laatste actie ongedaan")
 (undo-menu-item "&Ongedaan maken")

 (redo-info "Voer de als laatste ongedaangemaakte actie alsnog uit")
 (redo-menu-item "Opnieuw &uitvoeren") ;<**> - does the & have the correct position?

 (cut-info "Verplaats de selectie naar het klembord")
 (cut-menu-item "K&nippen")

 (copy-info "Kopieer de selectie naar het klembord")
 (copy-menu-item "&Kopiëren")

 (paste-info "Kopieer de inhoud van het klembord naar de plaats van de cursor")
 (paste-menu-item "&Plakken")

 (clear-info "Gooi de selectie weg")
 (clear-menu-item-others "Wis")
 (clear-menu-item-windows "&Wis")

 (select-all-info "Selecteer het gehele document")
 (select-all-menu-item "Selecteer a&lles")
 
 (find-info "Zoek tekst in het document")
 (find-menu-item "Zoek...")

 (find-again-info "Zoek verder naar dezelfde tekst")
 (find-again-menu-item "Zoek nogmaals")
 
 (replace-and-find-again-info "Vervang de huidige selectie, en zoek verder")
 (replace-and-find-again-menu-item "Vervang && Zoek nogmaals")

 (preferences-info "Stel uw voorkeuren in")
 (preferences-menu-item "Voorkeuren...")

 (keybindings-info "Toon de huidige toetsbetekenissen")
 (keybindings-menu-item "Toetsbetekenissen")
 (keybindings-frame-title "Toetsbetekenissen")
 (keybindings-sort-by-name "Op betekenis")
 (keybindings-sort-by-key "Op toets")

 (insert-text-box-item "Tekst")
 (insert-pb-box-item "Pasteboard Box") ;<**>
 (insert-image-item "Plaatje...")
 (wrap-text-item "Wrap Text")

 (windows-menu-label "&Vensters")
 (bring-frame-to-front "Kies venster")       ;;; title of dialog
 (bring-frame-to-front... "Kies venster...") ;;; corresponding title of menu item
 (next-window "Volgend venster")
 (previous-window "Vorig venster")

 (show-menu-label "&Toon")
 (show-overview "Toon overzicht") 
 (hide-overview "Toon overzicht niet")

 (help-menu-label "&Hulp")
 (about-info "Credits and details for this application")
 (about-menu-item "Info...")
 (help-menu-check-for-updates "Recentere versies...")
 
 ;;; help-desk-specific menus
 (new-help-desk "Nieuwe Hulpbron")

 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Wilt u een nieuw venster openen, of het huidige wissen?")
 (clear-current "Wis huidig")
 (new-window "Open nieuw")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; (exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Afsluiten")
 (quit "Afsluiten")
 (are-you-sure-exit "Weet u zeker dat u wilt afsluiten?")
 (are-you-sure-quit "Weet u zeker dat u wilt afsluiten?")
 
 ;;; autosaving
 (error-autosaving "Error autosaving \"~a\".")
 (autosaving-turned-off "Autosaving is turned off\nuntil the file is saved.")
 
 ;;; file modified warning
 (file-has-been-modified
  "Sinds de laatste opslag is dit bestand gewijzigd.  Veranderingen overschrijven?")
 (overwrite-file-button-label "Overschrijven")
 
 (definitions-modified
  "Het bestand met de definities is gewijzigd.  Gelieve op te slaan of te herladen.")
 (drscheme-internal-error "Interne Fout van DrScheme")
 
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
 (tool-error-phase1 "Error in phase 1 for tool ~s; ~s")
 (tool-error-phase2 "Error in phase 2 for tool ~s; ~s")


 ;;; define popup menu
 (end-of-buffer-define "<< buffereinde >>")
 (sort-by-name "Sort by name")
 (sort-by-position "Sort by position in file")
 (no-definitions-found "<< geen definities gevonden >>")
 (jump-to-defn "Jump to definition of ~a")

 (recent-items-sort-by-age "Sort by Age")
 (recent-items-sort-by-name "Sort by Name")
 
 ;;; show menu
 (hide-definitions-menu-item-label "Toon &Definities niet")
 (show-definitions-menu-item-label "Toon &Definities")
 (definitions-menu-item-help-string "Toon het definitievenster al dan niet")
 (show-interactions-menu-item-label "Toon &Interacties")
 (hide-interactions-menu-item-label "Toon &Interacties niet")
 (interactions-menu-item-help-string "Toon het interactievenster al dan niet")
 
 ;;; file menu
 (save-definitions-as "Definities ops&laan als...")
 (save-definitions "Definities opslaan")
 (print-definitions "Definitions af&drukken...")
 (about-drscheme "Omtrent DrScheme")
 (save-other "Anderszins opslaan")
 (save-definitions-as-text "Save Definitions As Text...")
 (save-interactions "Save Interactions")
 (save-interactions-as "Save Interactions As...")
 (save-interactions-as-text "Save Interactions As Text...")
 (print-interactions "Print Interactions...")
 
 ;;; edit-menu
 (split-menu-item-label "&Splits")
 (collapse-menu-item-label "V&oeg samen")
 
 ;;; language menu
 (language-menu-name "&Taal")
 
 ;;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Doen!")
 (execute-menu-item-help-string "Voer het programma in het definitievenster uit")
 (break-menu-item-label "Onderbreken")
 (break-menu-item-help-string "Onderbreek de huidige berekening")
 (kill-menu-item-label "Beëindigen")
 (kill-menu-item-help-string "Beëindig de huidige berekening")
 (clear-error-highlight-menu-item-label "Verwijder foutkleur")
 (clear-error-highlight-item-help-string "Verwijdert de roze kleur die de fout aangeeft")
 (reindent-menu-item-label "&Herindenteer")
 (reindent-all-menu-item-label "Herindenteer &Alles")
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
 (inline-saved-program-in-executable/windows/path
   "WARNING! The generated executable relies on three DLLs: libmred.dll, libmzsch.gll, and libgc.dll, which are located at\n\n~a\n\nThe executable finds the DLLs either in the executable's directory or through the PATH enviornment variable.\n\nWhen you installed DrScheme, the installer adjusted the user's PATH to include the directory where the DLLs were installed. Beware of configuration or user changes since installation.\n\nIf you move the executable to another machine, you must also copy the DLLs to the other machine --- either to the same directory as the executable, or to a directory in the other machine's PATH.")
 (launcher "Launcher")
 (stand-alone "Stand-alone")
 (executable-type "Type")
 (executable-base "Base")
 (filename "Filename: ")
 (create "Create")
 (please-choose-an-executable-filename "Please choose a filename to save the executable.")
 
 (create-servlet "Create Servlet...")
  
 ;;; buttons
 (execute-button-label "Doen!") 
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
 (insert-fraction-menu-item-label "Breuk...")

 ;; number snip popup menu
 (show-decimal-expansion "View decimal expansion")
 (show-fraction-view "View as fraction")
 (show-more-decimal-places "Show more decimal places")
 
 ;;; Teachpack messages
 (select-a-teachpack "Select a Teachpack")
 (clear-teachpack "Clear ~a Teachpack")
 (teachpack-error-label "DrScheme - Teachpack error")
 (teachpack-dne/cant-read "The teachpack file ~a does not exist or is not readable.")
 (teachpack-didnt-load "The teachpack file ~a did not load properly.")
 (teachpack-error-invoke "The teachpack file ~a raised an error when invoked.")
 (add-teachpack-menu-item-label "Add Teachpack...")
 (clear-all-teachpacks-menu-item-label "Clear All Teachpacks")
 (teachpack-not-only-one-import "The teachpack unit/sig in ~a must have exactly one import.")
 (drscheme-teachpack-message-title "DrScheme Teachpack")
 (already-added-teachpack "Already added ~a teachpack")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Kies een taal.  De verstekwaarde is normaliter de juiste voor beginnende cursisten.")
 (language-dialog-title "Configure Language")
 (case-sensitive-label "Case sensitive")
 (output-style-label "Output Style")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (sharing-printing-label "Show sharing in values")
 (use-pretty-printer-label "Insert newlines in printed values")
 (input-syntax "Invoersyntaxis")
 (dynamic-properties "Dynamische eigenschappen")
 (output-syntax "Uitvoersyntaxis")
 (no-debugging-or-profiling "No debugging or profiling")
 (debugging "Debugging")
 (debugging-and-profiling "Debugging and profiling")
 (whole/fractional-exact-numbers-label "Print numbers as fractions")
 (booleans-as-true/false-label "Schrijf true/false in plaats van #T/#F")
 (show-details-button-label "Toon details")
 (hide-details-button-label "Toon details niet")
 (choose-language-menu-item-label "Kies taal...")
 (revert-to-language-defaults "Herstel verstekwaarden")
 (language-docs-button-label "Taaldocumenten")
 (fraction-style "Breukweergave")
 (use-mixed-fractions "Gemengde breuken")
 (use-repeating-decimals "Repeterende breuken")
 (decimal-notation-for-rationals "Schrijf breuken decimaal")
 (please-select-a-language "Kies een taal")

 
 ;;; languages
 (beginning-student "Beginner")
 (beginning-one-line-summary "define, cond, structs, constanten, and primitieven")
 (beginning-student/abbrev "Beginner, met lijstnotatie")
 (beginning/abbrev-one-line-summary "Beginner, maar lijsten worden met lijstnotatie afgedrukt")
 (intermediate-student "Middenmoot")
 (intermediate-one-line-summary "Beginner, plus lexikaal bereik")
 (intermediate-student/lambda "Middenmoot, plus lambda")
 (intermediate/lambda-one-line-summary "Middenmoot, plus hogere-ordefuncties")
 (advanced-student "Gevorderde")
 (advanced-one-line-summary "Middenmoot, plus lambda en mutatie")
 (full-language "Compleet") ;; also in the HtDP languages section
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "R5RS-achtig")
 (pretty-big-scheme "Vrij groot (bevat MrEd en Gevorderde)")
 (pretty-big-scheme-one-line-summary "Inclusief syntaxis and functies van de HtDP-talen")
 (r5rs-lang-name "Standaard (R5RS)")
 (r5rs-one-line-summary "R5RS, kaal")
 (unknown-debug-frame "[onbekend]")
 
 (module-language-one-line-summary "Execute creates a REPL in the context of the module, including the module's declared language")
  
 ;;; debug language
 (backtrace-window-title "Spoor - DrScheme")
 (files-interactions "~a's interacties") ;; filled with a filename
 (current-interactions "interacties")
 (current-definitions "definities")
 (mzscheme-w/debug "Enkel tekst (MzScheme, omvat R5RS)")
 (mzscheme-one-line-summary "PLTs onderliggende Scheme")
 (mred-w/debug "Grafisch (MrEd, bevat MzScheme)")
 (mred-one-line-summary "MzScheme met een grafische schil")

 ;; profiling
 (profiling-low-color "Laag")
 (profiling-high-color "Hoog")
 (profiling-choose-low-color "Kies een onderkleur")
 (profiling-choose-high-color "Kies een bovenkleur")
 (profiling "Profiling") ; <**> - need to check actual usage.
 (profiling-example-text "(define (jippie) (jippie))")
 (profiling-color-config "Profiling Color Range") 
 (profiling-scale "Profiling Color Scale")
 (profiling-sqrt "Vierkantswortel")
 (profiling-linear "Lineair")
 (profiling-square "Kwadraat")
 (profiling-number "Aantal aanroepen")
 (profiling-time "Totaaltijd")
 (profiling-clear "Wis profiel")
 (profiling-update "Werk profiel bij")
 (profiling-col-percent-time "% Tijd")
 (profiling-col-function "Functie")
 (profiling-col-name "Naam")
 (profiling-col-time-in-msec "Msec")
 (profiling-col-calls "Aanroepen")
 (profiling-show-profile "Toon profiel")
 (profiling-hide-profile "Toon profiel niet")
 (profiling-unknown-src "<< onbekend >>")
 (profiling-no-information-available
 "Er is geen profielinformatie beschikbaar.  Daartoe moet profilering in uw taal mogelijk zijn, en uw programma al gelopen hebben.")

 (profiling-clear? "Veranderingen in het definitievenster maken de profielinformatie ongeldig.  Toch doorgaan?")
 
 ;;; repl stuff
 (evaluation-terminated "Berekening gestopt")
 (evaluation-terminated-explanation
  "De rekenthread loopt niet meer, dus tot de volgende uitvoering kan geen berekening plaatsvinden.")
 (last-stack-frame "toon top van de stapel")
 (last-stack-frames "toon top ~a van de stapel")
 (next-stack-frames "toon volgende ~a van de stapel")
 
 ;;; welcoming message in repl
 (language "Language")
 (custom "custom")
 (teachpack "Teachpack")
 (welcome-to "Welcome to")
 (version "version")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Wilt u de berekeing beëindigen?")
 (just-break "Enkel onderbreken")
 (kill "Beëindigen")
 (kill? "Beëindigen?")

 ;;; version checker
 (vc-update-check "Versie bijwerken")
 (vc-check-prompt "Check for PLT software updates over the Internet?")
 (vc-please-wait "Even wachten a.u.b.")
 (vc-connecting-version-server "Legt verbinding met PLT versieleverancier")
 (vc-network-timeout "Netwerk plat") 
 (vc-cannot-connect  "Geen verbinding met PLT versieleverancier")
 (vc-network-failure "Networkprobleem")
 (vc-old-binaries "Binaire code voor DrScheme (of MzScheme) is verouderd")
 (vc-binary-information-format "Binaire code heeft versie: ~a (stap ~a)")
 (vc-details-format "~a~nDetails:~n~a")
 (vc-details-text "Details:~n")
 (vc-error-format "Fout: ~a") 
 (vc-current-format "~a v.~a (stap ~a) is bij")
 (vc-update-format "~a v.~a (stap ~a) moet bijgewerkt worden tot v.~a (stap ~a)")
 (vc-binary-name "Binair") ; <**> - translated as adjective.  Actual use not checked.
 (vc-updates-available "Nieuwere versies verkrijgbaar te")
 (vc-latest-binary-information-format "Jongste vrijgegeven versie: ~a (stap ~a)")
 (vc-update-dialog-title "PLT update status")
 (vc-need-update-string "One or more installed PLT software packages needs updating")
 (vc-no-update-string "Alle geïnstalleerde PLT-programmatuur is bij")

 ;; special menu
 (special-menu "Invoegen")
 
 ;; large semi colon letters
 (insert-large-letters... "Insert Large Letters...") ; <**>
 (large-semicolon-letters "Large Semicolon Letters")
 (text-to-insert "Text to insert")

 (module-browser-filename-format "Full Filename: ~a (~a lines)")
 (module-browser-root-filename "Root Filename: ~a")
 (module-browser-font-size-gauge-label "Lettergrootte")
 (module-browser-progress-label "Module overview progress")
 (module-browser-adding-file "Toevoegen bestand: ~a...")
 (module-browser-laying-out-graph-label "Laying out graph")
 (module-browser-open-file-format "Open ~a")
 (module-browser "Module Browser") ;; frame title
 (module-browser... "Module Browser...") ;; menu item title
 (module-browser-error-expanding "Error expanding the program:\n\n~a")

 (happy-birthday-matthias "Lang zal Matthias leven!")

 (mrflow-using-default-language-title "Verstektaal gebruikt")
 (mrflow-using-default-language "The language currently used does not have a type table defined for its primitives. Using R5RS Scheme instead.")
 (mrflow-button-title "Analyseer")
 ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
 ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
 (mrflow-coloring-error-title "Onbekende kleur")
 (mrflow-coloring-error "Geen stijl opgegeven voor kleur: ~a")
 (mrflow-popup-menu-show-type "Toon type")
 (mrflow-popup-menu-hide-type "Toon type niet")
 (mrflow-popup-menu-show-errors "Toon fouten")
 (mrflow-popup-menu-hide-errors "Toon fouten niet")
 (mrflow-popup-menu-tack-all-arrows "Alle pijlen vast")
 (mrflow-popup-menu-untack-all-arrows "Alle pijlen los")
 ;(mrflow-read-exception-title "Read Exception")
 ;(mrflow-read-exception "Read exception: ~a")
 ;(mrflow-syntax-exception-title "Syntax Exception")
 ;(mrflow-syntax-exception "Syntax exception: ~a")
 ;(mrflow-unknown-exception-title "Unknown Exception")
 ;(mrflow-unknown-exception "Unknown exception: ~a")
 ;(mrflow-language-primitives-error-title "Fout in basisfuncties")
 ;(mrflow-language-primitives-error "Onjuiste bestandsnaam voor typentabel basisfuncties: ~a")

 (xml-tool-menu "XML")
 (xml-tool-insert-xml-box "XML Box")
 (xml-tool-insert-scheme-box "Scheme Box")
 (xml-tool-insert-scheme-splice-box "Scheme Splice Box")
 (xml-tool-xml-box "XML Box")
 (xml-tool-scheme-box "Scheme Box")
 (xml-tool-scheme-splice-box "Scheme Splice Box")
 (xml-tool-switch-to-scheme "Switch to Scheme box")
 (xml-tool-switch-to-scheme-splice "Switch to Scheme splice box")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "Verwijder wit in lege tags") ; <**> - don't know the official Dutch for 'tag' here..
 (xml-tool-leave-whitespace-alone
  "Laat wit staan")
 
 (show-recent-items-window-menu-item "Toon onlangs geopende bestanden in een apart venster")
 (show-recent-items-window-label "Onlangs geopende bestanden")
 (number-of-open-recent-items "Aantal recente items")
 (switch-anyway "Toch van bestand wisselen")

 (stepper-program-has-changed "WAARSCHUWING: Programma is veranderd.")
 (stepper-program-window-closed "WAARSCHUWING: Programmavenster is weg.")
 )

