; first question: what character set shall we use? how to represent 6 umlauted vowels and sharp s (s-zed)
; the ones that Reini has translated look wrong on my PC. hmmm, copied and pasted from Mac. maybe my one fault.
;
(
;;; when translating this constant, substitue name of actual langauge for `English'
(is-this-your-native-language
  "Ist Deutsch Ihre Muttersprache?")

 (are-you-sure-you-want-to-switch-languages
  "Sind Sie sicher, dass Sie die Sprache der GUI wechseln wollen? (Wenn ja, muss DrScheme neugestartet werden)")
 
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Abbruch")
 (untitled "Ohne Titel")
 (untitled-n "Ohne Titel ~a")
 (warning "Warnung")
 (error "Fehler")
 (close "Schliessen") ;; as in, close an open window

 ;;; bug report form
 (cancel-bug-report? "Bug Report abbrechen?")
 (are-you-sure-cancel-bug-report?
  "Sind Sie sicher, dass Sie diesen Bug Report *nicht* abschicken wollen?")
 (bug-report-form "Bug Report Formular")
 (bug-report-field-name "Name")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Zusammenfassung")
 (bug-report-field-severity "Schweregrad")
 (bug-report-field-class "Klasse")	; meaning what?
 (bug-report-field-priority "Priorit�t")
 (bug-report-field-description "Beschreibung")
 (bug-report-field-reproduce1 "Schritte, um das Problem zu")	; why in two pieces?
 (bug-report-field-reproduce2 "Reproduzieren")	; oder nachzuvollziehen
 (bug-report-field-environment "Environment")	; Umgebung? perhaps best left in English (pbliE :)
 (bug-report-field-tools "Tools")		; Werkzeuge? (pbliE)
 (bug-report-field-docs-installed "Dokumente installiert")
 (bug-report-field-language "Sprache")		; DR Scheme language?
 (bug-report-field-teachpacks "Teachpacks")
 (bug-report-field-collections "Collections")
 (bug-report-field-human-language "Menschensprache")	;
 (bug-report-field-version "Version")
 (bug-report-show-synthesized-info "Synthesized Info anzeigen")	; (an)zeigen
 (bug-report-hide-synthesized-info "Synthesized Info verstecken")	; verstecken
 (bug-report-submit "Abschicken")	
 (sending-bug-report "Bug Report wird gesendet")
 (error-sending-bug-report "Absenden des Bug Reports fehlgeschlagen")
 (error-sending-bug-report-expln "Ein Fehler ist beim Absenden des Bug Reports aufgetreten. Falls Ihre Internetverbindung sonst fehlerfrei funktioniert, besuchen Sie bitte:\n\n    http://www.cs.rice.edu/CS/PLT/Bugs/ \n\nund teilen Sie uns den Bug mit unserem Online-Formular mit. Wir bitten um Ihr Verst�ndnis.\n\nDie Fehlermeldung lautet:\n~a")
 (bug-report-sent "Bug Report abgesendet")
 (bug-report-sent-detail "Wir danken f�r Ihren Bugreport. Sie sollten eine Best�tigung innerhalb der n�chsten 30 Minuten per email bekommen; falls nicht, bitte sagen Sie uns hier bescheid: scheme@cs.rice.edu.")
 (illegal-bug-report "Illegaler Bug Report")
 (pls-fill-in-field "Bitte auch das \"~a\" Feld ausf�llen!")
 (malformed-email-address "Ung�ltige email Adresse")

 ;;; check syntax
 (check-syntax "Syntax pr�fen")
 (cs-italic "Kursiv")
 (cs-bold "Fett")
 (cs-underline "Unterstrichen")
 (cs-change-color "Farbe ver�ndern")
 (cs-tack/untack-arrow "Tack/Untack Arrow")	; i should learn what this means (islwtm :). mit Pfeil festnageln perhaps
 (cs-jump "Jump")	; islwtm
 (cs-error-message "Fehlermeldung")
 (cs-open-file "~a �ffnen")
 (cs-rename-var "~a umbenennen")
 (cs-rename-id "Identifier umbenennen")
 (cs-rename-var-to "~a wie folgt umbenennen:")
 (cs-name-duplication-error "Der neugew�hlte Name, ~s, ist in diesem Scope schon vorhanden.")	; best German word for scope? i could use a German copy of SICP! :)
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Collect")	; islwtm
 (read-only "Schreibgesch�tzt")
 (read/write "Zum Lesen und Schreiben")
 (auto-extend-selection "Auto-extend Selection")	; islwtm
 (overwrite "�berschreiben")
 (running "Wird ausgef�hrt")
 (not-running "Wird nicht ausgef�hrt")
 
 ;;; misc
 (welcome-to-something "Wilkommen in ~a")	; hmmm, not sure of the best preposition. islwtm
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Wilkommen! (DrScheme-Version ~a, ~a)")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Wilkommen bei DrScheme")
 (version/language "Version ~a, ~a")

 (goto-line "Gehe auf Zeile")	; no tilde a or the like? (in German it might be nice to place this inside the phrase)
	; German might prefer the object as a tilde parameter -- GmpOa~ :)
 (goto-line-invalid-number
  "~a ist keine g�ltige Zeilennummer. (Erforderlich w�re eine ganze Zahl zwischen 1 und ~a)")
 (goto-position "Gehe auf Position")
 (no-full-name-since-not-saved
  "Die Datei hat keinen Gesamtnamen, weil sie noch nicht abgespeichert wurde.")
 (cannot-open-because-dne "Die Datei ~a kann nicht ge�ffnet werden, weil sie nicht existiert.")
 (interactions-out-of-sync
  "WARNUNG: Das Interactions Fenster ist nicht mehr dem Stand des Definitionsfensters. Bitte 'Ausf�hren' anklicken.")
 (file-is-not-saved "Die Datei \"~a\" ist nicht abgespeichert.")
 (save "Abspeichern")
 (please-choose-either "Bitte entweder \"~a\" oder \"~a\" w�hlen")
 (close-anyway "Trotzdem schliessen")

 (url "URL")
 (url: "URL:")
 (open-url... "URL... �ffnen")	; not sure how these are used, but I would prefer "URL x �ffnen" in German
 (open-url "URL �ffnen")
 (browse... "Browsen...")	; pbliE
 (bad-url "Ung�ltiger URL")
 (bad-url:this "Ung�ltiger URL: ~a")
 
 ;; Help Desk
 (search-results "Suchergebnisse")
 (help-desk "Help Desk")	; pbliE. perhaps "Hilfe" ?
 (help-desk-n "Help Desk ~a")
 (about-help-desk "About Help Desk")
 (help-desk-about-string
  "Help Desk enth�lt ausf�hrliche Informationen �ber PLT software, einschliesslich DrScheme, MzScheme, und MrEd.\n\nVersion ~a\nCopyright (c) 1995-2001 PLT")
 (help-on-help "Hilfe zu Help Desk")
 (help-on-help-details "Um Hilfe dar�ber zu bekommen, wie man mit Help Desk umgeht, folgen Sie dem Link `How to use Help Desk' auf der Help Desk Startseite. (Um dahin zu kommen, dr�cken Sie den `Home' Knopf oben im Help Desk Fenster.)")
 (find-docs-for "Finde Dokumente �ber:")	; GmpOa~
 (search "Suche")
 ; next 3 are popup menu choices at bottom of help desk window
 (search-for-keyword "f�r Keyword")
 (search-for-keyword-or-index "f�r Keyword oder Indexeintrag")
 (search-for-keyword-or-index-or-text "f�r Keyword, Indexeintrag, oder Text")
 (exact-match "genauer Treffer")
 (containing-match "beinhaltet")
 (regexp-match "regexp match")
 (stop "Abbrechen")
 (feeling-lucky "Feeling Lucky")	;pbliE
 (nothing-found-for-search-key "Nichts gefunden f�r \"~a\".")
 (searching "wird gesucht...")
 (search-stopped "(Suche abgebrochen.)")
 (search-stopped-too-many-matches "(Suche abgebrouchen -- zu viele Treffer gefunden.)")
 (reload "neu laden")
 (help "Hilfe")
 (searching... "es wird gesucht...")
 (nothing-found-for-empty-search "Nichts gefunden -- leere Suche")
 (nothing-found-for "Nichts f�r ~a gefunden")
 (and "und")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "~a installieren oder zum editieren �ffnen?")
 (install-plt-file/yes "Installieren")
 (install-plt-file/no "Editieren")	; aka redigieren
 
 ;;; about box
 (about-drscheme-frame-title "�ber DrScheme")
 (take-a-tour "Take a Tour!")	; pbliE
 (release-notes "Release Notes"); pbliE
 (parenthetical-last-version "(vorige Version ~a)")
 (parenthetical-last-language "(Vorige Sprache ~a)")
 (parenthetical-last-version/language "(vorige Version ~a, Sprache ~a)")
 
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Diese Datei im Textformatt abspeichern?")
 (save-in-drs-format "Diese Datei in einem drscheme-spezifische nicht-text Formatt abspeichern?")
 (yes "Ja")
 (no "Nein")
 
 ;;; preferences
 (preferences "Preferenzen")	; Vorlieben? :)
 (preferences-category "Kategorie")
 (saving-preferences "Preferenzen werden gesichert")
 (error-unmarshalling "Error unmarshalling ~a preference")	;islwtm, pbliE! :)
 (error-saving-preferences "Fehler beim Abspeichern der Preferenzen: ~a")
 (error-reading-preferences "Fehler beim Lesen der Preferenzen")
 (found-bad-pref "Ung�ltige Preferenz in Datei \"~a\"")
 (expected-list-of-length2 "Eine Liste der L�nge 2 erwartet")
 (general-prefs-panel-label "Allgemein")
 (highlight-parens "zwischen entsprechenden Klammern Text hervorheben")
 (fixup-parens "Correct parens")	; verb correct or adjective? :)
 (flash-paren-match "bei abschliessender Klammer die Anfangsklammer leuchten lassen")
 (auto-save-files "Dateien automatisch abspeichern")
 (map-delete-to-backspace "L�schtaste als Backspace verstehen")
 (verify-exit "Beenden best�tigen")
 (ask-before-changing-format "�nderung des Dateiformatts vorher best�tigen")
 (wrap-words-in-editor-buffers "Zeilenumbruch in Editor")
 (show-status-line "Statuszeile anzeigen")
 (count-from-one "Zeilen- und Spaltenz�hler fangen bei eins an") 
 (display-line-numbers "Zeilenzahl in Puffer z�hlen, nicht Zeichenanzahl")
 (enable-keybindings-in-menus "Enable keybindings in menus")	; islwtm
 (automatically-to-ps "automatisch in eine Postscriptdatei drucken")	; islwtm
 (use-mdi "MDI Windows benutzen") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Getrenntes Dialogfenster f�r Suche benutzen")
 (default-fonts "Default Schriftarten")
 
 ; should have entire alphabet
 (font-example-string "The quick brown fox jumped over the lazy dogs.") ; oops, have to look this up; there is a German analog

 (change-font-button-label "�ndern")
 (fonts "Schriftarten")

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Bitte eine neue \"~a\" Schriftart w�hlen")

 (font-size-slider-label "Gr��e")
 (restart-to-see-font-changes "Neustart um Schriftart�nderungen zu sehen")

 (font-prefs-panel-title "Schriftart")
 (font-name "Schriftartname")
 (font-size "Schriftgr��e")
 (set-font "Set Font...")	; islwtm
 (select-font-name "Schriftartnamen ausw�hlen")
 (example-text "Beispieltext:")
 (general-ii "General II")	; islwtm
 (only-warn-once "Nur einmal warnen falls Execution- und Interaktionsfenster nicht synchron sind")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Einz�ge")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Enter new ~a-like keyword:")	; islwtm
 (x-keyword "~a Keyword")
 (x-like-keywords "~a-like Keywords")

 (expected-a-symbol "Symbol erwartet: ~a gefunden")
 (already-used-keyword "\"~a\" ist schon ein Keyword, das einen besondern Linkseinzug ausl�st.")
 (add-keyword "Hinzuf�gen")
 (remove-keyword "Entfernen")
 
 ;;; find/replace
 (find-and-replace "Suchen und Ersetzen")
 (find "Suchen")
 (replace "Ersetzen")
; hmmm, different from this:  (separate-dialog-for-searching "Getrenntes Dialogfenster f�r Suche benutzen")  ?

 (use-separate-dialog-for-searching "Getrenntes Dialogfenster f�r Suche benutzen")
 (replace&find-again "Ersetzen und nochmal suchen") ;;; need double & to get a single &
 (replace-to-end "Ab hier Alles ersetzen")
 (forward "Vorw�rts")
 (backward "R�ckw�rts")
 (hide "Verstecken")
 
 ;;;reverting a file
 (error-reverting "Fehler aufgetreten beim R�ckg�ngigmachen")	; islwtm
 (could-not-read "\"~a\" konnte nicht gelesen werden")
 
 ;;; finder dialog
 (must-specify-a-filename "Dateiname muss angegeben werden")
 (file-does-not-exist "Die Datei \"~a\" existiert nicht.")
 (ask-because-file-exists "Die Datei\"~a\" exististiert schon. Ersetzen?")
 (dne-or-cycle "Die Datei\"~a\" beinhaltet ein nicht existierendes Verzeichnis (oder ein cycle).")	; islwtm -- this file? or this path??
 (get-file "Get file")	; islwtm -- 
 (put-file "Put file")
 (full-pathname "Gesamtpfad")
 (show-dot-files "Dateien und Verzeichnisse anzeigen, deren Namen mit einem Punkt beginnen")
 (up-directory-button-label "Ein Verzeichnis nach oben")
 (add-button-label "Hinzuf�gen") ;;; for multi-file selection
 (add-all-button-label "Alle hinzuf�gen") ;;; for multi-file selection
 (remove-button-label "Entfernen") ;;; for multi-file selection
 (file-wrong-form "Dieser Dateiname ist nicht zul�ssig")	; islwtm
 (select-files "Dateien ausw�hlen")
 (select-file "Eine Datei ausw�hlen")
 (dir-dne "Verzeichnis existiert nicht.")
 (file-dne "Datei existiert nicht.")
 (empty-filename "Leerer Dateiname ung�ltig.")
 (that-is-dir-name "Das ist ein Verzeichnisname.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrScheme's menus will appear
 ;;; in the wrong order.
 (file-menu "Datei")
 (edit-menu "Bearbeiten")
 (help-menu "Hilfe")
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

 (file-menu-label-windows "&Datei")
 (file-menu-label-other "&Datei")	; islwtm

 (new-info  "Neue Datei �ffnen")

 (open-info "Datei von der Festplatte �ffnen")

 (revert-info "Inhalt entsprechend der Datei zur�cksetzen")

 (save-info "Diese Datei auf Platte abspeichern")

 (save-as-info "Dateiname festlegen und Datei auf Platte speichern.")

 (print-info "Drucke diese Datei")

 (close-info "Datei schliessen")

 (quit-info "Alle Fenster schliessen")
 
 (edit-menu-label "&Bearbeiten")
 
 (undo-info "Die letzte Aktion r�ckg�ngig machen")
 (undo-menu-item "&R�ckg�ngig machen")

 (redo-info "Die letzte r�ckg�ngig gemachte Aktion wieder machen")
 (redo-menu-item "&Wiederherstellen")

 (cut-info "Das Markierte l�schen und in der Zwischenablage speichern")
 (cut-menu-item "A&usschneiden")

 (copy-info "Das Markierte auf die Zwischenablage kopieren")
 (copy-menu-item "&Kopieren")

 (paste-info "Den Inhalt der Zwischenablage einf�gen, ggf. anstelle des markierten Inhaltes.")
 (paste-menu-item "E&inf�gen")

 (clear-info "Den markierten Inhalt l�schen, ohne den Inhalt der Zwischenablage zu �ndern.")
 (clear-menu-item-others "Markierung l�s&chen")
 (clear-menu-item-windows "Markierung l�s&chen")

 (select-all-info "Das gesamte Dokument markieren")
 (select-all-menu-item "&Alles markieren")
 
 (find-info "Nach einer Zeichenfolge suchen")

 (find-again-info "Search for the same string as before")
 
 (replace-and-find-again-info "Replace the current text and search for the same string as before")

 (preferences-info "Configure your preferences")

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

 (help-menu-label "&Hilfe")
 (about-info "Credits and details for this application")
 
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
 (autosaving-turned-off "Autosaving is turned off\nuntil die Datei is saved.")
 
 ;;; file modified warning
 (file-has-been-modified
  "The file has beeen modified since it was last saved. Overwrite the modifications?")
 (overwrite-file-button-label "Overwrite")
 
 (definitions-modified 
  "Der Definitionstext wurde im Dateisystem ge�ndert. Bitte speichern Sie ihn neu ab oder stellen Sie die alte Version wieder her.")
 (drscheme-internal-error "DrScheme Interner Fehler")
 
 ;;; tools
 (invalid-tool-spec "Die tool Spezifikation in der Collection von ~a's info.ss Datei ist ung�ltig. G�ltig sind ein String oder eine nicht-leere Liste von Strings. ~e")
 (error-loading-tool-title "DrScheme - Fehler beim Laden von ~s; ~s")
 (error-invoking-tool-title "Fehler beim Ausf�hren von ~s;~s")
 (tool-tool-names-same-length
  "`tool-names' und `tools' m�ssen Listen gleicher L�nge sein, in der info.ss Datei f�r ~s. Sie sind ~e und ~e")
 (tool-tool-icons-same-length
  "`tool-icons' und `tools' m�ssen Listen gleicher L�nge sein, in der info.ss Datei f�r ~s. Sie sind ~e und ~e")
 (error-getting-info-tool
  "Fehler beim Laden von info.ss bei ~s")
 
  ;; define popup menu
 (end-of-buffer-define "<< Dateiende >>")
 (sort-by-name "Sortiere nach Namen")
 (sort-by-position "Sortiere nach Dateiposition") ; fixme
 (no-definitions-found "<< keine Definitionen gefunden >>")

 ;;; show menu
 (hide-definitions-menu-item-label "&Definitionen verstecken")
 (show-definitions-menu-item-label "&Definitionen anzeigen")
 (definitions-menu-item-help-string "Definitionsfenster anzeigen/verstecken")
 (show-interactions-menu-item-label "&Interaktionen anzeigen")
 (hide-interactions-menu-item-label "&Interaktionen verstecken")
 (interactions-menu-item-help-string "Interaktionsfenster anzeigen/verstecken")
 
 ;;; file menu
 (save-other "Speichere Andere")
 (save-definitions-as-text "Speichere Definitionen als Text...")
 (save-interactions "Speichere Interaktionen") ; fixme: besserer Begriff?
 (save-interactions-as "Speichere Interaktionen als...")
 (save-interactions-as-text "Speichere Interaktionen als Text...")
 (print-interactions "Drucke Interaktionen...")
 
 ;; edit-menu
 (split-menu-item-label "&Teilen")
 (collapse-menu-item-label "&Verbinden") ; fixme: collapse

 ;; language menu
 (language-menu-name "&Sprache")

 ;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Ausf�hren")
 (execute-menu-item-help-string "Programm im Definitionsfenster neu starten")
 (break-menu-item-label "Abbruch")
 (break-menu-item-help-string "Abbruch des aktuellen Prozesses")
 (kill-menu-item-label "Prozess beenden")
 (kill-menu-item-help-string "Beende den aktuellen Prozess")
 (reindent-menu-item-label "&Neu einr�cken") ; fixme: besserer Begriff	(links einzug neu berechnen)
 (reindent-all-menu-item-label "&Alles neu einr�cken") ; fixme: besserer Begriff
 (comment-out-menu-item-label "&Auskommentieren")
 (uncomment-menu-item-label "Kommentare &entfernen")	; auskommentieren r�ckg�ngig (not strip comments! :)

 ;; launcher
 (create-launcher-title "Erzeuge externes Startprogramm")
 (must-save-before-launcher "Sie m�ssen zuerrst Ihr Programm abspeichern, befor Sie das Startprogramm erzeugen k�nnen.")
 (save-a-launcher "Sichere ein externes Startprogramm")

 ;; buttons
 (execute-button-label "Ausf�hren") 
 (save-button-label "Sichern")
 (break-button-label "Abbruch")
 
 ;; search help desk popup menu
 (search-help-desk-for "Suche im Help Desk nach \"~a\"")
 (exact-lucky-search-help-desk-for "Exakte Suche im Help Desk nach \"~a\"")
 
 ;; fraction dialog
 (enter-fraction "Eingabe Bruchzahl")
 (whole-part "Ganzzahl")
 (numerator "Z�hler")
 (denominator "Nenner")
 (invalid-number "Ung�ltige Zahl: Eine exakte reelle Zahl (jedoch keine Ganzzahl) erforderlich.")
 (insert-fraction-menu-item-label "Bruchzahl einf�gen...")
 
 ;; TeachPack messages
 (select-a-teachpack "W�hle ein TeachPack")
 (clear-teachpack "L�sche ~a TeachPack")
 (teachpack-error-label "DrScheme - TeachPack Fehler")
 (teachpack-dne/cant-read "Die TeachPack Datei ~a existiert nicht oder kann nicht gelesen werden.")
 (teachpack-didnt-load "Die TeachPack Datei ~a konnte nicht geladen werden.")
 (teachpack-error-invoke "Die TeachPack Datei ~a konnte nicht ausgef�hrt werden.")
 (add-teachpack-menu-item-label "F�ge Teachpack hinzu...")
 (clear-all-teachpacks-menu-item-label "L�sche alle Teachpack's")
 (teachpack-not-only-one-import "Die TeachPack unit/sig in ~a darf nur exakt eine Importdefinition besitzen.")
 (drscheme-teachpack-message-title "DrScheme Teachpack")
 (already-added-teachpack "~a Teachpack schon vorhanden")
 
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

 ;;; languages
 (beginning-student "Beginning Student")
 (beginning-student/abbrev "Beginning Student with List Abbreviations")
 (intermediate-student "Intermediate Student")
 (advanced-student "Advanced Student")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (mred-lang-name "Graphical without debugging (MrEd)")
 (mzscheme-lang-name "Textual without debugging (MzScheme)")
 (unknown-debug-frame "[unknown]")
 
 ;;; debug language
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "~a's interactions") ;; filled with a filename
 (stack-frame-in-current-interactions "interactions")
 (stack-frame-in-current-definitions "definitions")
 (mzscheme-w/debug "Textual (MzScheme)")
 (mred-w/debug "Graphical (MrEd)")
 
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
 )


