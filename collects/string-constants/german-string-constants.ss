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
 (plt "PLT")
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
 (bug-report-field-priority "Priorität")
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
 (bug-report-submit "Abschicken")	
 (sending-bug-report "Bug Report wird gesendet")
 (error-sending-bug-report "Absenden des Bug Reports fehlgeschlagen")
 (error-sending-bug-report-expln "Ein Fehler ist beim Absenden des Bug Reports aufgetreten. Falls Ihre Internetverbindung sonst fehlerfrei funktioniert, besuchen Sie bitte:\n\n    http://www.cs.rice.edu/CS/PLT/Bugs/ \n\nund teilen Sie uns den Bug mit unserem Online-Formular mit. Wir bitten um Ihr Verständnis.\n\nDie Fehlermeldung lautet:\n~a")
 (bug-report-sent "Bug Report abgesendet")
 (bug-report-sent-detail "Wir danken für Ihren Bugreport. Sie sollten eine Bestätigung innerhalb der nächsten 30 Minuten per email bekommen; falls nicht, bitte sagen Sie uns hier bescheid: scheme@plt-scheme.org.")
 (illegal-bug-report "Illegaler Bug Report")
 (pls-fill-in-field "Bitte auch das \"~a\" Feld ausfüllen!")
 (malformed-email-address "Ungültige email Adresse")

 ;;; check syntax
 (check-syntax "Syntax prüfen")
 (cs-italic "Kursiv")
 (cs-bold "Fett")
 (cs-underline "Unterstrichen")
 (cs-change-color "Farbe verändern")
 (cs-tack/untack-arrow "Tack/Untack Arrow")	; i should learn what this means (islwtm :). mit Pfeil festnageln perhaps
 (cs-jump "Jump")	; islwtm
 (cs-error-message "Fehlermeldung")
 (cs-open-file "~a öffnen")
 (cs-rename-var "~a umbenennen")
 (cs-rename-id "Identifier umbenennen")
 (cs-rename-var-to "~a wie folgt umbenennen:")
 (cs-name-duplication-error "Der neugewählte Name, ~s, ist in diesem Scope schon vorhanden.")	; best German word for scope? i could use a German copy of SICP! :)
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Collect")	; islwtm
 (read-only "Schreibgeschützt")
 (read/write "Zum Lesen und Schreiben")
 (auto-extend-selection "Auto-extend")	; islwtm
 (overwrite "Überschreiben")
 (running "Wird ausgeführt")
 (not-running "Wird nicht ausgeführt")
 
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
  "~a ist keine gültige Zeilennummer. (Erforderlich wäre eine ganze Zahl zwischen 1 und ~a)")
 (goto-position "Gehe auf Position")
 (no-full-name-since-not-saved
  "Die Datei hat keinen Gesamtnamen, weil sie noch nicht abgespeichert wurde.")
 (cannot-open-because-dne "Die Datei ~a kann nicht geöffnet werden, weil sie nicht existiert.")
 (interactions-out-of-sync
  "WARNUNG: Das Interactions Fenster ist nicht mehr dem Stand des Definitionsfensters. Bitte 'Ausführen' anklicken.")
 (file-is-not-saved "Die Datei \"~a\" ist nicht abgespeichert.")
 (save "Abspeichern")
 (please-choose-either "Bitte entweder \"~a\" oder \"~a\" wählen")
 (close-anyway "Trotzdem schliessen")

 (url "URL")
 (url: "URL:")
 (open-url... "URL öffnen...")
 (open-url "URL öffnen")
 (browse... "Browsen...")	; pbliE
 (bad-url "Ungültiger URL")
 (bad-url:this "Ungültiger URL: ~a")
 
 ;; Help Desk
 (search-results "Suchergebnisse")
 (help-desk "Help Desk")	; pbliE. perhaps "Hilfe" ?
 (help-desk-n "Help Desk ~a")
 (about-help-desk "About Help Desk")
 (help-desk-about-string
  "Help Desk enthält ausführliche Informationen über PLT software, einschliesslich DrScheme, MzScheme, und MrEd.\n\nVersion ~a\nCopyright (c) 1995-2001 PLT")
 (help-on-help "Hilfe zu Help Desk")
 (help-on-help-details "Um Hilfe darüber zu bekommen, wie man mit Help Desk umgeht, folgen Sie dem Link `How to use Help Desk' auf der Help Desk Startseite. (Um dahin zu kommen, drücken Sie den `Home' Knopf oben im Help Desk Fenster.)")
 (find-docs-for "Finde Dokumente über:")	; GmpOa~
 (search "Suche")
 ; next 3 are popup menu choices at bottom of help desk window
 (search-for-keyword "für Keyword")
 (search-for-keyword-or-index "für Keyword oder Indexeintrag")
 (search-for-keyword-or-index-or-text "für Keyword, Indexeintrag, oder Text")
 (exact-match "genauer Treffer")
 (containing-match "beinhaltet")
 (regexp-match "regexp match")
 (stop "Abbrechen")
 (feeling-lucky "Feeling Lucky")	;pbliE
 (nothing-found-for-search-key "Nichts gefunden für \"~a\".")
 (searching "wird gesucht...")
 (search-stopped "(Suche abgebrochen.)")
 (search-stopped-too-many-matches "(Suche abgebrouchen -- zu viele Treffer gefunden.)")
 (reload "neu laden")
 (help "Hilfe")
 (searching... "es wird gesucht...")
 (nothing-found-for-empty-search "Nichts gefunden -- leere Suche")
 (nothing-found-for "Nichts für ~a gefunden")
 (and "und")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "~a installieren oder zum editieren öffnen?")
 (install-plt-file/yes "Installieren")
 (install-plt-file/no "Editieren")	; aka redigieren
 
 ;;; about box
 (about-drscheme-frame-title "Über DrScheme")
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
 (found-bad-pref "Ungültige Preferenz in Datei \"~a\"")
 (expected-list-of-length2 "Eine Liste der Länge 2 erwartet")
 (general-prefs-panel-label "Allgemein")
 (highlight-parens "zwischen entsprechenden Klammern Text hervorheben")
 (fixup-parens "Correct parens")	; verb correct or adjective? :)
 (flash-paren-match "bei abschliessender Klammer die Anfangsklammer leuchten lassen")
 (auto-save-files "Dateien automatisch abspeichern")
 (map-delete-to-backspace "Löschtaste als Backspace verstehen")
 (verify-exit "Beenden bestätigen")
 (ask-before-changing-format "Änderung des Dateiformatts vorher bestätigen")
 (wrap-words-in-editor-buffers "Zeilenumbruch in Editor")
 (show-status-line "Statuszeile anzeigen")
 (count-from-one "Zeilen- und Spaltenzähler fangen bei eins an") 
 (display-line-numbers "Zeilenzahl in Puffer zählen, nicht Zeichenanzahl")
 (enable-keybindings-in-menus "Enable keybindings in menus")	; islwtm
 (automatically-to-ps "automatisch in eine Postscriptdatei drucken")	; islwtm
 (use-mdi "MDI Windows benutzen") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Getrenntes Dialogfenster für Suche benutzen")
 (default-fonts "Default Schriftarten")
 
 ; should have entire alphabet
 (font-example-string "The quick brown fox jumped over the lazy dogs.") ; oops, have to look this up; there is a German analog

 (change-font-button-label "Ändern")
 (fonts "Schriftarten")

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Bitte eine neue \"~a\" Schriftart wählen")

 (font-size-slider-label "Größe")
 (restart-to-see-font-changes "Neustart um Schriftartänderungen zu sehen")

 (font-prefs-panel-title "Schriftart")
 (font-name "Schriftartname")
 (font-size "Schriftgröße")
 (set-font "Set Font...")	; islwtm
 (select-font-name "Schriftartnamen auswählen")
 (example-text "Beispieltext:")
 (general-ii "General II")	; islwtm
 (only-warn-once "Nur einmal warnen falls Execution- und Interaktionsfenster nicht synchron sind")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Einzüge")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Enter new ~a-like keyword:")	; islwtm
 (x-keyword "~a Keyword")
 (x-like-keywords "~a-like Keywords")

 (expected-a-symbol "Symbol erwartet: ~a gefunden")
 (already-used-keyword "\"~a\" ist schon ein Keyword, das einen besondern Linkseinzug auslöst.")
 (add-keyword "Hinzufügen")
 (remove-keyword "Entfernen")
 
 ;;; find/replace
 (find-and-replace "Suchen und Ersetzen")
 (find "Suchen")
 (replace "Ersetzen")
; hmmm, different from this:  (separate-dialog-for-searching "Getrenntes Dialogfenster für Suche benutzen")  ?

 (use-separate-dialog-for-searching "Getrenntes Dialogfenster für Suche benutzen")
 (replace&find-again "Ersetzen und nochmal suchen") ;;; need double & to get a single &
 (replace-to-end "Ab hier Alles ersetzen")
 (forward "Vorwärts")
 (backward "Rückwärts")
 (hide "Verstecken")
 
 ;;;reverting a file
 (error-reverting "DrScheme - Fehler aufgetreten beim Rückgängigmachen")	; islwtm
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
 (add-button-label "Hinzufügen") ;;; for multi-file selection
 (add-all-button-label "Alle hinzufügen") ;;; for multi-file selection
 (remove-button-label "Entfernen") ;;; for multi-file selection
 (file-wrong-form "Dieser Dateiname ist nicht zulässig")	; islwtm
 (select-files "Dateien auswählen")
 (select-file "Eine Datei auswählen")
 (dir-dne "Verzeichnis existiert nicht.")
 (file-dne "Datei existiert nicht.")
 (empty-filename "Leerer Dateiname ungültig.")
 (that-is-dir-name "Das ist ein Verzeichnisname.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrScheme's menus will appear
 ;;; in the wrong order.
 (file-menu "Datei")
 (edit-menu "Bearbeiten")
 (help-menu "Hilfe")
 (windows-menu "Fenster")
 
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

 (new-info "Neue Datei öffnen")
 (new-menu-item "&Neu")

 (open-info "Datei von der Festplatte öffnen")
 (open-menu-item "Ö&ffnen...")

 (revert-info "Inhalt entsprechend der Datei zurücksetzen")
 (revert-menu-item "&Zurücksetzen")

 (save-info "Diese Datei auf Platte abspeichern")
 (save-menu-item "&Speichern")

 (save-as-info "Dateiname festlegen und Datei auf Platte speichern.")
 (save-as-menu-item "Speichern &unter ...")

 (print-info "Drucke diese Datei")
 (print-menu-item "&Drucken...")

 (close-info "Datei schliessen")
 (close-menu-item "S&chliessen")

 (quit-info "Alle Fenster schliessen")
 (quit-menu-item-windows "&Beenden")
 (quit-menu-item-others "&Beenden")
 
 (edit-menu-label "&Bearbeiten")
 
 (undo-info "Die letzte Aktion rückgängig machen")
 (undo-menu-item "&Rückgängig machen")

 (redo-info "Die letzte rückgängig gemachte Aktion wieder machen")
 (redo-menu-item "&Wiederherstellen")

 (cut-info "Das Markierte löschen und in der Zwischenablage speichern")
 (cut-menu-item "A&usschneiden")

 (copy-info "Das Markierte auf die Zwischenablage kopieren")
 (copy-menu-item "&Kopieren")

 (paste-info "Den Inhalt der Zwischenablage einfügen, ggf. anstelle des markierten Inhaltes.")
 (paste-menu-item "E&infügen")

 (clear-info "Den markierten Inhalt löschen, ohne den Inhalt der Zwischenablage zu ändern.")
 (clear-menu-item-others "Markierung lös&chen")
 (clear-menu-item-windows "Markierung lös&chen")

 (select-all-info "Das gesamte Dokument markieren")
 (select-all-menu-item "&Alles markieren")
 
 (find-info "Nach einer Zeichenfolge suchen")
 (find-menu-item "Suchen...")

 (find-again-info "Wiederholte Suche nach dem gleichen String")
 (find-again-menu-item "Weitersuchen")
 
 (replace-and-find-again-info "Ersetze den aktuellen Text und suche nach dem gleichen String weiter")
 (replace-and-find-again-menu-item "Ersetzen && und weitersuchen")

 (preferences-info "Preferenzen konfigurieren")
 (preferences-menu-item "Preferenzen...")

 (keybindings-info "Die jetzt aktiven Keybindings (Assoziation zwischen Tasten und Funktionen) anzeigen")
 (keybindings-menu-item "Keybindings")
 (keybindings-frame-title "Keybindings")
 (keybindings-sort-by-name "Nach Name sortieren")
 (keybindings-sort-by-key "Nach Schlüssel sortieren")

 (insert-text-box-item "Text Box einfügen")
 (insert-pb-box-item "Pasteboard Box einfügen")
 (insert-image-item "Image einfügen...")
 (wrap-text-item "Textitem umbrechen")

 (windows-menu-label "&Fenster")
 (bring-frame-to-front "Bring frame to front")       ;;; title of dialog
 (bring-frame-to-front... "Bring frame to front...") ;;; corresponding title of menu item
 
 (show-menu-label "&Anzeigen")

 (help-menu-label "&Hilfe")
 (about-info "Autoren dieser Anwendung und andere Details")	; islwtm authors?
 (about-menu-item "Info...")

 (new-help-desk "&Neu Help Desk")  ; islwtm
 
 ;;; exiting and quitting are you sure dialog
 ;;; (exit is used on windows, quit on macos. go figure)
 (exit-lc "beenden")
 (exit-cap "Beenden")
 (quit-lc "beenden")
 (quit-cap "Beenden")
 ;;; in are-you-sure-format, either exit or quit is filled in (from above)
 ;;; based on the platform drscheme is running on.
 (are-you-sure-format "Sind Sie sicher, dass Sie ~a wollen?")
 
 ;;; autosaving
 (error-autosaving "Fehler beim automatischen Abspeichern \"~a\".")
 (autosaving-turned-off "Automatisches Abspeichern ist abgeschaltet\nbis die Datei abgespeichert ist.")
 
 ;;; file modified warning
 (file-has-been-modified
  "Seitdem die Datei zuletzt gespeichert wurde, ist sie modifiziert worden. Änderungen überschreiben?")
 (overwrite-file-button-label "Überschreiben") ;verwerfen? (discard changes?)
 
 (definitions-modified 
  "Der Definitionstext wurde im Dateisystem geändert. Bitte speichern Sie ihn neu ab oder stellen Sie die alte Version wieder her.")
 (drscheme-internal-error "DrScheme Interner Fehler")
 
 ;;; tools
 (invalid-tool-spec "Die Tool-Spezifikation in der Collection von ~a's info.ss Datei ist ungültig. Gültig sind ein String oder eine nicht-leere Liste von Strings. ~e")
 (error-loading-tool-title "DrScheme - Fehler beim Laden von ~s; ~s")
 (error-invoking-tool-title "Fehler beim Ausführen von ~s;~s")
 (tool-tool-names-same-length
  "`tool-names' und `tools' müssen Listen gleicher Länge sein, in der info.ss Datei für ~s. Sie sind ~e und ~e")
 (tool-tool-icons-same-length
  "`tool-icons' und `tools' müssen Listen gleicher Länge sein, in der info.ss Datei für ~s. Sie sind ~e und ~e")
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
 (save-definitions-as "Speichere Definitionen als...")
 (save-definitions "Speichere Definitionen")
 (print-definitions "Drucke Definitionen...")
 (about-drscheme "Info DrScheme")
 (save-other "Speichere Andere")
 (save-definitions-as-text "Speichere Definitionen als Text...")
 (save-interactions "Speichere Interaktionen") ; fixme: besserer Begriff?
 (save-interactions-as "Speichere Interaktionen als...")
 (save-interactions-as-text "Speichere Interaktionen als Text...")
 (print-interactions "Drucke Interaktionen...")
 
 ;; edit-menu
 (split-menu-item-label "&Teilen")
 (collapse-menu-item-label "&Verbinden") ; fixme: collapse (wieder zusammenfügen?)

 ;; language menu
 (language-menu-name "&Sprache")

 ;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Ausführen")
 (execute-menu-item-help-string "Programm im Definitionsfenster neu starten")
 (break-menu-item-label "Abbruch")
 (break-menu-item-help-string "Abbruch des aktuellen Prozesses")
 (kill-menu-item-label "Prozess beenden")
 (kill-menu-item-help-string "Beende den aktuellen Prozess")
 (reindent-menu-item-label "&Neu einrücken") ; fixme: besserer Begriff	(ist besser als "links einzug neu berechnen", oder?)
 (reindent-all-menu-item-label "&Alles neu einrücken") ; fixme: besserer Begriff
 (comment-out-menu-item-label "&Auskommentieren")
 (uncomment-menu-item-label "Auskommentieren rückgängig")	;  (uncomment, not strip comments, i assume)

 ;; launcher
 (create-launcher-title "Erzeuge externes Startprogramm")
 (must-save-before-launcher "Sie müssen zuerst Ihr Programm abspeichern, bevor Sie das Startprogramm erzeugen können.")
 (save-a-launcher "Sichere ein externes Startprogramm")

 ;; buttons
 (execute-button-label "Ausführen") 
 (save-button-label "Sichern")
 (break-button-label "Abbruch")
 
 ;; search help desk popup menu
 (search-help-desk-for "Suche im Help Desk nach \"~a\"")
 (exact-lucky-search-help-desk-for "Exakte Suche im Help Desk nach \"~a\"")
 
 ;; collapsing and expanding sexpressions poup menu item label
 (collapse-sexp "Schrumpfen")
 (expand-sexp "Erweitern")

 ;; fraction dialog
 (enter-fraction "Eingabe Bruchzahl")
 (whole-part "Ganzzahl")
 (numerator "Zähler")
 (denominator "Nenner")
 (invalid-number "Ungültige Zahl: Eine exakte reelle Zahl (jedoch keine Ganzzahl) erforderlich.")
 (insert-fraction-menu-item-label "Bruchzahl einfügen...")
 
 ;; TeachPack messages
 (select-a-teachpack "Wähle ein TeachPack")
 (clear-teachpack "Lösche ~a TeachPack")
 (teachpack-error-label "DrScheme - TeachPack Fehler")
 (teachpack-dne/cant-read "Die TeachPack Datei ~a existiert nicht oder kann nicht gelesen werden.")
 (teachpack-didnt-load "Die TeachPack Datei ~a konnte nicht geladen werden.")
 (teachpack-error-invoke "Die TeachPack Datei ~a konnte nicht ausgeführt werden.")
 (add-teachpack-menu-item-label "Füge Teachpack hinzu...")
 (clear-all-teachpacks-menu-item-label "Lösche alle Teachpacks")
 (teachpack-not-only-one-import "Die TeachPack unit/sig in ~a darf nur exakt eine Importdefinition besitzen.")
 (drscheme-teachpack-message-title "DrScheme Teachpack")
 (already-added-teachpack "~a Teachpack schon vorhanden")
 
 ;;; Language dialog
 (language-dialog-title "Sprache konfigurieren")
 (case-sensitive-label "Groß-/Kleinschreibung beachten")
 (output-style-label "Ausgabestil")
 (constructor-printing-style "Constructor") ; these are pretty special, I'd leave them
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write") ; islwtm
 (sharing-printing-label "Show sharing in values") ; islwtm
 (use-pretty-printer-label "Insert newlines in printed values") ; Zeilenumbruchzeichen in Druckwerten einfügen ??
 (input-syntax "Input Syntax")
 (output-syntax "Output Syntax")
 (whole/fractional-exact-numbers-label "Zahlen als Bruchzahlen ausgeben")	; really "print"? or "output"?
 (booleans-as-true/false-label "Boolesche Werte als Wahr und Falsch ausgeben")	; in contrast to #T, #F? are true and false translated? ; 
 (show-details-button-label "Details anzeigen")
 (hide-details-button-label "Details verstecken")
 (choose-language-menu-item-label "Sprache auswählen...")
 (revert-to-language-defaults "Default-Sprache wieder aktivieren")

 ;;; languages
 (beginning-student "Anfänger/in")
 (beginning-student/abbrev "Anfänger/in mit List-Kürzungen")
 (intermediate-student "Mittelstufe")
 (advanced-student "Fortgeschrittene/r")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "R5RS-like")          ; islwtm
 (mred-lang-name "Grafische Oberfläche ohne Debugging (MrEd)")
 (mzscheme-lang-name "Textoberfläche ohne Debugging (MzScheme)")
 (r5rs-lang-name "R5RS without debugging")  ; islwtm
 (unknown-debug-frame "[unbekannt]")
 
 ;;; debug language
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "~a's interactions") ;; filled with a filename
 (stack-frame-in-current-interactions "interactions")
 (stack-frame-in-current-definitions "definitions")
 (mzscheme-w/debug "Textual (MzScheme)")
 (mred-w/debug "Graphical (MrEd)")
 
 ;;; repl stuff
 (evaluation-terminated "Evaluation Terminated")	; pretty special. perhaps we should compare with German SICP?
 (evaluation-terminated-explanation
  "The evaluation thread is no longer running, so no evaluation can take place until the next execution.")
 (last-stack-frame "show the last stack frame")
 (last-stack-frames "show the last ~a stack frames")
 (next-stack-frames "show the next ~a stack frames")
 
 ;;; welcoming message in repl
 (language "Sprache")
 (custom "angepasst")	; adjective (might need to decline in German?)
 (teachpack "Teachpack")
 (welcome-to "Willkommen zu")
 (version "Version")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Evaluation beenden?")
 (just-break "Nur Break")	; Break has a special meaning? Interrupt?
 (kill "Beenden")
 (kill? "Beenden?")
 )
