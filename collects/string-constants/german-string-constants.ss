; first question: what character set shall we use? how to represent 6 umlauted vowels and sharp s (s-zed)
; the ones that Reini has translated look wrong on my PC. hmmm, copied and pasted from Mac. maybe my one fault.
; (Latin-1 or better iso-8859-15 for the euro symbol: reini urban)
;
(
;;; when translating this constant, substitue name of actual langauge for `English'
(is-this-your-native-language
  "Ist Deutsch Ihre Muttersprache?")

 (are-you-sure-you-want-to-switch-languages
  "Sind Sie sicher, dass Sie die Sprache des GUI wechseln wollen? (Wenn ja, muss DrScheme neugestartet werden)")
 
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
 (stop "Stop")   
 (&stop "&Stop") ;; for use in button and menu item labels, with short cut.

 ;;; bug report form
 (cancel-bug-report? "Fehlerreport abbrechen?")
 (are-you-sure-cancel-bug-report?
  "Sind Sie sicher, dass Sie diesen Fehlerreport *nicht* abschicken wollen?")
 (bug-report-form "Fehlerreport Formular")
 (bug-report-field-name "Name")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Zusammenfassung")
 (bug-report-field-severity "Schwierigkeitsgrad")
 (bug-report-field-class "Klasse")	; meaning what?
 (bug-report-field-priority "Priorit�t")
 (bug-report-field-description "Beschreibung")
 (bug-report-field-reproduce1 "Schritte, um das Problem zu")	; why in two pieces?
 (bug-report-field-reproduce2 "reproduzieren")	; oder nachzuvollziehen
 (bug-report-field-environment "Environment")	; Umgebung? perhaps best left in English (pbliE :)
 (bug-report-field-tools "Tools")		; Werkzeuge? (pbliE)
 (bug-report-field-docs-installed "Dokumente installiert")
 (bug-report-field-language "Sprache")		; DR Scheme language?
 (bug-report-field-teachpacks "Teachpacks")
 (bug-report-field-collections "Collections")
 (bug-report-field-human-language "Sprache")	;
 (bug-report-field-version "Version")
 (bug-report-synthesized-information "Ermittelte Information")  ;; dialog title
 (bug-report-show-synthesized-info "Ermittelte Informationen anzeigen")	; (an)zeigen
 (bug-report-submit "Abschicken")	
 (sending-bug-report "Fehlerreport wird gesendet")
 (error-sending-bug-report "Versenden des Fehlerreports fehlgeschlagen")
 (error-sending-bug-report-expln "Ein Fehler ist beim Absenden des Fehlerreports aufgetreten. Falls Ihre Internetverbindung sonst fehlerfrei funktioniert, besuchen Sie bitte:\n\n    http://bugs.plt-scheme.org/ \n\nund teilen Sie uns den Bug mit unserem Online-Formular mit. Wir bitten um Ihr Verst�ndnis.\n\nDie Fehlermeldung lautet:\n~a")
 (bug-report-sent "Fehlerreport erfolgreich versendet")
 (bug-report-sent-detail "Wir danken f�r Ihren Fehlerreport. Sie sollten eine Best�tigung innerhalb der n�chsten 30 Minuten per Email bekommen. Falls nicht, bitte sagen Sie uns hier bescheid: scheme@plt-scheme.org.")
 (illegal-bug-report "Ung�ltiger Fehlerreport")
 (pls-fill-in-field "Bitte auch das \"~a\" Feld ausf�llen!")
 (malformed-email-address "Ung�ltige Email Adresse")

 ;;; check syntax
 (check-syntax "Syntax pr�fen")
 (cs-italic "Kursiv")
 (cs-bold "Fett")
 (cs-underline "Unterstrichen")
 (cs-change-color "Farbe ver�ndern")
 (cs-tack/untack-arrow "Tack/Untack Pfeil") 	; i should learn what this means (islwtm :). mit Pfeil festnageln perhaps
 (cs-jump "Jump")	; islwtm
 (cs-error-message "Fehlermeldung")
 (cs-open-file "~a �ffnen")
 (cs-rename-var "~a umbenennen")
 (cs-rename-id "Identifier umbenennen")
 (cs-rename-var-to "~a wie folgt umbenennen:")
 (cs-name-duplication-error "Der neugew�hlte Name, ~s, ist in dieser Umgebung schon vorhanden.")	; best German word for scope? i could use a German copy of SICP! : (There is none. Reini)
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")	; islwtm
 (read-only "Schreibgesch�tzt")
 (read/write "Lese-/Schreibrechte")
 (auto-extend-selection "Auto-extend")	; islwtm
 (overwrite "�berschreiben")
 (running "Moment...")
 (not-running "Inaktiv")
 
 ;;; misc
 (welcome-to-something "Willkommen bei ~a")	; hmmm, not sure of the best preposition. islwtm. "in", "bei" oder "zu", depends - ru
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Willkommen bei DrScheme! (Version ~a, ~a)")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Willkommen bei DrScheme")
 (version/language "Version ~a, ~a")

 (goto-line "Gehe zu Zeile")	; no tilde a or the like? (in German it might be nice to place this inside the phrase)
	; German might prefer the object as a tilde parameter -- GmpOa~ :
 (goto-line-invalid-number
  "~a ist keine g�ltige Zeilennummer. (Erforderlich w�re eine ganze Zahl zwischen 1 und ~a)")
 (goto-position "Gehe zu Position")
 (no-full-name-since-not-saved
  "Die Datei ist noch unbenannt, weil sie noch nicht abgespeichert wurde.")
 (cannot-open-because-dne "Die Datei ~a kann nicht ge�ffnet werden, weil sie nicht existiert.")
 (interactions-out-of-sync
  "WARNUNG: Das Interaktionsfenster ist nicht mehr mit dem Definitionsfensters synchron. Bitte 'Ausf�hren' anklicken.")
 (file-is-not-saved "Die Datei \"~a\" ist nicht abgespeichert.")
 (save "Abspeichern")
 (please-choose-either "Bitte entweder \"~a\" oder \"~a\" w�hlen")
 (close-anyway "Trotzdem schliessen")

 (url "URL")
 (url: "URL:")
 (open-url... "URL �ffnen...")
 (open-url "URL �ffnen")
 (browse... "Browsen...")	; pbliE
 (bad-url "Ung�ltige URL")
 (bad-url:this "Ung�ltige URL: ~a")
 
 ;; Help Desk
 (search-results "Suchergebnisse")
 (help-desk "Help Desk")	; pbliE. perhaps "Hilfe" ?
 (help-desk-n "Help Desk ~a")
 (about-help-desk "�ber Help Desk")
 (help-desk-about-string
  "Der Help Desk enth�lt ausf�hrliche Informationen �ber PLT Software, einschliesslich DrScheme, MzScheme, und MrEd.\n\nVersion ~a\nCopyright (c) 1995-2001 PLT")
 (help-on-help "Hilfe zum Help Desk")
 (help-on-help-details "Um Hilfe dar�ber zu bekommen, wie man mit Help Desk umgeht, folgen Sie dem Link `How to use Help Desk' auf der Help Desk Startseite. (Um dahin zu kommen, dr�cken Sie den `Home' Knopf oben im Help Desk Fenster.)")
 (find-docs-for "Finde Dokumente:")	; GmpOa~
 (search "Suche")
 ; next 3 are popup menu choices at bottom of help desk window
 (search-for-keyword "nach Schl�sselwort")
 (search-for-keyword-or-index "nach Schl�sselwort oder Indexeintrag")
 (search-for-keyword-or-index-or-text "nach Schl�sselwort, Indexeintrag, oder Text")
 (exact-match "genauer Treffer")
 (containing-match "beinhaltet")
 (regexp-match "Regexp Treffer")
 ;(stop "Abbrechen")
 (feeling-lucky "Feeling Lucky")	;pbliE
 (nothing-found-for-search-key "Nichts gefunden f�r \"~a\".")
 (searching "wird gesucht...")
 (search-stopped "(Suche abgebrochen.)")
 (search-stopped-too-many-matches "(Suche abgebrochen -- zu viele Treffer gefunden.)")
 (reload "neu laden")
 (help "Hilfe")
 (searching... "es wird gesucht...")
 (nothing-found-for-empty-search "Nichts gefunden -- leere Suche")
 (nothing-found-for "Nichts f�r ~a gefunden")
 (and "und")

 ;; browser
 (rewind-in-browser-history "Zur�ck")
 (forward-in-browser-history "Vorw�rts")
 (home "Home")
 (browser "Browser")
 (cannot-display-url "Kann URL ~s nicht anzeigen: ~a")
 (install? "Installieren?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "Sie haben eine installierbares Paket eusgew�hlt.")
 (do-you-want-to-install-it? "Wollen Sie es installieren?")
 (paren-file-size "(Die Datei ist ~a Byte gro�)")
 (download-and-install "Download && Installation") ;; button label
 (download "Download") ;; button label
 (save-downloaded-file/size "Sichere Datei (~a byte) als") ;; label for get-file dialog
 (save-downloaded-file "Sichere Datei als")  ;; label for get-file dialog
 (downloading "Downladen") ;; dialog title
 (downloading-file... "Lade Datei...")
 (package-was-installed "Das Paket wurde erfolgreich installiert.")
 (download-was-saved "Die Datei wurde erfolgreich abgespeichert.")
 (getting-page "Getting Page") ;; dialog title
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "~a installieren oder zum Editieren �ffnen?")
 (install-plt-file/yes "Installieren")
 (install-plt-file/no "Editieren")	; aka redigieren
 
 ;;; about box
 (about-drscheme-frame-title "�ber DrScheme")
 (take-a-tour "Take a Tour!")	; pbliE
 (release-notes "Release Notes"); pbliE
 (parenthetical-last-version "(vorige Version ~a)")
 (parenthetical-last-language "(vorige Sprache ~a)")
 (parenthetical-last-version/language "(vorige Version ~a, Sprache ~a)")
 
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Diese Datei im Textformat abspeichern?")
 (save-in-drs-format "Diese Datei in einem DrScheme-spezifischen (kein Text) Format abspeichern?")
 (yes "Ja")
 (no "Nein")
 
 ;;; preferences
 (preferences "Voreinstellungen")
 (preferences-category "Kategorie")
 (saving-preferences "Voreinstellungen werden gesichert")
 (error-unmarshalling "Fehler beim Lesen der ~a Voreinstellung")	;islwtm, pbliE! :
 (error-saving-preferences "Fehler beim Abspeichern der Voreinstellungen: ~a")
 (error-reading-preferences "Fehler beim Lesen der Voreinstellungen")
 (expected-list-of-length2 "Eine Liste der L�nge 2 erwartet")
 (general-prefs-panel-label "Allgemein")
 (highlight-parens "zwischen entsprechenden Klammern Text hervorheben")
 (fixup-parens "Korrigiere Klammern")	; verb correct or adjective? smiley
 (flash-paren-match "bei abschliessender Klammer die Anfangsklammer ausleuchten")
 (auto-save-files "Dateien automatisch abspeichern")
 (map-delete-to-backspace "L�schtaste als Backspace")
 (verify-exit "Beenden best�tigen")
 (ask-before-changing-format "�nderung des Dateiformats vorher best�tigen")
 (wrap-words-in-editor-buffers "Zeilenumbruch im Editor")
 (show-status-line "Statuszeile anzeigen")
 (count-from-one "Zeilen- und Spaltenz�hler beginnen bei Eins") 
 (display-line-numbers "Zeilenzahl in Puffer z�hlen, nicht Zeichenanzahl")
 (enable-keybindings-in-menus "Aktiviere Tastatureinstellungen im Men�")	; islwtm
 (automatically-to-ps "automatisch in eine Postscriptdatei drucken")	; islwtm
 (use-mdi "MDI Windows benutzen") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Getrenntes Dialogfenster f�r Suche benutzen")
 (default-fonts "Vorgabe Schriftarten")
 
 ; should have entire alphabet
 (font-example-string "The quick brown fox jumped over the lazy dogs.") ; oops, have to look this up; there is a German analog

 (change-font-button-label "�ndern")
 (fonts "Schriftarten")

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Bitte eine neue \"~a\" Schriftart w�hlen")

 (font-size-slider-label "Gr��e")
 (restart-to-see-font-changes "Neustart um �nderungen der Schriftart zu sehen")

 (font-prefs-panel-title "Schriftart")
 (font-name "Schriftartname")
 (font-size "Schriftgr��e")
 (set-font "Set Font...")	; islwtm
 (select-font-name "Schriftartnamen ausw�hlen")
 (example-text "Beispieltext:")
 (general-ii "General II")	; islwtm
 (only-warn-once "Nur einmal warnen falls Ausf�hrungs- und Interaktionsfenster nicht synchron sind")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Einz�ge")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Eingabe ~a-�hnliches Schl�sselwort:")	; islwtm
 (x-keyword "~a Schl�sselwort")
 (x-like-keywords "~a-�hnliche Schl�sselworte")

 (expected-a-symbol "Symbol erwartet: ~a gefunden")
 (already-used-keyword "\"~a\" ist schon ein Schl�sselwort, das einen besonderen Linkseinzug ausl�st.")
 (add-keyword "Hinzuf�gen")
 (remove-keyword "Entfernen")
 
 ;;; find/replace
 (find-and-replace "Suchen und Ersetzen")
 (find "Suchen")
 (replace "Ersetzen")
 (use-separate-dialog-for-searching "Eigenes Dialogfenster f�r Suche benutzen")
 (replace&find-again "Ersetzen und nochmal suchen") ;;; need double & to get a single &
 (replace-to-end "Ab hier Alles ersetzen")
 (forward "Vorw�rts")
 (backward "R�ckw�rts")
 (hide "Verstecken")
 
 ;;;reverting a file
 (error-reverting "DrScheme - Fehler aufgetreten beim R�ckg�ngigmachen")	; islwtm
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

 (new-info "Neue Datei �ffnen")
 (new-menu-item "&Neu")

 (open-info "Datei von der Festplatte �ffnen")
 (open-menu-item "�&ffnen...")
 (open-recent-menu-item "�ffne andere") ; not yet -ru

 (revert-info "Inhalt entsprechend der Datei zur�cksetzen")
 (revert-menu-item "&Zur�cksetzen")

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
 
 (undo-info "Die letzte Aktion r�ckg�ngig machen")
 (undo-menu-item "&R�ckg�ngig machen")

 (redo-info "Die letzte r�ckg�ngig gemachte Aktion wieder machen")
 (redo-menu-item "&Wiederherstellen")

 (cut-info "Markierung l�schen und in der Zwischenablage speichern")
 (cut-menu-item "A&usschneiden")

 (copy-info "Markierung in die Zwischenablage kopieren")
 (copy-menu-item "&Kopieren")

 (paste-info "Den Inhalt der Zwischenablage einf�gen, ggf. anstelle des markierten Inhaltes.")
 (paste-menu-item "E&inf�gen")

 (clear-info "Den markierten Inhalt l�schen, ohne den Inhalt der Zwischenablage zu �ndern.")
 (clear-menu-item-others "Markierung l�s&chen")
 (clear-menu-item-windows "Markierung l�s&chen")

 (select-all-info "Das gesamte Dokument markieren")
 (select-all-menu-item "&Alles markieren")
 
 (find-info "Nach einer Zeichenfolge suchen")
 (find-menu-item "Suchen...")

 (find-again-info "Wiederholte Suche nach der gleichen Zeichenkette")
 (find-again-menu-item "Weitersuchen")
 (mfs-multi-file-search-menu-item "Suche in Dateien...") ; not yet -ru
 
 (replace-and-find-again-info "Ersetze den aktuellen Text und suche nach der selben Zeichenkette weiter")
 (replace-and-find-again-menu-item "Ersetzen und weitersuchen")

 (preferences-info "Voreinstellungen konfigurieren")
 (preferences-menu-item "Voreinstellungen...")

 (keybindings-info "Die aktiven Tastatureinstellungen (Verkn�pfung zwischen Tasten und Funktionen) anzeigen")
 (keybindings-menu-item "Tastatureinstellungen")
 (keybindings-frame-title "Tastatureinstellungen")
 (keybindings-sort-by-name "Nach Name sortieren")
 (keybindings-sort-by-key "Nach Schl�ssel sortieren")

 (insert-text-box-item "Text Box einf�gen")
 (insert-pb-box-item "Pasteboard Box einf�gen")
 (insert-image-item "Bild einf�gen...")
 (wrap-text-item "In Textfeld umbrechen")

 (windows-menu-label "&Fenster")
 (bring-frame-to-front "Bringe Fenster nach vorne")       ;;; title of dialog
 (bring-frame-to-front... "Bringe Fenster nach vorne...") ;;; corresponding title of menu item
 
 (show-menu-label "&Anzeigen")
 (show-overview "Zeige Speedbar")
 (hide-overview "Verstecke Speedbar")

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
  "Seitdem die Datei zuletzt gespeichert wurde, ist sie modifiziert worden. �nderungen �berschreiben?")
 (overwrite-file-button-label "�berschreiben") ;verwerfen? (discard changes?)
 
 (definitions-modified 
  "Der Definitionstext wurde im Dateisystem ge�ndert. Bitte speichern Sie ihn neu ab oder stellen Sie die alte Version wieder her.")
 (drscheme-internal-error "DrScheme Interner Fehler")
 
 ;;; tools
 (invalid-tool-spec "Die Tool-Spezifikation in der Collection von ~a's info.ss Datei ist ung�ltig. G�ltig sind ein String oder eine nicht-leere Liste von Strings. ~e")
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
 (save-definitions-as "Speichere Definitionen als...")
 (save-definitions "Speichere Definitionen")
 (print-definitions "Drucke Definitionen...")
 (about-drscheme "Info DrScheme")
 (save-other "Speichere Andere")
 (save-definitions-as-text "Speichere Definitionen als Text...")
 (save-interactions "Speichere Interaktionen") ; fixme: Interaktionen - besserer Begriff?
 (save-interactions-as "Speichere Interaktionen als...")
 (save-interactions-as-text "Speichere Interaktionen als Text...")
 (print-interactions "Drucke Interaktionen...")
 
 ;; edit-menu
 (split-menu-item-label "&Teilen")
 (collapse-menu-item-label "&Verbinden") ; fixme: collapse (wieder zusammenf�gen?)

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
 (reindent-menu-item-label "&Neu einr�cken") ; fixme: besserer Begriff	(ist besser als "links einzug neu berechnen", oder?. ja -ru)
 (reindent-all-menu-item-label "&Alles neu einr�cken") ; fixme: besserer Begriff
 (comment-out-menu-item-label "&Auskommentieren")
 (uncomment-menu-item-label "Auskommentieren r�ckg�ngig")	;  (uncomment, not strip comments, i assume)

 ;; buttons
 (execute-button-label "Ausf�hren") 
 (save-button-label "Sichern")
 (break-button-label "Abbruch")
 
 ;; search help desk popup menu
 (search-help-desk-for "Suche im Help Desk nach \"~a\"")
 (exact-lucky-search-help-desk-for "Exakte Suche im Help Desk nach \"~a\"")
 
 ;; collapsing and expanding sexpressions poup menu item label
 (collapse-sexp "Schrumpfen") ; bl�der Ausdruck -ru
 (expand-sexp "Erweitern")

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
 (clear-all-teachpacks-menu-item-label "L�sche alle Teachpacks")
 (teachpack-not-only-one-import "Die TeachPack unit/sig in ~a darf nur exakt eine Importdefinition besitzen.")
 (drscheme-teachpack-message-title "DrScheme Teachpack")
 (already-added-teachpack "~a Teachpack schon vorhanden")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Bitte w�hlen Sie eine Scheme Sprachversion. Studenten der meisten Einf�hrungskurse sollten die Vorgabesprache verwenden.")
 (language-dialog-title "Sprache konfigurieren")
 (case-sensitive-label "Gro�-/Kleinschreibung beachten")
 (output-style-label "Ausgabestil")
 (constructor-printing-style "Constructor") ; these are pretty special, I'd leave them
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write") ; islwtm
 (sharing-printing-label "Zeige Sharing in Werten") ; islwtm
 (use-pretty-printer-label "F�ge Zeilenumbr�che in ausgegebenen Werten ein") ; Zeilenumbruchzeichen in Druckwerten einf�gen ??
 (input-syntax "Eingabe Syntax")
 (output-syntax "Ausgabe Syntax")
 (whole/fractional-exact-numbers-label "Zahlen als Bruchzahlen ausgeben")	; really "print"? or "output"?
 (booleans-as-true/false-label "Bool'sche Werte als Wahr und Falsch ausgeben")	; in contrast to #T, #F? are true and false translated? ; 
 (show-details-button-label "Details anzeigen")
 (hide-details-button-label "Details verstecken")
 (choose-language-menu-item-label "Sprache ausw�hlen...")
 (revert-to-language-defaults "Vorgabesprache wieder aktivieren")

 ;;; languages
 (beginning-student "Anf�nger/in")
 (beginning-student/abbrev "Anf�nger/in mit List-K�rzungen")
 (intermediate-student "Mittelstufe")
 (advanced-student "Fortgeschrittene/r")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "R5RS-like")          ; islwtm
 (mred-lang-name "Grafische Oberfl�che ohne Debugger (MrEd)")
 (mzscheme-lang-name "Textoberfl�che ohne Debugger (MzScheme)")
 (r5rs-lang-name "R5RS ohne Debugger")  ; islwtm
 (unknown-debug-frame "[unbekannt]")
 
 (bad-module-language-specs
  "The drscheme-language-position and drscheme-language-modules specifications aren't correct. Expected (listof (cons string (listof string))) and (listof (listof string)) respectively, where the lengths drscheme-language-position and drscheme-language-module lists are the same. Got ~e and ~e")

 ;;; debug language
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "~a's Interaktionen") ;; filled with a filename
 (stack-frame-in-current-interactions "Interaktionen")
 (stack-frame-in-current-definitions "Definitionen")
 (mzscheme-w/debug "Text (MzScheme)")
 (mred-w/debug "Graphisch (MrEd)")
 
 ;;; repl stuff
 (evaluation-terminated "Ausf�hrung (eval) beendet")	; pretty special. perhaps we should compare with German SICP?
 (evaluation-terminated-explanation
  "Der Evaluation Thread wird nicht l�nger ausgef�hrt, also kann keine Evaluation bis zur n�chsten Ausf�hrung stattfinden.")
 (last-stack-frame "zeige den letzten Stackbereich")
 (last-stack-frames "zeige die letzten ~a Stackbereiche")
 (next-stack-frames "zeige die n�chsten ~a Stackbereiche")
 
 ;;; welcoming message in repl
 (language "Sprache")
 (custom "angepasst")	; adjective (might need to decline in German?)
 (teachpack "Teachpack")
 (welcome-to "Willkommen bei")
 (version "Version")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Ausf�hrung (eval) beenden?")
 (just-break "Nur Abbrechen")	; Break has a special meaning? Interrupt?
 (kill "Beenden")
 (kill? "Beenden?")

 ;;; version checker
 (vc-update-check "Pr�fe Updates")
 (vc-check-prompt "Pr�fe PLT Software Updates �bers Internet?")
 (vc-please-wait "Bitte warten")
 (vc-connecting-version-server "Baue Verbindung zu PLT Versions Server auf")
 (vc-network-timeout "Netzwerk Timeout") 
 (vc-cannot-connect  "Kann keine Verbindung zum PLT Versions Server aufbauen")
 (vc-network-failure "Netzwerk Fehler")
 (vc-old-binaries "Installierte Binaries f�r DrScheme (oder MzScheme) sind �berholt")
 (vc-binary-information-format "Installierte Bin�r Version: ~a (iteration ~a)")
 (vc-update-format "~a v.~a (iteration ~a) mu� auf v.~a (iteration ~a) upgedatet werden")
 (vc-binary-name "Bin�r")
 (vc-updates-available "Updates sind erh�ltich bei")
 (vc-latest-binary-information-format "Letzte freigegebene Version: ~a (iteration ~a)")
 (vc-update-dialog-title "PLT Update Status")
 (vc-need-update-string "Eine oder mehrere installierte PLT Software Packete m�ssen upgedatet werden")
 (vc-no-update-string "installierte PLT Software Packete sind aktuell")
 
 ;; large semi colon letters
 (insert-large-letters... "Einf�ge Text als ASCII Grafik...")
 (large-semicolon-letters "Gro�e Semikolon Buchstaben")
 (text-to-insert "Text zum Einf�gen")

;;missing translations

 (accept-and-exit "Ja und Beende")
 (accept-and-quit "Ja und Beende")

   (advanced-one-line-summary "Intermediate plus lambda und mutation")
   (are-you-sure-revert "Sind Sie sicher, da� sie die letzte Version der Datei neu laden wollen und damit alle ihre �nderungen im Speicher verwerfen? Diese �nderung kann nicht r�ckg�ngig gemacht werden.")
   (are-you-sure-revert-title "Neu laden?")
   (beginning-one-line-summary "define, cond, structs, Konstanten und Primitive")
   (beginning/abbrev-one-line-summary "Anf�nger, mit list style printing in der REPL")
   (create-executable-menu-item-label "Erzeuge EXE...")
   (create-executable-title "Erzeuge ausf�hrbare Datei")
   (debugging "Debuggen")
   (definitions-not-saved "Das Definitionsfenster wurde nicht gesichert. Die ausf�hrbare Datei wird die letzte gesicherte Version des Definitionsfensters verwenden. Weiter?")
   (dock "Dock")
   (drscheme-homepage "DrScheme")
   (dynamic-properties "Dynamische Eigenschaften")
   (error-finding-docs "Kann Dokumentation nicht finden.\n\n~a")
   (error-saving "Sicherungsfehler")
   (error-saving-file/name "Fehler beim Sichern von ~a.")
   (full-language "Vollversion")
   (help-menu-check-for-updates "Pr�fe Updates...")
   (how-to-use-scheme "How to Use Scheme")
   (htdp-full-one-line-summary "Fortgeschritten, plus PLT Erweiterungen und GUI Bibliothek")
   (http-proxy "HTTP Proxy")
   (intermediate-one-line-summary "Anf�nger plus lexikalischer Umgebung")
   (intermediate-student/lambda "Intermediate Student mit lambda")
   (intermediate/lambda-one-line-summary "Intermediate plus Funktionen h�herer Ordnung")
   (language-docs-button-label "Sprachdokumentation")
   (mfs-case-sensitive-label "Gro�/Kleinschreibung exakt")
   (mfs-configure-search "Konfiguriere Suche")
   (mfs-dir "Verzeichnis")
   (mfs-drscheme-multi-file-search "DrScheme - Multidatei Suche")
   (mfs-files-section "Dateien")
   (mfs-no-matches-found "Keine Treffer gefunden.")
   (mfs-not-a-dir "\"~a\" ist kein Verzeichnis")
   (mfs-open-file "�ffne Datei")
   (mfs-recur-over-subdirectories "Auch Unterverzeichnisse")
   (mfs-regexp-filename-filter "Regex Dateiname Filter")
   (mfs-regexp-match/no-graphics "Regul�rer Ausdruck (nur RAW Textdateien)")
   (mfs-search-interrupted "Suche abgebrochen.")
   (mfs-search-section "Suche")
   (mfs-search-string "Suche Zeichenkette")
   (mfs-searching... "Suche...")
   (mfs-stop-search "Beende Suche")
   (mfs-string-match/graphics "Zeichensuche (auch Bin�rdateien)")
   (module-language-one-line-summary "Sprachversion mit Modulen als einzige Erweiterung")
   (mred-one-line-summary "PLT Scheme plus die GUI Bibliothek")
   (must-save-before-executable "Sie m�ssen Ihr Programm zuerst abspeichern, bevor Sie eine ausf�hrbare Datei erzeugen k�nnen.")
   (mzscheme-one-line-summary "PLT Scheme ohne GUI Bibliothek")
   (open-recent-info "Liste der zuletzt ge�ffneten Dateien")
   (pls-fill-in-either-description-or-reproduce "Bitte f�llen Sie das Beschreibungsfeld aus oder die Schritte zur Nachvollziehbarkeit aus.")
   (plt-homepage "PLT")
   (pref-lock-not-gone "Voreinstellungs-Sperrdatei:\n\n   ~a\n\nverhundert das Speichern der Voreinstellungen. Versichern Sie sich, da� kein PLT Programm mehr rennt und l�schen Sie ggf. diese Sperrdatei.")
   (proxy-bad-host "Ung�ltiger Proxy Host")
   (proxy-direct-connection "Direkte Internetverbindung")
   (proxy-host "Host")
   (proxy-port "Port")
   (proxy-use-proxy "Verwende Proxy:")
   (r5rs-one-line-summary "Pures R5RS, ohne Zusatzschm�h's")
   (save-an-executable "Sichere EXE")
   (still-locked-exit-anyway? "Die Voreinstellungen konnten nicht gesichert werden. Trotzdem beenden?")
   (teachscheme!-homepage "TeachScheme!")
   (undock "Undock")
   (use-mred-binary? "Verwende mred als Vorlage f�r die ausf�hrbare Datei?\n\nWenn ja, kann Ihr Programm die Bibliothek (lib \"mred.ss\" \"mred\") verwenden. Wenn nein, wird DrScheme mzscheme als Vorlage f�r die ausf�hrbare Datei verwenden und Sie k�nnen diese Bibiliothek nicht verwenden.\n\nIm Zweifel antworten Sie mit Ja.")
   (waiting-for-pref-lock "Warte auf die Voreinstellungs-Sperrdatei...")
   (web-materials "Verwandte Internet Seiten")

   (vc-current-format "~a v.~a (Serie ~a) ist aktuell")
   (vc-details-format "~a~nDetails:~n~a")
   (vc-details-text "Details:~n")
   (vc-error-format "Fehler: ~a")
   (inline-saved-program-in-executable? "D�rfen wir Ihr Programm in die ausf�hrbare Datei einbauen?\n\nFalls ja, k�nnen Sie die ausf�hrbare Datei auf einen anderen ~a Computer kopieren, aber die Datei wird sehr gross sein.\n\nFalls nein, dann k�nnen Sie die ausf�hrbare Datei nicht auf einen anderen ~a Computer kopieren und es wird relative klein sein. Dar�ber hinaus wird die ausf�hrbare Datei immer die neueste Version des Programms benutzen.")


 )
