
(

 ;; bad translation, but useful for testing this translation to have *something* here.
 (is-this-your-native-language
  "German")
 
 ;; general purpose
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Abbruch")
 (untitled "Ohne Titel")
 
 ;; misc
 (definitions-modified 
  "Der Definitionstext wurde im Dateisystem geändert. Bitte speichern Sie ihn neu ab oder stellen Sie die alte Version wieder her.")
 (drscheme-internal-error "DrScheme Interner Fehler")

 ;; tools
 (invalid-tool-spec "Die tool Spezifikation in der Collection von ~a's info.ss Datei ist ungültig. Gültig sind ein String oder eine nicht-leere Liste von Strings. ~e")
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
 
 ;; show menu
 (hide-definitions-menu-item-label "&Definitionen verstecken")
 (definitions-menu-item-help-string "Definitionsfenster anzeigen/verstecken")
 (show-interactions-menu-item-label "Anzeigen &Interaktionen")
 (interactions-menu-item-help-string "Interaktionsfenster anzeigen/verstecken")

 ;; file menu
 (definitions "Definitionen")
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
 (execute-menu-item-label "Ausführen")
 (execute-menu-item-help-string "Programm im Definitionsfenster neu starten")
 (break-menu-item-label "Abbruch")
 (break-menu-item-help-string "Abbruch des aktuellen Prozesses")
 (kill-menu-item-label "Prozess beenden")
 (kill-menu-item-help-string "Beende den aktuellen Prozess")
 (reindent-menu-item-label "&Neu einrücken") ; fixme: besserer Begriff
 (reindent-all-menu-item-label "&Alles neu einrücken") ; fixme: besserer Begriff
 (comment-out-menu-item-label "&Auskommentieren")
 (uncomment-menu-item-label "Kommentare &entfernen")

 ;; launcher
 (create-launcher-title "Erzeuge externes Startprogramm")
 (must-save-before-launcher "Sie müssen zuerrst Ihr Programm abspeichern, befor Sie das Startprogramm erzeugen können.")
 (save-a-launcher "Sichere ein externes Startprogramm")

 ;; buttons
 (execute-button-label "Ausführen") 
 (save-button-label "Sichern")
 (break-button-label "Abbruch")
 
 ;; search help desk popup menu
 (search-help-desk-for "Suche im Help Desk nach \"~a\"")
 (exact-lucky-search-help-desk-for "Exakte Suche im Help Desk nach \"~a\"")
 
 ;; fraction dialog
 (enter-fraction "Eingabe Bruchzahl")
 (whole-part "Ganzzahl")
 (numerator "Zähler")
 (denominator "Nenner")
 (invalid-number "Ungültige Zahl: Eine exakte reelle Zahl (Bruch, Ganzzahl oder Reell) erforderlich.")
 (insert-fraction-menu-item-label "Bruchzahl einfügen...")
 
 ;; TeachPack messages
 (select-a-teachpack "Wähle ein TeachPack")
 (clear-teachpack "Lösche ~a TeachPack")
 (teachpack-error-label "DrScheme - TeachPack Fehler")
 (teachpack-dne/cant-read "Die TeachPack Datei ~a existiert nicht oder kann nicht gelesen werden.")
 (teachpack-didnt-load "Die TeachPack Datei ~a konnte nicht geladen werden.")
 (teachpack-error-invoke "Die TeachPack Datei ~a konnte nicht ausgeführt werden.")
 (add-teachpack-menu-item-label "Füge Teachpack hinzu...")
 (clear-all-teachpacks-menu-item-label "Lösche alle Teachpack's")
 (teachpack-not-only-one-import "Die TeachPack unit/sig in ~a darf nur exakt eine Importdefinition besitzen.")
 
 ;; Language dialog
 (language-dialog-title "Konfiguriere Sprache")
 (case-sensitive-label "Groß/Kleinschreibung")
 (output-style-label "Ausgabestil")
 (constructor-printing-style "Konstruktor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "schreibe")
 (sharing-printing-label "Zeige gemeinsame Daten")
 (use-pretty-printer-label "Füge Zeilenumbrüche in den ausgegebenen Werten ein")
 (input-syntax "Eingabe Syntax")
 (output-syntax "Ausgabe Syntax")
 (whole/fractional-exact-numbers-label "Ausgabe Zahlen als Bruch")
 (booleans-as-true/false-label "Ausgabe boolscher Werte als true und false")
 (show-details-button-label "Details zeigen")
 (hide-details-button-label "Details ausblenden")
 (choose-language-menu-item-label "Wähle Sprache...")
 (revert-to-language-defaults "Wiederherstellen der Sprachvorgaben")
 
 ;; repl stuff
 (evaluation-terminated "Ausführung abbgebrochen")
 (evaluation-terminated-explanation
  "Der Ausführungsprozess wurde abgebrochen. Keine Ausführung ist möglich bis zum nächsten Programmstart.") ; checkme
 (last-stack-frame "Zeige den letzten Stackframe")
 (more-stack-frames "Zeige ~a von ~a Stackframes") ; checkme
 
 ;; welcoming message in repl
 (language "Sprache")
 (custom "benutzerabhängig") ; checkme: "custom"
 (teachpack "Teachpack")
 (welcome-to "Willkommen zu")
 (version "version")
 
 ;; kill evaluation dialog
 (kill-evaluation? "Wollen Sie den Prozess abbrechen?")
 (just-break "Nur abbrechen")
 (kill "Beenden")
 (kill? "Beenden?")
 )
