(module german-string-constants "string-constant-lang.ss"

 (is-this-your-native-language
  "Ist Deutsch Ihre Muttersprache?")

 (are-you-sure-you-want-to-switch-languages
  "Dies wird die Sprache der DrScheme-Benutzeroberfl�che �ndern und erfordert einen Neustart von DrScheme.  Sind Sie sicher?")

 (interact-with-drscheme-in-language "Deutsche Benutzeroberfl�che f�r DrScheme")

 (accept-and-quit "In Ordnung - Beenden")
 (accept-and-exit "In Ordnung - Beenden")
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Absagen")
 (abort "Abbrechen")
 (untitled "Namenlos")
 (untitled-n "Namenlos ~a")
 (warning "Warnung")
 (error "Fehler")
 (close "Schlie�en") ;; as in, close an open window
 (stop "Stop")   
 (&stop "&Stop") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "Sind Sie sicher, dass Sie ~a l�schen wollen?") ;; ~a is a filename or directory name
 (ignore "Ignorieren")
 (revert "�nderungen r�ckg�ngig machen")

 (dont-ask-again "Nicht wieder nachfragen (immer so wie jetzt)")

 (web-materials "Verwandte Web-Seiten")
 (tool-web-sites "Web-Seiten mit Tools")
 (drscheme-homepage "DrScheme")
 (plt-homepage "PLT")
 (how-to-use-scheme "How to Use Scheme")
 (teachscheme!-homepage "TeachScheme!")

 ;;; bug report form
 (cancel-bug-report? "Bug-Report verwerfen?")
 (are-you-sure-cancel-bug-report?
  "Sind Sie sicher, dass Sie diesen Bug-Report verwerfen wollen?")
 (bug-report-form "Formular f�r Bug-Report")
 (bug-report-field-name "Name")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Zusammenfassung")
 (bug-report-field-severity "Wie schlimm?")
 (bug-report-field-class "Art")
 (bug-report-field-priority "Priorit�t")
 (bug-report-field-description "Beschreibung")
 (bug-report-field-reproduce1 "Schritte, um das Problem zu")
 (bug-report-field-reproduce2 "reproduzieren")
 (bug-report-field-environment "Umgebung")
 (bug-report-field-tools "Tools")
 (bug-report-field-docs-installed "Installierte Dokumentation")
 (bug-report-field-language "Sprachebene")
 (bug-report-field-teachpacks "Teachpacks")
 (bug-report-field-collections "Kollektionen")
 (bug-report-field-human-language "Interaktionssprache")	;
 (bug-report-field-version "Version")
 (bug-report-synthesized-information "Generierte Information")  ;; dialog title
 (bug-report-show-synthesized-info "Generierte Informationen anzeigen")	; (an)zeigen
 (bug-report-submit "Abschicken")	
 (bug-report-submit-menu-item "Bug-Report abschicken") ;; in Help Menu (drs & help desk)
 (sending-bug-report "Bug-Report abschicken")
 (error-sending-bug-report "Versendung des Bug-Reports fehlgeschlagen")
 (error-sending-bug-report-expln "Ein Fehler ist beim Versenden des Bug-Reports aufgetreten. Falls Ihre Internet-Verbindung eigentlich funktioniert, besuchen Sie bitte:\n\n    http://bugs.plt-scheme.org/ \n\nund teilen Sie uns den Bug mit unserem Online-Formular mit. Wir bitten um Ihr Verst�ndnis.\n\nDie Fehlermeldung lautet:\n~a")
 (bug-report-sent "Bug-Report erfolgreich verschickt")
 (bug-report-sent-detail "Wir danken f�r Ihren Bug-Report. Sie sollten innerhalb der n�chsten 30 Minuten eine Best�tigung per Email bekommen. Falls nicht, schicken Sie eine Email an folgende Adresse: scheme@plt-scheme.org.")
 (illegal-bug-report "Ung�ltiger Bug-Report")
 (pls-fill-in-field "Bitte auch das \"~a\" Feld ausf�llen")
 (malformed-email-address "Ung�ltige Email-Adresse")
 (pls-fill-in-either-description-or-reproduce "Bitte f�llen Sie entweder das Feld \"Beschreibung\" oder das Feld \"Schritte, um das Problem zu reproduzieren\" aus.")

 ;;; check syntax
 (check-syntax "Syntaxpr�fung")
 (cs-italic "Kursiv")
 (cs-bold "Fett")
 (cs-underline "Unterstrichen")
 (cs-change-color "Farbe �ndern")
 (cs-tack/untack-arrow "Pfeil befestigen/l�sen")
 (cs-jump-to-next-bound-occurrence "Zum n�chsten gebundenen Vorkommen springen")
 (cs-jump-to-binding "Zu bindendem Vorkommen springen")
 (cs-jump-to-definition "Zu Definition springen")
 (cs-error-message "Fehlermeldung")
 (cs-open-file "~a �ffnen")
 (cs-rename-var "~a umbenennen")
 (cs-rename-id "Bezeichner umbenennen")
 (cs-rename-var-to "~a umbenennen nach:")
 (cs-name-duplication-error "Der neugew�hlte Name, ~s, ist hier schon gebunden.")
 (cs-rename-anyway "Trotzdem umbenennen")
 (cs-status-init "Syntaxpr�fung: Umgebung f�r den User-Code initialisieren")
 (cs-status-coloring-program "Syntaxpr�fung: Ausdruck einf�rben")
 (cs-status-eval-compile-time "Syntaxpr�fung: Compile-Time-Code ausf�hren")
 (cs-status-expanding-expression "Syntaxpr�fung: Ausdruck expandieren")
 (cs-mouse-over-import "Bindung ~s importiert aus ~s")

 (cs-lexical-variable "lexikalische Variable")
 (cs-lexical-syntax "lexikalische Syntax")
 (cs-imported-variable "importierte Variable")
 (cs-imported-syntax "importierte Syntax")

 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
 (read-only "Schreibgesch�tzt")
 (read/write "Lesen/Schreiben")
 (auto-extend-selection "Automatisch erweitern")
 (overwrite "�berschreiben")
 (running "Programm l�uft")
 (not-running "Programm inaktiv")
 
 ;;; misc
 (welcome-to-something "Willkommen bei ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Willkommen bei DrScheme! (Version ~a, ~a)")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Willkommen bei DrScheme")
 (version/language "Version ~a, ~a")

 (goto-line "Springe zu Zeile")
 (goto-line-invalid-number
  "~a ist keine g�ltige Zeilennummer. Es muss eine ganze Zahl zwischen 1 und ~a sein.")
 (goto-position "Springe zu Position")
 (no-full-name-since-not-saved
  "Die Datei hat noch keinen Namen, weil sie noch nicht abgespeichert wurde.")
 (cannot-open-because-dne "Die Datei ~a kann nicht ge�ffnet werden, weil sie nicht existiert.")
 (interactions-out-of-sync
  "WARNUNG: Das Interaktionsfenster ist nicht mehr mit dem Definitionsfensters synchron. Bitte 'Ausf�hren' anklicken.")
 (file-is-not-saved "Die Datei \"~a\" ist nicht gespeichert.")
 (save "Speichern")
 (please-choose-either "Bitte entweder \"~a\" oder \"~a\" w�hlen")
 (close-anyway "Trotzdem schlie�en")
 (clear-anyway "Trotzdem l�schen")

 (log-definitions-and-interactions "Definitionen and Interaktionen protokollieren...")
 (stop-logging "Protokoll stoppen")
 (please-choose-a-log-directory "Bitte w�hlen Sie ein Verzeichnis f?r das Protokoll")
 (logging-to "Protokoll: ")
 (erase-log-directory-contents "Inhalt von Protokoll-Verzeichnisses ~a l�schen?")
 (error-erasing-log-directory "Fehler beim L�schen des Protokoll-Verzeichnisses.\n\n~a\n")

 ;; modes
 (mode-submenu-label "Modi")
 (scheme-mode "Scheme-Modus")
 (text-mode "Text-Modus")

 (scheme-mode-color-symbol "Symbol")
 (scheme-mode-color-keyword "Schl�sselwort")
 (scheme-mode-color-comment "Kommentar")
 (scheme-mode-color-string "Zeichenkette")
 (scheme-mode-color-constant "Literal")
 (scheme-mode-color-parenthesis "Klammer")
 (scheme-mode-color-error "Fehler")
 (scheme-mode-color-other "Sonstiges")
 (syntax-coloring-choose-color "W�hlen Sie eine Farbe f�r ~a")
 (preferences-colors "Farben")

 (url "URL")
 (url: "URL:")
 (open-url... "URL �ffnen...")
 (open-url "URL �ffnen")
 (browse... "Brausen...")
 (bad-url "Ung�ltige URL")
 (bad-url:this "Ung�ltige URL: ~a")
 
 ;; Help Desk
 (help "Hilfe")
 (help-desk "Hilfezentrum")
 (plt:hd:search-results "Suchergebnisse")
 (plt:hd:search "Suchen")
 (plt:hd:search-for "Suchen nach")
 (plt:hd:lucky "Gl�ck gehabt!")
 (plt:hd:feeling-lucky "Auf gut Gl�ck")
 (plt:hd:stop "Stop")   
 (plt:hd:options "Optionen") 
 (plt:hd:configure "Konfiguration")
 (plt:hd:home "Hilfezentrum-Homepage") 
 (plt:hd:show-manuals "Handb�cher anzeigen") 
 (plt:hd:send-bug-report "Bug-Report")
 (plt:hd:query-bug-reports "Bug-Reports abfragen")
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "Stichworteintrag")
 (plt:hd:search-for-keyword-or-index "Stichwort- oder Index-Eintrag")
 (plt:hd:search-for-keyword-or-index-or-text "Stichwort- oder Index-Eintrag, oder Text")
 (plt:hd:exact-match "Exakte Treffer")
 (plt:hd:containing-match "Teilwort")
 (plt:hd:regexp-match "�ber regul�ren Ausdruck")
 (plt:hd:find-docs-for "Finde Dokumentation zu:")
 (plt:hd:nothing-found-for-search-key "Nichts zu \"~a\" gefunden.")
 (plt:hd:searching "Suche...")
 (plt:hd:search-stopped "[Suche gestopt.]")
 (plt:hd:search-stopped-too-many-matches "[Suche abgebrochen: zu viele Treffer]")
 (plt:hd:nothing-found-for "Nichts zu ~a gefunden")
 (plt:hd:error-finding-docs "Konnte die Dokumentation nicht finden.\n\n~a")
 (plt:hd:and "und")
 (plt:hd:refresh "aktualisieren")
 (plt:hd:refresh-all-manuals "alle Handb�cher aktualisieren")
 (plt:hd:manual-installed-date "(installiert ~a)")
 ; Help Desk configuration
 (plt:hd:configuration "Konfiguration PLT-Hilfezentrum")
 (plt:hd:no-frames "Ohne Frames")
 (plt:hd:use-frames "Mit Frames")
 (plt:hd:use-html-frames "Mit HTML-frames")
 (plt:hd:search-pane-options "Optionen Such-Panel")
 (plt:hd:height "H�he")
 (plt:hd:bg-color "Hintergrundfarbe")
 (plt:hd:pixels "Pixel")
 (plt:hd:text-color "Farbe Text")
 (plt:hd:link-color "Farbe Links")
 (plt:hd:text-sample "Text im Such-Panel erscheint in dieser Farbe")
 (plt:hd:link-sample "Links im Such-Panel erscheinen in dieser Farbe")
 (plt:hd:save-changes "�nderungen sichern")
 (plt:hd:reset "Zur�cksetzen")
 (plt:hd:defaults "Werkseinstellungen")
 (plt:hd:javascript-note
    "Ihre Selektionen werden hier erscheinen, falls Sie Javascript eingeschaltet haben und einen aktuellen standardkompatiblen Brauser benutzen.")
 ;; refreshing manuals
 (plt:hd:refresh-downloading "~a herunterladen")
 (plt:hd:refresh-installing "~a installieren")
 (plt:hd:refresh-progress "Fortschritt beim Herunterladen von PLT-Handbuch")
 (plt:hd:refresh-done "Aktualisierung der Handb�cher aus CVS abgeschlossen")
 (plt:hd:refresh-installation-log "Installations-Protokoll")
 (plt:hd:refresh-stopped "PLT-Handbuch-Aktualisierung gestoppt")
 (plt:hd:refreshing-manuals "Handb�cher aktualisieren")
 (plt:hd:refresh-downloading... "~a herunterladen...")
 (plt:hd:refresh-deleting... "Alte Version von ~a l�schen...")
 (plt:hd:refresh-installing... "Neue Version von ~a installieren...")
 (plt:hd:refresh-clearing-indicies "Gecachte Indizes l�schen")
 (plt:hd:refreshing-manuals-finished "Fertig.")
 (plt:hd:about-help-desk "�ber das Hilfezentrum")
 (plt:hd:help-desk-about-string
  "Das Hilfezentrum ist die prim�re Quelle f�r Information �ber die PLT-Software,insbesondere DrScheme, MzScheme und MrEd.\n\nVersion ~a\nCopyright (c) 1995-2003 PLT")
 (plt:hd:help-on-help "Hilfe zur Hilfe")
 (plt:hd:help-on-help-details "Hilfe zum Hilfezentrum finden Sie auf der Hilfezentrum-Hompage unter 'Help Desk'. (Um auf diese Homepage zu gelangen, dr�cken Sie den 'Home'-Knopf oben im Hilfezentrum.)")
  (reload "Aktualisieren") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "Sie haben einen Link selektiert, der ins Web zeigt. Wollen Sie die Seite im Hilfe-Brauser oder im externen Brauser anzeigen?")
  (plt:hd:homebrew-browser "Hilfe-Brauser") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Externer Brauser") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "Externe URLs im Hilfe-Brauser")
  (plt:hd:use-homebrew-browser "Den Hilfe-Brauser f�r externe URLs benutzen")
  (plt:hd:new-help-desk "Neues Hilfezentrum")
  (plt:hd:teaching-manuals "Handb�cher f�r Lehrende und Lernende")
  (plt:hd:professional-manuals "Handb�cher f�r Anwender")
  (plt:hd:all-manuals "Alle Handb�cher")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Suchreihenfolge Handbuch")


 ;; Help desk htty proxy
 (http-proxy "HTTP-Proxy")
 (proxy-direct-connection "Direkte Verbindung")
 (proxy-use-proxy "Proxy benutzen:")
 (proxy-host "Name")
 (proxy-port "Port")
 (proxy-bad-host "Unzul�ssiger Proxy")

 ;; browser
 (rewind-in-browser-history "Zur�ck")
 (forward-in-browser-history "Vor")
 (home "Home")
 (browser "Brauser")
 (external-browser-choice-title "Externer Brauser")
 (browser-command-line-label "Kommandzeile:")
 (choose-browser "Brauser ausw�hlen")
 (no-browser "Sp�ter")
 (use-internal-browser-for-help "Hilfe mit internem PLT-Brauser lesen")
 (use-external-browser-for-help "Hilfe mit externam Brauser lesen")
 (browser-cmdline-expl-line-1 "(Kommandozeile konstruiert durch Aneinanderh�ngen von Vor-Text, URL,")
 (browser-cmdline-expl-line-2 " und Nach-Text, ohne zus�tzliche Leerzeichen dazwischen.")
 (cannot-display-url "Kann URL ~s nicht anzeigen: ~a")
 (install? "Installieren?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "Sie haben eine installierbares Paket angew�hlt.")
 (do-you-want-to-install-it? "Wollen Sie es installieren?")
 (paren-file-size "(Die Datei hat ~a Bytes)")
 (download-and-install "Herunterladen && installieren") ;; button label
 (download "Herunterladen") ;; button label
 (save-downloaded-file/size "Datei (~a Bytes) speichern als") ;; label for get-file dialog
 (save-downloaded-file "Datei speichern als")  ;; label for get-file dialog
 (downloading "Herunterladen") ;; dialog title
 (downloading-file... "Datei herunterladen...")
 (package-was-installed "Das Paket wurde erfolgreich installiert.")
 (download-was-saved "Die Datei wurde erfolgreich gespeichert.")
 (getting-page "Seite laden") ;; dialog title

 (install-plt-file-menu-item... ".plt-Datei installieren...")
 (install-plt-file-dialog-title ".plt-Datei installieren")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "Datei")
 (install-plt-filename "Dateiname:")
 (install-plt-url "URL:")
  
 (install-plt-file "~a installieren oder editieren?")
 (install-plt-file/yes "Installieren")
 (install-plt-file/no "Editieren")
 
 (plt-installer-progress-window-title "Fortschritt Installation")
 (plt-installer-abort-installation "Installation abbrechen")
 (plt-installer-aborted "Abgebrochen.")
  
 ;;; about box
 (about-drscheme-frame-title "�ber DrScheme")
 (take-a-tour "Nehmen Sie die F�hrung!")
 (release-notes "Release-Notes")
 (parenthetical-last-version "(vorige Version ~a)")
 (parenthetical-last-language "(vorige Sprache ~a)")
 (parenthetical-last-version/language "(vorige Version ~a, Sprache ~a)")
 
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Diese Datei als Text speichern?")
 (save-in-drs-format "Diese Datei im DrScheme-Format (kein Text) speichern?")
 (yes "Ja")
 (no "Nein")
 
 ;;; preferences
 (preferences "Einstellungen")
 (saving-preferences "Einstellungen werden gesichert")
 (error-unmarshalling "Fehler beim Lesen der Einstellung f�r ~a")
 (error-saving-preferences "Fehler beim Speichern der Einstellungen f�r ~a")
 (error-reading-preferences "Fehler beim Lesen der Einstellungen")
 (expected-list-of-length2 "Eine Liste mit zwei Elementen erwartet")
 (scheme-prefs-panel-label "Scheme")
 (warnings-prefs-panel-label "Warnmeldungen")
 (editor-prefs-panel-label "Editieren")
 (general-prefs-panel-label "Allgemein")
 (highlight-parens "Geklammerten Text hervorheben")
 (fixup-parens "Klammern korrigieren")
 (flash-paren-match "Passende Klammer anblinken")
 (auto-save-files "Dateien automatisch abspeichern")
 (backup-files "Backup-Dateien")
 (map-delete-to-backspace "Entf l�scht r�ckw�rts")
 (verify-exit "Bei Verlassen nachfragen")
 (ask-before-changing-format "For Format�nderung beim Speichern nachfragen")
 (wrap-words-in-editor-buffers "Worte in Editor-Puffern umbrechen")
 (show-status-line "Status-Zeile anzeigen")
 (count-columns-from-one "Spaltennummern fangen mit 1 an")
 (display-line-numbers "Zeilennummern in Puffern anzeigen, keine Puffer-Indizes")
 (enable-keybindings-in-menus "Tastenbelegung f�r Men�s")
 (automatically-to-ps "Automatisch in PostScript-Datei drucken")
 (option-as-meta "Option-Taste als Mera behandeln") ;; macos/macos x only
 (use-mdi "MDI-Fenster verwenden") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "F�r Textsuche separaten Dialog verwenden")
 (reuse-existing-frames "Existierende Fenster f�r neu ge�ffnete Dateien wiederverwenden")
 (default-fonts "Standard-Fonts")
 (paren-match-color "Farbe f�r Klammern-Hervorhebung") ; in prefs dialog
 (choose-color "Farbe ausw�hlen") ; in prefs dialog
 (online-coloring-active "Syntax interaktiv einf�rben")
 (open-files-in-tabs "Dateien in separaten Tabs �ffnen (nicht separaten Fenstern)")
 (show-interactions-on-execute "Interaktionen beim Programmstart automatisch �ffnen")
 (limit-interactions-size "Umfang der Interaktionen einschr�nken")
 (background-color "Hintergrundfarbe")
 (default-text-color "Standard f�r Text") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Hintergrundfarbe ausw�hlen")

 ; title of the color choosing dialog
 (choose-paren-highlight-color "Farbe f�r Klammerhervorhebung w�hlen")

 ; should have entire alphabet
 (font-example-string "Zw�lf Boxk�mpfer jagen Victor quer �ber den gro�en Sylter Deich.") 

 (change-font-button-label "�ndern")
 (fonts "Schriften")

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Neuen Font f�r \"~a\" w�hlen")

 (font-size-slider-label "Gr��e")
 (restart-to-see-font-changes "Neu starten, damit die Schrift�nderung wirksam wird")

 (font-prefs-panel-title "Schriftart")
 (font-name "Name Schriftart")
 (font-size "Gr��e Schriftart")
 (set-font "Schriftart setzen...")
 (font-smoothing-label  "Weiche Kanten bei Schrift")
 (font-smoothing-none "Nicht")
 (font-smoothing-some "Bi�chen")
 (font-smoothing-all "Total")
 (font-smoothing-default "System-Einstellung verwenden")
 (select-font-name "Schriftart-Name ausw�hlen")
 (example-text "Beispieltext:")
 (only-warn-once "Nur einmal warnen, wenn Definitionen und Interaktionen nicht synchron sind")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Auf Lock-Datei f�r Einstellungen warten...")
 (pref-lock-not-gone
  "Die Lock-Datei f�r die Einstellungen:\n\n   ~a\n\nverhindert, dass die Einstellungen abgespeichert werden k�nnen. Bitte stellen Sie sicher, dass keine andere PLT-Software l�uft und l�schen Sie dann diese Datei.")
 (still-locked-exit-anyway? "Die Einstellungen wurden nicht korrekt gespeichert.  Trotzdem beenden?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Einr�cken")
 (indenting-prefs-extra-regexp "Zus�tzlicher Regexp")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Bitte ein Schl�sselwort wie ~a eingeben:")
 (x-keyword "~a-Schl�sselwort")
 (x-like-keywords "Schl�sselwort wie ~a")

 (expected-a-symbol "Symbol erwartet, stattdessen bekommen: ~a")
 (already-used-keyword "\"~a\" ist bereits ein Schl�sselwort mit Spezial-Einr�ckung")
 (add-keyword "Hinzuf�gen")
 (remove-keyword "Entfernen")
 
 ;;; find/replace
 (find-and-replace "Suchen und Ersetzen")
 (find "Suchen")
 (replace "Ersetzen")
 (dock "Andocken")
 (undock "Ablegen")
 (use-separate-dialog-for-searching "Separaten Dialog f�r Suchen verwenden")
 (replace&find-again "Nochmals Suchen && Ersetzen") ;;; need double & to get a single &
 (replace-to-end "Ersetzen bis zum Ende")
 (forward "Vorw�rts")
 (backward "R�ckw�rts")
 (hide "Ausblenden")
 
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "In Dateien suchen...")
 (mfs-string-match/graphics "per Text (auch in Dateien mit Grafik)")
 (mfs-regexp-match/no-graphics "per regul�rem Ausdruck (nur reine Textdateien)")
 (mfs-searching... "Suche...")
 (mfs-configure-search "Einstellungen Suche") ;; dialog title
 (mfs-files-section "Dateien")   ;; section in config dialog
 (mfs-search-section "Suche") ;; section in config dialog
 (mfs-dir "Verzeichnis")
 (mfs-recur-over-subdirectories "In Unterverzeichnisse abtauchen")
 (mfs-regexp-filename-filter "Regul�rer Ausdruck Dateinamen-Filter")
 (mfs-search-string "Zeichenkette suchen")
 (mfs-drscheme-multi-file-search "DrScheme - Suche in mehreren Dateien") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" ist kein Verzeichnis")
 (mfs-open-file "Datei �ffnen")
 (mfs-stop-search "Suche stoppen")
 (mfs-case-sensitive-label "Gro�-/Kleinschreibung beachten")
 (mfs-no-matches-found "Keine Treffer gefunden.")
 (mfs-search-interrupted "Suche abgebrochen.")
 
 ;;; reverting a file
 (error-reverting "DrScheme - Fehler beim Wiederherstellen")
 (could-not-read "Konnte \"~a\" nicht lesen")
 (are-you-sure-revert
  "Sind Sie sicher, dass Sie diese Datei wiederherstellen wollen? Diese Operation kann nicht r�ckg�ngig gemacht werden.")
 (are-you-sure-revert-title
  "Wiederherstellen?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Fehler beim Speichern") ;; title of error message dialog
 (error-saving-file/name "Fehler beim Speichern von ~a")
 (error-loading "Fehler beim Laden")
 (error-loading-file/name "Fehler beim Laden von ~a.")
 (unknown-filename "<< unbekannt >>")

 ;;; finder dialog
 (must-specify-a-filename "Sie m�ssen einen Dateinamen angeben")
 (file-does-not-exist "Die Datei \"~a\" existiert nicht.")
 (ask-because-file-exists "Die Datei \"~a\" existiert schon. Ersetzen?")
 (dne-or-cycle "Der Dateiname \"~a\" enth�lt ein nicht existentes Verzeichnis oder einen Zyklus.")
 (get-file "Datei lesen")
 (put-file "Datei schreiben")
 (full-pathname "Gesamter Dateiname")
 (show-dot-files "Dateien und Verzeichnisse anzeigen, die mit einem Punkt anfangen.")
 (up-directory-button-label "Verzeichnis nach oben")
 (add-button-label "Hinzuf�gen") ;;; for multi-file selection
 (add-all-button-label "Alle hinzuf�gen") ;;; for multi-file selection
 (remove-button-label "Entfernen") ;;; for multi-file selection
 (file-wrong-form "Der Dateiname hat nicht die richtige Form.")
 (select-files "Dateien ausw�hlen")
 (select-file "Datei ausw�hlen")
 (dir-dne "Das Verzeichnis existiert nicht.")
 (file-dne "Die Datei existiert nicht.")
 (empty-filename "Der Dateiname muss Buchstaben enthalten.")
 (that-is-dir-name "Dieser Name geh�rt zu einem Verzeichnis.")
 
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

 (file-menu-label "&Datei")

 (new-info  "Neue Datei �ffnen")
 (new-menu-item "&Neu")
 (new-...-menu-item "&Neu...")

 (open-info "Datei �ffnen")
 (open-menu-item "&�ffnen...")
 (open-here-menu-item "Hier &�ffnen...")

 (open-recent-info "Liste k�rzlich bearbeiteter Dateien")
 (open-recent-menu-item "�ffne noch einmal")
 
 (revert-info "Stelle diese Datei wieder her wie zuletzt gespeichert")
 (revert-menu-item "&Wiederherstellen")

 (save-info "Diese Datei auf der Platte speichern")
 (save-menu-item "&Speichern")

 (save-as-info "Dateinamen abfragen und dann Datei auf der Platte speichern")
 (save-as-menu-item "Speichern &unter...")

 (print-info "Diese Datei zum Drucker schicken")
 (print-menu-item "&Drucken...")

 (close-info "Diese Datei schlie�en")
 (close-menu-item "&Schlie�en")

 (quit-info "Alle Fenster schlie�en")
 (quit-menu-item-windows "Be&enden")
 (quit-menu-item-others "&Beenden")
 
 (edit-menu-label "&Bearbeiten")
 
 (undo-info "Letzte Aktion r�ckg�ngig machen")
 (undo-menu-item "&R�ckg�ngig")

 (redo-info "Letzte R�ckg�ngig-Operation r�ckg�ngig machen")
 (redo-menu-item "&Nochmal")

 (cut-info "Verschiebe die Selektion ins Clipboard, um sie sp�ter wieder einf�gen zu k�nnen")
 (cut-menu-item "&Ausschneiden")

 (copy-info "Kopiere die Selektion ins Clipboard, um sie sp�ter wieder einf�gen zu k�nne")
 (copy-menu-item "&Kopieren")

 (paste-info "Ersetze die aktuelle Selektion durch die zuletzt kopierte oder ausgeschnittene Selektion")
 (paste-menu-item "&Einf�gen")

 (clear-info "L�sche die Selektion, ohne das Clipboard dabei zu �ndern oder etwas einzuf�gen")
 (clear-menu-item-others "L�schen")
 (clear-menu-item-windows "&L�schen")

 (select-all-info "Selektiere das gesamte Dokument")
 (select-all-menu-item "&Alles selektieren")
 
 (find-info "Suche eine Zeichenkette")
 (find-menu-item "Suche...")

 (find-again-info "Suche die gleiche Zeichenkette nochmal")
 (find-again-menu-item "Suche nochmal")
 
 (replace-and-find-again-info "Ersetze den aktuellen Text und suche dann das gleiche nochmal")
 (replace-and-find-again-menu-item "Ersetzen && nochmal suchen")

 (preferences-info "Konfiguriere die Einstellungen")
 (preferences-menu-item "Einstellungen...")

 (keybindings-info "Aktuelle Tastaturbelegung anzeigen")
 (keybindings-menu-item "Tastaturbelegung")
 (keybindings-show-active "Aktive Tastenbelegungen anzeigen")
 (keybindings-frame-title "Tastaturbelegung")
 (keybindings-sort-by-name "Nach Name sortieren")
 (keybindings-sort-by-key "Nach Taste sortieren")
 (keybindings-add-user-defined-keybindings "Benutzerdefinierte Tastenbelegungen hinzuf�gen...")
 (keybindings-menu-remove "~a entfernen")
 (keybindings-choose-user-defined-file "Bitte eine Datei mit den Tastenbelegungen ausw�hlen.")

 (user-defined-keybinding-error "Fehler beim Ausf�hren der Tastenbelegung ~a\n\n~a")
 (user-defined-keybinding-malformed-file "Die Datei ~a enth�lt kein Modul, das in der Sprache (lib \"keybinding-lang.ss\" \"framework\") geschrieben ist.")  

 ;; menu items in the "special" menu
 (insert-text-box-item "Text-Kasten einf�gen")
 (insert-pb-box-item "Pinwand-Kasten einf�gen")
 (insert-image-item "Bild einf�gen...")
 (insert-comment-box-menu-item-label "Kommentarkasten einf�gen")
 (insert-lambda "&Lambda einf�gen")
 (insert-delta "&Delta (define) einf�gen")

 (wrap-text-item "Text umbrechen")

 (windows-menu-label "&Fenster")
 (bring-frame-to-front "Fenster nach vorn")       ;;; title of dialog
 (bring-frame-to-front... "Fenster nach vorn...") ;;; corresponding title of menu item
 (next-window "N�chstes Fenster")
 (previous-window "Voriges Fenster")
 (most-recent-window "Letztes Fenster")

 (view-menu-label "&Anzeigen")
 (show-overview "Programm-Umriss einblenden") 
 (hide-overview "Programm-Umriss ausblenden")
 (show-module-browser "Modul-Brauser einblenden")
 (hide-module-browser "Modul-Brauser ausblenden")

 (help-menu-label "&Hilfe")
 (about-info "Mehr �ber dieses Programm und seine Entstehung")
 (about-menu-item "�ber...")
 (help-menu-check-for-updates "Nach Updates schauen...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "W�rden Sie gern ein neues Fenster aufmachen oder dieses hier l�schen und wiederverwenden?")
 (clear-current "Dieses l�schen")
 (new-window "Neues Fenster")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Beenden")
 (quit "Beenden")
 (are-you-sure-exit "Sind Sie sicher, dass Sie das Programm beenden wollen?")
 (are-you-sure-quit "Sind Sie sicher, dass Sie das Programm beenden wollen?")
 
 ;;; autosaving
 (error-autosaving "Fehler beim automatischen Speichern von \"~a\".") ;; ~a will be a filename
 (autosaving-turned-off "Automatisches Speichern abgeschaltet\nbis die Datei wieder gespeichert wird.")
 (recover-autosave-files-frame-title "Automatisch gespeicherte Dateien zur�ckholen")
 (autosave-details "Details")
 (autosave-recover "Zur�ckholen")
 (autosave-unknown-filename "<<unbekannt>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrScheme
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Automatisch gespeicherte Datei:")
  (autosave-original-label: "Urspr�ngliche Datei:")
  (autosave-autosave-label "Automatisch gespeicherte Datei")
  (autosave-original-label "Urspr�ngliche Datei")
  (autosave-compare-files "Automatisch gespeicherte Dateien vergleichen")

  (autosave-show-autosave "Automatisch gespeicherte Datei") ;; title of a window showing the autosave file

  (autosave-explanation "DrScheme hat automatisch gespeicherte Dateien gefunden, die nicht regul�r gespeicherten Inhalt enthalten k�nnten.")

  (autosave-recovered! "Zur�ckgeholt!") ;; status of an autosave file
  (autosave-deleted "Gel�scht")       ;; status of an autosave file

  (autosave-error-deleting "Fehler beim L�schen von ~a\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "L�schen")
  (autosave-delete-title "L�schen")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Fertig")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "Bestimmen Sie, wo die automatisch gespeicherte Datei hin zur�ckgeholt werden soll")
  
  
 ;;; file modified warning
 (file-has-been-modified
  "Die Datei wurde ver�ndert, seit sie das letzte Mal gespeichert wurde. �nderungen �berschreiben?")
 (overwrite-file-button-label "�berschreiben")
 
 (definitions-modified 
  "Die Definitionen wurden auf der Platte ge�ndert; bitte speichern sie die Definitionen oder holen Sie diese von der Platte zur�ck.")
 (drscheme-internal-error "Interner Fehler in DrScheme")
 
 ;;; tools
 (invalid-tool-spec "Die Tool-Spezifikation in der Datei info.ss der Kollektion ~a enth�lt Fehler. Da sollte eine Zeichenkette oder eine Liste von Zeichenketten stehen, tats�chlich steht dort aber: ~e")
 (error-loading-tool-title "DrScheme - Fehler beim Laden von ~s; ~s")
 (error-invoking-tool-title "Fehler beim Starten von Tool ~s;~s")
 (tool-tool-names-same-length
  "`tool-names' und `tools' in info.ss f�r ~s m�ssen Listen der gleichen L�nge sein, tats�chlich stehen dort ~e und ~e")
 (tool-tool-icons-same-length
  "`tool-icons' und `tools' in info.ss f�r ~s m�ssen Listen der gleichen L�nge sein, tats�chlich stehen dort ~e und ~e")
 (tool-tool-urls-same-length
  "`tool-urls' und `tools' in info.ss f�r ~s m�ssen Listen der gleichen L�nge sein, tats�chlich stehen dort ~e und ~e")
 (error-getting-info-tool
  "Fehler beim Laden von info.ss file f�r ~s")
 (tool-error-phase1 "Fehler in Phase 1 von Tool ~s; ~s")
 (tool-error-phase2 "Fehler in Phase 2 von Tool ~s; ~s")


 ;;; define popup menu
 (end-of-buffer-define "<< Text-Ende >>")
 (sort-by-name "Nach Namen Sortieren")
 (sort-by-position "Nach Position in der Datei sortieren")
 (no-definitions-found "<< keine Definitionen gefunden>>")
 (jump-to-defn "Zur Definition von ~a springen")

 (recent-items-sort-by-age "Nach Alter sortieren")
 (recent-items-sort-by-name "Nach Name sortieren")
 
 ;;; view menu
 (hide-definitions-menu-item-label "&Definitionen ausblenden")
 (show-definitions-menu-item-label "&Definitionen einblenden")
 (definitions-menu-item-help-string "Definitionsfenster ein-/ausblenden")
 (show-interactions-menu-item-label "&Interaktionen einblenden")
 (hide-interactions-menu-item-label "&Interaktionen ausblenden")
 (interactions-menu-item-help-string "Interaktionsfenster ein-/ausblenden")
 (show-toolbar "&Toolbar einblenden")
 (hide-toolbar "&Toolbar ausblenden")

 ;;; file menu
 (save-definitions-as "Definitionen speichern unter...")
 (save-definitions "Definitionen speichern")
 (print-definitions "Definition drucken...")
 (about-drscheme "�ber DrScheme")
 (save-other "Speichern unter")
 (save-definitions-as-text "Definitionen als Text speichern...")
 (save-interactions "Interaktionen speichern")
 (save-interactions-as "Interaktionen speichern unter...")
 (save-interactions-as-text "Interaktionen als Text speichern...")
 (print-interactions "Interaktionen drucken...")
 (new-tab "Neuer Tab")
 (close-tab "Tab schlie�en")
 
 ;;; edit-menu
 (split-menu-item-label "&Splitten")
 (collapse-menu-item-label "Einfalten")
 
 ;;; language menu
 (language-menu-name "&Sprache")
 
 ;;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Start")
 (execute-menu-item-help-string "Das Programm im Definitionsfenster neu starten")
 (break-menu-item-label "Stop")
 (break-menu-item-help-string "Momentane Auswertung unterbrechen")
 (kill-menu-item-label "Abbrechen")
 (kill-menu-item-help-string "Momentante Auswertung abbrechen")
 (clear-error-highlight-menu-item-label "Fehlermarkierung entfernen")
 (clear-error-highlight-item-help-string "Entfernt die rosa Fehlermarkierung")
 (reindent-menu-item-label "&Einr�cken")
 (reindent-all-menu-item-label "&Alles einr�cken")
 (semicolon-comment-out-menu-item-label "Mit Semikolon auskommentieren")
 (box-comment-out-menu-item-label "Mit Kommentar-Kasten auskommentieren")
 (uncomment-menu-item-label "Einkommentieren")

 (convert-to-semicolon-comment "In Semikolon-Kommentar umwandeln")
 
 ;;; executables
 (create-executable-menu-item-label "Programmdatei generieren...")
 (create-executable-title "Programmdatei generieren")
 (must-save-before-executable "Sie m�ssen vor der Generierung einer Programmdatei speichern.")
 (save-an-executable "Programmdatei speichern")
 (save-a-mred-launcher "MrEd-Launcher speichern")
 (save-a-mzscheme-launcher "MzScheme-Launcher speichern")
 (save-a-mred-stand-alone-executable "MrEd-Stand-Alone-Programmdatei speichern")
 (save-a-mzscheme-stand-alone-executable "MzScheme-Stand-Alone-Programmdatei speichern")

 (definitions-not-saved "Die Definitionen sind nicht gespeichert. Die Programmdatei wird von der letzten gespeicherten Version gezogen. Weitermachen?")
 (inline-saved-program-in-executable?
  "Scheme-Code in das Programm einbinden? Dann k�nnten Sie die Programmdatei zu einem anderen ~a-Computer transferieren, aber die Programmdatei wird dann ziemlich gro�. Falls nicht, k�nnen Sie die Programmdatei nicht transferieren, aber sie wird deutlich kleiner. Au�erdem wird die Programmdatei dann die jeweils neueste Version des Scheme-Codes benutzen.")
 (use-mred-binary?
  "MrEd f�r diese Programmdatei verwenden?\n\nFalls ja, kann das Programm die Bibliothek (lib \"mred.ss\" \"mred\") verwenden. Falls nein, wird DrScheme MzScheme verwenden - dann kann das Programm die Bibliothek nicht verwenden.n\nFalls Sie nicht sicher sind, w�hlen Sie \"ja\".")
 (inline-saved-program-in-executable/windows/path
   "WARNUNG! Die generierte Programmdatei ben�tigt drei DLLs: libmred.dll, libmzsch.gll und libgc.dll, die sich in folgendem Verzeichnis befinden:\n\n~a\n\nDie Programmdatei findet DLLs entweder im selben Verzeichnis wie die Programmdatei selbst oder durch die Umgebungsvariable PATH.\n\nAls Sie DrScheme installierten, hat der Installer PATH derart verw�ndert, dass das DLL-Verzeichnis dabei ist. Diese Einstellung k�nnte seitdem ge�ndert worden sein.\n\nFalls Sie die Programmdatei auf eine andere Maschine tranferieren, m�ssen Sie die DLLs ebenfalls transferieren - entweder in das gleiche Verzeichnis wie die Programmdatei oder in ein Verzeichnis im PATH der anderen Maschine.")
 (launcher "Launcher")
 (stand-alone "Stand-alone")
 (executable-type "Typ")
 (executable-base "Hauptteil")
 (filename "Dateiname: ")
 (create "Erzeugen")
 (please-choose-an-executable-filename "Bitte Dateinamen f�r Programm ausw�hlen")
 (windows-executables-must-end-with-exe
  "Der Dateiname\n\n  ~a\n\nist unzul�ssig. Unter Windows m�ssen Programmdateien mit .exe enden.")
 (macosx-executables-must-end-with-app
  "Der Dateiname\n\n  ~a\n\nist unzul�ssig. Unter Mac OS X m�ssen Namen f�r Programme mit .app enden.")
 (warning-directory-will-be-replaced
  "WARNUNG: das Verzeichnis:\n\n  ~a\n\nwird �berschrieben werden. Weitermachen?")
 
 (create-servlet "Servlet erzeugen...")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "Servlet lassen sich nicht aus einem Programm in der Sprache \"~a\" erzeugen.")
  
 ;;; buttons
 (execute-button-label "Start") 
 (save-button-label "Speichern")
 (break-button-label "Stop")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Suche im Hilfezentrum nach \"~a\"")
 (exact-lucky-search-help-desk-for "Exakte Suche im Hilfezentrum auf gut Gl�ck nach \"~a\"")

 ;; collapse and expand popup menu items
 (collapse-sexp "S-Expression einfalten")
 (expand-sexp "S-Expression wieder ausfalten")
 
 ;;; fraction dialog
 (enter-fraction "Bruch eingeben")
 (whole-part "Ganzzahliger Anteil")
 (numerator "Z�hler")
 (denominator "Nenner")
 (invalid-number "Unzul�ssige Zahl: muss exakt, reell und nicht ganz sein.")
 (insert-fraction-menu-item-label "Bruch einf�gen...")

 ;; number snip popup menu
 (show-decimal-expansion "Als Dezimalexpansion anzeigen")
 (show-fraction-view "Als Bruch anzeigen")
 (show-mixed-fraction-view "Als gemischten Bruch anzeigen")
 (show-improper-fraction-view "Als ungemischten Bruch anzeigenn")
 (show-more-decimal-places "Mehr Dezimalziffern anzeigen")
 
 ;;; Teachpack messages
 (select-a-teachpack "Teachpack ausw�hlen")
 (clear-teachpack "Teachpack ~a herauswerfen")
 (teachpack-error-label "DrScheme - Teachpack-Fehler")
 (teachpack-dne/cant-read "Die Teachpack-Datei ~a existiert nicht oder ist nicht lesbar.")
 (teachpack-didnt-load "Die Teachpack-Datei ~a konnte nicht korrekt geladen werden.")
 (teachpack-error-invoke "Die Teachpack-Datei ~a hat beim Start ein Problem signalisiert.")
 (add-teachpack-menu-item-label "Teachpack hinzuf�gen...")
 (clear-all-teachpacks-menu-item-label "Alle Teachpacks herauswerfen")
 (drscheme-teachpack-message-title "DrScheme-Teachpack")
 (already-added-teachpack "Teachpack ~a ist schon dabei")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Bitte eine Sprache ausw�hlen. F�r den Anf�ngerkurs ist wahrscheinlich die voreingestellte Sprache die richtige.")
 (language-dialog-title "Sprache ausw�hlen")
 (case-sensitive-label "Gro�-/Kleinschreibung unterscheiden")
 (output-style-label "Ausgabenotation")
 (constructor-printing-style "Konstruktor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (print-printing-style "current-print")
 (sharing-printing-label "Zeige Sharing an")
 (use-pretty-printer-label "Zeilenumbr�che in Ausdruck einf�gen")
 (input-syntax "Eingabesyntax")
 (dynamic-properties "Laufzeit")
 (output-syntax "Ausgabesyntax")
 (no-debugging-or-profiling "Kein Debugging oder Profiling")
 (debugging "Debugging")
 (debugging-and-profiling "Debugging und Profiling")
 (test-coverage "Syntaktische Test-Suiten-Abdeckung")
 (whole/fractional-exact-numbers-label "Zahlen als Br�che ausdrucken")
 (booleans-as-true/false-label "Booleans als \"true\" und \"false\" ausdrucken")
 (show-details-button-label "Details einblenden")
 (hide-details-button-label "Details ausblenden")
 (choose-language-menu-item-label "Sprache ausw�hlen...")
 (revert-to-language-defaults "Standard-Spracheinstellungen wiederherstellen")
 (language-docs-button-label "Dokumentation f�r Sprache")
 (fraction-style "Bruch-Ausgabe")
 (use-mixed-fractions "gemischte Br�che")
 (use-repeating-decimals "Dezimalausgabe mit Perioden")
 (decimal-notation-for-rationals "Dezimalnotation f�r Br�che")
 (please-select-a-language "Bitte Sprache ausw�hlen")

 
 ;;; languages
 (beginning-student "Anf�nger")
 (beginning-one-line-summary "define, cond, Strukturen, Konstanten und Primitiva")
 (beginning-student/abbrev "Anf�nger mit Listen-Abk�rzungen")
 (beginning/abbrev-one-line-summary "Anf�nger, wobei Listen mit \"list\" in der REPL ausgedruckt werden")
 (intermediate-student "Zwischenstufe")
 (intermediate-one-line-summary "Anf�nger plus lexikalische Bindung")
 (intermediate-student/lambda "Zwischenstufe mit lambda")
 (intermediate/lambda-one-line-summary "Zwischenstufe plus Prozeduren h�herer Ordnung")
 (advanced-student "Fortgeschritten")
 (advanced-one-line-summary "Zwischenstufe plus lambda und Mutation")
 (full-language "Alles") ;; also in the HtDP languages section
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "R5RS-verwandet")
 (pretty-big-scheme "Kombo (enth�lt MrEd and Fortgeschritten)")
 (pretty-big-scheme-one-line-summary "Macht Syntax and Prozeduren der HtDP-Sprachen verf�gbar")
 (r5rs-lang-name "Standard (R5RS)")
 (r5rs-one-line-summary "R5RS, ohne alles andere")
 (expander "Expander")
 (expander-one-line-summary "Expandiert Ausdr�cke, statt sie auszuwerten")
 (professional-languages "Sprachen f�r Entwickler")
 (teaching-languages "Lehrsprachen")
 (experimental-languages "Experimentelle Sprachen")
 
 (module-language-one-line-summary "Start erzeugt eine REPL im Kontext des Moduls inklusive der deklarierten Sprache des Moduls.")
  

 ;;; debug language
 (unknown-debug-frame "[unbekannt]")
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "Interaktionen von ~a") ;; filled with a filename
 (current-interactions "Interaktionen")
 (current-definitions "Definitionen")
 (mzscheme-w/debug "Text (MzScheme, mit R5RS)")
 (mzscheme-one-line-summary "Die PLT-Version von Scheme")
 (mred-w/debug "Grafisch (MrEd, mit MzScheme)")
 (mred-one-line-summary "MzScheme + GUI-Bibliothek")

 ;; profiling
 (profiling-low-color "Wenig")
 (profiling-high-color "Viel")
 (profiling-choose-low-color "Bitte Farbe f�r \"wenig\" ausw�hlen")
 (profiling-choose-high-color "Bitte Farbe f�r \"viel\" ausw�hlen")
 (profiling "Profiling")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "Farbbereich f�r Profiling") 
 (profiling-scale "Farbskala f�r Profiling")
 (profiling-sqrt "Wurzel")
 (profiling-linear "Linear")
 (profiling-square "Quadrat")
 (profiling-number "Aufrufanzahl")
 (profiling-time "Gesamtzeit")
 (profiling-clear "Profile l�schen")
 (profiling-update "Profile atkualisieren")
 (profiling-col-percent-time "% Zeit")
 (profiling-col-function "Prozedur")
 (profiling-col-name "Name")
 (profiling-col-time-in-msec "ms")
 (profiling-col-calls "Aufrufe")
 (profiling-show-profile "Profile einblenden")
 (profiling-hide-profile "Profile ausblenden")
 (profiling-unknown-src "<< unbekannt >>")
 (profiling-no-information-available "Es ist keine Profiling-Information verf�gbar. Bitte stellen Sie sicher, dass Profiling eingeschaltet und Ihr Programm gelaufen ist.")
 (profiling-clear? "�nderungen im Definitionsfenster machen die Profiling-Informationen ung�ltig. Weitermachen?")
 
 ;; test coverage
 (test-coverage-clear? "�nderungen im Definitionsfenster machen die Information �ber Testabdeckung ung�ltig. Weitermachen?")
 (test-coverage-clear-and-do-not-ask-again "Ja, und nicht nicht wieder fragen")
 (test-coverage-ask? "Frage nach dem L�schen der Testabdeckungs-Information")
  
 ;; tracing
 (tracing-enable-tracing "Tracing einschalten")
 (tracing-show-tracing-window "Tracing einblenden")
 (tracing-hide-tracing-window "Tracing ausblenden")
 (tracing-tracing-nothing-to-show "Es liegen keine Tracing-Resultate vor. Stellen Sie sicher, dass die eingestellte Sprache Tracing unterst�tzt und dass Tracing eingeschaltet ist.")

 ;;; repl stuff
 (evaluation-terminated "Auswertung abgebrochen")
 (evaluation-terminated-explanation
  "Der Auswertungs-Thread l�uft nicht mehr; es findet also keine Auswertung bis zum n�chsten Programmlauf statt.")
 (last-stack-frame "letzten Stack-Frame zeigen")
 (last-stack-frames "die letzten ~a Stack-Frames zeigen")
 (next-stack-frames "die n�chsten ~a Stack-Frames zeigen")
 
 ;;; welcoming message in repl
 (language "Sprache")
 (custom "angepasst")
 (teachpack "Teachpack")
 (welcome-to "Willkommen bei")
 (version "Version")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Auswertung abbrechen?")
 (just-break "Nur unterbrechen")
 (kill "Abbrechen")
 (kill? "Abbrechen?")

 ;;; version checker
 ;; the next two are used in the initial wizard dialog.
 ;; Note that vc-wizard-check-prompt can (should) have newlines so
 ;; it will not make the dialog too wide.
 (vc-wizard-check-note "Die Version, die Sie gerade installieren wollen, k�nnte veraltet\n sein. Wenn Sie wollen, kann DrScheme nachsehen.")
 (vc-wizard-check-button "Nach Updates schauen")
 (vc-update-check "Update-Pr�fung")
 (vc-please-wait "Bitte warten")
 (vc-connecting-version-server "Mit PLT-Versions-Server verbinden")
 (vc-network-timeout "Netzwerk-Timeout") 
 (vc-cannot-connect  "Verbindungsversuch zum PLT-Versions-Server fehlgeschlagen")
 (vc-network-failure "Netzwerkproblem")
 (vc-old-binaries "Die installierten Programmdateien f�r DrScheme (oder MzScheme) sind veraltet")
 (vc-binary-information-format "Version installierte Programmdatei: ~a (Iteration ~a)")
 (vc-details-format "~a~nDetails:~n~a")
 (vc-details-text "Details:~n")
 (vc-error-format "Fehler: ~a") 
 (vc-current-format "~a v.~a (Iteration ~a) ist auf dem aktuellen Stand")
 (vc-update-format "~a v.~a (Iteration ~a) sollte aktualisiert werden auf v.~a (Iteration ~a)")
 (vc-binary-name "Programmdatei")
 (vc-updates-available "Updates sind verf�gbar auf")
 (vc-latest-binary-information-format "Neuestes Release ist Version ~a (Iteration ~a)")
 (vc-update-dialog-title "PLT-Update-Status")
 (vc-need-update-string "Ein oder mehrere installierte PLT-Software-Pakete sind veraltet")
 (vc-no-update-string "Alle installierten PLT-Software-Pakte sind auf dem neuesten Stand")

 ;; special menu
 (special-menu "S&pezial")
 
 ;; large semi colon letters
 (insert-large-letters... "Gro�e Buchstaben einf�gen...")
 (large-semicolon-letters "Gro�e Buchstaben aus Semikolons")
 (text-to-insert "Einzuf�gender Text")

 (module-browser-filename-format "Vollst�ndiger Dateiname: ~a (~a Zeilen)")
 (module-browser-root-filename "Basis-Dateiname: ~a")
 (module-browser-font-size-gauge-label "Schriftgr��e")
 (module-browser-progress-label "Fortschritt Modul-�bersicht")
 (module-browser-adding-file "Datei ~a hinzuf�gen...")
 (module-browser-laying-out-graph-label "Graph-Layout")
 (module-browser-open-file-format "~a �ffnen")
 (module-browser "Modul-Brauser") ;; frame title
 (module-browser... "Modul-Brauser...") ;; menu item title
 (module-browser-error-expanding "Fehler beim Expandieren des Programms:\n\n~a")
 (module-browser-show-lib-paths "Dateien anzeigen, die �ber (lib ..)-Pfade eingebunden wurden")
 (module-browser-progress "Modul-Brauser: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Modul-Brauser: Definition compilieren")
 (module-browser-show-lib-paths/short "\"lib\"-requires folgen") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Aktualisieren") ;; button label in show module browser pane in drscheme window.
 (module-browser-only-in-plt-and-module-langs
  "Der Modul-Brauser ist nur f�r Programme in den PLT-Sprachen und in der Modul-Sprache verf�gbar (und nur f�r Programme mit Modulen).")
 (module-browser-name-length "L�nge der Namen")
 (module-browser-name-short "Kurz")
 (module-browser-name-medium "Mittel")
 (module-browser-name-long "Lang")
 (module-browser-open-all "Alle hier angezeigten Datein �ffnen")

 (happy-birthday-matthias "Happy Birthday, Matthias!")
 (happy-birthday-matthew "Happy Birthday, Matthew!")
 (happy-birthday-shriram "Happy Birthday, Shriram!")

 (mrflow-using-default-language-title "Standard-Sprache verwendet")
 (mrflow-using-default-language "Die momentan verwendete Sprache hat keine Typ-Tabelle f�r ihre Primitiva.  Verwende stattdessen R5RS-Scheme.")
 (mrflow-button-title "Analyse")
 ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
 ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
 (mrflow-coloring-error-title "Farbe f�r \"unbekannt\"")
 (mrflow-coloring-error "Kein Style f�r Farbe ~a verwendet")
 (mrflow-popup-menu-show-type "Typ einblenden")
 (mrflow-popup-menu-hide-type "Typ ausblenden")
 (mrflow-popup-menu-show-errors "Fehler einblenden")
 (mrflow-popup-menu-hide-errors "Fehler ausblenden")
 ;(mrflow-read-exception-title "Read Exception")
 ;(mrflow-read-exception "Read exception: ~a")
 ;(mrflow-syntax-exception-title "Syntax Exception")
 ;(mrflow-syntax-exception "Syntax exception: ~a")
 ;(mrflow-unknown-exception-title "Unknown Exception")
 ;(mrflow-unknown-exception "Unknown exception: ~a")
 ;(mrflow-language-primitives-error-title "Language Primitives Error")
 ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")
  
 (snips-and-arrows-popup-menu-tack-all-arrows "Alle Pfeile befestigen")
 (snips-and-arrows-popup-menu-untack-all-arrows "Alle Pfeile l�sen")
 (snips-and-arrows-user-action-disallowed-title "�nderungen durch den Benutzer momentan nicht m�glich")
 (snips-and-arrows-user-action-disallowed "In Editoren, die von Tools erzeugte Snips enthalten, sind �nderungen durch den Benutzer nicht m�glich. Blenden Sie alle Snips aus, bevor Sie den Inhalt des Editors �ndern.")
 ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
 ;(snips-and-arrows-changing-terms-warning "Changing terms in an editor containing snips cannot be undone.  You can either cancel this action, remove the snips, and try the change again, or you can continue with the change, in which case the change will not be undoable (all others changes made before and afterward will still be undoable though).")
 (snips-and-arrows-hide-all-snips-in-editor "Alle Snips im Editor ausblenden")

 (xml-tool-menu "XML")
 (xml-tool-insert-xml-box "XML-Kasten einf�gen")
 (xml-tool-insert-scheme-box "Scheme-Kasten einf�gen")
 (xml-tool-insert-scheme-splice-box "Scheme-Splei�-Kasten einf�gen")
 (xml-tool-xml-box "XML-Kasten")
 (xml-tool-scheme-box "Scheme-Kasten")
 (xml-tool-scheme-splice-box "Scheme-Splei�-Kasten")
 (xml-tool-switch-to-scheme "In Scheme-Kasten verwandeln")
 (xml-tool-switch-to-scheme-splice "In Scheme-Splei�-Kasten verwandeln")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "�berfl�ssigen Whitespace in leeren Tags entfernen")
 (xml-tool-leave-whitespace-alone
  "Whitespace unver�ndert lassen")
 
 (show-recent-items-window-menu-item "K�rzlich ge�ffnete Dateien in separatem Fenster anzeigen")
 (show-recent-items-window-label "K�rzlich ge�ffnete Dateien")
 (number-of-open-recent-items "Anzahl k�rzlich ge�ffneter Dateien")
 (switch-anyway "Datei trotzdem wechseln")

 (stepper-program-has-changed "WARNUNG: Das Programm wurde ge�ndert.")
 (stepper-program-window-closed "WARNUNG: Das Programm-Fenster ist nicht mehr da.")

 (wizard-next "Weiter")
 (wizard-back "Zur�ck")
 (wizard-finish "Fertigstellen")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "Das Programm im Definitionsfenster l�uft noch.  Trotzdem schlie�en?")
  (program-has-open-windows "Das Programm im Definitionsfenster hat noch offene Fenster.  Trotzdem dieses Fenster schlie�en?")
 
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Kommandozeilen-Argumente als Vektoren von Zeichenketten, in Read-Syntax")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<Standard-Pfade f�r Kollektionen>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Bitte Pfad f�r Kollektion ausw�hlen")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Standard-Pfade f�r Kollektionen schon vorhanden")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Pfade f�r Kollektionen")

  ;; button labels
  (ml-cp-add "Hinzuf�gen")
  (ml-cp-add-default "Standard hinzuf�gen")
  (ml-cp-remove "Entfernen")
  (ml-cp-raise "H�her")
  (ml-cp-lower "Tiefer")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Java-Modus")
  (profj-java-mode-color-keyword "Schl�sselwort")
  (profj-java-mode-color-string "Zeichenkette")
  (profj-java-mode-color-literal "Literal")
  (profj-java-mode-color-comment "Kommentar")
  (profj-java-mode-color-error "Fehler")
  (profj-java-mode-color-identifier "Bezeichner")
  (profj-java-mode-color-default "sonstiges")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Leerer Testfall")
  (test-case-too-many-expressions-error "Zu viele Ausdr�cke in einem Testfall")
  (test-case-not-at-top-level "Testfall-Kasten nicht ganz am Top-Level")
  ;; Dr. Scheme window menu items
  (test-case-insert "Testfall einf�gen")
  (test-case-disable-all "Alle Testf�lle deaktivieren")
  (test-case-enable-all "Alle Testf�lle aktivieren")
  ;; NOTE: The following three string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Test")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Sollte sein")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "Tats�chlich")
  (test-case-predicate "Pr�dikate")
  (test-case-should-raise "Sollte verursachen")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "Fehlermeldung")

  (test-case-menu-title "Testfall")
  (test-case-switch-to-error-box "Zu Fehler-Testbox machen")
  (test-case-switch-to-nonerror-box "Zu Nicht-Fehler-Testbox machen")
  (test-case-collapse "Testfall einfalten")
  (test-case-show-actual "Tats�chlichen Wert zeigen")
  (test-case-enable "Testfall aktivieren")
  (test-case-show-predicate "Pr�dikat anzeigen")
  (test-case-show-error-message "Fehlermeldung anzeigen")
  (test-case-convert-to-text "In Text umwandeln")

  ;; Profj Boxes
  (profjBoxes-empty-error "Leere Interaktion")
  (profjBoxes-too-many-expressions-error "Zu viele Ausdr�cke in einem Kasten")
  (profjBoxes-interactions-label "Interaktionen")
  (profjBoxes-bad-java-id-error "Nicht-wohlgeformte Java-ID")
  (profjBoxes-examples-label "Beispiele")
  (profjBoxes-add-new-example-button "Neues Beispiel hinzuf�gen")
  (profjBoxes-type "Typ")
  ;; The Java identifier of an example of data
  (profjBoxes-name "Name")
  (profjBoxes-value "Wert")

  )
