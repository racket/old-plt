
; do not remove all these blank lines, I use them to make sure a translation 
; in this file appears on the same line as in english-string-constants.ss



; Note: http://www.linux-france.org/prj/jargonf/ peut etre utile... Les dictionnaires online
; anglais->francais sont tres pauvres en ce qui concerne le jargon technique, et l'academie
; francaise (http://www-rocq.inria.fr/qui/Philippe.Deschamp/RETIF/) a quelques longueures de
; retard. http://www.can.ibm.com/francais/dico/ peut aider aussi...
; http://www.dicofr.com/ permet les recherches a partir du mot anglais.
; http://www.francophonie.hachette-livre.fr/ est un dico standard de base
; http://zeus.inalf.fr/academie9.htm est le dico de l'academie (A-M pour l'instant seulement)
 












































































(module french-string-constants "string-constant-lang.ss"
 ;;; when translating this constant, substitue name of actual langauge for `English'
 (is-this-your-native-language "Le Fran�ais est-il votre langue maternelle ?")

 (are-you-sure-you-want-to-switch-languages
  "Ceci va changer le language utilis� par l'interface graphique, ce qui va n�cessiter un red�marrage de DrScheme. Etes-vous certain de vouloir continuer ?")
 
 (interact-with-drscheme-in-language "Interagir avec DrScheme en Fran�ais")
 
 ;; these two should probably be the same in all languages excepet English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "Accepter et Quitter")
 (accept-and-exit "Accepter et Quitter")
 
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Annuler")
 (untitled "Sans Nom")
 (untitled-n "Sans Nom ~a")
 (warning "Avertissement")
 (error "Erreur")
 (close "Fermer") ;; as in, close an open window
 (stop "Stop")   
 (&stop "&Stop") ;; for use in button and menu item labels, with short cut.
 
 ;;; important urls
 (web-materials "Sites web apparent�s") ;; menu item title
 (tool-web-sites "Sites web d'outils")   ;; menu item title
 (drscheme-homepage "DrScheme")
 (plt-homepage "PLT")
 (how-to-use-scheme "How to Use Scheme") ;; title of a book.
 (teachscheme!-homepage "TeachScheme!") ;; probably this should be a `word' in all languages
 
 ;;; bug report form
 (cancel-bug-report? "Annuler la soumission du formulaire de bogue ?")
 (are-you-sure-cancel-bug-report?
  "Etes-vous certain de vouloir annuler la soumission de ce formulaire de bogue ?")
 (bug-report-form "Formulaire de soumission de bogue")
 (bug-report-field-name "Nom")
 (bug-report-field-email "Email")
 (bug-report-field-summary "R�sum� du probl�me")
 (bug-report-field-severity "S�v�rit�")
 (bug-report-field-class "Classe")
 (bug-report-field-priority "Priorit�")
 (bug-report-field-description "Description")
 (bug-report-field-reproduce1 "Etapes � suivre pour")
 (bug-report-field-reproduce2 "reproduire le bogue")
 (bug-report-field-environment "Environnement")
 (bug-report-field-tools "Outils")
 (bug-report-field-docs-installed "Documentations install�es")
 (bug-report-field-language "Language")
 (bug-report-field-teachpacks "Teachpacks")
 (bug-report-field-collections "Collections")
 (bug-report-field-human-language "Language humain")
 (bug-report-field-version "Version")
 (bug-report-synthesized-information "Information Synth�tis�e")  ;; dialog title
 (bug-report-show-synthesized-info "Montrer l'information synth�tis�e")
 (bug-report-submit "Soumettre")
 (sending-bug-report "Soumission du formulaire de bogue en cours...")
 (error-sending-bug-report "Erreur durant la soumission du formulaire de bogue.")
 (error-sending-bug-report-expln "Une erreur s'est produite pendant la soumission de votre formulaire de bogue. Si votre connexion Internet fonctionne correctement, veuillez visiter :\n\n    http://bugs.plt-scheme.org/\n\net soumettre votre bogue en utilisant notre formulaire web en ligne. Je suis vraiment profond�ment d�sol� pour toutes vos difficult�s.\n\nLe message d'erreur est :\n~a")
 (bug-report-sent "Le bogue a �t� correctement soumis.")
 (bug-report-sent-detail "Merci pour votre soumission. Vous devriez recevoir une confirmation de votre soumission par email d'ici 30 minutes. Si vous ne recevez pas cette confirmation, veuillez envoyer un email � scheme@plt-scheme.org.")
 (illegal-bug-report "Formulaire de soumission de bogue incomplet.")
 (pls-fill-in-field "Merci de compl�ter le champ \"~a\".")
 (malformed-email-address "Adresse email malform�e.")
 (pls-fill-in-either-description-or-reproduce "Veuillez remplir soit le champ \"Description\", soit le champ \"Etapes � suivre pour reproduire le bogue\".")
 
 ;;; check syntax
 (check-syntax "V�rifier") ; "Syntaxe" ; "Correcteur de syntaxe" est long...
 (cs-italic "Italique")
 (cs-bold "Gras")
 (cs-underline "Soulign�")
 (cs-change-color "Changer la couleur")
 (cs-tack/untack-arrow "Coller/d�coller les fl�ches")
 (cs-jump "Suivre la fl�che")
 (cs-error-message "Message d'erreur")
 (cs-open-file "Ouvrir ~a")
 (cs-rename-var "Renommer ~a")
 (cs-rename-id "Renommer l'identifieur")
 (cs-rename-var-to "Renommer ~a en :")
 (cs-name-duplication-error "Le nouveau nom que vous avez choisi, ~s, est en conflit avec un autre nom pr�existant dans le m�me contexte.")
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Ramassage") ; de miettes
 (read-only "Lecture seulement")
 (read/write "Lecture/�criture")
 (auto-extend-selection "Autos�lection") ; "S�lection auto-�tendable" ?
 (overwrite "Correction") ; vs Insertion ? surimpression ?
 (running "en cours")
 (not-running "en attente") ; "en attente" ; pause ?
 
 ;;; misc
 (welcome-to-something "Bienvenue dans ~a.")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Bienvenue dans DrScheme, version ~a, ~a.")
 
 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Bienvenue dans DrScheme")
 (version/language "version ~a, ~a.")
 
 (goto-line "Aller � la ligne")
 (goto-line-invalid-number
  "~a n'est pas un num�ro de ligne valide. Ce doit �tre un entier entre 1 et ~a.")
 (goto-position "Aller � la position")
 (no-full-name-since-not-saved
  "Le fichier n'a pas encore de nom complet car il n'a pas encore �t� sauvegard�.")
 (cannot-open-because-dne "Impossible d'ouvrir ~a car le fichier n'existe pas.")
 (interactions-out-of-sync
  "ATTENTION : la fen�tre d'interaction et la fen�tre de d�finition ne sont pas synchronis�es. Cliquez sur Ex�cuter.")
 (file-is-not-saved "Le fichier \"~a\" n'a pas �t� sauvegard�.")
 (save "Sauvegarder")
 (please-choose-either "Choisissez \"~a\" ou \"~a\".")
 (close-anyway "Fermer quand m�me")
 (clear-anyway "Effacer quand m�me")
 
 ;; menu item title
 (log-definitions-and-interactions "Enregistrer les d�finitions et interactions...")
 (stop-logging "Stopper l'enregistrement")
 (please-choose-a-log-directory "S�lectionnez un r�pertoire d'enregistrement")
 (logging-to "Enregistrer dans : ")
 (erase-log-directory-contents "Effacer le contenu du r�pertoire d'enregistrement : ~a?")
 (error-erasing-log-directory "Erreur durant l'effacement du contenu du r�pertoire d'enregistrement.\n\n~a\n")

 (url "URL")
 (url: "URL :")
 (open-url... "Ouvrir l'URL...")
 (open-url "Ouvrir l'URL")
 (browse... "Naviguer...")
 (bad-url "URL incorrect")
 (bad-url:this "URL incorrect : ~a")
 
 ;; Help Desk
 (help "Aide")
 (help-desk "Aide")
 (plt:hd:search-results "R�sultats de la recherche")
 (plt:hd:search "Chercher")
 (plt:hd:search-for "Chercher")
 (plt:hd:lucky "Chanceux!")
 (plt:hd:feeling-lucky "D'humeur chanceuse")
 (plt:hd:stop "Stop")
 (plt:hd:options "Options")
 (plt:hd:configure "Configurer")
 (plt:hd:home "Page principale de l'Aide")
 (plt:hd:show-manuals "Liste des manuels")
 (plt:hd:send-bug-report "Signaler un bogue") ; Envoyer un formulaire de bogue
 (plt:hd:query-bug-reports "Bogues connus")
 ; next 3 are popup menu choices at bottom of help desk window
 (plt:hd:search-for-keyword "par mot clef")
 (plt:hd:search-for-keyword-or-index "par mot clef ou entr�e dans l'index")
 (plt:hd:search-for-keyword-or-index-or-text "par mot clef, entr�e dans l'index ou dans le texte")
 (plt:hd:exact-match "mot exact")
 (plt:hd:containing-match "contenant le mot")
 (plt:hd:regexp-match "expression r�guli�re")
 (plt:hd:find-docs-for "Chercher dans les docs :")
 (plt:hd:nothing-found-for-search-key "Rien n'a �t� trouv� pour \"~a\".")
 (plt:hd:searching "Recherche en cours...")
 (plt:hd:search-stopped "(Recherche stopp�e.)")
 (plt:hd:search-stopped-too-many-matches "(Recherche stopp�e - trop d'entr�es ont �t� trouv�es.)")
 (plt:hd:nothing-found-for "Rien n'a �t� trouv� pour ~a.")
 (plt:hd:error-finding-docs "Documentation introuvable.\n\n~a")
 (plt:hd:and "et")
 (plt:hd:refresh "rafra�chir") 
 (plt:hd:refresh-all-manuals "rafra�chir tous les manuels")
 (plt:hd:manual-installed-date "(install� le ~a)")
 ; Help Desk configuration
 (plt:hd:configuration "Configuration de l'Aide")
 (plt:hd:no-frames "Pas de cadres")
 (plt:hd:use-frames "Utiliser des cadres")
 (plt:hd:use-html-frames "Utiliser des cadres HTML")
 (plt:hd:search-pane-options "Options pour le cadre de recherche")
 (plt:hd:height "Hauteur")
 (plt:hd:bg-color "Couleur de fond")
 (plt:hd:pixels "pixels")
 (plt:hd:text-color "Couleur du texte")
 (plt:hd:link-color "Couleur des liens")
 (plt:hd:text-sample "Le texte dans le cadre de recherche appara�t dans cette couleur")
 (plt:hd:link-sample "Les liens dans le cadre de recherche appara�ssent dans cette couleur")
 (plt:hd:save-changes "Sauvegarder les modifications")
 (plt:hd:reset "R�initialisation des options")
 (plt:hd:defaults "Valeurs par d�faut")
 (plt:hd:javascript-note
    "Les choix que vous faites sont montr�s ici si vous avez Javascript et un navigateur r�cent")
 ;; refreshing manuals
 (plt:hd:refresh-downloading "T�l�chargement de ~a")
 (plt:hd:refresh-deleting "Effacement de l'ancienne version de ~a")
 (plt:hd:refresh-installing "Installation de la nouvelle version de ~a")
 (plt:hd:refresh-progress "Progr�s du t�l�chargement des manuels")
 (plt:hd:refresh-done "Fin du t�l�chargement des manuels par CVS")
 (plt:hd:refresh-installation-log "Journal de l'installation")
 (plt:hd:refresh-stopped "T�l�chargement des manuels stopp�")

 ; help desk htty proxy
 (http-proxy "Proxy HTTP")
 ;(proxy-direct-connection "Connexion directe") ; more precise: "for HTTP downloads"
 ;(proxy-use-proxy "Utiliser le proxy :") ; more precise: "for HTTP downloads"
 (proxy-host "Machine")
 (proxy-port "Port")
 (proxy-bad-host "Mauvaise machine proxy")
 
 ;; browser
 (rewind-in-browser-history "Retourner")
 (forward-in-browser-history "Avancer")
 (home "Maison")
 (browser "Navigateur")
 (choose-browser "Choisissez un navigateur")
 (external-browser-choice-title "Navigateur externe") ; title for radio-button set
 (browser-command-line-label "Ligne de commande :") ; label for radio button that is followed by text boxes
 (no-browser "Demander plus tard")
 (use-internal-browser-for-help "Lire l'Aide � l'aide du navigateur PLT interne") ; radio-button label
 (use-external-browser-for-help "Lire l'Aide � l'aide d'un navigateur externe") ; radio-button label
 (browser-cmdline-expl-line-1 "(La ligne de commande est la concat�nation du pr�fixe, de l'URL,") ; explanatory text for dialog, line 1
 (browser-cmdline-expl-line-2 "et du suffixe, sans espace additionel entre eux.)") ; ... line 2. (Anyone need more lines?)
 (cannot-display-url "Impossible de montrer l'URL ~s : ~a")
 (install? "Installer ?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "Vous avez s�lectionn� un logiciel qui peut �tre install�.") ; package => paquetage, pas tres clair...
 (do-you-want-to-install-it? "Voulez-vous l'installer ?")
 (paren-file-size "(Le fichier fait ~a octets)")
 (download-and-install "T�l�charger && Installer") ;; button label
 (download "T�l�charger") ;; button label
 (save-downloaded-file/size "Sauvegarder le fichier t�l�charg� (~a octets) sous le nom") ;; label for get-file dialog
 (save-downloaded-file "Sauvegarder le fichier t�l�charg� sous le nom")  ;; label for get-file dialog
 (downloading "T�l�chargement") ;; dialog title
 (downloading-file... "T�l�chargement du fichier en cours...")
 (package-was-installed "Le logiciel � �t� install�.")
 (download-was-saved "Le fichier t�l�charg� � �t� sauvegard�.")
 (getting-page "Page en cours de r�ception") ;; dialog title
 
 (install-plt-file-menu-item... "Installer un fichier .plt...")
 (install-plt-file-dialog-title "Installer un fichier .plt")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "Fichier")
 (install-plt-filename "Nom de fichier :")
 (install-plt-url "URL :")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "Installer ~a ou l'ouvrir pour �dition ?")
 (install-plt-file/yes "Installation")
 (install-plt-file/no "Edition")
 
 (plt-installer-progress-window-title "Progresssion de l'installation") ;; frame title
 (plt-installer-abort-installation "Abandonner l'installation") ;; button label
 (plt-installer-aborted "Installation abandonn�e.") ;; msg that appears in the installation window when installation is aborted

 ;;; about box
 (about-drscheme-frame-title "A propos de DrScheme")
 (take-a-tour "Faire un tour !")
 (release-notes "Notes pour la r�vision")
 (parenthetical-last-version "(version pr�c�dente ~a)")
 (parenthetical-last-language "(language pr�c�dent ~a)")
 (parenthetical-last-version/language "(version pr�c�dente ~a, language pr�c�dent ~a)")
 
  
 ;;; save file in particular format prompting.
 (save-as-plain-text "Sauvegarder ce fichier au format texte ?")
 (save-in-drs-format "Sauvegarder ce fichier au format DrScheme (non-texte) ?")
 (yes "Oui")
 (no "Non")
 
 ;;; preferences
 (preferences "Pr�f�rences")
 (saving-preferences "Sauvegarde des pr�f�rences")
 (error-unmarshalling "Erreur durant la dess�rialisation de la pr�f�rence ~a.")
 (error-saving-preferences "Erreur durant la sauvegarde des pr�f�rences : ~a.")
 (error-reading-preferences "Erreur durant la lecture des pr�f�rences.")
 (expected-list-of-length2 "esp�rait une liste de longueur 2.")
 (scheme-prefs-panel-label "Scheme")
 (warnings-prefs-panel-label "Avertissements")
 (editor-prefs-panel-label "Edition")
 (general-prefs-panel-label "G�n�ral")
 (highlight-parens "Griser les paires de parenth�ses.")
 (fixup-parens "Corriger les parenth�ses.")
 (flash-paren-match "Montrer le pairage de parenth�ses.")
 (auto-save-files "Sauvegarde automatique des fichiers.")
 (backup-files "Fichiers de sauvegarde")
 (map-delete-to-backspace "La touche Delete g�n�re Backspace.")
 (verify-exit "Confirmation pour quitter.")
 (ask-before-changing-format "Confirmation avant de changer le format de sauvegarde.")
 (wrap-words-in-editor-buffers "Continuer une longue ligne sur la ligne suivante, dans les �diteurs.")
 (show-status-line "Montrer la barre de status.")
 (count-columns-from-one "Compter les lignes et colonnes � partir de un.") 
 (display-line-numbers "Montrer le num�ro de ligne et de colonne, pas la distance depuis le d�but d'�diteur.")
 (enable-keybindings-in-menus "Raccourcis clavier dans les menus.")
 (automatically-to-ps "Imprimer automatiquement dans un fichier postscript.")
 (use-mdi "Utiliser les fen�tres MDI.") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Utiliser un dialogue s�par� pour les recherches.")
 (reuse-existing-frames "R�utiliser les fen�tre existantes lors de l'ouverture de nouveaux fichiers")
 (default-fonts "Polices par d�faut")
 
 ; should have entire alphabet
 (font-example-string "a��bc�de����fghi�jklmno�pqrstu��vwxyz")
 
 (change-font-button-label "Changer")
 (fonts "Polices")
 
 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "S�lectionnez une nouvelle police \"~a\".")
 
 (font-size-slider-label "Taille")
 (restart-to-see-font-changes "Red�marrez pour voir le changement de polices.")
 
 (font-prefs-panel-title "Police")
 (font-name "Nom de la police")
 (font-size "Taille de la police")
 (set-font "Appliquer la police...")
 (select-font-name "S�lectionnez une police")
 (example-text "Example de texte :")
 (only-warn-once "Pr�venir une fois seulement quand ex�cutions et interactions n'ont pas �t� synchronis�es.")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Attente sur le fichier de verrouillage des pr�f�rences...")
 (pref-lock-not-gone
  "Les pr�f�rences sont verrouill�es par le fichier :\n\n   ~a\n\nqui emp�che les pr�f�rences d'�tre sauvegard�es. Assurez-vous qu'aucun logiciel PLT n'est en cours d'ex�cution et effacer le fichier.")
 (still-locked-exit-anyway? "Les pr�f�rences n'ont pu �tre sauvegard�es correctement. Quitter quand m�me ?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Indentation")
 
 ; filled with define, lambda, or begin
 (enter-new-keyword "Entrez un nouveau mot clef ressemblant � ~a :")
 (x-keyword "Mot clef ~a")
 (x-like-keywords "Mots clefs ressemblant � ~a")
 
 (expected-a-symbol "esp�rait un symbole, trouv� : ~a")
 (already-used-keyword "\"~a\" est d�j� un mot clef avec une indentation sp�ciale.")
 (add-keyword "Ajouter")
 (remove-keyword "Enlever")
 
 ;;; find/replace
 (find-and-replace "Chercher et remplacer")
 (find "Chercher")
 (replace "Remplacer")
 (dock "Attacher")
 (undock "S�parer")
 (use-separate-dialog-for-searching "Utiliser un menu s�par� pour chercher.")
 (replace&find-again "Remplacer && chercher � nouveau") ;;; need double & to get a single &
 (replace-to-end "Remplacer jusqu'� la fin")
 (forward "En avant")
 (backward "En arri�re")
 (hide "Cacher")
 
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "Rechercher dans les fichiers...")
 (mfs-string-match/graphics "Une cha�ne de caract�res (y compris dans les fichiers avec graphiques)")
 (mfs-regexp-match/no-graphics "Une expression r�guli�re (fichiers texts seulement)")
 (mfs-searching... "Recherche en cours...")
 (mfs-configure-search "Configurer la recherche") ;; dialog title
 (mfs-files-section "Fichiers")   ;; section in config dialog
 (mfs-search-section "Rechercher") ;; section in config dialog
 (mfs-dir "R�pertoire")
 (mfs-recur-over-subdirectories "R�cursion dans les sous-r�pertoires")
 (mfs-regexp-filename-filter "Filtre de nom de fichiers pour les expressions r�guli�res")
 (mfs-search-string "Chercher la cha�ne de caract�res")
 (mfs-drscheme-multi-file-search "DrScheme - Recherche dans des fichiers multiples") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" n'est pas un r�pertoire")
 (mfs-open-file "Ouvrir le fichier")
 (mfs-stop-search "Stopper la recherche")
 (mfs-case-sensitive-label "Diff�rentier les lettres majuscules des minuscules.")
 (mfs-no-matches-found "Rien n'a �t� trouv�.")
 (mfs-search-interrupted "Recherche avort�e.")
 
 ;;;reverting a file
 (error-reverting "DrScheme - Erreur durant le retour � l'original.")
 (could-not-read "impossible de lire \"~a\".")
 (are-you-sure-revert
  "Etes-vous certain de vouloir retourner � la version de ce fichier qui est sur le disque dur ? Ce changement ne pourra pas �tre d�fait.")
 (are-you-sure-revert-title
  "Retourner ?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Erreur durant la sauvegarde") ;; title of error message dialog
 (error-saving-file/name "Une erreur s'est produite durant la sauvegarde de ~a.")
 (error-loading "Erreur durant le chargement")
 (error-loading-file/name "Une erreur s'est produite durant le chargement de ~a.")
 (unknown-filename "<< inconnu >>")
 
 ;;; finder dialog
 (must-specify-a-filename "Vous devez sp�cifier un nom de fichier.")
 (file-does-not-exist "Le fichier \"~a\" n'existe pas.")
 (ask-because-file-exists "Le fichier \"~a\" existe d�j�. Voulez-vous le remplacer ?")
 (dne-or-cycle "Le fichier \"~a\" contient un r�pertoire non-existant, ou une boucle.")
 (get-file "Obtenir fichier")
 (put-file "Donner fichier")
 (full-pathname "Chemin de fichier complet")
 (show-dot-files "Montrer les fichiers et r�pertoires dont le nom commence par un point.")
 (up-directory-button-label "R�pertoire parent")
 (add-button-label "Ajouter") ;;; for multi-file selection
 (add-all-button-label "Ajouter tous") ;;; for multi-file selection
 (remove-button-label "Enlever") ;;; for multi-file selection
 (file-wrong-form "Le format de ce nom de fichier est incorrect.")
 (select-files "S�lectionnez des fichiers")
 (select-file "S�lectionnez un fichier")
 (dir-dne "Ce r�pertoire n'existe pas.")
 (file-dne "Ce fichier n'existe pas.")
 (empty-filename "Le nom de fichier doit contenir au moins quelques lettres.")
 (that-is-dir-name "Ceci est un nom de r�pertoire.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrScheme's menus will appear
 ;;; in the wrong order.
 (file-menu "Fichier")
 (edit-menu "Editer")
 (help-menu "Aide")
 (windows-menu "Fen�tres")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.
 
 (file-menu-label-windows "Fichier")
 (file-menu-label-other "F&ichier")
 
 (new-info  "Ouvrir un nouveau fichier.")
 (new-menu-item "&Nouvelle fen�tre")
 (new-...-menu-item "&Nouvelle...")
 
 (open-info "Ouvrir un fichier � partir du disque dur.")
 (open-menu-item "&Ouvrir")
 (open-here-menu-item "&Ouvrir ici...")
 
 (open-recent-info "Une liste des fichiers ouverts r�cemment.")
 (open-recent-menu-item "Ouvrir r�cent")
 
 (revert-info "Retour � la version originale de ce fichier sur le disque dur.")
 (revert-menu-item "&Retour version disque")
 
 (save-info "Sauvegarder ce fichier sur le disque dur.")
 (save-menu-item "&Sauvegarder")
 
 (save-as-info "Demander un nom de fichier et sauver ce fichier sur le disque dur.")
 (save-as-menu-item "Sauvegarder �")
 
 (print-info "Envoyer ce fichier � une imprimante.")
 (print-menu-item "&Imprimer...")
 
 (close-info "Fermer ce fichier.")
 (close-menu-item "&Fermer")
 
 (quit-info "Fermer toutes les fen�tres.")
 (quit-menu-item-windows "&Quitter")
 (quit-menu-item-others "&Quitter")
 
 (edit-menu-label "&Editer")
 
 (undo-info "D�faire l'action la plus r�cente.")
 (undo-menu-item "&D�faire")
 
 (redo-info "Refaire l'action qui vient d'�tre d�faite.")
 (redo-menu-item "&Refaire")
 
 (cut-info "D�placer dans le porte-bloc les �l�ments s�lection�s, pour collage ult�rieur.")
 (cut-menu-item "&Couper")
 
 (copy-info "Copier dans le porte-bloc les �l�ments s�lection�s, pour collage ult�rieur.")
 (copy-menu-item "Co&pier")
 
 (paste-info "Coller � la place des �l�ments s�lectionn�s les �l�ments qui ont �t� copi�s ou coup�s le plus r�cemment.")
 (paste-menu-item "C&oller")
 
 (clear-info "Effacer les �l�ments s�lectionn�s sans modifier le porte-bloc ou le collage.")
 (clear-menu-item-others "Effacer")
 (clear-menu-item-windows "&Effacer")
 
 (select-all-info "S�lectionner tout le document.")
 (select-all-menu-item "&S�lectionner tout")
 
 (find-info "Rechercher une cha�ne de caract�res.")
 (find-menu-item "Rechercher...")
 
 (find-again-info "Recherche � nouveau la m�me cha�ne de caract�res.")
 (find-again-menu-item "Rechercher � nouveau")
 
 (replace-and-find-again-info "Remplacer le texte s�lectionn� et rechercher � nouveau le m�me texte.")
 (replace-and-find-again-menu-item "Remplacer && rechercher � nouveau")
 
 (preferences-info "Configurer vos pr�f�rences.")
 (preferences-menu-item "Pr�f�rences...")
 
 (keybindings-info "Montrer les raccourcis clavier actuellement actifs.")
 (keybindings-menu-item "Raccourcis clavier")
 (keybindings-frame-title "Raccourcis clavier")
 (keybindings-sort-by-name "Trier par nom")
 (keybindings-sort-by-key "Trier par raccourci")
 
 (insert-text-box-item "Ins�rer une boite texte")
 (insert-pb-box-item "Ins�rer une boite � dessin")
 (insert-image-item "Ins�rer une image...")
 (insert-comment-box-menu-item-label "Ins�rer une boite � commentaires")
 (wrap-text-item "Replier le texte")
 
 (windows-menu-label "Fe&n�tres")
 (bring-frame-to-front "Amener une fen�tre au premier plan")       ;;; title of dialog
 (bring-frame-to-front... "Amener une fen�tre au premier plan...") ;;; corresponding title of menu item
 (next-window "Fen�tre suivante")
 (previous-window "Fen�tre pr�c�dente")
 (most-recent-window "Fen�tre la plus r�cente")
 
 (show-menu-label "&Montrer")
 (show-overview "Montrer le contour") 
 (hide-overview "Cacher le contour")
 
 (help-menu-label "&Aide")
 (about-info "Auteurs et d�tails concernant ce logiciel.")
 (about-menu-item "A propos de ...")
 (help-menu-check-for-updates "Regarder les mises � jour...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Voulez-vous cr�er une nouvelle fen�tre ou effacer celle-ci ?")
 (clear-current "Effacer celle-ci")
 (new-window "Nouvelle fen�tre")
 
 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Quitter")
 (quit "Quitter")
 (are-you-sure-exit "Etes-vous certain de vouloir quitter ?")
 (are-you-sure-quit "Etes-vous certain de vouloir quitter ?")
 
 ;;; autosaving
 (error-autosaving "Erreur durant l'auto-sauvegarde de \"~a\".")
 (autosaving-turned-off "L'auto-sauvegarde est suspendue\njusqu'� ce que le fichier soit sauvegard�.")
 (recover-autosave-files-frame-title "Recouvrer des fichiers auto-sauvegard�s")
 (autosave-details "D�tails")
 (autosave-recover "Recouvrer")
 (autosave-unknown-filename "<<inconnu>>")
  
 ;; these are labels in a dialog that drscheme displays
 ;; if you have leftover autosave files. to see the dialog,
 ;; start up drscheme and modify (but don't save) a file
 ;; (also, do this with an unsaved file). Wait for the autosave
 ;; files to appear (typically 5 minutes). Kill DrScheme
 ;; and restart it. You'll see the dialog
 (autosave-autosave-label: "Fichier auto-sauvegard� :")
 (autosave-original-label: "Fichier original :")
 (autosave-autosave-label "Fichier auto-sauvegard�")
 (autosave-original-label "Fichier original")
 (autosave-compare-files "Comparer les fichiers auto-sauvegard�s")

 (autosave-show-autosave "Auto-sauvegarder un fichier") ;; title of a window showing the autosave file
 
 (autosave-explanation "DrScheme a trouv� des fichiers auto-sauvegard�s, qui peuvent contenir votre travail non-sauvegard�.")
 
 (autosave-recovered! "Recouvr� !") ;; status of an autosave file
 (autosave-deleted "Effac�")       ;; status of an autosave file

 (autosave-error-deleting "Erreur durant l'effacement de ~a\n\n~a") ;; first is a filename, second is an error message from mz.
 (autosave-are-you-sure-delete? "Etes-vous certain de vouloir effacer ~a?") ;; ~a is a filename
 (autosave-delete-button "Effacer")
 (autosave-delete-title "Effacer")  ;; title of a dialog asking for deletion confirmation
 (autosave-done "Fait")
  
 ;; appears in the file dialog
 (autosave-restore-to-where? "S�lectionnez un r�pertoire o� sauvegarder le fichier auto-sauvegard�.")
 
  
 ;;; file modified warning
 (file-has-been-modified
  "Ce fichier a �t� modifi� depuis sa derni�re sauvegarde. Voulez-vous �craser les modifications ?")
 (overwrite-file-button-label "Ecraser")
 
 (definitions-modified 
  "Le texte de la fen�tre de d�finition a �t� modifi� directement sur le disque dur. Sauvegardez ou retournez � la version sur le disque.")
 (drscheme-internal-error "Erreur interne de DrScheme.")
 
 ;;; tools
 (invalid-tool-spec "La sp�cification d'outil qui se trouve dans le fichier info.ss de la collection ~a est invalide. Esp�rait soit une cha�ne de caract�res, soit une liste de cha�nes de caract�res, trouv� : ~e")
 (error-loading-tool-title "DrScheme - Erreur durant le chargement de l'outil ~s; ~s")
 (error-invoking-tool-title "Erreur durant l'invocation de l'outil ~s;~s")
 (tool-tool-names-same-length
  "`tool-names' et `tools' ne sont pas des listes de la m�me longueur, dans le fichier info.ss pour ~s. Trouv� ~e et ~e")
 (tool-tool-icons-same-length
  "`tool-icons' et `tools' ne sont pas des listes de la m�me longueur, dans le fichier info.ss pour ~s. Trouv� ~e et ~e")
 (tool-tool-urls-same-length
  "`tool-urls' et `tools' ne sont pas des listes de la m�me longueur, dans le fichier info.ss pour ~s. Trouv� ~e et ~e")
 (error-getting-info-tool
  "erreur durant le chargement du fichier info.ss pour ~s")
 (tool-error-phase1 "Erreur durant la phase 1 pour l'outil ~s; ~s")
 (tool-error-phase2 "Erreur durant la phase 2 oour l'outil ~s; ~s")
 
  
 ;;; define popup menu
 (end-of-buffer-define "<< fin du tampon >>")
 (sort-by-name "Trier par nom")
 (sort-by-position "Trier par position dans le fichier")
 (no-definitions-found "<< aucune d�finition trouv�e >>")
 (jump-to-defn "Aller � la d�finition de ~a")
 
 (recent-items-sort-by-age "Trier par age")
 (recent-items-sort-by-name "Trier par nom")
 
 ;;; show menu
 (hide-definitions-menu-item-label "Cacher les &d�finitions")
 (show-definitions-menu-item-label "Montrer les &d�finitions")
 (definitions-menu-item-help-string "Cacher/montrer la fen�tre de d�finition")
 (show-interactions-menu-item-label "Montrer les &interactions")
 (hide-interactions-menu-item-label "Cacher les &interactions")
 (interactions-menu-item-help-string "Montrer/cacher la fen�tre d'interaction")
 
 ;;; file menu
 (save-definitions-as "Sauvegarder les d�finitions...")
 (save-definitions "&Sauvegarder les d�finitions")
 (print-definitions "&Imprimer les d�finitions...")
 (about-drscheme "A propos de DrScheme")
 (save-other "Sauvegarder autre")
 (save-definitions-as-text "Sauvegarder les d�finitions au format texte...")
 (save-interactions "Sauvegarder les interactions")
 (save-interactions-as "Sauvegarder les interactions...")
 (save-interactions-as-text "Sauvegarder les interactions au format texte...")
 (print-interactions "Imprimer les interactions...")
 
 ;;; edit-menu
 (split-menu-item-label "Di&viser")
 (collapse-menu-item-label "&Rassembler")
 
 ;;; language menu
 (language-menu-name "&Language")
 
 ;;; scheme-menu
 (scheme-menu-name "&Scheme")
 (execute-menu-item-label "Ex�cuter")
 (execute-menu-item-help-string "R�ex�cuter le program de la fen�tre de d�finition.")
 (break-menu-item-label "Stopper")
 (break-menu-item-help-string "Stopper l'ex�cution.")
 (kill-menu-item-label "Tuer")
 (kill-menu-item-help-string "Tuer l'ex�cution.")
 (clear-error-highlight-menu-item-label "Effacer le surlignage d'erreur")
 (clear-error-highlight-item-help-string "Efface le surlignage rose apr�s une erreur")
 (reindent-menu-item-label "&R�indenter")
 (reindent-all-menu-item-label "R�indenter &tout")
 (semicolon-comment-out-menu-item-label "&Commenter � l'aide de points-virgules")
 (box-comment-out-menu-item-label "&Commenter � l'aide d'une boite")
 (uncomment-menu-item-label "&D�commenter")
 
 (convert-to-semicolon-comment "Convertir en un commentaire avec points-virgules")
 
 ;;; executables
 (create-executable-menu-item-label "Cr�er un ex�cutable...")
 (create-executable-title "Cr�er un ex�cutable")
 (must-save-before-executable "Vous devez sauvegarder votre programme avant de cr�er un ex�cutable.")
 (save-an-executable "Sauvegarder un ex�cutable")
 (save-a-mred-launcher "Sauvegarder un lanceur de type MrEd")
 (save-a-mzscheme-launcher "Sauvegarder un lanceur de type MzScheme")
 (save-a-mred-stand-alone-executable "Sauvegarder un ex�cutable autonome de type MrEd")
 (save-a-mzscheme-stand-alone-executable "Sauvegarder un ex�cutable autonome de type MzScheme")
  
 (definitions-not-saved "La fen�tre de d�finition n'a pas �t� sauvegard�e. L'ex�cutable va utiliser la derni�re version sauvegard�e de la fen�tre de d�finition. Continuer ?")
 (inline-saved-program-in-executable?
  "Ins�rer dans l'ex�cutable le programme sauvegard� ? Si oui, vous pourrez copier l'ex�cutable sur un autre ordinateur ~a, mais l'ex�cutable sera probablement gros. Si vous choisissez non, vous ne pourrez pas copier l'ex�cutable sur un autre ordinateur, mais il sera bien plus petit. De plus, si vous choisissez non, l'ex�cutable utilisera toujours la version la plus r�cente du programme.")
 (use-mred-binary?
  "Utiliser le binaire de mred pour cet ex�cutable ?\n\nSi oui, votre program peut utiliser la biblioth�que (lib \"mred.ss\" \"mred\"). Si non, DrScheme va choisir mzscheme comme binaire pour cet ex�cutable.\n\nASi vous n'�tes pas s�r, choisissez oui.")
 (inline-saved-program-in-executable/windows/path
  "ATTENTION ! L'ex�cutable g�n�r� a besoin de trois DLLs : libmred.dll, libmzsch.gll et libgc.dll, qui sont localis�es dans\n\n~a\n\nL'ex�cutable trouve les DLLs soit dans le r�pertoire ou se trouve l'ex�cutable soit en utilisant la variable d'environnement PATH.\n\nQuand vous avez install� DrScheme, l'installateur a modifi� le PATH de l'utilisateur pour y inclure le r�pertoire dans lequel les DLLs se trouvent. M�fiez-vous des changements de configuration ou des changements faits par l'utilisateur depuis l'installation.\n\nSi vous d�placez l'ex�cutable sur une autre machine, vous devez �galement copier les DLLS sur l'autre machine, soit dans le m�me r�pertoire que l'ex�cutable, soit dans un r�pertoire qui figure dans le PATH sur l'autre machine.")
 (launcher "Lanceur")
 (stand-alone "Autonome")
 (executable-type "Type")
 (executable-base "Base")
 (filename "Nom de fichier : ")
 (create "Cr�er")
 (please-choose-an-executable-filename "Veuillez s�lectionner un nom de fichier pour sauvegarder l'ex�cutable.")
 (windows-executables-must-end-with-exe
  "Le nom de fichier\n\n  ~a\n\nest ill�gal. Sous Windows, le nom d'un ex�cutable doit se terminer par .exe.")
 (macosx-executables-must-end-with-app
  "Le nom de fichier\n\n  ~a\n\nest ill�gal. Sous MacOS X, le nom d'un ex�cutable doit se terminer par .app.")
 (warning-directory-will-be-replaced
  "ATTENTION : le r�pertoire :\n\n  ~a\n\nva �tre remplac�. Voulez-vous continuer ?")
 
 (create-servlet "Cr�er un servlet...") ;servlet = greffon, extension serveur?
 
 ;;; buttons
 (execute-button-label "Ex�cuter") 
 (save-button-label "Sauvegarder")
 (break-button-label "Stopper")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Rechercher \"~a\" dans l'Aide.")
 (exact-lucky-search-help-desk-for "Faire une recherche \"J'ai de la chance\" dans l'Aide pour le texte exact \"~a\".")
 
 ;; collapse and expand popup menu items
 (collapse-sexp "R�tr�cir une sexpression")
 (expand-sexp "Elargir une sexpression")
 
 ;;; fraction dialog
 (enter-fraction "Entrer une fraction")
 (whole-part "Partie enti�re")
 (numerator "Num�rateur")
 (denominator "D�nominateur")
 (invalid-number "Nombre invalide : doit �tre un nombre r�el exact non-entier.")
 (insert-fraction-menu-item-label "Ins�rer une fraction...")
 
 ;; number snip popup menu
 (show-decimal-expansion "Montrer l'expansion d�cimale")
 (show-fraction-view "Montrer sous forme de fraction")
 (show-mixed-fraction-view "Montrer sous forme partie-enti�re plus fraction")
 (show-improper-fraction-view "Montrer sous forme de fraction")
 (show-more-decimal-places "Montrer plus de d�cimales")
 
 ;;; Teachpack messages
 (select-a-teachpack "S�lectionner un teachpack")
 (clear-teachpack "Enlever le teachpack ~a")
 (teachpack-error-label "DrScheme - erreur avec un teachpack.")
 (teachpack-dne/cant-read "Le fichier teachpack ~a n'existe pas ou n'est pas lisible.")
 (teachpack-didnt-load "Le fichier teachpack ~a n'a pas �t� correctement charg�.")
 (teachpack-error-invoke "Le fichier teachpack ~a a produit une erreur au moment de son invocation.")
 (add-teachpack-menu-item-label "Ajouter un teachpack...")
 (clear-all-teachpacks-menu-item-label "Enlever tous les teachpacks")
 (drscheme-teachpack-message-title "DrScheme teachpack")
 (already-added-teachpack "Le teachpack ~a a d�j� �t� ajout�.")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Veuillez s�lectionner un language. Un �tudiant dans un cours d'introduction pr�f�rera le language par d�faut.")
 (language-dialog-title "Configurer le language")
 (case-sensitive-label "Diff�rentier les lettres majuscules des minuscules.")
 (output-style-label "Style d'impression des r�sultats")
 (constructor-printing-style "Constructeur")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (print-printing-style "current-print")
 (sharing-printing-label "Montrer le partage entre valeurs.")
 (use-pretty-printer-label "Ins�rer des retours-chariots lors de l'impression des r�sultats.")
 (input-syntax "Syntaxe d'entr�e")
 (dynamic-properties "Propri�t�s dynamiques")
 (output-syntax "Syntaxe de sortie")
 (no-debugging-or-profiling "Pas de d�bogage ou profilage") ; Profilage. Eurk...
 (debugging "D�bogage")
 (debugging-and-profiling "D�bogage et profilage")
 (whole/fractional-exact-numbers-label "Imprimer les nombres sous forme de fractions.")
 (booleans-as-true/false-label "Imprimer les bool�ens sous forme true et false.")
 (show-details-button-label "Montrer les d�tails")
 (hide-details-button-label "Cacher les d�tails")
 (choose-language-menu-item-label "S�lectionner le language...")
 (revert-to-language-defaults "Retourner aux valeurs par d�faut pour le language.")
 (language-docs-button-label "Docs languages")
 (fraction-style "Style de fractions")
 (use-mixed-fractions "Fractions m�l�es")
 (use-repeating-decimals "D�cimales r�p�titives")
 (decimal-notation-for-rationals "Utiliser la notation d�cimale pour les nombres rationnels")
 (please-select-a-language "Veuillez s�lectionner un language")

  
 ;;; languages
 (beginning-student "Etudiant niveau d�butant")
 (beginning-one-line-summary "define, cond, structs, constantes, et primitives")
 (beginning-student/abbrev "Etudiant niveau d�butant avec abr�viations pour les listes")
 (beginning/abbrev-one-line-summary "D�butant, avec impression des r�sultats dans le REPL sous forme de listes")
 (intermediate-student "Etudiant niveau interm�diaire")
 (intermediate-one-line-summary "D�butant plus port�e lexicale")
 (intermediate-student/lambda "Etudiant niveau interm�diaire, plus lambda")
 (intermediate/lambda-one-line-summary "Interm�diaire plus fonctions d'ordre sup�rieur")
 (advanced-student "Etudiant niveau avanc�")
 (advanced-one-line-summary "Interm�diaire plus lambda et mutation")
 (full-language "Complet") ;; also in the HtDP languages section
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "R5RS et languages semblabes")
 (pretty-big-scheme "Assez gros Scheme")
 (pretty-big-scheme-one-line-summary "Graphique, plus de nombreuses biblioth�ques standards")
 (r5rs-lang-name "Standard (R5RS)")
 (r5rs-one-line-summary "R5RS, de base")
 (unknown-debug-frame "[inconnu]")
 
 (module-language-one-line-summary "Language avec module comme seule forme")
 
 ;;; debug language
 (backtrace-window-title "Trace - DrScheme")
 (files-interactions "les interactions de ~a") ;; filled with a filename
 (current-interactions "interactions")
 (current-definitions "d�finitions")
 (mzscheme-w/debug "Textuel (MzScheme)")
 (mzscheme-one-line-summary "PLT Scheme sans la biblioth�que graphique")
 (mred-w/debug "Graphique (MrEd)")
 (mred-one-line-summary "PLT Scheme plus la biblioth�que graphique")
 
 ;; profiling
 (profiling-low-color "Bas")
 (profiling-high-color "Elev�")
 (profiling-choose-low-color "S�lectionnez une couleur pour Bas")
 (profiling-choose-high-color "S�lectionnez une couleur pour Elev�")
 (profiling "Profilage")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "Gamme de couleurs pour le profil") 
 (profiling-scale "Echelle de couleurs pour le profil")
 (profiling-sqrt "Racine Carr�e")
 (profiling-linear "Lin�aire")
 (profiling-square "Quadratique")
 (profiling-number "Numbre d'appels de fonctions")
 (profiling-time "Temps cumulatif")
 (profiling-clear "Effacer le profil")
 (profiling-update "Mettre � jour le profil")
 (profiling-col-percent-time "% Temps")
 (profiling-col-function "Fonction")
 (profiling-col-name "Nom")
 (profiling-col-time-in-msec "ms")
 (profiling-col-calls "Appels de fonctions")
 (profiling-show-profile "Montrer le profil")
 (profiling-hide-profile "Cacher le profil")
 (profiling-unknown-src "<< inconnu >>")
 (profiling-no-information-available "Pas d'information de profilage disponible. Assurez vous que l'option de profilage ait �t� sp�cifi�e pour ce language et que vous ayez ex�cut� le programme.")
 (profiling-clear? "Modifier le contenu de la fen�tre de d�finition invalide le profil. Voulez-vous continuer ?")
 
 ;;; repl stuff
 (evaluation-terminated "Evaluation termin�e.")
 (evaluation-terminated-explanation
  "Le thread d'�valuation n'est plus en ex�cution, toute �valuation est donc impossible jusqu'� la prochaine ex�cution.")
 (last-stack-frame "Montrer le dernier appel de fonction sur la pile.")
 (last-stack-frames "Montrer les derniers ~a appels de fonction sur la pile.")
 (next-stack-frames "Montrer les ~a appels de fonction suivants sur la pile.")
 
 ;;; welcoming message in repl
 (language "Language")
 (custom "personnalis�")
 (teachpack "Teachpack")
 (welcome-to "Bienvenue dans")
 (version "version")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Voulez-vous tuer l'�valuation ?")
 (just-break "Simplement stopper")
 (kill "Tuer")
 (kill? "Tuer ?")
 
 ;;; version checker
 (vc-update-check "V�rification des mises � jour")
 (vc-check-prompt "Regarder sur Internet pour d'�ventuelles mises � jour des logiciels PLT?")
 (vc-please-wait "Veuillez patienter")
 (vc-connecting-version-server "Connexion au server de version de PLT en cours")
 (vc-network-timeout "Expiration du compte-�-rebours lors de la connexion r�seau") 
 (vc-cannot-connect "Impossible de se connecter au serveur de version de PLT")
 (vc-network-failure "Erreur r�seau")
 (vc-old-binaries "Les fichiers binaires install�s pour DrScheme (ou MzScheme) ne sont pas � jour")
 (vc-binary-information-format "Version binaire install�e : ~a (it�ration ~a)")
 (vc-details-format "~a~nD�tails :~n~a")
 (vc-details-text "D�tails :~n")
 (vc-error-format "Erreur : ~a") 
 (vc-current-format "~a v.~a (it�ration ~a) est � jour")
 (vc-update-format "~a v.~a (it�ration ~a) doit �tre remplac� par v.~a (it�ration ~a)")
 (vc-binary-name "Binaire")
 (vc-updates-available "Les mises � jour sont disponibles �")
 (vc-latest-binary-information-format "Version la plus r�cente : ~a (it�ration ~a)")
 (vc-update-dialog-title "Etat des mises � jour")
 (vc-need-update-string "Un ou plusieurs des logiciels PLT install�s doivent �tre mis � jour")
 (vc-no-update-string "Tous les logiciels PLT install�s sont � jour")
 
 ;; special menu
 (special-menu "Sp�cial")
 
 ;; large semi colon letters
 (insert-large-letters... "Inserer de grandes lettres...")
 (large-semicolon-letters "Grandes lettres en points-virgules")
 (text-to-insert "Texte � inserer")
 
 (module-browser-filename-format "Nom de fichier complet : ~a (~a lignes)")
 (module-browser-root-filename "Nom de fichier de la racine : ~a")
 (module-browser-font-size-gauge-label "Taille de la police")
 (module-browser-progress-label "Avancement du navigateur de modules")
 (module-browser-adding-file "Ajout du fichier : ~a...")
 (module-browser-laying-out-graph-label "Tracer le graph")
 (module-browser-open-file-format "Ouvrir ~a")
 (module-browser "Navigateur de modules") ;; frame title
 (module-browser... "Navigateur de modules...") ;; menu item title
 (module-browser-error-expanding "Erreur durant l'expansion du programme :\n\n~a")
 
 (happy-birthday-matthias "Joyeux anniversaire, Matthias !")
 
 (mrflow-using-default-language-title "Language par d�faut utilis�")
 (mrflow-using-default-language "Le language actuellement utilis� n'a pas de table de types d�fini pour ses primitives. R5RS Scheme est utilis� � la place.")
 (mrflow-button-title "Analyzer")
 ;(mrflow-unknown-style-delta-error-title "Delta de Style de bo�te inconnu")
 ;(mrflow-unknown-style-delta-error "Delta de style de bo�te inconnu : ~a")
 (mrflow-coloring-error-title "Couleur inconnue")
 (mrflow-coloring-error "Pas de style d�fini pour la couleur : ~a")
 (mrflow-popup-menu-show-type "Montrer le type")
 (mrflow-popup-menu-hide-type "Cacher le type")
 (mrflow-popup-menu-show-errors "Montrer les erreurs")
 (mrflow-popup-menu-hide-errors "Cacher les erreurs")
 ;(mrflow-read-exception-title "Exception lecture")
 ;(mrflow-read-exception "Exception durant la lecture : ~a")
 ;(mrflow-syntax-exception-title "Exception syntaxique")
 ;(mrflow-syntax-exception "Exception syntaxique : ~a")
 ;(mrflow-unknown-exception-title "Exception inconnue")
 ;(mrflow-unknown-exception "Exception inconnue : ~a")
 ;(mrflow-language-primitives-error-title "Erreur pour les primitives du language")
 ;(mrflow-language-primitives-error "Mauvais nom de fichier pour la table des types des primitives du language : ~a")

 (snips-and-arrows-popup-menu-tack-all-arrows "Coller toutes les fl�ches")
 (snips-and-arrows-popup-menu-untack-all-arrows "D�coller toutes les fl�ches")
 (snips-and-arrows-user-action-disallowed-title "Insertions et effacements actuellement interdits")
 (snips-and-arrows-user-action-disallowed "Insertions et effacements sont interdits dans les editeurs qui contiennent des bo�tes inser�es par un outil.  Cachez toutes les bo�tes avant de modifier le contenu de l'editeur.")
 (snips-and-arrows-hide-all-snips-in-editor "Cacher les bo�tes de cet editeur")
 
 (xml-tool-menu "XML")
 (xml-tool-insert-xml-box "Ins�rer une bo�te XML")
 (xml-tool-insert-scheme-box "Ins�rer une bo�te Scheme")
 (xml-tool-insert-scheme-splice-box "Ins�rer une bo�te Scheme � raccord")
 (xml-tool-xml-box "Bo�te XML")
 (xml-tool-scheme-box "Bo�te Scheme")
 (xml-tool-scheme-splice-box "Bo�te Scheme � raccord")
 (xml-tool-switch-to-scheme "Changer pour une bo�te Scheme")
 (xml-tool-switch-to-scheme-splice "Changer pour une bo�te Scheme � raccord")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "Eliminer les espaces dans les d�limiteurs vides")
 (xml-tool-leave-whitespace-alone
  "Laisser les espaces tel quel")
 
 (show-recent-items-window-menu-item "Montrer les fichiers r�cemment ouverts dans une fen�tre s�par�e")
 (show-recent-items-window-label "Fichiers r�cemment ouverts")
 (number-of-open-recent-items "Nombre de fichiers r�cents")
 (switch-anyway "Changer de fichier quand m�me")
 
 (stepper-program-has-changed "Avertissement : le programme a �t� modifi�.")
 (stepper-program-window-closed "Avertissement : la fen�tre du programme a disparu.")

 (wizard-next "Suivant")
 (wizard-back "Pr�c�dent")
 (wizard-finish "Fin")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "Le programme dans la fen�tre de d�finition est toujours en cours d'ex�cution. Fermer la fen�tre quand m�me ?")
  (program-has-open-windows "Le programme dans la fen�tre de d�finition a d'autres fen�tres ouvertes. Fermer la fen�tre quand m�me ?")
 
  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<chemins de r�pertoires pour les collections par d�faut>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Choisissez un chemin de r�pertoire pour une collection")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Les chemins de r�pertoires pour les collections par d�faut sont d�j� pr�sents")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Chemins de r�pertoires pour les collections")

  ;; button labels
  (ml-cp-add "Ajouter")
  (ml-cp-add-default "Ajouter les chemins par d�faut")
  (ml-cp-remove "Enlever")
  (ml-cp-raise "Monter")
  (ml-cp-lower "Descendre")
  )
