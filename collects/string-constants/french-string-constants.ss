
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
  (is-this-your-native-language "Le Français est-il votre langue maternelle ?")
  
  (are-you-sure-you-want-to-switch-languages
   "Ceci va changer le language utilisé par l'interface graphique, ce qui va nécessiter un redémarrage de DrScheme. Etes-vous certain de vouloir continuer ?")
  
  (interact-with-drscheme-in-language "Interagir avec DrScheme en Français")
  
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
  (are-you-sure-delete? "Etes-vous certain de vouloir effacer ~a ?") ;; ~a is a filename
  (ignore "Ignorer")
  (revert "Retourner") ; revenir?
  
  ;; label for a generic check box, often supported on dialogs
  ;; that ask a binary choice of the user. If checked, the
  ;; dialog isn't going to be shown again.
  (dont-ask-again "Ne jamais redemander (utilisera toujours votre présent choix)")
  
  ;;; important urls
  (web-materials "Sites web apparentés") ;; menu item title
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
  (bug-report-field-summary "Résumé du problème")
  (bug-report-field-severity "Sévérité")
  (bug-report-field-class "Classe")
  (bug-report-field-priority "Priorité")
  (bug-report-field-description "Description")
  (bug-report-field-reproduce1 "Etapes à suivre pour")
  (bug-report-field-reproduce2 "reproduire le bogue")
  (bug-report-field-environment "Environnement")
  (bug-report-field-tools "Outils")
  (bug-report-field-docs-installed "Documentations installées")
  (bug-report-field-language "Language")
  (bug-report-field-teachpacks "Teachpacks")
  (bug-report-field-collections "Collections")
  (bug-report-field-human-language "Language humain")
  (bug-report-field-version "Version")
  (bug-report-synthesized-information "Information Synthétisée")  ;; dialog title
  (bug-report-show-synthesized-info "Montrer l'information synthétisée")
  (bug-report-submit "Soumettre")
  (sending-bug-report "Soumission du formulaire de bogue en cours...")
  (error-sending-bug-report "Erreur durant la soumission du formulaire de bogue.")
  (error-sending-bug-report-expln "Une erreur s'est produite pendant la soumission de votre formulaire de bogue. Si votre connexion Internet fonctionne correctement, veuillez visiter :\n\n    http://bugs.plt-scheme.org/\n\net soumettre votre bogue en utilisant notre formulaire web en ligne. Je suis vraiment profondément désolé pour toutes vos difficultés.\n\nLe message d'erreur est :\n~a")
  (bug-report-sent "Le bogue a été correctement soumis.")
  (bug-report-sent-detail "Merci pour votre soumission. Vous devriez recevoir une confirmation de votre soumission par email d'ici 30 minutes. Si vous ne recevez pas cette confirmation, veuillez envoyer un email à scheme@plt-scheme.org.")
  (illegal-bug-report "Formulaire de soumission de bogue incomplet.")
  (pls-fill-in-field "Merci de compléter le champ \"~a\".")
  (malformed-email-address "Adresse email malformée.")
  (pls-fill-in-either-description-or-reproduce "Veuillez remplir soit le champ \"Description\", soit le champ \"Etapes à suivre pour reproduire le bogue\".")
  
  ;;; check syntax
  (check-syntax "Vérifier") ; "Syntaxe" ; "Vérificateur de syntaxe" est long...
  (cs-italic "Italique")
  (cs-bold "Gras")
  (cs-underline "Souligné")
  (cs-change-color "Changer la couleur")
  (cs-tack/untack-arrow "Coller/décoller les flèches")
  (cs-jump-to-next-bound-occurrence "Aller à l'occurence suivante")
  (cs-jump-to-binding "Aller à l'occurence liant celle-ci")
  (cs-jump-to-definition "Aller à la définition")
  (cs-error-message "Message d'erreur")
  (cs-open-file "Ouvrir ~a")
  (cs-rename-var "Renommer ~a")
  (cs-rename-id "Renommer l'identifieur")
  (cs-rename-var-to "Renommer ~a en :")
  (cs-name-duplication-error "Le nouveau nom que vous avez choisi, ~s, est en conflit avec un autre nom préexistant dans le même contexte.")
  (cs-status-init "Vérificateur de syntaxe : initialisation de l'environnement pour le code de l'utilisateur")
  (cs-status-coloring-program "Vérificateur de syntaxe : coloriage d'une expression")
  (cs-status-eval-compile-time "Vérificateur de syntaxe : évaluation pour l'expansion") ; peut mieux faire?
  (cs-status-expanding-expression "Vérificateur de syntaxe : expansion d'une expression")
  (cs-mouse-over-variable-import "la variable ~s est importée de ~s")
  (cs-mouse-over-syntax-import "la macro ~s est importée de ~s")
  
  (cs-lexical-variable "variable lexicale")
  (cs-lexical-syntax "syntaxe lexicale")
  (cs-imported-variable "variable importée")
  (cs-imported-syntax "syntaxe importée")

  ;;; info bar at botttom of drscheme frame
  (collect-button-label "Ramassage") ; de miettes
  (read-only "Lecture seulement")
  (read/write "Lecture/écriture")
  (auto-extend-selection "Autosélection") ; "Sélection auto-étendable" ?
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
  
  (goto-line "Aller à la ligne")
  (goto-line-invalid-number
   "~a n'est pas un numéro de ligne valide. Ce doit être un entier entre 1 et ~a.")
  (goto-position "Aller à la position")
  (no-full-name-since-not-saved
   "Le fichier n'a pas encore de nom complet car il n'a pas encore été sauvegardé.")
  (cannot-open-because-dne "Impossible d'ouvrir ~a car le fichier n'existe pas.")
  (interactions-out-of-sync
   "ATTENTION : la fenêtre d'interaction et la fenêtre de définition ne sont pas synchronisées. Cliquez sur Exécuter.")
  (file-is-not-saved "Le fichier \"~a\" n'a pas été sauvegardé.")
  (save "Sauvegarder")
  (please-choose-either "Choisissez \"~a\" ou \"~a\".")
  (close-anyway "Fermer quand même")
  (clear-anyway "Effacer quand même")
  
  ;; menu item title
  (log-definitions-and-interactions "Enregistrer les définitions et interactions...")
  (stop-logging "Stopper l'enregistrement")
  (please-choose-a-log-directory "Sélectionnez un répertoire d'enregistrement")
  (logging-to "Enregistrer dans : ")
  (erase-log-directory-contents "Effacer le contenu du répertoire d'enregistrement : ~a ?")
  (error-erasing-log-directory "Erreur durant l'effacement du contenu du répertoire d'enregistrement.\n\n~a\n")
  
  ;; modes
  (mode-submenu-label "Modes")
  (scheme-mode "Mode scheme")
  (text-mode "Mode texte")
 
  (scheme-mode-color-symbol "Symbole")
  (scheme-mode-color-keyword "Mot réservé")
  (scheme-mode-color-comment "Commentaire")
  (scheme-mode-color-string "Chaîne de caractères")
  (scheme-mode-color-constant "Constante")
  (scheme-mode-color-parenthesis "Parenthèses")
  (scheme-mode-color-error "Erreur")
  (scheme-mode-color-other "Autre")
 
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
  (plt:hd:search-results "Résultats de la recherche")
  (plt:hd:search "Chercher")
  (plt:hd:search-for "Chercher")
  (plt:hd:lucky "Chanceux !")
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
  (plt:hd:search-for-keyword-or-index "par mot clef ou entrée dans l'index")
  (plt:hd:search-for-keyword-or-index-or-text "par mot clef, entrée dans l'index ou dans le texte")
  (plt:hd:exact-match "mot exact")
  (plt:hd:containing-match "contenant le mot")
  (plt:hd:regexp-match "expression régulière")
  (plt:hd:find-docs-for "Chercher dans les docs :")
  (plt:hd:nothing-found-for-search-key "Rien n'a été trouvé pour \"~a\".")
  (plt:hd:searching "Recherche en cours...")
  (plt:hd:search-stopped "(Recherche stoppée.)")
  (plt:hd:search-stopped-too-many-matches "(Recherche stoppée - trop d'entrées ont été trouvées.)")
  (plt:hd:nothing-found-for "Rien n'a été trouvé pour ~a.")
  (plt:hd:error-finding-docs "Documentation introuvable.\n\n~a")
  (plt:hd:and "et")
  (plt:hd:refresh "rafraîchir")
  (plt:hd:refresh-all-manuals "rafraîchir tous les manuels")
  (plt:hd:manual-installed-date "(installé le ~a)")
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
  (plt:hd:text-sample "Le texte dans le cadre de recherche apparaît dans cette couleur")
  (plt:hd:link-sample "Les liens dans le cadre de recherche apparaîssent dans cette couleur")
  (plt:hd:save-changes "Sauvegarder les modifications")
  (plt:hd:reset "Réinitialisation des options")
  (plt:hd:defaults "Valeurs par défaut")
  (plt:hd:javascript-note
   "Les choix que vous faites sont montrés ici si vous avez Javascript et un navigateur récent")
  ;; refreshing manuals
  (plt:hd:refresh-downloading "Téléchargement de ~a")
  (plt:hd:refresh-installing "Installation de la nouvelle version de ~a")
  (plt:hd:refresh-progress "Progrès du téléchargement des manuels")
  (plt:hd:refresh-done "Fin du téléchargement des manuels par CVS")
  (plt:hd:refresh-installation-log "Journal de l'installation")
  (plt:hd:refresh-stopped "Téléchargement des manuels stoppé")
  (plt:hd:refreshing-manuals "Retéléchargement des manuels")
  (plt:hd:refresh-downloading... "Téléchargement de ~a...")
  (plt:hd:refresh-deleting... "Effacement de l'ancienne version de ~a...")
  (plt:hd:refresh-installing... "Installation de la nouvelle version de ~a...")
  (plt:hd:refresh-clearing-indicies "Effacement des indices cachés")
  (plt:hd:refreshing-manuals-finished "Terminé.")
  (plt:hd:about-help-desk "A propos de l'Aide")
  (plt:hd:help-desk-about-string
   "L'Aide est une source complète d'information à propos des logiciels du PLT, y compris DrScheme, MzScheme et MrEd.\n\nVersion ~a\nCopyright (c) 1995-2003 PLT.")
  (plt:hd:help-on-help "Aide de l'Aide")
  (plt:hd:help-on-help-details "Pour obtenir de l'aide sur comment utiliser l'Aide, suivez le lien `How to use Help Desk' à partir de la page principale de l'Aide (pour trouver la page principale, si vous n'y êtes pas déjà, cliquez sur le boutton `Home' qui apparaît en haut de la fenêtre de l'Aide).")
  (reload "Rafraîchir")
  (plt:hd:ask-about-separate-browser
   "Vous avez sélectionné un lien vers une page sur le world-wide web. Voulez-vous voir cette page en utilisant le navigateur de l'Aide ou voulez-vous utiliser un navigateur séparé ?")
  (plt:hd:homebrew-browser "Navigateur de l'Aide") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Navigateur séparé") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "URLs externes dans l'Aide")
  (plt:hd:use-homebrew-browser "Utiliser le navigateur de l'Aide pour les URLs externes")
  (plt:hd:new-help-desk "&Nouvelle Aide")
  (plt:hd:teaching-manuals "Manuels pour étudiants")
  (plt:hd:professional-manuals "Manuels pour professionnels")
  (plt:hd:all-manuals "Tous les manuels")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Ordre de recherche dans les manuels")

  
  ; help desk htty proxy
  (http-proxy "Proxy HTTP")
  (proxy-direct-connection "Connexion directe")
  (proxy-use-proxy "Utiliser le proxy :")
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
  (use-internal-browser-for-help "Lire l'Aide à l'aide du navigateur PLT interne") ; radio-button label
  (use-external-browser-for-help "Lire l'Aide à l'aide d'un navigateur externe") ; radio-button label
  (browser-cmdline-expl-line-1 "(La ligne de commande est la concaténation du préfixe, de l'URL,") ; explanatory text for dialog, line 1
  (browser-cmdline-expl-line-2 "et du suffixe, sans espace additionel entre eux.)") ; ... line 2. (Anyone need more lines?)
  (cannot-display-url "Impossible de montrer l'URL ~s : ~a")
  (install? "Installer ?")  ;; if a .plt file is found (title of dialog)
  (you-have-selected-an-installable-package "Vous avez sélectionné un logiciel qui peut être installé.") ; package => paquetage, pas tres clair...
  (do-you-want-to-install-it? "Voulez-vous l'installer ?")
  (paren-file-size "(Le fichier fait ~a octets)")
  (download-and-install "Télécharger && Installer") ;; button label
  (download "Télécharger") ;; button label
  (save-downloaded-file/size "Sauvegarder le fichier téléchargé (~a octets) sous le nom") ;; label for get-file dialog
  (save-downloaded-file "Sauvegarder le fichier téléchargé sous le nom")  ;; label for get-file dialog
  (downloading "Téléchargement") ;; dialog title
  (downloading-file... "Téléchargement du fichier en cours...")
  (package-was-installed "Le logiciel à été installé.")
  (download-was-saved "Le fichier téléchargé à été sauvegardé.")
  (getting-page "Page en cours de réception") ;; dialog title
  
  (install-plt-file-menu-item... "Installer un fichier .plt...")
  (install-plt-file-dialog-title "Installer un fichier .plt")
  (install-plt-web-tab "Web")
  (install-plt-file-tab "Fichier")
  (install-plt-filename "Nom de fichier :")
  (install-plt-url "URL :")
  
  ;; install plt file when opened in drscheme strings
  (install-plt-file "Installer ~a ou l'ouvrir pour édition ?")
  (install-plt-file/yes "Installation")
  (install-plt-file/no "Edition")
  
  (plt-installer-progress-window-title "Progresssion de l'installation") ;; frame title
  (plt-installer-abort-installation "Abandonner l'installation") ;; button label
  (plt-installer-aborted "Installation abandonnée.") ;; msg that appears in the installation window when installation is aborted
  
  ;;; about box
  (about-drscheme-frame-title "A propos de DrScheme")
  (take-a-tour "Faire un tour !")
  (release-notes "Notes pour la révision")
  (parenthetical-last-version "(version précédente ~a)")
  (parenthetical-last-language "(language précédent ~a)")
  (parenthetical-last-version/language "(version précédente ~a, language précédent ~a)")
  
  
  ;;; save file in particular format prompting.
  (save-as-plain-text "Sauvegarder ce fichier au format texte ?")
  (save-in-drs-format "Sauvegarder ce fichier au format DrScheme (non-texte) ?")
  (yes "Oui")
  (no "Non")
  
  ;;; preferences
  (preferences "Préférences")
  (saving-preferences "Sauvegarde des préférences")
  (error-unmarshalling "Erreur durant la dessérialisation de la préférence ~a.")
  (error-saving-preferences "Erreur durant la sauvegarde des préférences : ~a.")
  (error-reading-preferences "Erreur durant la lecture des préférences.")
  (expected-list-of-length2 "espérait une liste de longueur 2.")
  (scheme-prefs-panel-label "Scheme")
  (warnings-prefs-panel-label "Avertissements")
  (editor-prefs-panel-label "Edition")
  (general-prefs-panel-label "Général")
  (highlight-parens "Surligner les paires de parenthèses.")
  (fixup-parens "Corriger les parenthèses.")
  (flash-paren-match "Montrer la parenthèse correspondante.")
  (auto-save-files "Sauvegarde automatique des fichiers.")
  (backup-files "Fichiers de sauvegarde.")
  (map-delete-to-backspace "La touche Delete génére Backspace.")
  (verify-exit "Confirmation pour quitter.")
  (ask-before-changing-format "Confirmation avant de changer le format de sauvegarde.")
  (wrap-words-in-editor-buffers "Continuer une longue ligne sur la ligne suivante, dans les éditeurs.")
  (show-status-line "Montrer la barre de status.")
  (count-columns-from-one "Compter les lignes et colonnes à partir de un.")
  (display-line-numbers "Montrer le numéro de ligne et de colonne, pas la distance depuis le début d'éditeur.")
  (enable-keybindings-in-menus "Raccourcis clavier dans les menus.")
  (automatically-to-ps "Imprimer automatiquement dans un fichier postscript.")
  (use-mdi "Utiliser les fenêtres MDI.") ;;; ms windows only -- use that window in a window thingy
  (separate-dialog-for-searching "Utiliser un dialogue séparé pour les recherches.")
  (reuse-existing-frames "Réutiliser les fenêtres existantes lors de l'ouverture de nouveaux fichiers")
  (default-fonts "Polices par défaut")
  (paren-match-color "Couleur de surlignage des parenthèses") ; in prefs dialog
  (choose-color "Sélectionnez une couleur") ; in prefs dialog
  (online-coloring-active "Colorier la syntaxe interactivement")
  (open-files-in-tabs "Ouvrir les fichiers dans de nouveaux onglets (pas dans de nouvelles fen�tres)")
  
  ; title of the color choosing dialog
  (choose-paren-highlight-color "Sélectionnez une couleur pour surligner les parenthèses")
  
  ; should have entire alphabet
  (font-example-string "aâàbcçdeéêèëfghiîïjklmnoôpqrstuûùüvwxyz")
  
  (change-font-button-label "Changer")
  (fonts "Polices")
  
  ; filled with type of font, eg modern, swiss, etc.
  (choose-a-new-font "Sélectionnez une nouvelle police \"~a\".")
  
  (font-size-slider-label "Taille")
  (restart-to-see-font-changes "Redémarrez pour voir le changement de polices.")
  
  (font-prefs-panel-title "Police")
  (font-name "Nom de la police")
  (font-size "Taille de la police")
  (set-font "Appliquer la police...")
  (font-smoothing-label  "Lissage de polices")
  (font-smoothing-none "Aucune")
  (font-smoothing-some "Certaines")
  (font-smoothing-all "Toutes")
  (font-smoothing-default "Utiliser la configuration par défaut du système")
  (select-font-name "Sélectionnez une police")
  (example-text "Example de texte :")
  (only-warn-once "Prévenir une fois seulement quand exécutions et interactions n'ont pas été synchronisées.")
  
  ; warning message when lockfile is around
  (waiting-for-pref-lock "Attente sur le fichier de verrouillage des préférences...")
  (pref-lock-not-gone
   "Les préférences sont verrouillées par le fichier :\n\n   ~a\n\nqui empêche les préférences d'être sauvegardées. Assurez-vous qu'aucun logiciel PLT n'est en cours d'exécution et effacer le fichier.")
  (still-locked-exit-anyway? "Les préférences n'ont pu être sauvegardées correctement. Quitter quand même ?")
  
  ;;; indenting preferences panel
  (indenting-prefs-panel-label "Indentation")
  
  ; filled with define, lambda, or begin
  (enter-new-keyword "Entrez un nouveau mot clef ressemblant à ~a :")
  (x-keyword "Mot clef ~a")
  (x-like-keywords "Mots clefs ressemblant à ~a")
  
  (expected-a-symbol "espérait un symbole, trouvé : ~a")
  (already-used-keyword "\"~a\" est déjà un mot clef avec une indentation spéciale.")
  (add-keyword "Ajouter")
  (remove-keyword "Enlever")
  
  ;;; find/replace
  (find-and-replace "Chercher et remplacer")
  (find "Chercher")
  (replace "Remplacer")
  (dock "Attacher")
  (undock "Séparer")
  (use-separate-dialog-for-searching "Utiliser un menu séparé pour chercher.")
  (replace&find-again "Remplacer && chercher à nouveau") ;;; need double & to get a single &
  (replace-to-end "Remplacer jusqu'à la fin")
  (forward "En avant")
  (backward "En arrière")
  (hide "Cacher")
  
  ;;; multi-file-search
  (mfs-multi-file-search-menu-item "Rechercher dans les fichiers...")
  (mfs-string-match/graphics "Une chaîne de caractères (y compris dans les fichiers avec graphiques)")
  (mfs-regexp-match/no-graphics "Une expression régulière (fichiers textuels seulement)")
  (mfs-searching... "Recherche en cours...")
  (mfs-configure-search "Configurer la recherche") ;; dialog title
  (mfs-files-section "Fichiers")   ;; section in config dialog
  (mfs-search-section "Rechercher") ;; section in config dialog
  (mfs-dir "Répertoire")
  (mfs-recur-over-subdirectories "Récursion dans les sous-répertoires")
  (mfs-regexp-filename-filter "Filtre de nom de fichiers pour les expressions régulières")
  (mfs-search-string "Chercher la chaîne de caractères")
  (mfs-drscheme-multi-file-search "DrScheme - Recherche dans des fichiers multiples") ;; results window and error message title
  (mfs-not-a-dir "\"~a\" n'est pas un répertoire")
  (mfs-open-file "Ouvrir le fichier")
  (mfs-stop-search "Stopper la recherche")
  (mfs-case-sensitive-label "Différentier les lettres majuscules des minuscules.")
  (mfs-no-matches-found "Rien n'a été trouvé.")
  (mfs-search-interrupted "Recherche avortée.")
  
  ;;;reverting a file
  (error-reverting "DrScheme - Erreur durant le retour à l'original.")
  (could-not-read "impossible de lire \"~a\".")
  (are-you-sure-revert
   "Etes-vous certain de vouloir retourner à la version de ce fichier qui est sur le disque dur ? Ce changement ne pourra pas être défait.")
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
  (must-specify-a-filename "Vous devez spécifier un nom de fichier.")
  (file-does-not-exist "Le fichier \"~a\" n'existe pas.")
  (ask-because-file-exists "Le fichier \"~a\" existe déjà. Voulez-vous le remplacer ?")
  (dne-or-cycle "Le fichier \"~a\" contient un répertoire non-existant, ou une boucle.")
  (get-file "Obtenir fichier")
  (put-file "Donner fichier")
  (full-pathname "Chemin de fichier complet")
  (show-dot-files "Montrer les fichiers et répertoires dont le nom commence par un point.")
  (up-directory-button-label "Répertoire parent")
  (add-button-label "Ajouter") ;;; for multi-file selection
  (add-all-button-label "Ajouter tous") ;;; for multi-file selection
  (remove-button-label "Enlever") ;;; for multi-file selection
  (file-wrong-form "Le format de ce nom de fichier est incorrect.")
  (select-files "Sélectionnez des fichiers")
  (select-file "Sélectionnez un fichier")
  (dir-dne "Ce répertoire n'existe pas.")
  (file-dne "Ce fichier n'existe pas.")
  (empty-filename "Le nom de fichier doit contenir au moins quelques lettres.")
  (that-is-dir-name "Ceci est un nom de répertoire.")
  
  ;;; raw menu names -- these must match the 
  ;;; versions below, once the &s have been stripped.
  ;;; if they don't, DrScheme's menus will appear
  ;;; in the wrong order.
  (file-menu "Fichier")
  (edit-menu "Editer")
  (help-menu "Aide")
  (windows-menu "Fenêtres")
  
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
  (new-menu-item "&Nouvelle fenêtre")
  (new-...-menu-item "&Nouvelle...")
  
  (open-info "Ouvrir un fichier à partir du disque dur.")
  (open-menu-item "&Ouvrir")
  (open-here-menu-item "&Ouvrir ici...")
  
  (open-recent-info "Une liste des fichiers ouverts récemment.")
  (open-recent-menu-item "Ouvrir récent")
  
  (revert-info "Retour à la version originale de ce fichier sur le disque dur.")
  (revert-menu-item "&Retour version disque")
  
  (save-info "Sauvegarder ce fichier sur le disque dur.")
  (save-menu-item "&Sauvegarder")
  
  (save-as-info "Demander un nom de fichier et sauver ce fichier sur le disque dur.")
  (save-as-menu-item "Sauvegarder à")
  
  (print-info "Envoyer ce fichier à une imprimante.")
  (print-menu-item "&Imprimer...")
  
  (close-info "Fermer ce fichier.")
  (close-menu-item "&Fermer")
  
  (quit-info "Fermer toutes les fenêtres.")
  (quit-menu-item-windows "&Quitter")
  (quit-menu-item-others "&Quitter")
  
  (edit-menu-label "&Editer")
  
  (undo-info "Défaire l'action la plus récente.")
  (undo-menu-item "&Défaire")
  
  (redo-info "Refaire l'action qui vient d'être défaite.")
  (redo-menu-item "&Refaire")
  
  (cut-info "Déplacer dans le porte-bloc les éléments sélectionés, pour collage ultérieur.")
  (cut-menu-item "&Couper")
  
  (copy-info "Copier dans le porte-bloc les éléments sélectionés, pour collage ultérieur.")
  (copy-menu-item "Co&pier")
  
  (paste-info "Coller à la place des éléments sélectionnés les éléments qui ont été copiés ou coupés le plus récemment.")
  (paste-menu-item "C&oller")
  
  (clear-info "Effacer les éléments sélectionnés sans modifier le porte-bloc ou le collage.")
  (clear-menu-item-others "Effacer")
  (clear-menu-item-windows "&Effacer")
  
  (select-all-info "Sélectionner tout le document.")
  (select-all-menu-item "&Sélectionner tout")
  
  (find-info "Rechercher une chaîne de caractères.")
  (find-menu-item "Rechercher...")
  
  (find-again-info "Recherche à nouveau la même chaîne de caractères.")
  (find-again-menu-item "Rechercher à nouveau")
  
  (replace-and-find-again-info "Remplacer le texte sélectionné et rechercher à nouveau le même texte.")
  (replace-and-find-again-menu-item "Remplacer && rechercher à nouveau")
  
  (preferences-info "Configurer vos préférences.")
  (preferences-menu-item "Préférences...")
  
  (keybindings-info "Montrer les raccourcis clavier actuellement actifs.")
  (keybindings-menu-item "Raccourcis clavier")
  (keybindings-frame-title "Raccourcis clavier")
  (keybindings-sort-by-name "Trier par nom")
  (keybindings-sort-by-key "Trier par raccourci")
  
  ;; menu items in the "special" menu
  (insert-text-box-item "Insérer une boite texte")
  (insert-pb-box-item "Insérer une boite à dessin")
  (insert-image-item "Insérer une image...")
  (insert-comment-box-menu-item-label "Insérer une boite à commentaires")
  (insert-lambda "Insérer un &Lambda")
  (insert-delta "Insérer un &Delta (define)")
  
  (wrap-text-item "Replier le texte")
  
  (windows-menu-label "Fe&nêtres")
  (bring-frame-to-front "Amener une fenêtre au premier plan")       ;;; title of dialog
  (bring-frame-to-front... "Amener une fenêtre au premier plan...") ;;; corresponding title of menu item
  (next-window "Fenêtre suivante")
  (previous-window "Fenêtre précédente")
  (most-recent-window "Fenêtre la plus récente")
  
  (view-menu-label "&Montrer")
  (show-overview "Montrer le contour")
  (hide-overview "Cacher le contour")
  (show-module-browser "Montrer le navigateur de modules")
  (hide-module-browser "Cacher le navigateur de modules")
  
  (help-menu-label "&Aide")
  (about-info "Auteurs et détails concernant ce logiciel.")
  (about-menu-item "A propos de ...")
  (help-menu-check-for-updates "Regarder les mises à jour...")
  
  ;; open here's new menu item
  (create-new-window-or-clear-current
   "Voulez-vous créer une nouvelle fenêtre ou effacer celle-ci ?")
  (clear-current "Effacer celle-ci")
  (new-window "Nouvelle fenêtre")
  
  ;;; exiting and quitting ``are you sure'' dialog
  ;;; exit is used on windows, quit on macos, in English. Other
  ;;; languages probably use the same word on both platforms.
  (exit "Quitter")
  (quit "Quitter")
  (are-you-sure-exit "Etes-vous certain de vouloir quitter ?")
  (are-you-sure-quit "Etes-vous certain de vouloir quitter ?")
  
  ;;; autosaving
  (error-autosaving "Erreur durant l'auto-sauvegarde de \"~a\".")
  (autosaving-turned-off "L'auto-sauvegarde est suspendue\njusqu'à ce que le fichier soit sauvegardé.")
  (recover-autosave-files-frame-title "Recouvrer des fichiers auto-sauvegardés")
  (autosave-details "Détails")
  (autosave-recover "Recouvrer")
  (autosave-unknown-filename "<<inconnu>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrScheme
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Fichier auto-sauvegardé :")
  (autosave-original-label: "Fichier original :")
  (autosave-autosave-label "Fichier auto-sauvegardé")
  (autosave-original-label "Fichier original")
  (autosave-compare-files "Comparer les fichiers auto-sauvegardés")
  
  (autosave-show-autosave "Auto-sauvegarder un fichier") ;; title of a window showing the autosave file
  
  (autosave-explanation "DrScheme a trouvé des fichiers auto-sauvegardés, qui peuvent contenir votre travail non-sauvegardé.")
  
  (autosave-recovered! "Recouvré !") ;; status of an autosave file
  (autosave-deleted "Effacé")       ;; status of an autosave file
  
  (autosave-error-deleting "Erreur durant l'effacement de ~a\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "Effacer")
  (autosave-delete-title "Effacer")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Continuer")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "Sélectionnez un répertoire où sauvegarder le fichier auto-sauvegardé.")
  
  
  ;;; file modified warning
  (file-has-been-modified
   "Ce fichier a été modifié depuis sa dernière sauvegarde. Voulez-vous écraser les modifications ?")
  (overwrite-file-button-label "Ecraser")
  
  (definitions-modified 
   "Le texte de la fenêtre de définition a été modifié directement sur le disque dur. Sauvegardez ou retournez à la version sur le disque.")
  (drscheme-internal-error "Erreur interne de DrScheme.")
  
  ;;; tools
  (invalid-tool-spec "La spécification d'outil qui se trouve dans le fichier info.ss de la collection ~a est invalide. Espérait soit une chaîne de caractères, soit une liste de chaînes de caractères, trouvé : ~e")
  (error-loading-tool-title "DrScheme - Erreur durant le chargement de l'outil ~s; ~s")
  (error-invoking-tool-title "Erreur durant l'invocation de l'outil ~s;~s")
  (tool-tool-names-same-length
   "`tool-names' et `tools' ne sont pas des listes de la même longueur, dans le fichier info.ss pour ~s. Trouvé ~e et ~e")
  (tool-tool-icons-same-length
   "`tool-icons' et `tools' ne sont pas des listes de la même longueur, dans le fichier info.ss pour ~s. Trouvé ~e et ~e")
  (tool-tool-urls-same-length
   "`tool-urls' et `tools' ne sont pas des listes de la même longueur, dans le fichier info.ss pour ~s. Trouvé ~e et ~e")
  (error-getting-info-tool
   "erreur durant le chargement du fichier info.ss pour ~s")
  (tool-error-phase1 "Erreur durant la phase 1 pour l'outil ~s; ~s")
  (tool-error-phase2 "Erreur durant la phase 2 oour l'outil ~s; ~s")
  
  
  ;;; define popup menu
  (end-of-buffer-define "<< fin du tampon >>")
  (sort-by-name "Trier par nom")
  (sort-by-position "Trier par position dans le fichier")
  (no-definitions-found "<< aucune définition trouvée >>")
  (jump-to-defn "Aller à la définition de ~a")
  
  (recent-items-sort-by-age "Trier par age")
  (recent-items-sort-by-name "Trier par nom")
  
  ;;; view menu
  (hide-definitions-menu-item-label "Cacher les &définitions")
  (show-definitions-menu-item-label "Montrer les &définitions")
  (definitions-menu-item-help-string "Cacher/montrer la fenêtre de définition")
  (show-interactions-menu-item-label "Montrer les &interactions")
  (hide-interactions-menu-item-label "Cacher les &interactions")
  (interactions-menu-item-help-string "Montrer/cacher la fenêtre d'interaction")
  (show-toolbar "Montrer la barre d'ou&tils")
  (hide-toolbar "Cacher la barre d'ou&tils")
  
  ;;; file menu
  (save-definitions-as "Sauvegarder les définitions...")
  (save-definitions "&Sauvegarder les définitions")
  (print-definitions "&Imprimer les définitions...")
  (about-drscheme "A propos de DrScheme")
  (save-other "Sauvegarder autre")
  (save-definitions-as-text "Sauvegarder les définitions au format texte...")
  (save-interactions "Sauvegarder les interactions")
  (save-interactions-as "Sauvegarder les interactions...")
  (save-interactions-as-text "Sauvegarder les interactions au format texte...")
  (print-interactions "Imprimer les interactions...")
  (new-tab "Nouvel onglet")
  (close-tab "Fermer l'onglet")
  
  ;;; edit-menu
  (split-menu-item-label "Di&viser")
  (collapse-menu-item-label "&Rassembler")
  
  ;;; language menu
  (language-menu-name "&Language")
  
  ;;; scheme-menu
  (scheme-menu-name "&Scheme")
  (execute-menu-item-label "Exécuter")
  (execute-menu-item-help-string "Réexécuter le program de la fenêtre de définition.")
  (break-menu-item-label "Stopper")
  (break-menu-item-help-string "Stopper l'exécution.")
  (kill-menu-item-label "Tuer")
  (kill-menu-item-help-string "Tuer l'exécution.")
  (clear-error-highlight-menu-item-label "Effacer le surlignage d'erreur")
  (clear-error-highlight-item-help-string "Efface le surlignage rose après une erreur")
  (reindent-menu-item-label "&Réindenter")
  (reindent-all-menu-item-label "Réindenter &tout")
  (semicolon-comment-out-menu-item-label "&Commenter à l'aide de points-virgules")
  (box-comment-out-menu-item-label "&Commenter à l'aide d'une boite")
  (uncomment-menu-item-label "&Décommenter")
  
  (convert-to-semicolon-comment "Convertir en un commentaire avec points-virgules")
  
  ;;; executables
  (create-executable-menu-item-label "Créer un exécutable...")
  (create-executable-title "Créer un exécutable")
  (must-save-before-executable "Vous devez sauvegarder votre programme avant de créer un exécutable.")
  (save-an-executable "Sauvegarder un exécutable")
  (save-a-mred-launcher "Sauvegarder un lanceur de type MrEd")
  (save-a-mzscheme-launcher "Sauvegarder un lanceur de type MzScheme")
  (save-a-mred-stand-alone-executable "Sauvegarder un exécutable autonome de type MrEd")
  (save-a-mzscheme-stand-alone-executable "Sauvegarder un exécutable autonome de type MzScheme")
  
  (definitions-not-saved "La fenêtre de définition n'a pas été sauvegardée. L'exécutable va utiliser la dernière version sauvegardée de la fenêtre de définition. Continuer ?")
  (inline-saved-program-in-executable?
   "Insérer dans l'exécutable le programme sauvegardé ? Si oui, vous pourrez copier l'exécutable sur un autre ordinateur ~a, mais l'exécutable sera probablement gros. Si vous choisissez non, vous ne pourrez pas copier l'exécutable sur un autre ordinateur, mais il sera bien plus petit. De plus, si vous choisissez non, l'exécutable utilisera toujours la version la plus récente du programme.")
  (use-mred-binary?
   "Utiliser le binaire de mred pour cet exécutable ?\n\nSi oui, votre program peut utiliser la bibliothèque (lib \"mred.ss\" \"mred\"). Si non, DrScheme va choisir mzscheme comme binaire pour cet exécutable.\n\nASi vous n'êtes pas sûr, choisissez oui.")
  (inline-saved-program-in-executable/windows/path
   "ATTENTION ! L'exécutable généré a besoin de trois DLLs : libmred.dll, libmzsch.gll et libgc.dll, qui sont localisées dans\n\n~a\n\nL'exécutable trouve les DLLs soit dans le répertoire ou se trouve l'exécutable soit en utilisant la variable d'environnement PATH.\n\nQuand vous avez installé DrScheme, l'installateur a modifié le PATH de l'utilisateur pour y inclure le répertoire dans lequel les DLLs se trouvent. Méfiez-vous des changements de configuration ou des changements faits par l'utilisateur depuis l'installation.\n\nSi vous déplacez l'exécutable sur une autre machine, vous devez également copier les DLLS sur l'autre machine, soit dans le même répertoire que l'exécutable, soit dans un répertoire qui figure dans le PATH sur l'autre machine.")
  (launcher "Lanceur")
  (stand-alone "Autonome")
  (executable-type "Type")
  (executable-base "Base")
  (filename "Nom de fichier : ")
  (create "Créer")
  (please-choose-an-executable-filename "Veuillez sélectionner un nom de fichier pour sauvegarder l'exécutable.")
  (windows-executables-must-end-with-exe
   "Le nom de fichier\n\n  ~a\n\nest illégal. Sous Windows, le nom d'un exécutable doit se terminer par .exe.")
  (macosx-executables-must-end-with-app
   "Le nom de fichier\n\n  ~a\n\nest illégal. Sous MacOS X, le nom d'un exécutable doit se terminer par .app.")
  (warning-directory-will-be-replaced
   "ATTENTION : le répertoire :\n\n  ~a\n\nva être remplacé. Voulez-vous continuer ?")
  
  (create-servlet "Créer un servlet...") ;servlet = greffon, extension serveur?
  
  ; the ~a is a language such as "module" or "algol60"
  (create-servlet-unsupported-language
   "La création de servlets n'est pas possible avec le language ~a.")
  
  ;;; buttons
  (execute-button-label "Exécuter")
  (save-button-label "Sauvegarder")
  (break-button-label "Stopper")
  
  ;;; search help desk popup menu
  (search-help-desk-for "Rechercher \"~a\" dans l'Aide.")
  (exact-lucky-search-help-desk-for "Faire une recherche \"J'ai de la chance\" dans l'Aide pour le texte exact \"~a\".")
  
  ;; collapse and expand popup menu items
  (collapse-sexp "Rétrécir une sexpression")
  (expand-sexp "Elargir une sexpression")
  
  ;;; fraction dialog
  (enter-fraction "Entrer une fraction")
  (whole-part "Partie entière")
  (numerator "Numérateur")
  (denominator "Dénominateur")
  (invalid-number "Nombre invalide : doit être un nombre réel exact non-entier.")
  (insert-fraction-menu-item-label "Insérer une fraction...")
  
  ;; number snip popup menu
  (show-decimal-expansion "Montrer l'expansion décimale")
  (show-fraction-view "Montrer sous forme de fraction")
  (show-mixed-fraction-view "Montrer sous forme partie-entière plus fraction")
  (show-improper-fraction-view "Montrer sous forme de fraction")
  (show-more-decimal-places "Montrer plus de décimales")
  
  ;;; Teachpack messages
  (select-a-teachpack "Sélectionner un teachpack")
  (clear-teachpack "Enlever le teachpack ~a")
  (teachpack-error-label "DrScheme - erreur avec un teachpack.")
  (teachpack-dne/cant-read "Le fichier teachpack ~a n'existe pas ou n'est pas lisible.")
  (teachpack-didnt-load "Le fichier teachpack ~a n'a pas été correctement chargé.")
  (teachpack-error-invoke "Le fichier teachpack ~a a produit une erreur au moment de son invocation.")
  (add-teachpack-menu-item-label "Ajouter un teachpack...")
  (clear-all-teachpacks-menu-item-label "Enlever tous les teachpacks")
  (drscheme-teachpack-message-title "DrScheme teachpack")
  (already-added-teachpack "Le teachpack ~a a déjà été ajouté.")
  
  ;;; Language dialog
  (introduction-to-language-dialog
   "Veuillez sélectionner un language. Un étudiant dans un cours d'introduction préférera le language par défaut.")
  (language-dialog-title "Configurer le language")
  (case-sensitive-label "Différentier les lettres majuscules des minuscules.")
  (output-style-label "Style d'impression des résultats")
  (constructor-printing-style "Constructeur")
  (quasiquote-printing-style "Quasiquote")
  (write-printing-style "write")
  (print-printing-style "current-print")
  (sharing-printing-label "Montrer le partage entre valeurs.")
  (use-pretty-printer-label "Insérer des retours-chariots lors de l'impression des résultats.")
  (input-syntax "Syntaxe d'entrée")
  (dynamic-properties "Propriétés dynamiques")
  (output-syntax "Syntaxe de sortie")
  (no-debugging-or-profiling "Pas de débogage ou profilage") ; Profilage. Eurk...
  (debugging "Débogage")
  (debugging-and-profiling "Débogage et profilage")
  (test-coverage "Couverture syntaxique de vos tests")
  (whole/fractional-exact-numbers-label "Imprimer les nombres sous forme de fractions.")
  (booleans-as-true/false-label "Imprimer les booléens sous forme true et false.")
  (show-details-button-label "Montrer les détails")
  (hide-details-button-label "Cacher les détails")
  (choose-language-menu-item-label "Sélectionner le language...")
  (revert-to-language-defaults "Retourner aux valeurs par défaut pour le language.")
  (language-docs-button-label "Docs languages")
  (fraction-style "Style de fractions")
  (use-mixed-fractions "Fractions mêlées")
  (use-repeating-decimals "Décimales répétitives")
  (decimal-notation-for-rationals "Utiliser la notation décimale pour les nombres rationnels")
  (please-select-a-language "Veuillez sélectionner un language")
  
  
  ;;; languages
  (beginning-student "Etudiant niveau débutant")
  (beginning-one-line-summary "define, cond, structs, constantes, et primitives")
  (beginning-student/abbrev "Etudiant niveau débutant avec abréviations pour les listes")
  (beginning/abbrev-one-line-summary "Débutant, avec impression des résultats dans le REPL sous forme de listes")
  (intermediate-student "Etudiant niveau intermédiaire")
  (intermediate-one-line-summary "Débutant plus portée lexicale")
  (intermediate-student/lambda "Etudiant niveau intermédiaire, plus lambda")
  (intermediate/lambda-one-line-summary "Intermédiaire plus fonctions d'ordre supérieur")
  (advanced-student "Etudiant niveau avancé")
  (advanced-one-line-summary "Intermédiaire plus lambda et mutation")
  (full-language "Complet") ;; also in the HtDP languages section
  (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
  (r5rs-like-languages "R5RS et languages semblabes")
  (pretty-big-scheme "Assez gros Scheme")
  (pretty-big-scheme-one-line-summary "Graphique, plus de nombreuses bibliothèques standards")
  (r5rs-lang-name "Standard (R5RS)")
  (r5rs-one-line-summary "R5RS, de base")
  (expander "Expanseur") ; compression, compresseur, compresser => expansion, expanseur, expanser (expandeur, expander fait trop franglais et expandion n'existe pas)
  (expander-one-line-summary "Expanse les expressions au lieu de les évaluer")
  (professional-languages "Languages professionnels")
  (teaching-languages "Languages d'enseignement")
  (experimental-languages "Languages expérimentaux")
  
  (module-language-one-line-summary "Language avec module comme seule forme")
  
  
  ;;; debug language
  (unknown-debug-frame "[inconnu]")
  (backtrace-window-title "Trace - DrScheme")
  (files-interactions "les interactions de ~a") ;; filled with a filename
  (current-interactions "interactions")
  (current-definitions "définitions")
  (mzscheme-w/debug "Textuel (MzScheme)")
  (mzscheme-one-line-summary "PLT Scheme sans la bibliothèque graphique")
  (mred-w/debug "Graphique (MrEd)")
  (mred-one-line-summary "PLT Scheme plus la bibliothèque graphique")
  
  ;; profiling
  (profiling-low-color "Bas")
  (profiling-high-color "Elevé")
  (profiling-choose-low-color "Sélectionnez une couleur pour Bas")
  (profiling-choose-high-color "Sélectionnez une couleur pour Elevé")
  (profiling "Profilage")
  (profiling-example-text "(define (whee) (whee))")
  (profiling-color-config "Gamme de couleurs pour le profil")
  (profiling-scale "Echelle de couleurs pour le profil")
  (profiling-sqrt "Racine Carrée")
  (profiling-linear "Linéaire")
  (profiling-square "Quadratique")
  (profiling-number "Numbre d'appels de fonctions")
  (profiling-time "Temps cumulatif")
  (profiling-clear "Effacer le profil")
  (profiling-update "Mettre à jour le profil")
  (profiling-col-percent-time "% Temps")
  (profiling-col-function "Fonction")
  (profiling-col-name "Nom")
  (profiling-col-time-in-msec "ms")
  (profiling-col-calls "Appels de fonctions")
  (profiling-show-profile "Montrer le profil")
  (profiling-hide-profile "Cacher le profil")
  (profiling-unknown-src "<< inconnu >>")
  (profiling-no-information-available "Pas d'information de profilage disponible. Assurez vous que l'option de profilage ait été spécifiée pour ce language et que vous ayez exécuté le programme.")
  (profiling-clear? "Modifier le contenu de la fenêtre de définition invalide le profil. Voulez-vous continuer ?")
  
  ;; test coverage
  (test-coverage-clear? "Modifier le contenu de la fenêtre de définition invalide l'information de couverture de vos tests. Voulez-vous continuer ?")
  (test-coverage-clear-and-do-not-ask-again "Oui, et ne me demandez pas à nouveau")
  (test-coverage-ask? "Demander à propos de l'invalidation de l'information de couverture des tests ?")
  
  ;;; repl stuff
  (evaluation-terminated "Evaluation terminée.")
  (evaluation-terminated-explanation
   "Le thread d'évaluation n'est plus en exécution, toute évaluation est donc impossible jusqu'à la prochaine exécution.")
  (last-stack-frame "Montrer le dernier appel de fonction sur la pile.")
  (last-stack-frames "Montrer les derniers ~a appels de fonction sur la pile.")
  (next-stack-frames "Montrer les ~a appels de fonction suivants sur la pile.")
  
  ;;; welcoming message in repl
  (language "Language")
  (custom "personnalisé")
  (teachpack "Teachpack")
  (welcome-to "Bienvenue dans")
  (version "version")
  
  ;;; kill evaluation dialog
  (kill-evaluation? "Voulez-vous tuer l'évaluation ?")
  (just-break "Simplement stopper")
  (kill "Tuer")
  (kill? "Tuer ?")
  
  ;;; version checker
  ;; the next two are used in the initial wizard dialog.
  ;; Note that vc-wizard-check-prompt can (should) have newlines so
  ;; it will not make the dialog too wide.
  (vc-wizard-check-note "La version des logiciels que vous êtes en train d'installer n'est peut-être pas la dernière.~nSi vous le voulez DrScheme peut regarder pour une version plus récente.")
  (vc-wizard-check-button "Regarder !")
  (vc-update-check "Vérification des mises à jour")
  (vc-please-wait "Veuillez patienter")
  (vc-connecting-version-server "Connexion au server de version de PLT en cours")
  (vc-network-timeout "Expiration du compte-à-rebours lors de la connexion réseau")
  (vc-cannot-connect "Impossible de se connecter au serveur de version de PLT")
  (vc-network-failure "Erreur réseau")
  (vc-old-binaries "Les fichiers binaires installés pour DrScheme (ou MzScheme) ne sont pas à jour")
  (vc-binary-information-format "Version binaire installée : ~a (itération ~a)")
  (vc-details-format "~a~nDétails :~n~a")
  (vc-details-text "Détails :~n")
  (vc-error-format "Erreur : ~a")
  (vc-current-format "~a v.~a (itération ~a) est à jour")
  (vc-update-format "~a v.~a (itération ~a) doit être remplacé par v.~a (itération ~a)")
  (vc-binary-name "Binaire")
  (vc-updates-available "Les mises à jour sont disponibles à")
  (vc-latest-binary-information-format "Version la plus récente : ~a (itération ~a)")
  (vc-update-dialog-title "Etat des mises à jour")
  (vc-need-update-string "Un ou plusieurs des logiciels PLT installés doivent être mis à jour")
  (vc-no-update-string "Tous les logiciels PLT installés sont à jour")
  
  ;; special menu
  (special-menu "Spécial")
  
  ;; large semi colon letters
  (insert-large-letters... "Inserer de grandes lettres...")
  (large-semicolon-letters "Grandes lettres en points-virgules")
  (text-to-insert "Texte à inserer")
  
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
  (module-browser-show-lib-paths "Montrer les fichiers chargés à l'aide de chemins de fichiers du type (lib ..)")
  (module-browser-progress "Navigateur de modules : ~a") ;; prefix in the status line
  (module-browser-compiling-defns "Navigateur de modules : compilation des définitions")
  (module-browser-show-lib-paths/short "(require (lib ...))") ;; check box label in show module browser pane in drscheme window.
  (module-browser-refresh "Rafraîchir") ;; button label in show module browser pane in drscheme window.
  (module-browser-only-in-plt-and-module-langs
   "Le navigateur de modules n'est disponible que pour les programmes écrits dans l'un des languages PLT ou dans le language \"module\", et seulement pour les programmes qui contiennent des modules.")
  
  (happy-birthday-matthias "Joyeux anniversaire, Matthias !")
  (happy-birthday-matthew "Joyeux anniversaire, Matthew !")
  
  (mrflow-using-default-language-title "Language par défaut utilisé")
  (mrflow-using-default-language "Le language actuellement utilisé n'a pas de table de types défini pour ses primitives. R5RS Scheme est utilisé à la place.")
  (mrflow-button-title "Analyzer")
  ;(mrflow-unknown-style-delta-error-title "Delta de Style de boîte inconnu")
  ;(mrflow-unknown-style-delta-error "Delta de style de boîte inconnu : ~a")
  (mrflow-coloring-error-title "Couleur inconnue")
  (mrflow-coloring-error "Pas de style défini pour la couleur : ~a")
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
  
  (snips-and-arrows-popup-menu-tack-all-arrows "Coller toutes les flèches")
  (snips-and-arrows-popup-menu-untack-all-arrows "Décoller toutes les flèches")
  (snips-and-arrows-user-action-disallowed-title "Changements actuellement interdits")
  (snips-and-arrows-user-action-disallowed "Les changements sont interdits dans les éditeurs qui contiennent des boîtes inserées par un outil.  Cachez toutes les boîtes avant de modifier le contenu de l'éditeur.")
  ;(snips-and-arrows-changing-terms-warning-title "Changer les termes ne pourra être défait")
  ;(snips-and-arrows-changing-terms-warning "Changer des termes dans un éditeur n'est pas une action qui peut être défaite. Vous pouvez soit annuler cette action, cacher les boîtes, et réessayer le changement, soit vous pouvez continuer le changement, auquel cas le changement ne pourra pas être défait (tous les autres changements effectués avant ou après pourront cependant toujours être défaits).")
  (snips-and-arrows-hide-all-snips-in-editor "Cacher les boîtes de cet éditeur")
  
  (xml-tool-menu "XML")
  (xml-tool-insert-xml-box "Insérer une boîte XML")
  (xml-tool-insert-scheme-box "Insérer une boîte Scheme")
  (xml-tool-insert-scheme-splice-box "Insérer une boîte Scheme à raccord")
  (xml-tool-xml-box "Boîte XML")
  (xml-tool-scheme-box "Boîte Scheme")
  (xml-tool-scheme-splice-box "Boîte Scheme à raccord")
  (xml-tool-switch-to-scheme "Changer pour une boîte Scheme")
  (xml-tool-switch-to-scheme-splice "Changer pour une boîte Scheme à raccord")
  (xml-tool-eliminate-whitespace-in-empty-tags
   "Eliminer les espaces dans les délimiteurs vides")
  (xml-tool-leave-whitespace-alone
   "Laisser les espaces tel quel")
  
  (show-recent-items-window-menu-item "Montrer les fichiers récemment ouverts dans une fenêtre séparée")
  (show-recent-items-window-label "Fichiers récemment ouverts")
  (number-of-open-recent-items "Nombre de fichiers récents")
  (switch-anyway "Changer de fichier quand même")
  
  (stepper-program-has-changed "Avertissement : le programme a été modifié.")
  (stepper-program-window-closed "Avertissement : la fenêtre du programme a disparu.")
  
  (wizard-next "Suivant")
  (wizard-back "Précédent")
  (wizard-finish "Fin")
  
  ;; warnings about closing a drscheme frame when the program
  ;; might still be doing something interesting
  (program-is-still-running "Le programme dans la fenêtre de définition est toujours en cours d'exécution. Fermer la fenêtre quand même ?")
  (program-has-open-windows "Le programme dans la fenêtre de définition a d'autres fenêtres ouvertes. Fermer la fenêtre quand même ?")
  
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Arguments de ligne de commande, sous forme d'un vecteur de chaînes de caractères (syntaxe de read)")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<chemins de répertoires pour les collections par défaut>>")
  
  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Choisissez un chemin de répertoire pour une collection")
  
  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Les chemins de répertoires pour les collections par défaut sont déjà présents")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Chemins de répertoires pour les collections")
  
  ;; button labels
  (ml-cp-add "Ajouter")
  (ml-cp-add-default "Ajouter les chemins par défaut")
  (ml-cp-remove "Enlever")
  (ml-cp-raise "Monter")
  (ml-cp-lower "Descendre")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "mode Java")
  (profj-java-mode-color-keyword "mot réservé")
  (profj-java-mode-color-string "chaîne de caractères")
  (profj-java-mode-color-literal "valeur litérale")
  (profj-java-mode-color-comment "commentaire")
  (profj-java-mode-color-error "erreur")
  (profj-java-mode-color-identifier "identificateur") ; l'académie française ne reconnaît pas ce mot
  (profj-java-mode-color-default "valeur par défaut")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Test vide")
  (test-case-too-many-expressions-error "Expressions trop nombreuses dans un test.")
  (test-case-not-at-top-level "Test n'est pas au premier niveau")
  ;; Dr. Scheme window menu items
  (test-case-insert "Insérer un test")
  (test-case-disable-all "Invalider tous les tests")
  (test-case-enable-all "Revalider tous les tests")
  ;; NOTE: The following three string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "À tester")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Attendu")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "Reçu")
  
  )
