(

 ;;; when translating this constant, substitue name of actual langauge for `English'
 (is-this-your-native-language "Le Français est-il votre langue maternelle ?")

 (are-you-sure-you-want-to-switch-languages
  "Ceci va changer le language utilisé par l'interface graphique, ce qui va nécessiter un redémarrage de DrScheme. Etes-vous certain de vouloir continuer ?")
 
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Annuler")
 (untitled "Sans Nom")
 (untitled-n "Sans Nom ~a")
 (warning "Avertissement")
 (error "Erreur")
 (close "Fermer") ;; as in, close an open window

 ;;; bug report form
 (cancel-bug-report? "Annuler la soumission du bug ?")
 (are-you-sure-cancel-bug-report?
  "Etes-vous certain de vouloir annuler la soumission de ce bug ?")
 (bug-report-form "Formulaire de soumission de bug")
 (bug-report-field-name "Nom")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Résumé du problème")
 (bug-report-field-severity "Sévérité")
 (bug-report-field-class "Classe")
 (bug-report-field-priority "Priorité")
 (bug-report-field-description "Description")
 (bug-report-field-reproduce1 "Etapes à suivre pour")
 (bug-report-field-reproduce2 "reproduire le bug")
 (bug-report-field-environment "Environnement")
 (bug-report-field-tools "Outils")
 (bug-report-field-docs-installed "Documentations installées")
 (bug-report-field-language "Language")
 (bug-report-field-teachpacks "Teachpacks")
 (bug-report-field-collections "Collections")
 (bug-report-field-human-language "Language humain")
 (bug-report-field-version "Version")
 (bug-report-show-synthesized-info "Montrer l'information synthétisée")
 (bug-report-hide-synthesized-info "Cacher l'information synthétisée")
 (bug-report-submit "Soumettre")
 (sending-bug-report "Soumission du formulaire de bug en cours...")
 (error-sending-bug-report "Erreur durant la soumission du formulaire de bug.")
 (error-sending-bug-report-expln "Une erreur s'est produite pendant la soumission de votre formulaire de bug. Si votre connexion Internet fonctionne correctement, veuillez visiter:\n\n    http://www.cs.rice.edu/CS/PLT/Bugs/\n\net soumettre votre bug en utilisant notre formulaire web en ligne. Je suis vraiment profondément désolé pour toutes vos difficultés.\n\nLe message d'erreur est:\n~a")
 (bug-report-sent "Le bug a été correctement soumis.")
 (bug-report-sent-detail "Merci pour votre soumission. Vous devriez recevoir une confirmation de votre soumission par email d'ici 30 minutes. Si vous ne recevez pas cette confirmation, envoyez un email à scheme@cs.rice.edu.")
 (illegal-bug-report "Formulaire de soumission de bug incomplet.")
 (pls-fill-in-field "Merci de compléter le champ \"~a\".")
 (malformed-email-address "Adresse email malformée.")

 ;;; check syntax
 (check-syntax "Syntaxe") ; "Correcteur de syntaxe" est long...
 (cs-italic "Italique")
 (cs-bold "Gras")
 (cs-underline "Souligné")
 (cs-change-color "Changer la couleur")
 (cs-tack/untack-arrow "Coller/décoller les flèches")
 (cs-jump "Suivre la flèche")
 (cs-error-message "Message d'erreur")
 (cs-open-file "Ouvrir ~a")
 (cs-rename-var "Renommer ~a")
 (cs-rename-id "Renommer l'identifieur")
 (cs-rename-var-to "Renommer ~a en:")
 (cs-name-duplication-error "Le nouveau nom que vous avez choisi, ~s, est en conflit avec un autre nom préexistant dans le même contexte.")
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Ramassage")
 (read-only "Lecture seulement")
 (read/write "Lecture/écriture")
 (auto-extend-selection "Sélection auto-étendable")
 (overwrite "Correction") ; vs Insertion ? surimpression ?
 (running "en cours")
 (not-running "en attente") ; pause ?
 
 ;;; misc
 (welcome-to-something "Bienvenue dans ~a.")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Bienvenue dans DrScheme, version ~a, ~a.")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Bienvenue dans DrScheme.")
 (version/language "version ~a, ~a.")

 (goto-line "Aller à la ligne")
 (goto-line-invalid-number
  "~a n'est pas un numéro de ligne valide. Ce doit être un entier entre 1 et ~a.")
 (goto-position "Aller à la position")
 (no-full-name-since-not-saved
  "Le fichier n'a pas encore de nom complet car il n'a pas encore été sauvegardé.")
 (cannot-open-because-dne "Impossible d'ouvrir ~a car il n'existe pas.")
 (interactions-out-of-sync
  "ATTENTION: la fenêtre d'interaction et la fenêtre de définition ne sont pas synchronisées. Cliquez sur Exécuter.")
 (file-is-not-saved "Le fichier \"~a\" n'a pas été sauvegardé.")
 (save "Sauvegarder")
 (please-choose-either "Choisissez \"~a\" ou \"~a\".")
 (close-anyway "Fermer quand même")

 (url "URL")
 (url: "URL:")
 (open-url... "Ouvrir l'URL...")
 (open-url "Ouvrir l'URL")
 (browse... "Naviguer...")
 (bad-url "URL incorrect")
 (bad-url:this "URL incorrect: ~a")
 
 ;; Help Desk
 (search-results "Résultats de la recherche")
 (help-desk "Aide")
 (help-desk-n "Aide ~a")
 (about-help-desk "A propos de l'Aide")
 (help-desk-about-string
  "L'Aide est une source complète d'information à propos des logiciels du PLT, y compris DrScheme, MzScheme et MrEd.\n\nVersion ~a\nCopyright (c) 1995-2001 PLT.")
 (help-on-help "Aide de l'Aide")
 (help-on-help-details "Pour obtenir de l'aide sur comment utiliser l'Aide, suivez le lien `How to use Help Desk' à partir de la page principale de l'Aide (pour trouver la page principale, si vous n'y êtes pas déjà, cliquez sur le boutton `Home' qui apparaît en haut de la fenêtre de l'Aide).")
 (find-docs-for "Chercher dans les docs:")
 (search "Chercher")
 ; next 3 are popup menu choices at bottom of help desk window
 (search-for-keyword "par mot clef")
 (search-for-keyword-or-index "par mot clef ou entrée dans l'index")
 (search-for-keyword-or-index-or-text "par mot clef, entrée dans l'index ou dans le texte")
 (exact-match "mot exact")
 (containing-match "contenant le mot")
 (regexp-match "expression régulière")
 (stop "Stop")
 (feeling-lucky "J'ai de la chance")
 (nothing-found-for-search-key "Rien n'a été trouvé pour \"~a\".")
 (searching "Recherche en cours...")
 (search-stopped "(Recherche stoppée.)")
 (search-stopped-too-many-matches "(Recherche stoppée - trop d'entrées ont été trouvées.)")
 (reload "Rafraîchir")
 (help "Aide")
 (searching... "Recherche en cours...")
 (nothing-found-for-empty-search "Rien n'a été trouvé pour cette recherche vide.")
 (nothing-found-for "Rien n'a été trouvé pour ~a.")
 (and "et")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "Installer ~a ou l'ouvrir pour édition ?")
 (install-plt-file/yes "Installation")
 (install-plt-file/no "Edition")
 
 ;;; about box
 (about-drscheme-frame-title "A propos de DrScheme")
 (take-a-tour "Faire un tour !")
 (release-notes "Notes pour la révision")
 (parenthetical-last-version "(version précédente ~a)")
 (parenthetical-last-language "(language précédent ~a)")
 (parenthetical-last-version/language "(version précédente ~a, language précédent ~a)")
 
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Sauvegarder ce fichier en format texte ?")
 (save-in-drs-format "Sauvegarder ce fichier en format DrScheme (non-texte) ?")
 (yes "Oui")
 (no "Non")
 
 ;;; preferences
 (preferences "Préférences")
 (preferences-category "Catégorie")
 (saving-preferences "Sauvegarde des préférences")
 (error-unmarshalling "Erreur durant la dessérialisation de la préférence ~a.")
 (error-saving-preferences "Erreur durant la sauvegarde des préférences: ~a.")
 (error-reading-preferences "Erreur durant la lecture des préférences.")
 (found-bad-pref "Mauvaise préférence dans le fichier \"~a\".")
 (expected-list-of-length2 "espérait une liste de longueur 2.")
 (general-prefs-panel-label "Général")
 (highlight-parens "Griser les paires de parenthèses.")
 (fixup-parens "Corriger les parenthèses.")
 (flash-paren-match "Montrer le pairage de parenthèses.")
 (auto-save-files "Sauvegarde automatique des fichiers.")
 (map-delete-to-backspace "La touche Delete génére Backspace.")
 (verify-exit "Confirmation pour quitter.")
 (ask-before-changing-format "Confirmation avant de changer le format de sauvegarde.")
 (wrap-words-in-editor-buffers "Continuer une longue ligne sur la ligne suivante, dans les éditeurs.")
 (show-status-line "Montrer la barre de status.")
 (count-from-one "Compter les lignes et colonnes à partir de un.") 
 (display-line-numbers "Montrer le numéro de ligne et de colonne, pas la distance depuis le début d'éditeur.")
 (enable-keybindings-in-menus "Raccourcis clavier dans les menus.")
 (automatically-to-ps "Imprimer automatiquement dans un fichier postscript.")
 (use-mdi "Utiliser les fenêtres MDI.") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Utiliser un dialogue séparé pour les recherches.")
 (default-fonts "Polices par défaut")
 
 ; should have entire alphabet
 (font-example-string "aàbcçdeéêèfghiîjklmnoôpqrstuûvwxyz") 

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
 (select-font-name "Sélectionnez une police")
 (example-text "Example de texte:")
 (general-ii "Général II")
 (only-warn-once "Prévenir une fois seulement quand exécutions et interactions n'ont pas été synchronées.")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Indentation")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Entrez un nouveau mot clef ressemblant à ~a:")
 (x-keyword "Mot clef ~a")
 (x-like-keywords "Mots clefs ressemblant à ~a")

 (expected-a-symbol "espérait un symbole, trouvé: ~a")
 (already-used-keyword "\"~a\" est déjà un mot clef avec une indentation spéciale.")
 (add-keyword "Ajouter")
 (remove-keyword "Enlever")
 
 ;;; find/replace
 (find-and-replace "Chercher et remplacer")
 (find "Chercher")
 (replace "Remplacer")
 (use-separate-dialog-for-searching "Utiliser un menu séparé pour chercher.")
 (replace&find-again "Remplacer && chercher à nouveau") ;;; need double & to get a single &
 (replace-to-end "Remplacer jusqu'à la fin")
 (forward "En avant")
 (backward "En arrière")
 (hide "Cacher")
 
 ;;;reverting a file
 (error-reverting "Erreur durant le retour à l'original.")
 (could-not-read "impossible de lire \"~a\".")
 
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
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label-windows "&Fichier")
 (file-menu-label-other "F&ichier")

 (new-info  "Ouvrir un nouveau fichier.")
 (new-menu-item "&Nouvelle fenêtre")

 (open-info "Ouvrir un fichier à partir du disque dur.")
 (open-menu-item "&Ouvrir")

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
 (quit-menu-item-windows "S&ortir")
 (quit-menu-item-others "&Quitter")
 
 (edit-menu-label "&Editer")
 
 (undo-info "Défaire l'action la plus récente.")
 (undo-menu-item "&Défaire")

 (redo-info "Refaire l'action qui vient d'être défaite.")
 (redo-menu-item "&Refaire")

 (cut-info "Déplacer dans le porte-bloc les éléments sélectionés, pour collage ultérieur.")
 (cut-menu-item "Coupe&r")

 (copy-info "Copier dans le porte-bloc les éléments sélectionés, pour collage ultérieur.")
 (copy-menu-item "&Copier")

 (paste-info "Coller à la place des éléments sélectionnés les éléments qui ont été copiés ou coupés le plus récemment.")
 (paste-menu-item "&Coller")

 (clear-info "Effacer les éléments sélectionnés sans modifier le porte-bloc ou le collage.")
 (clear-menu-item-others "Effacer")
 (clear-menu-item-windows "&Effacer")

 (select-all-info "Sélectionner tout le document.")
 (select-all-menu-item "Sélectionner t&out")
 
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

 (insert-text-box-item "Insérer une boite texte")
 (insert-pb-box-item "Insérer une boite à dessin")
 (insert-image-item "Insérer une image...")
 (wrap-text-item "Replier le texte")

 (windows-menu-label "&Fenêtres")
 (bring-frame-to-front "Amener une fenêtre au premier plan")       ;;; title of dialog
 (bring-frame-to-front... "Amener une fenêtre au premier plan...") ;;; corresponding title of menu item
 
 (show-menu-label "&Montrer")

 (help-menu-label "&Aide")
 (about-info "Auteurs et détails concernant ce logiciel.")
 (about-menu-item "A propos de ")
 (about-menu-item-after "...")

 ;;; help-desk-specific menus
 (new-help-desk "&Nouvelle Aide")
 
 ;;; exiting and quitting are you sure dialog
 ;;; (exit is used on windows, quit on macos. go figure)
 (exit-lc "quitter")
 (exit-cap "Quitter")
 (quit-lc "quitter")
 (quit-cap "Quitter")
 ;;; in are-you-sure-format, either exit or quit is filled in (from above)
 ;;; based on the platform drscheme is running on.
 (are-you-sure-format "Etes-vous certain de vouloir ~a ?")
 
 ;;; autosaving
 (error-autosaving "Erreur durant l'auto-sauvegarde de \"~a\".")
 (autosaving-turned-off "L'auto-sauvegarde est suspendue\njusqu'à ce que le fichier soit sauvegardé.")
 
 ;;; file modified warning
 (file-has-been-modified
  "Ce fichier a été modifié depuis sa dernière sauvegarde. Voulez-vous écraser les modifications ?")
 (overwrite-file-button-label "Ecraser")
 
 (definitions-modified 
  "Le texte de la fenêtre de définition a été modifié directement sur le disque dur. Sauvegardez ou retournez à la version sur le disque.")
 (drscheme-internal-error "Erreur interne de DrScheme.")
 
 ;;; tools
 (invalid-tool-spec "La spécification d'outil qui se trouve dans le fichier info.ss de la collection ~a est invalide. Espérait soit une chaîne de caractères, soit une liste de chaînes de caractères, trouvé: ~e")
 (error-loading-tool-title "DrScheme - Erreur durant le chargement de l'outil ~s; ~s")
 (error-invoking-tool-title "Erreur durant l'invocation de l'outil ~s;~s")
 (tool-tool-names-same-length
  "`tool-names' et `tools' ne sont pas des listes de la même longueur, dans le fichier info.ss pour ~s. Trouvé ~e et ~e")
 (tool-tool-icons-same-length
  "`tool-icons' et `tools' ne sont pas des listes de la même longueur, dans le fichier info.ss pour ~s. Trouvé ~e et ~e")
 (error-getting-info-tool
  "erreur durant le chargement du fichier info.ss pour ~s")
 
 ;;; define popup menu
 (end-of-buffer-define "<< fin du tampon >>")
 (sort-by-name "Trier par nom")
 (sort-by-position "Trier par position dans le fichier")
 (no-definitions-found "<< aucune définition trouvée >>")
 
 ;;; show menu
 (hide-definitions-menu-item-label "Cacher les &définitions")
 (show-definitions-menu-item-label "Montrer les &définitions")
 (definitions-menu-item-help-string "Cacher/montrer la fenêtre de définition")
 (show-interactions-menu-item-label "Montrer les &interactions")
 (hide-interactions-menu-item-label "Cacher les &interactions")
 (interactions-menu-item-help-string "Montrer/cacher la fenêtre d'interaction")
 
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
 
 ;;; edit-menu
 (split-menu-item-label "&Diviser")
 (collapse-menu-item-label "R&assembler")
 
 ;;; language menu
 (language-menu-name "&Language")
 
 ;;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Exécuter")
 (execute-menu-item-help-string "Réexécuter le program de la fenêtre de définition.")
 (break-menu-item-label "Stopper")
 (break-menu-item-help-string "Stopper l'exécution.")
 (kill-menu-item-label "Tuer")
 (kill-menu-item-help-string "Tuer l'exécution.")
 (reindent-menu-item-label "&Réindenter")
 (reindent-all-menu-item-label "Réindenter &tout")
 (comment-out-menu-item-label "&Commenter")
 (uncomment-menu-item-label "&Décommenter")
 
 ;;; launcher
 (create-launcher-title "Créer un lanceur")
 (must-save-before-launcher "Vous devez sauver votre programme avant de créer un lanceur.")
 (save-a-launcher "Sauvegarder un lanceur")
 
 ;;; buttons
 (execute-button-label "Exécuter") 
 (save-button-label "Sauvegarder")
 (break-button-label "Stopper")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Rechercher \"~a\" dans l'Aide.")
 (exact-lucky-search-help-desk-for "Faire une recherche \"J'ai de la chance\" dans l'Aide pour le texte exact \"~a\".")
 
 ;;; fraction dialog
 (enter-fraction "Entrer une fraction")
 (whole-part "Partie entière")
 (numerator "Numérateur")
 (denominator "Dénominateur")
 (invalid-number "Nombre invalide: doit être un nombre réel exact non-entier.")
 (insert-fraction-menu-item-label "Insérer une fraction...")
 
 ;;; TeachPack messages
 (select-a-teachpack "Sélectionner un TeachPack")
 (clear-teachpack "Enlever le TeachPack ~a")
 (teachpack-error-label "DrScheme - erreur avec un TeachPack.")
 (teachpack-dne/cant-read "Le fichier TeachPack ~a n'existe pas ou n'est pas lisible.")
 (teachpack-didnt-load "Le fichier TeachPack ~a n'a pas été correctement chargé.")
 (teachpack-error-invoke "Le fichier TeachPack ~a a produit une erreur au moment de son invocation.")
 (add-teachpack-menu-item-label "Ajouter un Teachpack...")
 (clear-all-teachpacks-menu-item-label "Enlever tous les Teachpacks")
 (teachpack-not-only-one-import "La unit/sig du TeachPack dans ~a doit avoir exactement un import.")
 (drscheme-teachpack-message-title "DrScheme Teachpack")
 (already-added-teachpack "Le TeachPack ~a a déjà été ajouté.")
 
 ;;; Language dialog
 (language-dialog-title "Configurer le language")
 (case-sensitive-label "Différentier les lettres majuscules des minuscules.")
 (output-style-label "Style d'impression des résultats")
 (constructor-printing-style "Constructeur")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (sharing-printing-label "Montrer le partage entre valeurs.")
 (use-pretty-printer-label "Insérer des retours-chariots lors de l'impression des résultats.")
 (input-syntax "Syntaxe d'entrée")
 (output-syntax "Syntaxe de sortie")
 (whole/fractional-exact-numbers-label "Imprimer les nombres sous forme de fractions.")
 (booleans-as-true/false-label "Imprimer les booléens sous forme true et false.")
 (show-details-button-label "Montrer les détails")
 (hide-details-button-label "Cacher les détails")
 (choose-language-menu-item-label "Sélectionner le language...")
 (revert-to-language-defaults "Retourner aux valeurs par défaut pour le language.")

 ;;; languages
 (beginning-student "Etudiant niveau débutant")
 (beginning-student/abbrev "Etudiant niveau débutant avec abréviations pour les listes")
 (intermediate-student "Etudiant niveau intermédiaire")
 (advanced-student "Etudiant niveau avancé")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "comme R5RS")
 (mred-lang-name "Graphique sans débuggage (MrEd)")
 (mzscheme-lang-name "Textuel sans débuggage (MzScheme)")
 (r5rs-lang-name "R5RS sans débuggage")
 (unknown-debug-frame "[inconnu]")
 
 ;;; debug language
 (backtrace-window-title "Trace - DrScheme")
 (files-interactions "les interactions de ~a") ;; filled with a filename
 (stack-frame-in-current-interactions "interactions")
 (stack-frame-in-current-definitions "définitions")
 (mzscheme-w/debug "Textuel (MzScheme)")
 (mred-w/debug "Graphique (MrEd)")
 (r5rs-w/debug "R5RS")
 
 ;;; repl stuff
 (evaluation-terminated "Evaluation terminée.")
 (evaluation-terminated-explanation
  "Le thread d'évaluation n'est plus en exécution, toute évaluation est donc impossible jusqu'à la prochaine exécution.")
 (last-stack-frame "Montrer le dernier appel de fonction sur la pile.")
 (last-stack-frames "Montrer les derniers ~a appels de fonction sur la pile.")
 (next-stack-frames "Montrer les ~a appels de fonction suivants sur la pile.")
 
 ;;; welcoming message in repl
 (language "Language")
 (custom "personnaliser")
 (teachpack "TeachPack")
 (welcome-to "Bienvenue dans")
 (version "version")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Voulez-vous tuer l'évaluation ?")
 (just-break "Simplement stopper")
 (kill "Tuer")
 (kill? "Tuer ?")
 )
