(

 ;;; when translating this constant, substitue name of actual langauge for `English'
 (is-this-your-native-language "Le Fran�ais est-il votre langue maternelle ?")

 (are-you-sure-you-want-to-switch-languages
  "Ceci va changer le language utilis� par l'interface graphique, ce qui va n�cessiter un red�marrage de DrScheme. Etes-vous certain de vouloir continuer ?")
 
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
 (bug-report-field-summary "R�sum� du probl�me")
 (bug-report-field-severity "S�v�rit�")
 (bug-report-field-class "Classe")
 (bug-report-field-priority "Priorit�")
 (bug-report-field-description "Description")
 (bug-report-field-reproduce1 "Etapes � suivre pour")
 (bug-report-field-reproduce2 "reproduire le bug")
 (bug-report-field-environment "Environnement")
 (bug-report-field-tools "Outils")
 (bug-report-field-docs-installed "Documentations install�es")
 (bug-report-field-language "Language")
 (bug-report-field-teachpacks "Teachpacks")
 (bug-report-field-collections "Collections")
 (bug-report-field-human-language "Language humain")
 (bug-report-field-version "Version")
 (bug-report-show-synthesized-info "Montrer l'information synth�tis�e")
 (bug-report-hide-synthesized-info "Cacher l'information synth�tis�e")
 (bug-report-submit "Soumettre")
 (sending-bug-report "Soumission du formulaire de bug en cours...")
 (error-sending-bug-report "Erreur durant la soumission du formulaire de bug.")
 (error-sending-bug-report-expln "Une erreur s'est produite pendant la soumission de votre formulaire de bug. Si votre connexion Internet fonctionne correctement, veuillez visiter:\n\n    http://www.cs.rice.edu/CS/PLT/Bugs/\n\net soumettre votre bug en utilisant notre formulaire web en ligne. Je suis vraiment profond�ment d�sol� pour toutes vos difficult�s.\n\nLe message d'erreur est:\n~a")
 (bug-report-sent "Le bug a �t� correctement soumis.")
 (bug-report-sent-detail "Merci pour votre soumission. Vous devriez recevoir une confirmation de votre soumission par email d'ici 30 minutes. Si vous ne recevez pas cette confirmation, envoyez un email � scheme@cs.rice.edu.")
 (illegal-bug-report "Formulaire de soumission de bug incomplet.")
 (pls-fill-in-field "Merci de compl�ter le champ \"~a\".")
 (malformed-email-address "Adresse email malform�e.")

 ;;; check syntax
 (check-syntax "Syntaxe") ; "Correcteur de syntaxe" est long...
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
 (cs-rename-var-to "Renommer ~a en:")
 (cs-name-duplication-error "Le nouveau nom que vous avez choisi, ~s, est en conflit avec un autre nom pr�existant dans le m�me contexte.")
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Ramassage")
 (read-only "Lecture seulement")
 (read/write "Lecture/�criture")
 (auto-extend-selection "S�lection auto-�tendable")
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

 (goto-line "Aller � la ligne")
 (goto-line-invalid-number
  "~a n'est pas un num�ro de ligne valide. Ce doit �tre un entier entre 1 et ~a.")
 (goto-position "Aller � la position")
 (no-full-name-since-not-saved
  "Le fichier n'a pas encore de nom complet car il n'a pas encore �t� sauvegard�.")
 (cannot-open-because-dne "Impossible d'ouvrir ~a car il n'existe pas.")
 (interactions-out-of-sync
  "ATTENTION: la fen�tre d'interaction et la fen�tre de d�finition ne sont pas synchronis�es. Cliquez sur Ex�cuter.")
 (file-is-not-saved "Le fichier \"~a\" n'a pas �t� sauvegard�.")
 (save "Sauvegarder")
 (please-choose-either "Choisissez \"~a\" ou \"~a\".")
 (close-anyway "Fermer quand m�me")

 (url "URL")
 (url: "URL:")
 (open-url... "Ouvrir l'URL...")
 (open-url "Ouvrir l'URL")
 (browse... "Naviguer...")
 (bad-url "URL incorrect")
 (bad-url:this "URL incorrect: ~a")
 
 ;; Help Desk
 (search-results "R�sultats de la recherche")
 (help-desk "Aide")
 (help-desk-n "Aide ~a")
 (about-help-desk "A propos de l'Aide")
 (help-desk-about-string
  "L'Aide est une source compl�te d'information � propos des logiciels du PLT, y compris DrScheme, MzScheme et MrEd.\n\nVersion ~a\nCopyright (c) 1995-2001 PLT.")
 (help-on-help "Aide de l'Aide")
 (help-on-help-details "Pour obtenir de l'aide sur comment utiliser l'Aide, suivez le lien `How to use Help Desk' � partir de la page principale de l'Aide (pour trouver la page principale, si vous n'y �tes pas d�j�, cliquez sur le boutton `Home' qui appara�t en haut de la fen�tre de l'Aide).")
 (find-docs-for "Chercher dans les docs:")
 (search "Chercher")
 ; next 3 are popup menu choices at bottom of help desk window
 (search-for-keyword "par mot clef")
 (search-for-keyword-or-index "par mot clef ou entr�e dans l'index")
 (search-for-keyword-or-index-or-text "par mot clef, entr�e dans l'index ou dans le texte")
 (exact-match "mot exact")
 (containing-match "contenant le mot")
 (regexp-match "expression r�guli�re")
 (stop "Stop")
 (feeling-lucky "J'ai de la chance")
 (nothing-found-for-search-key "Rien n'a �t� trouv� pour \"~a\".")
 (searching "Recherche en cours...")
 (search-stopped "(Recherche stopp�e.)")
 (search-stopped-too-many-matches "(Recherche stopp�e - trop d'entr�es ont �t� trouv�es.)")
 (reload "Rafra�chir")
 (help "Aide")
 (searching... "Recherche en cours...")
 (nothing-found-for-empty-search "Rien n'a �t� trouv� pour cette recherche vide.")
 (nothing-found-for "Rien n'a �t� trouv� pour ~a.")
 (and "et")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "Installer ~a ou l'ouvrir pour �dition ?")
 (install-plt-file/yes "Installation")
 (install-plt-file/no "Edition")
 
 ;;; about box
 (about-drscheme-frame-title "A propos de DrScheme")
 (take-a-tour "Faire un tour !")
 (release-notes "Notes pour la r�vision")
 (parenthetical-last-version "(version pr�c�dente ~a)")
 (parenthetical-last-language "(language pr�c�dent ~a)")
 (parenthetical-last-version/language "(version pr�c�dente ~a, language pr�c�dent ~a)")
 
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Sauvegarder ce fichier en format texte ?")
 (save-in-drs-format "Sauvegarder ce fichier en format DrScheme (non-texte) ?")
 (yes "Oui")
 (no "Non")
 
 ;;; preferences
 (preferences "Pr�f�rences")
 (preferences-category "Cat�gorie")
 (saving-preferences "Sauvegarde des pr�f�rences")
 (error-unmarshalling "Erreur durant la dess�rialisation de la pr�f�rence ~a.")
 (error-saving-preferences "Erreur durant la sauvegarde des pr�f�rences: ~a.")
 (error-reading-preferences "Erreur durant la lecture des pr�f�rences.")
 (found-bad-pref "Mauvaise pr�f�rence dans le fichier \"~a\".")
 (expected-list-of-length2 "esp�rait une liste de longueur 2.")
 (general-prefs-panel-label "G�n�ral")
 (highlight-parens "Griser les paires de parenth�ses.")
 (fixup-parens "Corriger les parenth�ses.")
 (flash-paren-match "Montrer le pairage de parenth�ses.")
 (auto-save-files "Sauvegarde automatique des fichiers.")
 (map-delete-to-backspace "La touche Delete g�n�re Backspace.")
 (verify-exit "Confirmation pour quitter.")
 (ask-before-changing-format "Confirmation avant de changer le format de sauvegarde.")
 (wrap-words-in-editor-buffers "Continuer une longue ligne sur la ligne suivante, dans les �diteurs.")
 (show-status-line "Montrer la barre de status.")
 (count-from-one "Compter les lignes et colonnes � partir de un.") 
 (display-line-numbers "Montrer le num�ro de ligne et de colonne, pas la distance depuis le d�but d'�diteur.")
 (enable-keybindings-in-menus "Raccourcis clavier dans les menus.")
 (automatically-to-ps "Imprimer automatiquement dans un fichier postscript.")
 (use-mdi "Utiliser les fen�tres MDI.") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Utiliser un dialogue s�par� pour les recherches.")
 (default-fonts "Polices par d�faut")
 
 ; should have entire alphabet
 (font-example-string "a�bc�de���fghi�jklmno�pqrstu�vwxyz") 

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
 (example-text "Example de texte:")
 (general-ii "G�n�ral II")
 (only-warn-once "Pr�venir une fois seulement quand ex�cutions et interactions n'ont pas �t� synchron�es.")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Indentation")

 ; filled with define, lambda, or begin
 (enter-new-keyword "Entrez un nouveau mot clef ressemblant � ~a:")
 (x-keyword "Mot clef ~a")
 (x-like-keywords "Mots clefs ressemblant � ~a")

 (expected-a-symbol "esp�rait un symbole, trouv�: ~a")
 (already-used-keyword "\"~a\" est d�j� un mot clef avec une indentation sp�ciale.")
 (add-keyword "Ajouter")
 (remove-keyword "Enlever")
 
 ;;; find/replace
 (find-and-replace "Chercher et remplacer")
 (find "Chercher")
 (replace "Remplacer")
 (use-separate-dialog-for-searching "Utiliser un menu s�par� pour chercher.")
 (replace&find-again "Remplacer && chercher � nouveau") ;;; need double & to get a single &
 (replace-to-end "Remplacer jusqu'� la fin")
 (forward "En avant")
 (backward "En arri�re")
 (hide "Cacher")
 
 ;;;reverting a file
 (error-reverting "Erreur durant le retour � l'original.")
 (could-not-read "impossible de lire \"~a\".")
 
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
 (new-menu-item "&Nouvelle fen�tre")

 (open-info "Ouvrir un fichier � partir du disque dur.")
 (open-menu-item "&Ouvrir")

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
 (quit-menu-item-windows "S&ortir")
 (quit-menu-item-others "&Quitter")
 
 (edit-menu-label "&Editer")
 
 (undo-info "D�faire l'action la plus r�cente.")
 (undo-menu-item "&D�faire")

 (redo-info "Refaire l'action qui vient d'�tre d�faite.")
 (redo-menu-item "&Refaire")

 (cut-info "D�placer dans le porte-bloc les �l�ments s�lection�s, pour collage ult�rieur.")
 (cut-menu-item "Coupe&r")

 (copy-info "Copier dans le porte-bloc les �l�ments s�lection�s, pour collage ult�rieur.")
 (copy-menu-item "&Copier")

 (paste-info "Coller � la place des �l�ments s�lectionn�s les �l�ments qui ont �t� copi�s ou coup�s le plus r�cemment.")
 (paste-menu-item "&Coller")

 (clear-info "Effacer les �l�ments s�lectionn�s sans modifier le porte-bloc ou le collage.")
 (clear-menu-item-others "Effacer")
 (clear-menu-item-windows "&Effacer")

 (select-all-info "S�lectionner tout le document.")
 (select-all-menu-item "S�lectionner t&out")
 
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
 (wrap-text-item "Replier le texte")

 (windows-menu-label "&Fen�tres")
 (bring-frame-to-front "Amener une fen�tre au premier plan")       ;;; title of dialog
 (bring-frame-to-front... "Amener une fen�tre au premier plan...") ;;; corresponding title of menu item
 
 (show-menu-label "&Montrer")

 (help-menu-label "&Aide")
 (about-info "Auteurs et d�tails concernant ce logiciel.")
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
 (autosaving-turned-off "L'auto-sauvegarde est suspendue\njusqu'� ce que le fichier soit sauvegard�.")
 
 ;;; file modified warning
 (file-has-been-modified
  "Ce fichier a �t� modifi� depuis sa derni�re sauvegarde. Voulez-vous �craser les modifications ?")
 (overwrite-file-button-label "Ecraser")
 
 (definitions-modified 
  "Le texte de la fen�tre de d�finition a �t� modifi� directement sur le disque dur. Sauvegardez ou retournez � la version sur le disque.")
 (drscheme-internal-error "Erreur interne de DrScheme.")
 
 ;;; tools
 (invalid-tool-spec "La sp�cification d'outil qui se trouve dans le fichier info.ss de la collection ~a est invalide. Esp�rait soit une cha�ne de caract�res, soit une liste de cha�nes de caract�res, trouv�: ~e")
 (error-loading-tool-title "DrScheme - Erreur durant le chargement de l'outil ~s; ~s")
 (error-invoking-tool-title "Erreur durant l'invocation de l'outil ~s;~s")
 (tool-tool-names-same-length
  "`tool-names' et `tools' ne sont pas des listes de la m�me longueur, dans le fichier info.ss pour ~s. Trouv� ~e et ~e")
 (tool-tool-icons-same-length
  "`tool-icons' et `tools' ne sont pas des listes de la m�me longueur, dans le fichier info.ss pour ~s. Trouv� ~e et ~e")
 (error-getting-info-tool
  "erreur durant le chargement du fichier info.ss pour ~s")
 
 ;;; define popup menu
 (end-of-buffer-define "<< fin du tampon >>")
 (sort-by-name "Trier par nom")
 (sort-by-position "Trier par position dans le fichier")
 (no-definitions-found "<< aucune d�finition trouv�e >>")
 
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
 (split-menu-item-label "&Diviser")
 (collapse-menu-item-label "R&assembler")
 
 ;;; language menu
 (language-menu-name "&Language")
 
 ;;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Ex�cuter")
 (execute-menu-item-help-string "R�ex�cuter le program de la fen�tre de d�finition.")
 (break-menu-item-label "Stopper")
 (break-menu-item-help-string "Stopper l'ex�cution.")
 (kill-menu-item-label "Tuer")
 (kill-menu-item-help-string "Tuer l'ex�cution.")
 (reindent-menu-item-label "&R�indenter")
 (reindent-all-menu-item-label "R�indenter &tout")
 (comment-out-menu-item-label "&Commenter")
 (uncomment-menu-item-label "&D�commenter")
 
 ;;; launcher
 (create-launcher-title "Cr�er un lanceur")
 (must-save-before-launcher "Vous devez sauver votre programme avant de cr�er un lanceur.")
 (save-a-launcher "Sauvegarder un lanceur")
 
 ;;; buttons
 (execute-button-label "Ex�cuter") 
 (save-button-label "Sauvegarder")
 (break-button-label "Stopper")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Rechercher \"~a\" dans l'Aide.")
 (exact-lucky-search-help-desk-for "Faire une recherche \"J'ai de la chance\" dans l'Aide pour le texte exact \"~a\".")
 
 ;;; fraction dialog
 (enter-fraction "Entrer une fraction")
 (whole-part "Partie enti�re")
 (numerator "Num�rateur")
 (denominator "D�nominateur")
 (invalid-number "Nombre invalide: doit �tre un nombre r�el exact non-entier.")
 (insert-fraction-menu-item-label "Ins�rer une fraction...")
 
 ;;; TeachPack messages
 (select-a-teachpack "S�lectionner un TeachPack")
 (clear-teachpack "Enlever le TeachPack ~a")
 (teachpack-error-label "DrScheme - erreur avec un TeachPack.")
 (teachpack-dne/cant-read "Le fichier TeachPack ~a n'existe pas ou n'est pas lisible.")
 (teachpack-didnt-load "Le fichier TeachPack ~a n'a pas �t� correctement charg�.")
 (teachpack-error-invoke "Le fichier TeachPack ~a a produit une erreur au moment de son invocation.")
 (add-teachpack-menu-item-label "Ajouter un Teachpack...")
 (clear-all-teachpacks-menu-item-label "Enlever tous les Teachpacks")
 (teachpack-not-only-one-import "La unit/sig du TeachPack dans ~a doit avoir exactement un import.")
 (drscheme-teachpack-message-title "DrScheme Teachpack")
 (already-added-teachpack "Le TeachPack ~a a d�j� �t� ajout�.")
 
 ;;; Language dialog
 (language-dialog-title "Configurer le language")
 (case-sensitive-label "Diff�rentier les lettres majuscules des minuscules.")
 (output-style-label "Style d'impression des r�sultats")
 (constructor-printing-style "Constructeur")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (sharing-printing-label "Montrer le partage entre valeurs.")
 (use-pretty-printer-label "Ins�rer des retours-chariots lors de l'impression des r�sultats.")
 (input-syntax "Syntaxe d'entr�e")
 (output-syntax "Syntaxe de sortie")
 (whole/fractional-exact-numbers-label "Imprimer les nombres sous forme de fractions.")
 (booleans-as-true/false-label "Imprimer les bool�ens sous forme true et false.")
 (show-details-button-label "Montrer les d�tails")
 (hide-details-button-label "Cacher les d�tails")
 (choose-language-menu-item-label "S�lectionner le language...")
 (revert-to-language-defaults "Retourner aux valeurs par d�faut pour le language.")

 ;;; languages
 (beginning-student "Etudiant niveau d�butant")
 (beginning-student/abbrev "Etudiant niveau d�butant avec abr�viations pour les listes")
 (intermediate-student "Etudiant niveau interm�diaire")
 (advanced-student "Etudiant niveau avanc�")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "comme R5RS")
 (mred-lang-name "Graphique sans d�buggage (MrEd)")
 (mzscheme-lang-name "Textuel sans d�buggage (MzScheme)")
 (r5rs-lang-name "R5RS sans d�buggage")
 (unknown-debug-frame "[inconnu]")
 
 ;;; debug language
 (backtrace-window-title "Trace - DrScheme")
 (files-interactions "les interactions de ~a") ;; filled with a filename
 (stack-frame-in-current-interactions "interactions")
 (stack-frame-in-current-definitions "d�finitions")
 (mzscheme-w/debug "Textuel (MzScheme)")
 (mred-w/debug "Graphique (MrEd)")
 (r5rs-w/debug "R5RS")
 
 ;;; repl stuff
 (evaluation-terminated "Evaluation termin�e.")
 (evaluation-terminated-explanation
  "Le thread d'�valuation n'est plus en ex�cution, toute �valuation est donc impossible jusqu'� la prochaine ex�cution.")
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
 (kill-evaluation? "Voulez-vous tuer l'�valuation ?")
 (just-break "Simplement stopper")
 (kill "Tuer")
 (kill? "Tuer ?")
 )
