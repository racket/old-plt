(
 ; Note: http://www.linux-france.org/prj/jargonf/ peut etre utile... Les dictionnaires online
 ; anglais->francais sont tres pauvres en ce qui concerne le jargon technique, et l'academie
 ; francaise (http://www-rocq.inria.fr/qui/Philippe.Deschamp/RETIF/) a quelques longueures de
 ; retard. http://www.can.ibm.com/francais/dico/ peut aider aussi...
 ; http://www.dicofr.com/ permet les recherches a partir du mot anglais.
 ; http://www.francophonie.hachette-livre.fr/ est un dico standard de base
 ; http://zeus.inalf.fr/academie9.htm est le dico de l'academie (A-M pour l'instant seulement)
 
 ;;; when translating this constant, substitue name of actual langauge for `English'
 (is-this-your-native-language "Le Fran�ais est-il votre langue maternelle ?")
 
 (are-you-sure-you-want-to-switch-languages
  "Ceci va changer le language utilis� par l'interface graphique, ce qui va n�cessiter un red�marrage de DrScheme. Etes-vous certain de vouloir continuer ?")
 
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
 (bug-report-field-teachpacks "TeachPacks")
 (bug-report-field-collections "Collections")
 (bug-report-field-human-language "Language humain")
 (bug-report-field-version "Version")
 (bug-report-synthesized-information "Information Synth�tis�e")  ;; dialog title
 (bug-report-show-synthesized-info "Montrer l'information synth�tis�e")
 (bug-report-submit "Soumettre")
 (sending-bug-report "Soumission du formulaire de bogue en cours...")
 (error-sending-bug-report "Erreur durant la soumission du formulaire de bogue.")
 (error-sending-bug-report-expln "Une erreur s'est produite pendant la soumission de votre formulaire de bogue. Si votre connexion Internet fonctionne correctement, veuillez visiter:\n\n    http://bugs.plt-scheme.org/\n\net soumettre votre bogue en utilisant notre formulaire web en ligne. Je suis vraiment profond�ment d�sol� pour toutes vos difficult�s.\n\nLe message d'erreur est:\n~a")
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
 (cs-rename-var-to "Renommer ~a en:")
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
 (welcome-to-drscheme "Bienvenue dans DrScheme.")
 (version/language "version ~a, ~a.")
 
 (goto-line "Aller � la ligne")
 (goto-line-invalid-number
  "~a n'est pas un num�ro de ligne valide. Ce doit �tre un entier entre 1 et ~a.")
 (goto-position "Aller � la position")
 (no-full-name-since-not-saved
  "Le fichier n'a pas encore de nom complet car il n'a pas encore �t� sauvegard�.")
 (cannot-open-because-dne "Impossible d'ouvrir ~a car le fichier n'existe pas.")
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
 (error-finding-docs
  "Documentation introuvable.\n\n~a")
 (manual-installed-date "(~a install�)")
 
 ;; refreshing manuals
 (refreshing-manuals "Re-t�l�chargement des manuels")
 (refresh-downloading... "T�l�chargement de ~a...")
 (refresh-deleting... "Effacement de l'ancienne version de ~a...")
 (refresh-installing... "Installation de la nouvelle version de ~a...")
 
 ; help desk htty proxy
 (http-proxy "Proxy HTTP")
 (proxy-direct-connection "Connexion directe")
 (proxy-use-proxy "Utiliser le proxy:")
 (proxy-host "Machine")
 (proxy-port "Port")
 (proxy-bad-host "Mauvaise machine proxy")
 
 ;; browser
 (rewind-in-browser-history "Retourner")
 (forward-in-browser-history "Avancer")
 (home "Maison")
 (browser "Navigateur")
 (cannot-display-url "Impossible de montrer l'URL ~s: ~a")
 (install? "Installer ?")  ;; if a .plt file is found (title of dialog)
 ; package => paquetage, pas tres clair...
 (you-have-selected-an-installable-package "Vous avez s�lectionn� un logiciel qui peut �tre install�.")
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
 (save-as-plain-text "Sauvegarder ce fichier au format texte ?")
 (save-in-drs-format "Sauvegarder ce fichier au format DrScheme (non-texte) ?")
 (yes "Oui")
 (no "Non")
 
 ;;; preferences
 (preferences "Pr�f�rences")
 (preferences-category "Cat�gorie")
 (saving-preferences "Sauvegarde des pr�f�rences")
 (error-unmarshalling "Erreur durant la dess�rialisation de la pr�f�rence ~a.")
 (error-saving-preferences "Erreur durant la sauvegarde des pr�f�rences: ~a.")
 (error-reading-preferences "Erreur durant la lecture des pr�f�rences.")
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
 (only-warn-once "Pr�venir une fois seulement quand ex�cutions et interactions n'ont pas �t� synchronis�es.")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Attente sur le fichier de verrouillage des pr�f�rences...")
 (pref-lock-not-gone
  "Les pr�f�rences sont verrouill�es par le fichier:\n\n   ~a\n\nqui emp�che les pr�f�rences d'�tre sauvegard�es. Assurez-vous qu'aucun logiciel PLT n'est en cours d'ex�cution et effacer le fichier.")
 (still-locked-exit-anyway? "Les pr�f�rences n'ont pu �tre sauvegard�es correctement. Quitter quand m�me ?")

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
 
 (open-info "Ouvrir un fichier � partir du disque dur.")
 (open-menu-item "&Ouvrir")
 
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
 (wrap-text-item "Replier le texte")
 
 (windows-menu-label "Fe&n�tres")
 (bring-frame-to-front "Amener une fen�tre au premier plan")       ;;; title of dialog
 (bring-frame-to-front... "Amener une fen�tre au premier plan...") ;;; corresponding title of menu item
 
 (show-menu-label "&Montrer")
 (show-overview "Montrer le contour") 
 (hide-overview "Cacher le contour")
 
 (help-menu-label "&Aide")
 (about-info "Auteurs et d�tails concernant ce logiciel.")
 (about-menu-item "A propos de ...")
 (help-menu-check-for-updates "Regarder les mises � jour...")

 ;;; help-desk-specific menus
 (new-help-desk "&Nouvelle Aide")
 
 ;;; exiting and quitting are you sure dialog
 ;;; (exit is used on windows, quit on macos. go figure)
 (exit "Quitter")
 (quit "Quitter")
 ;;; in are-you-sure-format, either exit or quit is filled in (from above)
 ;;; based on the platform drscheme is running on.
 (are-you-sure-exit "Etes-vous certain de vouloir quitter ?")
 (are-you-sure-quit "Etes-vous certain de vouloir quitter ?")
 
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
 (tool-error-phase1 "Erreur durant la phase 1 pour l'outil ~s; ~s")
 (tool-error-phase2 "Erreur durant la phase 2 oour l'outil ~s; ~s")
 
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
 (reindent-menu-item-label "&R�indenter")
 (reindent-all-menu-item-label "R�indenter &tout")
 (comment-out-menu-item-label "&Commenter")
 (uncomment-menu-item-label "&D�commenter")
 
 ;;; executables
 (create-executable-menu-item-label "Cr�er un ex�cutable...")
 (create-executable-title "Cr�er un ex�cutable")
 (must-save-before-executable "Vous devez sauvegarder votre programme avant de cr�er un ex�cutable.")
 (save-an-executable "Sauvegarder un ex�cutable")
 (definitions-not-saved "La fen�tre de d�finition n'a pas �t� sauvegard�e. L'ex�cutable va utiliser la derni�re version sauvegard�e de la fen�tre de d�finition. Continuer ?")
 (inline-saved-program-in-executable?
  "Ins�rer dans l'ex�cutable le programme sauvegard� ? Si oui, vous pourrez copier l'ex�cutable sur un autre ordinateur ~a, mais l'ex�cutable sera probablement gros. Si vous choisissez non, vous ne pourrez pas copier l'ex�cutable sur un autre ordinateur, mais il sera bien plus petit. De plus, si vous choisissez non, l'ex�cutable utilisera toujours la version la plus r�cente du programme.")
 (inline-saved-program-in-executable/windows
  "ATTENTION: sous Windows, vous avez �galement besoin de libmred.dll, libmzsch.dll et libmzgc.dll pour pouvoir ex�cuter un ex�cutable.")
 (use-mred-binary?
  "Utiliser le binaire de mred pour cet ex�cutable ?\n\nSi oui, votre program peut utiliser la biblioth�que (lib \"mred.ss\" \"mred\"). Si non, DrScheme va choisir mzscheme comme binaire pour cet ex�cutable.\n\nASi vous n'�tes pas s�r, choisissez oui.")
 
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
 (invalid-number "Nombre invalide: doit �tre un nombre r�el exact non-entier.")
 (insert-fraction-menu-item-label "Ins�rer une fraction...")
 
 ;; number snip popup menu
 (show-decimal-expansion "Montrer l'expansion d�cimale")
 (show-fraction-view "Montrer sous forme de fraction")
 (show-more-decimal-places "Montrer plus de d�cimales")
 
 ;;; TeachPack messages
 (select-a-teachpack "S�lectionner un TeachPack")
 (clear-teachpack "Enlever le TeachPack ~a")
 (teachpack-error-label "DrScheme - erreur avec un TeachPack.")
 (teachpack-dne/cant-read "Le fichier TeachPack ~a n'existe pas ou n'est pas lisible.")
 (teachpack-didnt-load "Le fichier TeachPack ~a n'a pas �t� correctement charg�.")
 (teachpack-error-invoke "Le fichier TeachPack ~a a produit une erreur au moment de son invocation.")
 (add-teachpack-menu-item-label "Ajouter un TeachPack...")
 (clear-all-teachpacks-menu-item-label "Enlever tous les TeachPacks")
 (teachpack-not-only-one-import "La unit/sig du TeachPack dans ~a doit avoir exactement un import.")
 (drscheme-teachpack-message-title "DrScheme TeachPack")
 (already-added-teachpack "Le TeachPack ~a a d�j� �t� ajout�.")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Veuillez s�lectionner un language. Un �tudiant dans un cours d'introduction pr�f�rera le language par d�faut.")
 (language-dialog-title "Configurer le language")
 (case-sensitive-label "Diff�rentier les lettres majuscules des minuscules.")
 (output-style-label "Style d'impression des r�sultats")
 (constructor-printing-style "Constructeur")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (sharing-printing-label "Montrer le partage entre valeurs.")
 (use-pretty-printer-label "Ins�rer des retours-chariots lors de l'impression des r�sultats.")
 (input-syntax "Syntaxe d'entr�e")
 (dynamic-properties "Propri�t�s dynamiques")
 (output-syntax "Syntaxe de sortie")
 (debugging "D�bogage")
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
 (htdp-full-one-line-summary "Avanc�, plus extensions PLT et biblioth�ques graphiques")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "R5RS et languages semblabes")
 (mred-lang-name "Graphique sans d�bogage (MrEd)")
 (mzscheme-lang-name "Textuel sans d�bogage (MzScheme)")
 (r5rs-lang-name "Standard (R5RS)")
 (r5rs-one-line-summary "R5RS, de base")
 (unknown-debug-frame "[inconnu]")
 
 (module-language-one-line-summary "Language avec module comme seule forme")
 (bad-module-language-specs
  "Les sp�cifications de drscheme-language-position et drscheme-language-modules sont incorrectes. Esp�rait (listof (cons string (listof string))) et (listof (listof string)) respectivement, avec les listes drscheme-language-position et drscheme-language-module ayant la m�me longueur. Trouv� ~e et ~e.")

 ;;; debug language
 (backtrace-window-title "Trace - DrScheme")
 (files-interactions "les interactions de ~a") ;; filled with a filename
 (stack-frame-in-current-interactions "interactions")
 (stack-frame-in-current-definitions "d�finitions")
 (mzscheme-w/debug "Textuel (MzScheme)")
 (mzscheme-one-line-summary "PLT Scheme sans la biblioth�que graphique")
 (mred-w/debug "Graphique (MrEd)")
 (mred-one-line-summary "PLT Scheme plus la biblioth�que graphique")
 
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
 (teachpack "TeachPack")
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
 ;(collections-not-installed "Les collections suivantes ne sont pas install�es:")
 ;(collections-missing-version "Les collections suivantes ont un num�ro de version incomplet ou manquant:")
 (vc-network-failure "Erreur r�seau")
 (vc-old-binaries "Les fichiers binaires install�s pour DrScheme (ou MzScheme) ne sont pas � jour")
 (vc-binary-information-format "Version binaire install�e: ~a (it�ration ~a)")
 (vc-details-format "~a~nD�tails:~n~a")
 (vc-details-text "D�tails:~n")
 (vc-error-format "Erreur: ~a") 
 (vc-current-format "~a v.~a (it�ration ~a) est � jour")
 (vc-update-format "~a v.~a (it�ration ~a) doit �tre remplac� par v.~a (it�ration ~a)")
 (vc-binary-name "Binaire")
 (vc-updates-available "Les mises � jour sont disponibles �")
 (vc-latest-binary-information-format "Version la plus r�cente: ~a (it�ration ~a)")
 (vc-update-dialog-title "Etat des mises � jour")
 (vc-need-update-string "Un ou plusieurs des logiciels PLT install�s doivent �tre mis � jour")
 (vc-no-update-string "Tous les logiciels PLT install�s sont � jour")
 
 ;; large semi colon letters
 (insert-large-letters... "Inserer de grandes lettres...")
 (large-semicolon-letters "Grandes lettres en points-virgules")
 (text-to-insert "Texte � inserer")

 (module-browser-filename-format "Nom de fichier complet: ~a (~a lignes)")
 (module-browser-root-filename "Nom de fichier de la racine: ~a")
 (module-browser-font-size-gauge-label "Taille de la police")
 (module-browser-progress-label "Avancement du navigateur de modules")
 (module-browser-adding-file "Ajout du fichier: ~a...")
 (module-browser-laying-out-graph-label "Tracer le graph")
 (module-browser-open-file-format "Ouvrir ~a")
 (module-browser "Navigateur de modules") ;; frame title
 (module-browser... "Navigateur de modules...") ;; menu item title
 (module-browser-error-expanding "Erreur durant l'expansion du programme:\n\n~a")
 
 )
