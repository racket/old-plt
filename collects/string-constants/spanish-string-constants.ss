(

 ;;; when translating this constant, substitue name of actual langauge for `English'
 (is-this-your-native-language
  "�Es espa�ol tu idioma materno?")

 (interact-with-drscheme-in-language "Interact�a con DrScheme en Espa�ol")

 (are-you-sure-you-want-to-switch-languages
  "Esto cambiar� el idioma de la interfaz, lo que requiere que reinicies DrScheme. �Estas seguro(a)?")
 

 (accept-and-quit "Aceptar y abandonar")
 (accept-and-exit "Aceptar y salir")

 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Cancelar")
 (untitled "Sin t�tulo")
 (untitled-n "Sin t�tulo ~a")
 (warning "Advertencia")
 (error "Error")
 (close "Cerrar") ;; as in, close an open window

 ;;; bug report form
 (cancel-bug-report? "�Cancelar el reporte de problemas?")
 (are-you-sure-cancel-bug-report?
  "�Estas seguro que quieres cancelar el env�o de �ste reporte de problemas?")
 (bug-report-form "Forma para reportar problemas")
 (bug-report-field-name "Nombre")
 (bug-report-field-email "Correo Electr�nico")
 (bug-report-field-summary "Resumen")
 (bug-report-field-severity "Gravedad")
 (bug-report-field-class "Clase")
 (bug-report-field-priority "Propiedad")
 (bug-report-field-description "Descripci�n")
 (bug-report-field-reproduce1 "Pasos para")
 (bug-report-field-reproduce2 "Reproducir")
 (bug-report-field-environment "Ambiente")
 (bug-report-field-tools "Herramientas")
 (bug-report-field-docs-installed "Documentos instalados")
 (bug-report-field-language "Lenguaje")
 (bug-report-field-teachpacks "Paquetes de ense�anza")
 (bug-report-field-collections "Colecciones")
 (bug-report-field-human-language "Idioma")
 (bug-report-field-version "Versi�n")
 (bug-report-show-synthesized-info "Muestra informaci�n resumida")
 (bug-report-submit "Enviar")
 (sending-bug-report "Enviando reporte de problemas")
 (error-sending-bug-report "Error al enviar el reporte de problemas")
 (error-sending-bug-report-expln "Un error ocurri� mientras enviaba �ste reporte de problemas.  Si tu conexi�n a Internet est� funcionando bien, por favor visita:\n\n    http://bugs.plt-scheme.org/\n\ny env�a el reporte de problemas por medio de la forma de web en esea p�gina de WEB. Sentimos mucho las molestias que esto te ocasiona.\n\nEl mensaje de error es:\n~a")
 (bug-report-sent "Reporte de problemas enviado")
 (bug-report-sent-detail "Gracias por el reporte. Recibir�s una confirmaci�n por correo electr�nico en los siguientes 30 minutos. Si dicho mensaje no llega, env�a un mensaje a scheme@plt-scheme.org.")
 (illegal-bug-report "Reporte de problemas ilegal")
 (pls-fill-in-field "Por favor requisita el campo \"~a\"")
 (malformed-email-address "Direcci�n de correo electr�nico inv�lida")

 ;;; check syntax
 (check-syntax "Revisa la sint�xis")
 (cs-italic "It�lica")
 (cs-bold "Negrita")
 (cs-underline "Subrayado")
 (cs-change-color "Cambiar color")
 (cs-tack/untack-arrow "Anclar/Liberar flecha")
 (cs-jump "Brincar")
 (cs-error-message "Mensaje de error")
 (cs-open-file "Abrir ~a")
 (cs-rename-var "Renombrar ~a")
 (cs-rename-id "Renombrar identificador")
 (cs-rename-var-to "Renombrar ~a a:")
 (cs-name-duplication-error "El nuevo nombre que has seleccionado, ~s, colisiona con otro nombre en este ambiente.")
 
 ;;; info bar at botttom of drscheme frame
 (collect-button-label "Recolectar")
 (read-only "S�lo lectura")
 (read/write "Lectura/Escritura")
 (auto-extend-selection "Selecci�n Auto-Extendida")
 (overwrite "Sobreescribir")
 (running "ejecutando")
 (not-running "suspendido") 
 
 ;;; misc
 (welcome-to-something "Bienvenido a ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Bienvenido a DrScheme, versi�n ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Bienvenido a DrScheme")
 (version/language "versi�n ~a, ~a")

 (goto-line "Ir a la l�nea")
 (goto-line-invalid-number
  "~a no es un n�mero de l�nea v�lido. Debe ser un entero entre 1 y ~a")
 (goto-position "Ir a la posici�n")
 (no-full-name-since-not-saved
  "El archivo no tienen un nombre completo porque no ha sido salvado a�n.")
 (cannot-open-because-dne "~a no puede ser abierto, porque no existe.")
 (interactions-out-of-sync
  "ADVERTENCIA: La ventana de interacci�n est� fuera de sincron�a con la ventana de definici�n.  Presione el bot�n Ejecutar.")
 (file-is-not-saved "El archivo \"~a\" no ha sido salvado.")
 (save "Salvar")
 (please-choose-either "Por favor seleccione una de \"~a\" o \"~a\"")
 (close-anyway "Cerrar y descartar cambios")

 (url "URL")
 (url: "URL:")
 (open-url... "Abre URL...")
 (open-url "Abre URL")
 (browse... "Navega...")
 (bad-url "URL Inv�lido")
 (bad-url:this "URL Inv�lido: ~a")
 
 ;; Help Desk
 (search-results "Buscar resultados")
 (help-desk "M�dulo de Ayuda")
 (help-desk-n "M�dulo de Ayuda ~a")
 (about-help-desk "Acerca del M�dulo de Ayuda")
 (help-desk-about-string
  "El M�dulo de Ayuda es una fuente complete de informaci�n acerca del software del grupo PLT, incluyendo DrScheme, MzScheme y MrEd.\n\nVersi�n ~a\nCopyright (c) 1995-2001 PLT")
 (help-on-help "Ayuda para la ayuda")
 (help-on-help-details "Para ayuda sobre el uso del M�dulo de Ayuda, sigue la liga `C�mo usar el M�dulo de Ayuda' desde el p�gina principal del M�dulo de Ayuda.  (Para llegar a la p�gina principal si no est�s ah� ya, presiona el bot�n marcado `Hogar' en la parte superior de la ventana del M�dulo de Ayuda.")
 (find-docs-for "Localiza la documentaci�n para:")
 (search "Buscar")
 ; next 3 are popup menu choices at bottom of help desk window
 (search-for-keyword "por Palabra clave")
 (search-for-keyword-or-index "por palabra clave o entrada en el �ndice")
 (search-for-keyword-or-index-or-text "por palabra clave, entrada en el �ndice o texto")
 (exact-match "patr�n exacto") ;; exact match??
 (containing-match "subcadena") ;; containing match ??
 (regexp-match "expresi�n regular") ;; match regexp
 (stop "Detener")
 (feeling-lucky "Me siento afortunado")
 (nothing-found-for-search-key "No se encontr� nada con \"~a\".")
 (searching "Buscando...")
 (search-stopped "(B�squeda detenida.)")
 (search-stopped-too-many-matches "(B�squeda detenida - demasiados patrones casan.)")
 (reload "Volver a cargar") ;; Reload
 (help "Ayuda")
 (searching... "Buscando...")
 (nothing-found-for-empty-search "Nada casa con la b�squeda vac�a")
 (nothing-found-for "Nada casa con ~a")
 (and "y")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "�Deseas instalar ~a, o quieres abrirlo para edici�n?")
 (install-plt-file/yes "Instalar")
 (install-plt-file/no "Editar")
 
 ;;; about box
 (about-drscheme-frame-title "Acerca de DrScheme")
 (take-a-tour "�Haz un recorrido!")
 (release-notes "Notas sobre �sta versi�n")
 (parenthetical-last-version "(versi�n anterior ~a)")
 (parenthetical-last-language "(idioma anterior ~a)")
 (parenthetical-last-version/language "(versi�n anterior ~a, lenguaje ~a)")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "�Salvar este archivo como texto plano?")
 (save-in-drs-format "�Salvar este archivo en el formato, no texto, particular de DrScheme?")
 (yes "Si")
 (no "No")
 
 ;;; preferences
 (preferences "Preferencias")
 (saving-preferences "Salvar preferencias")
 (error-unmarshalling "Error mientras desempacaba la preferencia ~a")
 (error-saving-preferences "Error al salvar preferencias: ~a")
 (error-reading-preferences "Error al leer preferencias")
 (expected-list-of-length2 "esperaba un lista de longitud 2")
 (highlight-parens "Resaltar entre parejas de par�ntesis")
 (fixup-parens "Corrige par�ntesis")
 (flash-paren-match "Se�ala el par�ntesis que casa")
 (auto-save-files "Auto-salva archivos")
 (map-delete-to-backspace "Cambia suprimir por backspace")
 (verify-exit "Confirmar salida")
 (ask-before-changing-format "Preguntar antes de cambiar el formato de salida")
 (wrap-words-in-editor-buffers "Ajustar al border palabras en el editor")
 (show-status-line "Mostrar l�nea de estado")
 (count-from-one "Cuenta n�meros de columna y l�neas a partir de uno") 
 (display-line-numbers "Muestra n�meros de l�nea en el contenedor; sin desplazamiento de caracteres")
 (enable-keybindings-in-menus "Habilita enlaces de teclas en los men�s")
 (automatically-to-ps "Imprime autom�ticamente a un archivo en postscript")
 (use-mdi "Utiliza Ventanas MDI") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Use el di�logo adecuado para buscar")
 (default-fonts "Fuentes por omisi�n")
 
 ; should have entire alphabet
 (font-example-string "abcdefghijklmn�opqrstuvxyz�!�?��.")  ;; FIXME: shoulde this have special characters?

 (change-font-button-label "Cambiar")
 (fonts "Fuentes")

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Por favor selecciona una nueva fuente \"~a\"")

 (font-size-slider-label "Tama�o")
 (restart-to-see-font-changes "Re-inicia para ver los cambios de las fuentes")

 (font-prefs-panel-title "Fuente")
 (font-name "Nombre de fuente")
 (font-size "Tama�o de fuente")
 (set-font "Ver Fuente...")
 (select-font-name "Selecciona un nombre de Fuente")
 (example-text "Texto de ejemplo:")
 (only-warn-once "S�lo advierte una vez cuando las ejecuci�n e interacciones no est�n sincronizadas")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Sangrar")  ;; To indent is "Sangrado de m�rgenes"

 ; filled with define, lambda, or begin
 (enter-new-keyword "Teclea una nueva palabra clave parecida a ~a:")
 (x-keyword "Palabra clave ~a")
 (x-like-keywords "Palabra clave parecida a ~a")

 (expected-a-symbol "esperaba un s�mbolo, encontr�: ~a")
 (already-used-keyword "la palabra clave \"~a\" ya ten�a un sangrado especial asignado")
 (add-keyword "A�ade")
 (remove-keyword "Borra")
 
 ;;; find/replace
 (find-and-replace "Buscar y reemplazar")
 (find "Buscar")
 (replace "Reemplazar")
 (use-separate-dialog-for-searching "Use el di�logo adecuado para buscar")
 (replace&find-again "Reemplazar && Vuelve a buscar") ;;; need double & to get a single &
 (replace-to-end "Reemplazar hasta el final")
 (forward "Hacia adelante")
 (backward "Hacia atr�s")
 (hide "Esconder")
 
 ;;;reverting a file
 (error-reverting "DrScheme - Error al Revertir")
 (could-not-read "no pude leer \"~a\"")
 
 ;;; finder dialog
 (must-specify-a-filename "Debes especificar un nombre de archivo")
 (file-does-not-exist "El archivo \"~a\" no existe.")
 (ask-because-file-exists "El archivo \"~a\" ya exist�a. �Deseas reemplazarlo?")
 (dne-or-cycle "El archivo \"~a\" contiene un directorio inexistente o un ciclo.")
 (get-file "Obtener archivo")
 (put-file "Poner archivo")
 (full-pathname "Ruta completa")
 (show-dot-files "Muestra archivos y directorio que comiencen con un punto.")
 (up-directory-button-label "Arriba")
 (add-button-label "A�adir") ;;; for multi-file selection
 (add-all-button-label "A�adir todos") ;;; for multi-file selection
 (remove-button-label "Eliminar") ;;; for multi-file selection
 (file-wrong-form "Ese archivo no tiene el formato correcto.")
 (select-files "Selecciona archivos")
 (select-file "Selecciona un archivo")
 (dir-dne "El directorio no existe.")
 (file-dne "El archivo no existe.")
 (empty-filename "El nombre del archivo debe contener al menos algunas letras.")
 (that-is-dir-name "Eso es un nombre de directorio.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrScheme's menus will appear
 ;;; in the wrong order.
 (file-menu "Archivo")
 (edit-menu "Edici�n")
 (help-menu "Ayuda")
 (windows-menu "Ventana")
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label-windows "&Archivo")
 (file-menu-label-other "A&rchivo")

 (new-info  "Abre un nuevo archivo")
 (new-menu-item "&Nuevo")

 (open-info "Abre un nuevo archivo del disco")
 (open-menu-item "&Abrir...")

 (open-recent-info "Una lista de archivos abiertos recientemente")
 (open-recent-menu-item "Abrir reciente")

 (revert-info "Revertir este archivo a la copia en disco")
 (revert-menu-item "&Revertir")

 (save-info "Salva este archivo a disco")
 (save-menu-item "&Guardar")

 (save-as-info "Te pide un nombre para salvar este archivo a disco")
 (save-as-menu-item "Guardar &como ...")

 (print-info "Env�a este archivo a una impresora")
 (print-menu-item "&Imprimir...")

 (close-info "Cierra este archivo")
 (close-menu-item "&Cerrar")

 (quit-info "Cierra todas las ventanas")
 (quit-menu-item-windows "&Salir")
 (quit-menu-item-others "&Salir") ;; FIXME: salir is exit, so quit is ???
 
 (edit-menu-label "&Edici�n")
 
 (undo-info "Deshace la acci�n m�s reciente")
 (undo-menu-item "&Deshacer")

 (redo-info "Deshace el m�s reciente deshacer")
 (redo-menu-item "&Rehacer")

 (cut-info "Mueve los elementos seleccionados al porta-papeles para pegarlos m�s tarde")
 (cut-menu-item "Cor&tar")

 (copy-info "Copia los elementos seleccionados al porta-papeles para pegarlos m�s tarde")
 (copy-menu-item "&Copiar")

 (paste-info "Pega los elementos copiados o cortados m�s recientemente en lugar de los objetos seleccionados")
 (paste-menu-item "&Pegar")

 (clear-info "Borra los elementos seleccionados sin afectar el porta-papeles o el pegado")
 (clear-menu-item-others "Limpiar")
 (clear-menu-item-windows "Borr&ar")

 (select-all-info "Selecciona el documento completo")
 (select-all-menu-item "&Selecciona todo")
 
 (find-info "Busca una cadena")
 (find-menu-item "&Buscar...")

 (find-again-info "Busca la misma cadena que antes")
 (find-again-menu-item "Volver a buscar")
 
 (replace-and-find-again-info "Reemplaza el texto actual y busca por la misma cadena que antes")
 (replace-and-find-again-menu-item "Reemplaza && buscar otra vez")

 (preferences-info "Configura tus preferencias")
 (preferences-menu-item "Personalizar...")

 (keybindings-info "Muestra los enlaces de tecla activos")
 (keybindings-menu-item "Enlaces de teclas")
 (keybindings-frame-title "Enlaces de teclas")
 (keybindings-sort-by-name "Ordena por Nombre")
 (keybindings-sort-by-key "Ordena por Llave")

 (insert-text-box-item "Inserta caja de texto")
 (insert-pb-box-item "Inserta caja de porta-papeles")
 (insert-image-item "Inserta imagen...")
 (wrap-text-item "Ajustar texto al borde")

 (windows-menu-label "&Ventana")
 (bring-frame-to-front "Traer ventana al frente")
 (bring-frame-to-front... "Traer ventana al frente...")

 (show-menu-label "&Muestra")
 (show-overview "Mostrar Panorama")
 (hide-overview "Esconder Panorama")

 (help-menu-label "&Ayuda")
 (about-info "Cr�ditos y detalles de esta apliaci�n")
 (about-menu-item "Acerca ...")

 ;;; help-desk-specific menus
 (new-help-desk "&Nuevo M�dulo de Ayuda")
 
 ;;; exiting and quitting are you sure dialog
 ;;; (exit is used on windows, quit on macos. go figure)
 (exit "Salir")
 (quit "Abandonar") ;; FIXME: Quit
 ;;; in are-you-sure-format, either exit or quit is filled in (from above)
 ;;; based on the platform drscheme is running on.
 (are-you-sure-exit "�Estas seguro(a) que deseas salir?")
 (are-you-sure-quit "�Estas seguro(a) que deseas abandonar?") ;; FIXME: Quit
 
 ;;; autosaving
 (error-autosaving "Error al auto-salvar \"~a\".")
 (autosaving-turned-off "Auto-salvar est� desactivado hasta\n que el archivo sea salvado.")
 
 ;;; file modified warning
 (file-has-been-modified
  "El archivo ha sido modificado desde la �ltima vez que fue salvado. �Sobreescribo las modificaciones?")
 (overwrite-file-button-label "Sobreescribir")
 
 (definitions-modified 
  "El texto de las definiciones ha sido modificado en el sistema de archivos; por favor salve o regrese el texto de las definiciones.")
 (drscheme-internal-error "Error interno de DrScheme")
 
 ;;; tools
 (invalid-tool-spec "La especificaci�n de la herramienta, especificada en el archivo info.ss de la colecci�n ~a, es inv�lida.  Esperaba una cadena o una lista no vac�a de cadenas y recib�: ~e")
 (error-loading-tool-title "DrScheme - Error al cargar la herramienta ~s; ~s")
 (error-invoking-tool-title "Error al invocar la herramienta ~s;~s")
 (tool-tool-names-same-length
  "esperaba que `tool-names' y `tools', en el archivo info.ss de ~s, fueran listas de la misma longitud, pero obtuve ~e y ~e")
 (tool-tool-icons-same-length
  "esperaba que `tool-icons' y  `tools', en el archivo info.ss de ~s, fueran listas de la misma longitud, pero obtuve ~e y ~e")
 (error-getting-info-tool
  "error al cargar el archivo info.ss de ~s")
 
 ;;; define popup menu
 (end-of-buffer-define "<< fin de contenedor (buffer) >>") ;; FIXME: buffer --> almacenador intermediario ?
 (sort-by-name "Ordena por nombre")
 (sort-by-position "Ordena por posici�n en el archivo")
 (no-definitions-found "<< no se encontraron definiciones >>")

  ;;; show menu
 (hide-definitions-menu-item-label "Esconder &Definiciones")
 (show-definitions-menu-item-label "Mostrar &Definiciones")
 (definitions-menu-item-help-string "Mostrar/Esconder la ventana de definiciones")
 (show-interactions-menu-item-label "Mostrar &Interacciones")
 (hide-interactions-menu-item-label "Esconder &Interacciones")
 (interactions-menu-item-help-string "Mostrar/Esconder la ventana de Interacciones")

 ;;; file menu
 (save-definitions-as "Salvar Definiciones como...")
 (save-definitions "Salvar Definiciones")
 (print-definitions "Imprimir Definiciones...")
 (about-drscheme "Acerca DrScheme")
 (save-other "Salvar otros")
 (save-definitions-as-text "Salvar Definiciones como Texto...")
 (save-interactions "Salvar Interacciones")
 (save-interactions-as "Salvar Interacciones como...")
 (save-interactions-as-text "Salvar Interacciones como Texto...")
 (print-interactions "Imprimir Interacciones...")
 
 ;;; edit-menu
 (split-menu-item-label "&Dividir")
 (collapse-menu-item-label "C&olapsar")
 
 ;;; language menu
 (language-menu-name "&Lenguaje")
 
 ;;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Ejecutar")
 (execute-menu-item-help-string "Reinicia el programa en la ventana de definiciones")
 (break-menu-item-label "Interrumpir")
 (break-menu-item-help-string "Interrumpe la evaluaci�n actual")
 (kill-menu-item-label "Terminar")
 (kill-menu-item-help-string "Terminar la evaluaci�n actual")
 (reindent-menu-item-label "&Re-sangrar")
 (reindent-all-menu-item-label "Re-sangrar &TODO")
 (comment-out-menu-item-label "&Comentar")
 (uncomment-menu-item-label "&Des-comentar")
 
 ;;; buttons
 (execute-button-label "Ejecutar") 
 (save-button-label "Salvar")
 (break-button-label "Interrumpir")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Busca en el M�dulo de Ayuda \"~a\"")
 (exact-lucky-search-help-desk-for "B�squeda precisa y con suerte en el M�dulo de Ayuda \"~a\"") ;; FIXME:  Exact lucky search in Help Desk for
 
 ;; collapse and expand popup menus
 (collapse-sexp "Colapsar expresi�n-s")
 (expand-sexp "Expandir expresi�n-s")

 ;;; fraction dialog
 (enter-fraction "Introducir Fracci�n")
 (whole-part "Parte entera")
 (numerator "Numerador")
 (denominator "Denominador")
 (invalid-number "N�mero inv�lido: debe ser un n�mero real exacto, no entero.")
 (insert-fraction-menu-item-label "Insertar Fracci�n...")
 
 ;;; TeachPack messages
 (select-a-teachpack "Selecciona un Paquete de Ense�anza")
 (clear-teachpack "Limpia el Paquete de Ense�anza ~a")
 (teachpack-error-label "DrScheme - Paquete de Ense�anza error")
 (teachpack-dne/cant-read "El archivo del Paquete de Ense�anza ~a no existe o no puede ser le�do.")
 (teachpack-didnt-load "El archivo del Paquete de Ense�anza ~a no se carg� apropiadamente.")
 (teachpack-error-invoke "El archivo del Paquete de Ense�anza ~a lanz� un error durante su invocaci�n.")
 (add-teachpack-menu-item-label "A�adir un Paquete de Ense�anza...")
 (clear-all-teachpacks-menu-item-label "Limpia Todos los Paquetes de Ense�anza")
 (teachpack-not-only-one-import "La unit/sig del Paquete de Ense�anza ~a debe tener exactamente una clausula de importaci�n.")
 (drscheme-teachpack-message-title "DrScheme Paquete de Ense�anza")
 (already-added-teachpack "El paquete de ense�anza ~a ya estaba cargado")
 
 ;;; Language dialog
 (introduction-to-language-dialog
  "Por favor selecciona un lenguaje.  La mayor�a de los estudiantes de cursos introductorios deber�an usar el lenguaje por omisi�n.")
 (language-dialog-title "Configurar Lenguaje")
 (case-sensitive-label "Sensible a may�sculas") ;; FIXME: Case sensitive
 (output-style-label "Estilo de salida")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (sharing-printing-label "Show sharing in values")
 (use-pretty-printer-label "Insertar caracteres de nueva l�nea en valores impresos")
 (input-syntax "Sint�xis de Entrada")
 (dynamic-properties "Propiedades din�micas")
 (output-syntax "Sint�xis de Salida")
 (debugging "Depurando")
 (whole/fractional-exact-numbers-label "Imprimir n�meros como fracciones")
 (booleans-as-true/false-label "Imprimir valores booleanos usando true y false")
 (show-details-button-label "Mostrar Detalles")
 (hide-details-button-label "Esconder Detalles")
 (choose-language-menu-item-label "Escoger Lenguaje...")
 (revert-to-language-defaults "Revertir a los Valores por Omisi�n del Lenguaje")

 ;;; languages
 (beginning-student "Estudiante Principiante")
 (beginning-student/abbrev "Estudiante Principiante con Abreviaturas de Listas")
 (intermediate-student "Estudiante Intermedio")
 (advanced-student "Estudiante Avanzado")
 (full-language "Completo")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (r5rs-like-languages "Similar a R5RS")
 (r5rs-lang-name "R5RS sin depuraci�n (MzScheme)")
 (unknown-debug-frame "[desconocido]")
 
 ;;; debug language
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "Interacciones de ~a") ;; filled with a filename
 (mzscheme-w/debug "Texto (MzScheme)")
 (mred-w/debug "Gr�fico (MrEd)")
 
 ;;; repl stuff
 (evaluation-terminated "Evaluaci�n Terminada")
 (evaluation-terminated-explanation
  "El hilo de control de la evaluaci�n ya no se est� ejecutando, por lo que no se puede efectuar ninguna evaluaci�n hasta la siguiente ejecuci�n.")
 (last-stack-frame "Mostrar el �ltimo marco (frame) en el stack")
 (last-stack-frames "Mostrar los �ltimos ~a marcos (frames)")
 (next-stack-frames "Mostrar los siguientes ~a marcos (frames) en el stack")
 
 ;;; welcoming message in repl
 (language "Idioma")
 (custom "custom")
 (teachpack "Paquete de Ense�anza")
 (welcome-to "Bienvenido a")
 (version "versi�n")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "�Quieres terminar la evaluaci�n?")
 (just-break "Interrupci�n Simple")
 (kill "Terminar")
 (kill? "�Terminar?")

   (&stop "&Detener")
   (advanced-one-line-summary "Intermedio con lambda y mutaci�n")
   (are-you-sure-revert "�Estas seguro que deseas revertir este archivo? Este cambio no puede deshacerse.")
   (are-you-sure-revert-title "�Revertir?")
   (beginning-one-line-summary "define, cond, structuras, constantes, y primitivas")
   (beginning/abbrev-one-line-summary "Principiante, con impresi�n de listas en el REPL")
   ;; REPL --> Ciclo de Lectura Escritura e Impresi�n (�CLEI?)
   ;; Nobody will know what CLEI is, I think. OTHO, most people won't know what REPL stands for either.
   (browser "Navegador")
   (bug-report-synthesized-information "Informaci�n Sintetizada")
   (cannot-display-url "No puedo desplegar URL ~s: ~a")
   (create-executable-menu-item-label "Crear ejecutable...")
   (create-executable-title "Crear Ejecutable")
   (definitions-not-saved "La ventana de definiciones no ha sido salvada. El ejecutable usar� la �ltima versi�n salvada de la ventana de definiciones. �Desea continuar?")
   (do-you-want-to-install-it? "�Desea intalarlo?")
   (dock "Atracar")
   (download "Descargar")
   (download-and-install "Descargar && Instalar")
   (download-was-saved "Se ha salvado el archivo descargado.")
   (downloading "Descargar")
   (downloading-file... "Descargando archivo...")
   (drscheme-homepage "DrScheme")
   (error-finding-docs "No pude encontrar la documentaci�n.\n\n~a")
   (error-saving "Error al Salvar")
   (error-saving-file/name "Hubo un error mientras salvaba ~a.")
   (forward-in-browser-history "Adelante")
   (getting-page "Obteniendo P�gina")
   (help-menu-check-for-updates "Buscando Actualizaciones...")
   (home "Hogar")
   (how-to-use-scheme "C�mo Usar Scheme")
   (http-proxy "Proxy de HTTP")
   (insert-large-letters... "Insertar Letras Grandes...")
   (install? "�Instalar?")
   (intermediate-one-line-summary "Principiante m�s alcance l�xico")
   (intermediate-student/lambda "Estudiante Intermedio con lambda")
   (intermediate/lambda-one-line-summary "Intermedio m�s funciones de alto nivel")
   (language-docs-button-label "Documentaci�n de Lenguajes")
   (large-semicolon-letters "Letas Dos Puntos Grandes")
   (mfs-case-sensitive-label "Sensible al Tama�o")
   (mfs-configure-search "Configuraci�n de B�squeda")
   (mfs-dir "Dir")
   (mfs-drscheme-multi-file-search "DrScheme - B�squeda Multi Archivo")
   (mfs-files-section "Archivos")
   (mfs-multi-file-search-menu-item "Buscar en archivos...")
   (mfs-no-matches-found "Nada cas� con la b�squeda.")
   (mfs-not-a-dir "\"~a\" no es un directorio")
   (mfs-open-file "Abrir Archivo")
   (mfs-recur-over-subdirectories "Recurrir sobre subdirectorios")
   (mfs-regexp-filename-filter "Filtro de nombres de archivo con expresiones regulares")
   (mfs-regexp-match/no-graphics "Expresi�n Regular (s�lo archivos de texto plano)")
   (mfs-search-interrupted "B�squeda abortada.")
   (mfs-search-section "Buscar")
   (mfs-search-string "Buscar cadena")
   (mfs-searching... "Buscando...")
   (mfs-stop-search "Detener B�squeda")
   (mfs-string-match/graphics "B�squeda de Cadenas (maneja archivos con gr�ficas)")
   (module-language-one-line-summary "Lenguaje con  module como la �nica construcci�n")
   (mred-one-line-summary "PLT Scheme m�s biblioteca IGU")
   (must-save-before-executable "Debes salvar tu programa antes de hacer un ejecutable.")
   (mzscheme-one-line-summary "PLT Scheme sin biblioteca IGU")
   (package-was-installed "El paquete fu� instalado.")
   (paren-file-size "(El archivo mide ~a bytes)")
   (pls-fill-in-either-description-or-reproduce "Por favor llena ya sea el campo Descripci�n o el campo Pasos para reproducir")
   (plt-homepage "PLT")
   (pref-lock-not-gone "El candado de preferencias:\n\n   ~a\n\nevita que las preferencias puedan salvarse. Aseg�rese de que ninguna otra instancia de software del PLT est� corriendo y borre dicho archivo.")
   (proxy-bad-host "El Host Proxy est� mal especificado")
   (proxy-direct-connection "Coneci�n Directa")
   (proxy-host "Host")
   (proxy-port "Port")
   (proxy-use-proxy "Utilizar proxy:")
   (r5rs-one-line-summary "R5RS, sin adornos extra")
   (rewind-in-browser-history "Revertir")
   (save-an-executable "Salvar un Ejecutable")
   (save-downloaded-file "Salvar el archivo descargado como")
   (save-downloaded-file/size "Salvar el archivo descargado (~a bytes) como")
   (still-locked-exit-anyway? "Las preferencias no fueron salvadas exitosamente. �Salir de cualquier manera?")
   (teachscheme!-homepage "TeachScheme!")
   (text-to-insert "Texto a insertar")
   (undock "Des-atracar")
   (use-mred-binary? "�Utilizar el binario mred para este ejecutable?\n\nSi s�, tu programa puede usar la biblioteca  (lib \"mred.ss\" \"mred\").  Si no, DrScheme usar� mzscheme como binario para este ejecutable y no podr�s usar la biblioteca mencionada.\n\nSi no est� seguro(a) escoga S�")
   (vc-binary-information-format "Versi�n binaria instalada: ~a (iteraci�n ~a)")
   (vc-binary-name "Binario")
   (vc-cannot-connect "No puedo conectarme al servidor de versi�n PLT")
   (vc-check-prompt "�Buscar actualizaciones de software del PLT en Internet?")
   (vc-connecting-version-server "Conectando al servidor de versi�n PLT")
   (vc-latest-binary-information-format "�ltima versi�n liberada: ~a (iteraci�n ~a)")
   (vc-need-update-string "Uno o m�s de los paquetes de software PLT necesitan ser actualizados")
   (vc-network-failure "Falla de Red")
   (vc-network-timeout "La conexi�n de red ha expirado")
   (vc-no-update-string "Todo el software PLT instalado est� al d�a")
   (vc-old-binaries "Los binarios instalados para DrScheme (o MzScheme) no est�n actualizados")
   (vc-please-wait "Por favor espere")
   (vc-update-check "Revisar Actualizaci�n")
   (vc-update-dialog-title "PLT estatus de actualizaci�n")
   (vc-update-format "~a v.~a (iteraci�n ~a) requiere ser actualizado a v.~a (iteraci�n ~a)")
   (vc-updates-available "Las actualizaciones est�n disponibles en")
   (waiting-for-pref-lock "Esperando por el candado para las preferencias...")
   (web-materials "Sitios de Web Relacionados")
   (you-have-selected-an-installable-package "Ha seleccionado un paquete instalable.")

   (vc-current-format "~a v.~a (iteraci�n ~a) est� actualizada")
   (vc-details-format "~a~nDetalles:~n~a")
   (vc-details-text "Detalles:~n")
   (vc-error-format "Error: ~a")

(backup-files "Archivos de respaldo")
(current-definitions "definiciones")
(current-interactions "interacciones")
(debugging-and-profiling "Depurando y delineando")
(decimal-notation-for-rationals "Use notaci�n decimal para los racionales")
(editor-prefs-panel-label "Editando")
(fraction-style "Estilo de fracci�n")
(happy-birthday-matthias "�Feliz cumplea�os Matthias!")
(inline-saved-program-in-executable/windows "Advertencia: en windows tambi�n necesitas libmred.dll, libmzsch.dll, y libmzgc.dll para correr ejecutables auto-contenidos.")
(manual-installed-date "(~a instalado)")
(module-browser "M�dulo Navegador")
(module-browser-adding-file "A�adiendo archivo: ~a...")
(module-browser-error-expanding "Error al expandir el programa:\n\n~a")
(module-browser-filename-format "Nombre de archivo completo: ~a (~a l�neas)")
(module-browser-font-size-gauge-label "Tama�o de fuente")
(module-browser-laying-out-graph-label "Depositando gr�fica")
(module-browser-open-file-format "Abriendo ~a")
(module-browser-progress-label "M�dulo de resumen de progreso")
(module-browser-root-filename "Ra�z del archivo: ~a")
(module-browser... "M�dulo Navegador...")
(mrflow-button-title "Analizar")
(mrflow-coloring-error "Estilo no definido para el color: ~a")
(mrflow-coloring-error-title "Color desconocido")
;(mrflow-language-primitives-error "Nombre de archivo incorrecto para la tabla de tipos primitivos del lenguaje: ~a")
;(mrflow-language-primitives-error-title "Error de Primitivas del Lenguaje")
(mrflow-popup-menu-hide-errors "Esconder Errores")
(mrflow-popup-menu-hide-type "Esconder Tipo")
(mrflow-popup-menu-show-errors "Mostrar Errores")
(mrflow-popup-menu-show-type "Mostrar Tipo")
(mrflow-popup-menu-tack-all-arrows "Ligar todas las Flechas")
(mrflow-popup-menu-untack-all-arrows "Desligar todas las Flechas")
;(mrflow-read-exception "Leer excepci�n: ~a")
;(mrflow-read-exception-title "Leer Excepci�n")
;(mrflow-syntax-exception "Excepci�n de sintaxis: ~a")
;(mrflow-syntax-exception-title "Excepci�n de Sintaxis")
;(mrflow-unknown-exception-title "Excepci�n Desconocida")
;(mrflow-unknown-exception "Excepci�n Desconocida: ~a")
(no-debugging-or-profiling "No depurando o delineando")
(open-here-menu-item "&Abrir aqu�...")
(pretty-big-scheme "Scheme muy grande")
(pretty-big-scheme-one-line-summary "Gr�fico, con muchas bibliotecas est�ndar")
(profiling "Delineando")
(profiling-choose-high-color "Por favor selecciona un color fuerte")
(profiling-choose-low-color "Por favor selecciona un color tenue")
(profiling-clear "Limpiar delineado")
(profiling-clear? "Cambiar la ventana de definiciones invalida la informaci�n de delineado. �Continuar?")
(profiling-col-calls "Llamadas")
(profiling-col-function "Funci�n")
(profiling-col-name "Nombre")
(profiling-col-percent-time "% Tiempo")
(profiling-col-time-in-msec "Msec")
(profiling-color-config "Delineando Rango de Color")
(profiling-example-text "(define (whee) (whee))")
(profiling-hide-profile "Esconder Profile")
(profiling-high-color "Fuerte")
(profiling-linear "Lineal")
(profiling-low-color "Tenue")
(profiling-no-information-available "No hay informaci�n de delineado disponible. Por favor, aseg�rate que la opci�n de delineado est� activada para el lenguaje y de haber ejecutado su programa.")
(profiling-number "N�mero de llamadas")
(profiling-scale "Delineando Escala de Color")
(profiling-show-profile "Muestra Delineado")
(profiling-sqrt "Ra�z cuadrada")
(profiling-square "Cuadrado")
(profiling-time "Tiempo acumulado")
(profiling-unknown-src "<< desconocido >>")
(profiling-update "Actualiza Delineado")
(recent-items-sort-by-age "Ordena por Edad")
(recent-items-sort-by-name "Ordena por Nombre")
(refresh-deleting... "Borrando la versi�n vieja de ~a...")
(refresh-downloading... "Bajando ~a...")
(refresh-installing... "Instalando nueva versi�n de ~a...")
(refreshing-manuals "Bajando (nuevamente) los Manuales")
(reuse-existing-frames "Reutilizar marcos existentes cuando se abre un nuevo archivo")
(scheme-prefs-panel-label "Scheme")
(show-decimal-expansion "Ver expansi�n decimal")
(show-fraction-view "Ver como fracci�n")
(show-more-decimal-places "Muestra m�s posiciones decimales")
(show-recent-items-window-label "Archivos abiertos recientemente")
(show-recent-items-window-menu-item "Muestra Archivos Abiertos recientemente en otra ventana")
(special-menu "Especial")
(switch-anyway "Cambia de archivo de cualquier forma")
(tool-error-phase1 "Error en la fase 1 de la herramienta ~s; ~s")
(tool-error-phase2 "Error en la fase  2 de la herramienta ~s; ~s")
(use-mixed-fractions "Fracciones mixtas")
(use-repeating-decimals "Repetici�n de decimales")
(warnings-prefs-panel-label "Advertencias")
(xml-tool-eliminate-whitespace-in-empty-tags "Eliminar blancos en etiquetas vac�as")
(xml-tool-insert-scheme-box "Insertar Caja de Scheme")
(xml-tool-insert-scheme-splice-box "Insertar Caja de Uni�n de Scheme")
(xml-tool-insert-xml-box "Insertar Caja de XML")
(xml-tool-leave-whitespace-alone "Dejar blancos por la paz")
(xml-tool-menu "XML")
(xml-tool-scheme-box "Caja de Scheme")
(xml-tool-scheme-splice-box "Caja de Uni�n de Scheme")
(xml-tool-switch-to-scheme "Cambiar a caja de  Scheme")
(xml-tool-switch-to-scheme-splice "Cambiar a caja de uni�n de Scheme")
(xml-tool-xml-box "Caja de XML")

 )
