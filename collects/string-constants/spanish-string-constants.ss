(

 ;;; when translating this constant, substitue name of actual langauge for `English'
 (is-this-your-native-language
  "¿es español tu idioma materno?")

 (are-you-sure-you-want-to-switch-languages
  "Esto cambiará el idioma de la interfaz, lo que requiere que reinicies DrScheme. ¿Estas seguro(a)?")
 
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Cancelar")
 (untitled "Sin título")
 (untitled-n "Sin título ~a")
 (warning "Advertencia")
 (error "Error")
 (close "Cerrar") ;; as in, close an open window

 ;;; bug report form
 (cancel-bug-report? "¿Cancelar el reporte de problemas?")
 (are-you-sure-cancel-bug-report?
  "¿Estas seguro que quieres cancelar el envío de éste reporte de problemas?")
 (bug-report-form "Forma para reportar problemas")
 (bug-report-field-name "Nombre")
 (bug-report-field-email "Correo Electrónico")
 (bug-report-field-summary "Resumen")
 (bug-report-field-severity "Gravedad")
 (bug-report-field-class "Clase")
 (bug-report-field-priority "Propiedad")
 (bug-report-field-description "Descripción")
 (bug-report-field-reproduce1 "Pasos para")
 (bug-report-field-reproduce2 "Reproducir")
 (bug-report-field-environment "Ambiente")
 (bug-report-field-tools "Herramientas")
 (bug-report-field-docs-installed "Documentos instalados")
 (bug-report-field-language "Lenguaje")
 (bug-report-field-teachpacks "Paquetes de enseñanza")
 (bug-report-field-collections "Colecciones")
 (bug-report-field-human-language "Idioma")
 (bug-report-field-version "Versión")
 (bug-report-show-synthesized-info "Muestra información resumida")
 (bug-report-hide-synthesized-info "Esconde información resumida")
 (bug-report-submit "Enviar")
 (sending-bug-report "Enviando reporte de problemas")
 (error-sending-bug-report "Error al enviar el reporte de problemas")
 (error-sending-bug-report-expln "Un error ocurrió mientras enviaba éste reporte de problemas.  Si tu conexión a Internet está funcionando bien, por favor visita:\n\n    http://www.cs.rice.edu/CS/PLT/Bugs/\n\ny envía el reporte de problemas por medio de la forma de web en esea página de WEB. Sentimos mucho las molestias que esto te ocasiona.\n\nEl mensaje de error es:\n~a")
 (bug-report-sent "Reporte de problemas enviado")
 (bug-report-sent-detail "Gracias por el reporte. Recibirás una confirmación por correo electrónico en los siguientes 30 minutos. Si dicho mensaje no llega, envía un mensaje a scheme@cs.rice.edu.")
 (illegal-bug-report "Reporte de problemas ilegal")
 (pls-fill-in-field "Por favor requisita el campo \"~a\"")
 (malformed-email-address "Dirección de correo electrónico inválida")

 ;;; check syntax
 (check-syntax "Revisa la sintáxis")
 (cs-italic "Itálica")
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
 (read-only "Sólo lectura")
 (read/write "Lectura/Escritura")
 (auto-extend-selection "Selección Auto-Extendida")
 (overwrite "Sobreescribir")
 (running "ejecutando")
 (not-running "suspendido") 
 
 ;;; misc
 (welcome-to-something "Bienvenido a ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Bienvenido a DrScheme, versión ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Bienvenido a DrScheme")
 (version/language "versión ~a, ~a")

 (goto-line "Ir a la línea")
 (goto-line-invalid-number
  "~a no es un número de línea válido. Debe ser un entero entre 1 y ~a")
 (goto-position "Ir a la posición")
 (no-full-name-since-not-saved
  "El archivo no tienen un nombre completo porque no ha sido salvado aún.")
 (cannot-open-because-dne "~a no puede ser abierto, porque no existe.")
 (interactions-out-of-sync
  "ADVERTENCIA: La ventana de interacción está fuera de sincronía con la ventana de definición.  Presione el botón Ejecutar.")
 (file-is-not-saved "El archivo \"~a\" no ha sido salvado.")
 (save "Salvar")
 (please-choose-either "Por favor seleccione una de \"~a\" o \"~a\"")
 (close-anyway "Cerrar y descartar cambios")

 (url "URL")
 (url: "URL:")
 (open-url... "Abre URL...")
 (open-url "Abre URL")
 (browse... "Navega...")
 (bad-url "URL Inválido")
 (bad-url:this "URL Inválido: ~a")
 
 ;; Help Desk
 (search-results "Buscar resultados")
 (help-desk "Módulo de Ayuda")
 (help-desk-n "Módulo de Ayuda ~a")
 (about-help-desk "Acerca del Módulo de Ayuda")
 (help-desk-about-string
  "El Módulo de Ayuda es una fuente complete de información acerca del software del grupo PLT, incluyendo DrScheme, MzScheme y MrEd.\n\nVersión ~a\nCopyright (c) 1995-2001 PLT")
 (help-on-help "Ayuda para la ayuda")
 (help-on-help-details "Para ayuda sobre el uso del Módulo de Ayuda, sigue la liga `Cómo usar el Módulo de Ayuda' desde el página principal del Módulo de Ayuda.  (Para llegar a la página principal si no estás ahí ya, presiona el botón marcado `Hogar' en la parte superior de la ventana del Módulo de Ayuda.")
 (find-docs-for "Localiza la documentación para:")
 (search "Buscar")
 ; next 3 are popup menu choices at bottom of help desk window
 (search-for-keyword "por Palabra clave")
 (search-for-keyword-or-index "por palabra clave o entrada en el índice")
 (search-for-keyword-or-index-or-text "por palabra clave, entrada en el índice o texto")
 (exact-match "patrón exacto") ;; exact match??
 (containing-match "subcadena") ;; containing match ??
 (regexp-match "expresión regular") ;; match regexp
 (stop "Detener")
 (feeling-lucky "Me siento afortunado")
 (nothing-found-for-search-key "No se encontró nada con \"~a\".")
 (searching "Buscando...")
 (search-stopped "(Búsqueda detenida.)")
 (search-stopped-too-many-matches "(Búsqueda detenida - demasiados patrones casan.)")
 (reload "Volver a cargar") ;; Reload
 (help "Ayuda")
 (searching... "Buscando...")
 (nothing-found-for-empty-search "Nada casa con la búsqueda vacía")
 (nothing-found-for "Nada casa con ~a")
 (and "y")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "¿Deseas instalar ~a, o quieres abrirlo para edición?")
 (install-plt-file/yes "Instalar")
 (install-plt-file/no "Editar")
 
 ;;; about box
 (about-drscheme-frame-title "Acerca de DrScheme")
 (take-a-tour "¡Haz un recorrido!")
 (release-notes "Notas sobre ésta versión")
 (parenthetical-last-version "(versión anterior ~a)")
 (parenthetical-last-language "(idioma anterior ~a)")
 (parenthetical-last-version/language "(versión anterior ~a, lenguaje ~a)")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "¿Salvar este archivo como texto plano?")
 (save-in-drs-format "¿Salvar este archivo en el formato, no texto, particular de DrScheme?")
 (yes "Si")
 (no "No")
 
 ;;; preferences
 (preferences "Preferencias")
 (preferences-category "Categoría")
 (saving-preferences "Salvar preferencias")
 (error-unmarshalling "Error mientras desempacaba la preferencia ~a")
 (error-saving-preferences "Error al salvar preferencias: ~a")
 (error-reading-preferences "Error al leer preferencias")
 (found-bad-pref "Encontré una mala preferencia, \"~a\", en el archivo.")
 (expected-list-of-length2 "esperaba un lista de longitud 2")
 (general-prefs-panel-label "General")
 (highlight-parens "Resaltar entre parejas de paréntesis")
 (fixup-parens "Corrige paréntesis")
 (flash-paren-match "Señala el paréntesis que casa")
 (auto-save-files "Auto-salva archivos")
 (map-delete-to-backspace "Cambia suprimir por backspace")
 (verify-exit "Confirmar salida")
 (ask-before-changing-format "Preguntar antes de cambiar el formato de salida")
 (wrap-words-in-editor-buffers "Ajustar al border palabras en el editor")
 (show-status-line "Mostrar línea de estado")
 (count-from-one "Cuenta números de columna y líneas a partir de uno") 
 (display-line-numbers "Muestra números de línea en el contenedor; sin desplazamiento de caracteres")
 (enable-keybindings-in-menus "Habilita enlaces de teclas en los menús")
 (automatically-to-ps "Imprime automáticamente a un archivo en postscript")
 (use-mdi "Utiliza Ventanas MDI") ;;; ms windows only -- use that window in a window thingy
 (separate-dialog-for-searching "Use el diálogo adecuado para buscar")
 (default-fonts "Fuentes por omisión")
 
 ; should have entire alphabet
 (font-example-string "abcdefghijklmnñopqrstuvxyz¡!¿?«».")  ;; FIXME: shoulde this have special characters?

 (change-font-button-label "Cambiar")
 (fonts "Fuentes")

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Por favor selecciona una nueva fuente \"~a\"")

 (font-size-slider-label "Tamaño")
 (restart-to-see-font-changes "Re-inicia para ver los cambios de las fuentes")

 (font-prefs-panel-title "Fuente")
 (font-name "Nombre de fuente")
 (font-size "Tamaño de fuente")
 (set-font "Ver Fuente...")
 (select-font-name "Selecciona un nombre de Fuente")
 (example-text "Texto de ejemplo:")
 (general-ii "General II")
 (only-warn-once "Sólo advierte una vez cuando las ejecución e interacciones no están sincronizadas")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Sangrar")  ;; To indent is "Sangrado de márgenes"

 ; filled with define, lambda, or begin
 (enter-new-keyword "Teclea una nueva palabra clave parecida a ~a:")
 (x-keyword "Palabra clave ~a")
 (x-like-keywords "Palabra clave parecida a ~a")

 (expected-a-symbol "esperaba un símbolo, encontré: ~a")
 (already-used-keyword "la palabra clave \"~a\" ya tenía un sangrado especial asignado")
 (add-keyword "Añade")
 (remove-keyword "Borra")
 
 ;;; find/replace
 (find-and-replace "Buscar y reemplazar")
 (find "Buscar")
 (replace "Reemplazar")
 (use-separate-dialog-for-searching "Use el diálogo adecuado para buscar")
 (replace&find-again "Reemplazar && Vuelve a buscar") ;;; need double & to get a single &
 (replace-to-end "Reemplazar hasta el final")
 (forward "Hacia adelante")
 (backward "Hacia atrás")
 (hide "Esconder")
 
 ;;;reverting a file
 (error-reverting "Error al Revertir")
 (could-not-read "no pude leer \"~a\"")
 
 ;;; finder dialog
 (must-specify-a-filename "Debes especificar un nombre de archivo")
 (file-does-not-exist "El archivo \"~a\" no existe.")
 (ask-because-file-exists "El archivo \"~a\" ya existía. ¿Deseas reemplazarlo?")
 (dne-or-cycle "El archivo \"~a\" contiene un directorio inexistente o un ciclo.")
 (get-file "Obtener archivo")
 (put-file "Poner archivo")
 (full-pathname "Ruta completa")
 (show-dot-files "Muestra archivos y directorio que comiencen con un punto.")
 (up-directory-button-label "Arriba")
 (add-button-label "Añadir") ;;; for multi-file selection
 (add-all-button-label "Añadir todos") ;;; for multi-file selection
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
 (edit-menu "Edición")
 (help-menu "Ayuda")
 
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
 (new-menu-item-before "&Nuevo")
 (new-menu-item-after "")

 (open-info "Abre un nuevo archivo del disco")
 (open-menu-item-before "&Abrir")
 (open-menu-item-after "...")

 (revert-info "Revertir este archivo a la copia en disco")
 (revert-menu-item-before "&Revertir")
 (revert-menu-item-after "")

 (save-info "Salva este archivo a disco")
 (save-menu-item-before "&Guardar")
 (save-menu-item-after "")

 (save-as-info "Te pide un nombre para salvar este archivo a disco")
 (save-as-menu-item-before "Guardar")
 (save-as-menu-item-after " &como ...")

 (print-info "Envía este archivo a una impresora")
 (print-menu-item-before "&Imprimir")
 (print-menu-item-after "...")

 (close-info "Cierra este archivo")
 (close-menu-item-before "&Cerrar")
 (close-menu-item-after "")

 (quit-info "Cierra todas las ventanas")
 (quit-menu-item-before-windows "&Salir")
 (quit-menu-item-before-others "&Salir") ;; FIXME: salir is exit, so quit is ???
 (quit-menu-item-after "")
 
 (edit-menu-label "&Edición")
 
 (undo-info "Deshace la acción más reciente")
 (undo-menu-item "&Deshacer")

 (redo-info "Deshace el más reciente deshacer")
 (redo-menu-item "&Rehacer")

 (cut-info "Mueve los elementos seleccionados al porta-papeles para pegarlos más tarde")
 (cut-menu-item "Cor&tar")

 (copy-info "Copia los elementos seleccionados al porta-papeles para pegarlos más tarde")
 (copy-menu-item "&Copiar")

 (paste-info "Pega los elementos copiados o cortados más recientemente en lugar de los objetos seleccionados")
 (paste-menu-item "&Pegar")

 (clear-info "Borra los elementos seleccionados sin afectar el porta-papeles o el pegado")
 (clear-menu-item-others "Limpiar")
 (clear-menu-item-windows "Borr&ar")

 (select-all-info "Selecciona el documento completo")
 (select-all-menu-item "&Selecciona todo")
 
 (find-info "Busca una cadena")
 (find-menu-item-before "&Buscar")
 (find-menu-item-after "...")

 (find-again-info "Busca la misma cadena que antes")
 (find-again-menu-item-before "Volver a buscar")
 (find-again-menu-item-after "")
 
 (replace-and-find-again-info "Reemplaza el texto actual y busca por la misma cadena que antes")
 (replace-and-find-again-menu-item-before "Reemplaza && buscar otra vez")
 (replace-and-find-again-menu-item-after "")

 (preferences-info "Configura tus preferencias")
 (preferences-menu-item-before "")
 (preferences-menu-item-after "Personalizar...")

 (keybindings-info "Muestra los enlaces de tecla activos")
 (keybindings-menu-item "Enlaces de teclas")
 (keybindings-frame-title "Enlaces de teclas")
 (keybindings-sort-by-name "Ordena por Nombre")
 (keybindings-sort-by-key "Ordena por Llave")

 (insert-text-box-item "Inserta caja de texto")
 (insert-pb-box-item "Inserta caja de porta-papeles")
 (insert-image-item "Inserta imagen...")
 (wrap-text-item "Wrap Text")

 (windows-menu-label "&Ventana")
 (show-menu-label "&Muestra")

 (help-menu-label "&Ayuda")
 (about-info "Créditos y detalles de esta apliación")
 (about-menu-item-before "Acerca ")
 (about-menu-item-after "...")
 
 ;;; exiting and quitting are you sure dialog
 ;;; (exit is used on windows, quit on macos. go figure)
 (exit-lc "salir")
 (exit-cap "Salir")
 (quit-lc "abandonar")
 (quit-cap "Abandonar") ;; FIXME: Quit
 ;;; in are-you-sure-format, either exit or quit is filled in (from above)
 ;;; based on the platform drscheme is running on.
 (are-you-sure-format "¿Estas seguro(a) que deseas ~a?")
 
 ;;; autosaving
 (error-autosaving "Error al auto-salvar \"~a\".")
 (autosaving-turned-off "Auto-salvar está desactivado hasta\n que el archivo sea salvado.")
 
 ;;; file modified warning
 (file-has-been-modified
  "El archivo ha sido modificado desde la última vez que fue salvado. ¿Sobreescribo las modificaciones?")
 (overwrite-file-button-label "Sobreescribir")
 
 (definitions-modified 
  "El texto de las definiciones ha sido modificado en el sistema de archivos; por favor salve o regrese el texto de las definiciones.")
 (drscheme-internal-error "Error interno de DrScheme")
 
 ;;; tools
 (invalid-tool-spec "La especificación de la herramienta, especificada en el archivo info.ss de la colección ~a, es inválida.  Esperaba una cadena o una lista no vacía de cadenas y recibí: ~e")
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
 (sort-by-position "Ordena por posición en el archivo")
 (no-definitions-found "<< no se encontraron definiciones >>")

  ;;; show menu
 (hide-definitions-menu-item-label "Esconder &Definiciones")
 (show-definitions-menu-item-label "Mostrar &Definiciones")
 (definitions-menu-item-help-string "Mostrar/Esconder la ventana de definiciones")
 (show-interactions-menu-item-label "Mostrar &Interacciones")
 (hide-interactions-menu-item-label "Esconder &Interacciones")
 (interactions-menu-item-help-string "Mostrar/Esconder la ventana de Interacciones")

 ;;; file menu
 (definitions "Definiciones")
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
 (break-menu-item-help-string "Interrumpe la evaluación actual")
 (kill-menu-item-label "Terminar")
 (kill-menu-item-help-string "Terminar la evaluación actual")
 (reindent-menu-item-label "&Re-sangrar")
 (reindent-all-menu-item-label "Re-sangrar &TODO")
 (comment-out-menu-item-label "&Comentar")
 (uncomment-menu-item-label "&Des-comentar")
 
 ;;; launcher
 (create-launcher-title "Crear acceso directo")
 (must-save-before-launcher "Debes salvar tu programa antes de crear un acceso directo.")
 (save-a-launcher "Salvar un acceso directo")
 
 ;;; buttons
 (execute-button-label "Ejecutar") 
 (save-button-label "Salvar")
 (break-button-label "Interrumpir")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Busca en el Módulo de Ayuda \"~a\"")
 (exact-lucky-search-help-desk-for "Búsqueda precisa y con suerte en el Módulo de Ayuda \"~a\"") ;; FIXME:  Exact lucky search in Help Desk for
 
 ;;; fraction dialog
 (enter-fraction "Introducir Fracción")
 (whole-part "Parte entera")
 (numerator "Numerador")
 (denominator "Denominador")
 (invalid-number "Número inválido: debe ser un número real exacto, no entero.")
 (insert-fraction-menu-item-label "Insertar Fracción...")
 
 ;;; TeachPack messages
 (select-a-teachpack "Selecciona un Paquete de Enseñanza")
 (clear-teachpack "Limpia el Paquete de Enseñanza ~a")
 (teachpack-error-label "DrScheme - Paquete de Enseñanza error")
 (teachpack-dne/cant-read "El archivo del Paquete de Enseñanza ~a no existe o no puede ser leído.")
 (teachpack-didnt-load "El archivo del Paquete de Enseñanza ~a no se cargó apropiadamente.")
 (teachpack-error-invoke "El archivo del Paquete de Enseñanza ~a lanzó un error durante su invocación.")
 (add-teachpack-menu-item-label "Añadir un Paquete de Enseñanza...")
 (clear-all-teachpacks-menu-item-label "Limpia Todos los Paquetes de Enseñanza")
 (teachpack-not-only-one-import "La unit/sig del Paquete de Enseñanza ~a debe tener exactamente una clausula de importación.")
 (drscheme-teachpack-message-title "DrScheme Paquete de Enseñanza")
 (already-added-teachpack "El paquete de enseñanza ~a ya estaba cargado")
 
 ;;; Language dialog
 (language-dialog-title "Configurar Lenguaje")
 (case-sensitive-label "Sensible a mayúsculas") ;; FIXME: Case sensitive
 (output-style-label "Estilo de salida")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (sharing-printing-label "Show sharing in values")
 (use-pretty-printer-label "Insertar caracteres de nueva línea en valores impresos")
 (input-syntax "Sintáxis de Entrada")
 (output-syntax "Sintáxis de Salida")
 (whole/fractional-exact-numbers-label "Imprimir números como fracciones")
 (booleans-as-true/false-label "Imprimir valores booleanos usando true y false")
 (show-details-button-label "Mostrar Detalles")
 (hide-details-button-label "Esconder Detalles")
 (choose-language-menu-item-label "Escoger Lenguaje...")
 (revert-to-language-defaults "Revertir a los Valores por Omisión del Lenguaje")

 ;;; languages
 (beginning-student "Estudiante Principiante")
 (beginning-student/abbrev "Estudiante Principiante con Abreviaturas de Listas")
 (intermediate-student "Estudiante Intermedio")
 (advanced-student "Estudiante Avanzado")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (full-languages "Completo")
 (mred-lang-name "Gráfico sin depuración (MrEd)")
 (mzscheme-lang-name "Texto sin depuración (MzScheme)")
 (r5rs-lang-name "R5RS sin depuración (MzScheme)")
 (unknown-debug-frame "[desconocido]")
 
 ;;; debug language
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "Interacciones de ~a") ;; filled with a filename
 (stack-frame-in-current-interactions "interacciones")
 (stack-frame-in-current-definitions "definiciones")
 (mzscheme-w/debug "Texto (MzScheme)")
 (mred-w/debug "Gráfico (MrEd)")
 (r5rs-w/debug "R5RS")
 
 ;;; repl stuff
 (evaluation-terminated "Evaluación Terminada")
 (evaluation-terminated-explanation
  "El hilo de control de la evaluación ya no se está ejecutando, por lo que no se puede efectuar ninguna evaluación hasta la siguiente ejecución.")
 (last-stack-frame "Mostrar el último marco (frame) en el stack")
 (last-stack-frames "Mostrar los últimos ~a marcos (frames)")
 (next-stack-frames "Mostrar los siguientes ~a marcos (frames) en el stack")
 
 ;;; welcoming message in repl
 (language "Idioma")
 (custom "custom")
 (teachpack "Paquete de Enseñanza")
 (welcome-to "Bienvenido a")
 (version "versión")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "¿Quieres terminar la evaluación?")
 (just-break "Interrupción Simple")
 (kill "Terminar")
 (kill? "¿Terminar?")
 )
