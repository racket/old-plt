(module info (lib "infotab.ss" "setup")

  (define name "DrScheme Editing Tools")
  (define tool-names (list "Compile-on-save" "Tab-Completion" "Online-error-checking"))

  (define tools (list (list "compile-on-save-tool.ss" "compile-on-save")
		      (list "tab-completion-tool.ss" "tab-completion")
		      (list "online-error-checking-tool.ss" "online-error-checking")))

  )
