(module start mzscheme
  (require "tex2page.ss"
	   "fresh-tex2page.ss"
	   (lib "cmdline.ss"))

  (command-line
   "tex2page"
   (current-command-line-arguments)
   [once-each
    [("--version") "Reports long help and version information"
     (tex2page/fresh-namespace "--version")]]
   [args file "Processes each <file>"
	 (map tex2page/fresh-namespace file)]))
