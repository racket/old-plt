;;
;; $Id: frameworkr.ss,v 1.5 1998/09/08 02:53:50 robby Exp $
;;

(compound-unit/sig (import [core : mzlib:core^]
			   [mred : mred^])
  (link [date : mzlib:date^ ((require-library-unit/sig "dater.ss")
			     (core function))]
	[application : framework:application^ ((require-unit/sig "app.ss"))]
	[version : framework:version^ ((require-unit/sig "version.ss") (core string))]
	[exn : framework:exn^ ((require-unit/sig "exn.ss"))]
	[exit : framework:exit^ ((require-unit/sig "exit.ss") preferences gui-utils)]
	[preferences : framework:preferences^
		     ((require-unit/sig "prefs.ss") 
		      exn exit (core pretty-print) (core function))]
	[autosave : framework:autosave^
		  ((require-unit/sig "autosave.ss") exit preferences)]
	[handler : framework:handler^
		 ((require-unit/sig "handler.ss")
		  gui-utils finder group (hyper frame)
		  edit preferences (core file))] 
	[keymap : framework:keymap^
		((require-unit/sig "keys.ss") preferences finder handler scheme-paren)]
	[match-cache : framework:match-cache^ ((require-unit/sig "mcache.ss"))]
	[paren : framework:paren^ ((require-unit/sig "paren.ss"))]
	[scheme-paren : framework:scheme-paren^
		      ((require-unit/sig "sparen.ss") paren)]
	[path-utils : framework:path-utils^ ((require-unit/sig "fileutil.ss"))]
	[icon : framework:icon^ ((require-unit/sig "icon.ss"))]

	;; these three to come later
	[editor : framework:editor^ ((require-library "editor.ss"))]
	[pasteboard : framework:pasteboard^ ((require-library "pasteboard.ss") editor)]
	[text : framework:text^ ((require-library "text.ss") editor)]

	[gui-utils : mred:gui-utils^ ((require-unit/sig "guiutils.ss"))]

	[finder : framework:finder^
		((require-unit/sig "finder.ss") preferences
		 gui-utils
		 (core string) (core function) (core file))]

	[group : framework:group^ 
	       ((require-unit/sig "group.ss") wx 
		(minimal constants) preferences editor-frame gui-utils
		exit autosave handler (core function@)
		(core file@))]

	[canvas : framework:canvas^ ((require-unit/sig "canvas.ss") mred preferences)]

	[panel : framework:panel^ ((require-unit/sig "panel.ss") mred)]

	[frame : framework:frame^ 
	       ((require-unit/sig "frame.ss") wx 
		(minimal constants) console
		preferences edit (minimal container) canvas icon
		menu group finder find-string hyper-frame
		handler keymap exit autosave
		panel gui-utils application
		(core function@) (core file@)
		date)]
	[scheme-mode : framework:scheme-mode^ 
		     ((require-unit/sig "scheme.ss")
		      mred preferences match-cache paren
		      scheme-paren icon keymap)])
  (export (unit version)
	  (unit (exn : mred:exn-external^))
	  (unit preferences)
	  (unit autoload) (unit autosave) (unit exit)
	  (unit gui-utils) (unit console) (unit path-utils)
	  (unit finder)
	  (unit find-string) (unit edit) (unit canvas)
	  (unit frame) (unit editor-frame)
	  (unit group) (unit handler) (unit icon) (unit keymap)
	  (unit match-cache) (unit menu) (unit mode) 
	  (unit panel) (unit paren) (unit project)
	  (unit scheme-paren) (unit scheme-mode) 
	  (unit hyper-edit) (unit hyper-dialog) (unit hyper-frame)
	  (unit url)
	  (unit graph)
	  (unit application)
	  (unit control)))
