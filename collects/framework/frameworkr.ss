;;
;; $Id: frameworkr.ss,v 1.2 1998/08/31 21:35:41 robby Exp $
;;

(compound-unit/sig (import [core : mzlib:core^])
  (link [date : mzlib:date^ ((require-library-unit/sig "dater.ss")
			     (core function@))]
	[application : framework:application^ ((require-unit/sig "app.ss"))]
	[version : mred:version^ ((require-unit/sig "version.ss")
				  (core function)
				  (core string))]
	[exn : mred:exn^ ((require-unit/sig "exn.ss"))]
	[exit : mred:exit^ ((require-unit/sig "exit.ss") preferences gui-utils)]
	[preferences : mred:preferences^
		     ((require-unit/sig "prefs.ss") 
		      exn exit (core pretty-print) (core function))]
	[autosave : mred:autosave^
		  ((require-unit/sig "autosave.ss") exit preferences)]
	[handler : mred:handler^
		 ((require-unit/sig "handler.ss")
		  gui-utils finder group (hyper frame)
		  edit preferences (core file))] 
	[keymap : mred:keymap^
		((require-unit/sig "keys.ss") preferences finder handler scheme-paren)]
	[match-cache : mred:match-cache^ ((require-unit/sig "mcache.ss"))]
	[paren : mred:paren^ ((require-unit/sig "paren.ss"))]
	[scheme-paren : mred:scheme-paren^
		      ((require-unit/sig "sparen.ss") paren)]
	[path-utils : mred:path-utils^ ((require-unit/sig "fileutil.ss"))]
	[icon : mred:icon^ ((require-unit/sig "icon.ss"))]
	[editor : framework:editor^ ((require-library "editor.ss"))]
	[pasteboard : framework:pasteboard^ ((require-library "pasteboard.ss") editor)]
	[text : framework:text^ ((require-library "text.ss") editor)]
	[gui-utils : mred:gui-utils^ ((require-unit/sig "guiutils.ss"))]
	[finder : mred:finder^
		((require-unit/sig "finder.ss") preferences
		 gui-utils
		 (core string) (core function) (core file))]

	[group : mred:group^ 
	       ((require-unit/sig "group.ss") wx 
		(minimal constants) preferences editor-frame gui-utils
		exit autosave handler (core function@)
		(core file@))]
	[canvas : mred:canvas^ 
		((require-unit/sig "canvas.ss") wx 
		 (minimal constants) (minimal container) edit preferences 
		 (core file@) (core function@))]
	[panel : mred:panel^ ((require-unit/sig "panel.ss") wx 
			      (minimal constants)
			      (minimal container) canvas (core function@))]
	[frame : mred:frame^ 
	       ((require-unit/sig "frame.ss") wx 
		(minimal constants) console
		preferences edit (minimal container) canvas icon
		menu group finder find-string hyper-frame
		handler keymap exit autosave
		panel gui-utils application
		(core function@) (core file@)
		date)]
	[control : mred:control^
		 ((require-unit/sig "control.ss") wx 
		   edit canvas (minimal container))]
	[scheme-mode : mred:scheme-mode^ 
		     ((require-unit/sig "ssmode.ss") wx 
		      (minimal constants)
		      preferences gui-utils (minimal container)
		      mode match-cache paren scheme-paren icon
		      handler keymap (core string@)
		      (core function@))]
	[self-test : mred:self-test^
		   ((require-unit/sig  "stlink.ss")  wx  (minimal testable)  keymap)])
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
