;;
;; $Id: linkwx.ss,v 1.9 1998/04/20 20:49:21 robby Exp $
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
	[match-cache : mred:match-cache^ 
		     ((require-unit/sig "mcache.ss") wx (minimal constants))]
	[scheme-paren : mred:scheme-paren^
		      ((require-unit/sig "sparen.ss") wx 
		       (minimal constants) paren)]
	[paren : mred:paren^ ((require-unit/sig "paren.ss") wx (minimal constants))]
	[path-utils : mred:path-utils^ ((require-unit/sig "fileutil.ss") wx 
					(minimal constants))]
	[icon : mred:icon^ ((require-unit/sig "icon.ss") wx (minimal constants))]
	[menu : mred:menu^ ((require-unit/sig "menu.ss") wx 
			    (minimal constants)
			    preferences
			    (core function@))]
	[edit : mred:edit^ 
	      ((require-unit/sig "edit.ss") wx 
	       (minimal constants) (minimal connections) autosave finder path-utils mode
	       frame scheme-paren keymap icon preferences gui-utils
	       (core function@))]
	[gui-utils : mred:gui-utils^
		   ((require-unit/sig "guiutils.ss") wx 
		    (minimal constants) frame (minimal container) 
		    canvas edit
		    (core function@))]
        [graph : mred:graph^ ((require-unit/sig "graph.ss") wx
			      (minimal constants) edit gui-utils
			      (core string@)
			      (core function@))]
	[finder : mred:finder^
		((require-unit/sig "finder.ss") wx 
		 (minimal constants) (minimal container) preferences
		 gui-utils edit canvas
		 (core string@) (core function@) (core file@))]
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
	[find-string : mred:find-string^ 
		     ((require-unit/sig "findstr.ss") wx 
		      (minimal constants)
		      (minimal container) canvas edit frame keymap)]
	[editor-frame : mred:editor-frame^ 
		      ((require-unit/sig "edframe.ss") wx 
		       (minimal constants)
		       preferences edit frame (minimal container)
		       canvas find-string menu
		       finder handler exit autosave gui-utils
		       (core function@) (core file@))]
	[project : mred:project^ 
		 ((require-unit/sig "project.ss") wx 
		  (minimal constants)
		  group (minimal container) gui-utils exit finder
		  frame handler (core file@) (core function@))]
	[console : mred:console^ 
		 ((require-unit/sig "console.ss") wx 
		  (minimal constants) (minimal container)
		  preferences edit frame group canvas find-string
		  exit finder handler gui-utils scheme-mode
		  scheme-paren icon hyper-frame version application
		  (core function@) (core string@) (core pretty-print@))]
	[scheme-mode : mred:scheme-mode^ 
		     ((require-unit/sig "ssmode.ss") wx 
		      (minimal constants)
		      preferences gui-utils (minimal container)
		      mode match-cache paren scheme-paren icon
		      handler keymap (core string@)
		      (core function@))]
	[url : mred:url^ ((require-unit/sig "url.ss") wx 
			  (minimal constants) (core function@) gui-utils
			  exn (core file@))]
	[hyper-dialog : mred:hyper-dialog^ 
		      ((require-unit/sig "hyprdial.ss") wx 
		       (minimal constants) hyper-edit gui-utils (core file@))]
	[hyper-edit : mred:hyper-edit^ 
		    ((require-unit/sig "hypredit.ss") wx 
		     (minimal constants) edit hyper-dialog html url gui-utils
		     (core file@) (core string@))]
	[hyper-frame : mred:hyper-frame^ 
		     ((require-unit/sig "hyprfram.ss") wx 
		      (minimal constants)
		      hyper-edit hyper-dialog (minimal container)
		      frame canvas group find-string
		      preferences handler)]
	[self-test : mred:self-test^
		   ((require-unit/sig  "stlink.ss")  wx  (minimal testable)  keymap)]
	[html : mred:html^ ((require-unit/sig "html.ss") wx 
			    (minimal constants)
			    url (core file@)
			    (core string@))])
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
