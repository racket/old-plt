;;
;; $Id: linkwx.ss,v 1.2 1997/12/04 21:27:36 mflatt Exp robby $
;;

(compound-unit/sig (import [core : mzlib:core^]
			   [wx : wx^])
  (link [minimal : mred:minimal^ ((reference-library-unit/sig "minimal.ss" "mred")
				  (core function@)
				  wx)]
	[date : mzlib:date^ ((reference-library-unit/sig "dater.ss")
			     (core function@))]
	[application : mred:application^ ((reference-unit/sig "app.ss"))]
	[version : mred:version^ ((reference-unit/sig "version.ss")
				  wx
				  (minimal constants)
				  (core function@)
				  (core string@))]
	[exn : mred:exn^ ((reference-unit/sig "exn.ss") (minimal constants))]
	[exit : mred:exit^ ((reference-unit/sig "exit.ss") wx 
			    (minimal constants) preferences gui-utils)]
	[preferences : mred:preferences^
		     ((reference-unit/sig "prefs.ss") wx 
		      (minimal constants)
		      exn (minimal container) exit gui-utils
		      edit (core pretty-print@)
		      (core function@))]
	[autoload : mred:autoload^
		  ((reference-unit/sig "autoload.ss") wx 
		   (minimal constants) preferences (core file@))]
	[autosave : mred:autosave^
		  ((reference-unit/sig "autosave.ss") wx 
		   (minimal constants) exit preferences)]
	[mode : mred:mode^
	      ((reference-unit/sig "mode.ss") wx 
	       (minimal constants) keymap)]
	[handler : mred:handler^
		 ((reference-unit/sig "handler.ss") wx 
		  (minimal constants) gui-utils 
		  editor-frame finder group hyper-frame
		  canvas (minimal container) edit preferences
		  (core file@))] 
	[keymap : mred:keymap^
		((reference-unit/sig "keys.ss") wx 
		 (minimal constants) preferences exit finder handler
		 find-string scheme-paren gui-utils)]
	[match-cache : mred:match-cache^ 
		     ((reference-unit/sig "mcache.ss") wx (minimal constants))]
	[scheme-paren : mred:scheme-paren^
		      ((reference-unit/sig "sparen.ss") wx 
		       (minimal constants) paren)]
	[paren : mred:paren^ ((reference-unit/sig "paren.ss") wx (minimal constants))]
	[path-utils : mred:path-utils^ ((reference-unit/sig "fileutil.ss") wx 
					(minimal constants))]
	[icon : mred:icon^ ((reference-unit/sig "icon.ss") wx (minimal constants))]
	[menu : mred:menu^ ((reference-unit/sig "menu.ss") wx 
			    (minimal constants) (core function@))]
	[edit : mred:edit^ 
	      ((reference-unit/sig "edit.ss") wx 
	       (minimal constants) (minimal connections) autosave finder path-utils mode
	       frame scheme-paren keymap icon preferences gui-utils
	       (core function@))]
	[gui-utils : mred:gui-utils^
		   ((reference-unit/sig "guiutils.ss") wx 
		    (minimal constants) frame (minimal container) canvas edit
		    (core function@))]
	[graph : mred:graph^ ((reference-unit/sig "graph.ss") wx
			      (minimal constants) edit gui-utils
			      (core string@)
			      (core function@))]
	[finder : mred:finder^
		((reference-unit/sig "finder.ss") wx 
		 (minimal constants) (minimal container) preferences
		 gui-utils edit canvas
		 (core string@) (core function@) (core file@))]
	[group : mred:group^ 
	       ((reference-unit/sig "group.ss") wx 
		(minimal constants) preferences editor-frame gui-utils
		exit autosave handler (core function@)
		(core file@))]
	[canvas : mred:canvas^ 
		((reference-unit/sig "canvas.ss") wx 
		 (minimal constants) (minimal container) edit preferences 
		 (core file@) (core function@))]
	[panel : mred:panel^ ((reference-unit/sig "panel.ss") wx 
			      (minimal constants)
			      (minimal container) canvas (core function@))]
	[frame : mred:frame^ 
	       ((reference-unit/sig "frame.ss") wx 
		(minimal constants) console
		preferences edit (minimal container) canvas icon
		menu group finder find-string hyper-frame
		handler keymap exit autosave
		panel gui-utils application
		(core function@) (core file@)
		date)]
	[control : mred:control^
		 ((reference-unit/sig "control.ss") wx 
		   edit canvas (minimal container))]
	[find-string : mred:find-string^ 
		     ((reference-unit/sig "findstr.ss") wx 
		      (minimal constants)
		      (minimal container) canvas edit frame keymap)]
	[editor-frame : mred:editor-frame^ 
		      ((reference-unit/sig "edframe.ss") wx 
		       (minimal constants)
		       preferences edit frame (minimal container)
		       canvas find-string menu
		       finder handler exit autosave gui-utils
		       (core function@) (core file@))]
	[project : mred:project^ 
		 ((reference-unit/sig "project.ss") wx 
		  (minimal constants)
		  group (minimal container) gui-utils exit finder
		  frame handler (core file@) (core function@))]
	[console : mred:console^ 
		 ((reference-unit/sig "console.ss") wx 
		  (minimal constants) (minimal container)
		  preferences edit frame canvas find-string
		  exit finder handler gui-utils scheme-mode
		  scheme-paren icon hyper-frame version application
		  (core function@) (core string@) (core pretty-print@))]
	[scheme-mode : mred:scheme-mode^ 
		     ((reference-unit/sig "ssmode.ss") wx 
		      (minimal constants)
		      preferences gui-utils (minimal container)
		      mode match-cache paren scheme-paren icon
		      handler keymap (core string@)
		      (core function@))]
	[url : mred:url^ ((reference-unit/sig "url.ss") wx 
			  (minimal constants) (core function@) gui-utils
			  exn (core file@))]
	[hyper-dialog : mred:hyper-dialog^ 
		      ((reference-unit/sig "hyprdial.ss") wx 
		       (minimal constants) hyper-edit gui-utils (core file@))]
	[hyper-edit : mred:hyper-edit^ 
		    ((reference-unit/sig "hypredit.ss") wx 
		     (minimal constants) edit hyper-dialog html url gui-utils
		     (core file@) (core string@))]
	[hyper-frame : mred:hyper-frame^ 
		     ((reference-unit/sig "hyprfram.ss") wx 
		      (minimal constants)
		      hyper-edit hyper-dialog (minimal container)
		      frame canvas group find-string
		      preferences handler)]
	[self-test : mred:self-test^
		   ((reference-unit/sig  "stlink.ss")  wx  (minimal testable)  keymap)]
	[html : mred:html^ ((reference-unit/sig "html.ss") wx 
			    (minimal constants)
			    url (core file@)
			    (core string@))])
  (export (unit (minimal constants) constants)
	  (open version)
	  (open (exn : mred:exn-external^))
	  (open (minimal connections)) (open (minimal container)) (open preferences)
	  (open autoload) (open autosave) (open exit)
	  (open gui-utils) (open console) (open path-utils)
	  (open finder)
	  (open find-string) (open edit) (open canvas)
	  (open frame) (open editor-frame)
	  (open group) (open handler) (open icon) (open keymap)
	  (open match-cache) (open menu) (open mode) 
	  (open panel) (open paren) (open project)
	  (open scheme-paren) (open scheme-mode) 
	  (open hyper-edit) (open hyper-dialog) (open hyper-frame)
	  (open (minimal testable))
	  (unit (self-test : mred:self-test-export^) test)
	  (open url)
	  (open graph)
	  (open application)
	  (open control)))
