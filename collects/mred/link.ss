  (compound-unit/sig
    (import [core : mzlib:core^]
	    [trigger : mzlib:trigger^]
	    [application : mred:application^])
    (link [wx : mred:wx^ ((begin-elaboration-time
			   (if mred:explicit-wx? 
			       `(reference-unit/sig "wxr.ss")
			       `(unit/sig () (import)))))]
	  [date : mzlib:date^ ((reference-library-unit/sig "dater.ss")
			       (core function@))]
	  [constants : mred:constants^ ((reference-unit/sig "constant.ss"))]
	  [version : mred:version^ ((reference-unit/sig "version.ss")
				    wx
				    constants
				    (core function@)
				    (core string@))]
	  [connections : mred:connections^
		       ((reference-unit/sig "connect.ss")
			wx
			constants (core function@))]
	  [exn : mred:exn^ ((reference-unit/sig "exn.ss") constants)]
	  [container : mred:container^
		     ((reference-unit/sig "containr.ss") wx 
		      constants connections (core function@))]
	  [exit : mred:exit^ ((reference-unit/sig "exit.ss") wx 
			      constants preferences gui-utils)]
	  [preferences : mred:preferences^
		       ((reference-unit/sig "prefs.ss") wx 
			constants
			exn container exit gui-utils
			edit (core pretty-print@)
			(core function@))]
	  [autoload : mred:autoload^
		    ((reference-unit/sig "autoload.ss") wx 
		     constants preferences (core file@))]
	  [autosave : mred:autosave^
		    ((reference-unit/sig "autosave.ss") wx 
		     constants exit preferences)]
	  [mode : mred:mode^
		((reference-unit/sig "mode.ss") wx 
		 constants keymap)]
	  [handler : mred:handler^
		   ((reference-unit/sig "handler.ss") wx 
		    constants gui-utils 
		    editor-frame finder group hyper-frame
		    canvas container edit preferences
		    (core file@))] 
	  [keymap : mred:keymap^
		  ((reference-unit/sig "keys.ss") wx 
		   constants preferences exit finder handler
		   find-string scheme-paren gui-utils)]
	  [match-cache : mred:match-cache^ 
		       ((reference-unit/sig "mcache.ss") wx constants)]
	  [scheme-paren : mred:scheme-paren^
			((reference-unit/sig "sparen.ss") wx 
			 constants paren)]
	  [paren : mred:paren^ ((reference-unit/sig "paren.ss") wx constants)]
	  [path-utils : mred:path-utils^ ((reference-unit/sig "fileutil.ss") wx 
					  constants)]
	  [icon : mred:icon^ ((reference-unit/sig "icon.ss") wx constants)]
	  [menu : mred:menu^ ((reference-unit/sig "menu.ss") wx 
			      constants (core function@))]
	  [edit : mred:edit^ 
		((reference-unit/sig "edit.ss") wx 
		 constants connections autosave finder path-utils mode
		 frame scheme-paren keymap icon preferences gui-utils
		 (core function@))]
	  [gui-utils : mred:gui-utils^
		     ((reference-unit/sig "guiutils.ss") wx 
		      constants frame container canvas edit
		      (core function@) trigger)]
	  [finder : mred:finder^
		  ((reference-unit/sig "finder.ss") wx 
		   constants container preferences
		   gui-utils edit canvas
		   (core string@) (core function@) (core file@))]
	  [group : mred:group^ 
		 ((reference-unit/sig "group.ss") wx 
		  constants preferences editor-frame gui-utils
		  exit autosave handler application
		  (core function@)
		  (core file@))]
	  [canvas : mred:canvas^ 
		  ((reference-unit/sig "canvas.ss") wx 
		   constants container edit preferences (core file@))]
	  [panel : mred:panel^ ((reference-unit/sig "panel.ss") wx 
				constants
				container canvas (core function@))]
	  [frame : mred:frame^ 
		 ((reference-unit/sig "frame.ss") wx 
		  constants
		  preferences edit container canvas icon
		  menu group finder find-string hyper-frame
		  handler exit autosave
		  panel gui-utils application
		  (core function@) (core file@)
		  date)]
	  [find-string : mred:find-string^ 
		       ((reference-unit/sig "findstr.ss") wx 
			constants
			container canvas edit frame keymap)]
	  [editor-frame : mred:editor-frame^ 
			((reference-unit/sig "edframe.ss") wx 
			 constants
			 preferences edit frame container
			 canvas find-string menu
			 finder handler exit autosave gui-utils
			 (core function@) (core file@))]
	  [project : mred:project^ 
		   ((reference-unit/sig "project.ss") wx 
		    constants
		    group container gui-utils exit finder
		    frame handler (core file@) (core function@))]
	  [console : mred:console^ 
		   ((reference-unit/sig "console.ss") wx 
		    constants container
		    preferences edit frame canvas find-string
		    exit finder handler gui-utils scheme-mode
		    scheme-paren icon hyper-frame version application
		    (core function@) (core string@) (core pretty-print@)
		    trigger)]
	  [scheme-mode : mred:scheme-mode^ 
		       ((reference-unit/sig "ssmode.ss") wx 
			constants
			preferences application container
			mode match-cache paren scheme-paren icon
			handler keymap (core string@)
			 (core function@))]
	  [url : mred:url^ ((reference-unit/sig "url.ss") wx 
			    constants (core function@) gui-utils
			    exn (core file@))]
	  [hyper-dialog : mred:hyper-dialog^ 
			((reference-unit/sig "hyprdial.ss") wx 
			 constants hyper-edit (core file@))]
	  [hyper-edit : mred:hyper-edit^ 
		      ((reference-unit/sig "hypredit.ss") wx 
		       constants edit hyper-dialog html url gui-utils
		       (core file@) (core string@))]
	  [hyper-frame : mred:hyper-frame^ 
		       ((reference-unit/sig "hyprfram.ss") wx 
			constants
			hyper-edit hyper-dialog container
			frame canvas group find-string
			preferences handler)]
	  [html : mred:html^ ((reference-unit/sig "html.ss") wx 
			      constants
			      url (core file@)
			      (core string@))])
    (export (unit constants)
	    (open version)
	    (open (exn : mred:exn-external^))
	    (open connections) (open container) (open preferences)
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
	    (open url)))
