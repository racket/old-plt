  (compound-unit/sig
    (import [core : mzlib:core^]
	    [trigger : mzlib:trigger^]
	    [application : mred:application^])
    (link [constants : mred:constants^ ((reference-unit/sig "constant.ss"))]
	  [version : mred:version^ ((reference-unit/sig "version.ss")
				    constants
				    (core function@)
				    (core string@))]
	  [connections : mred:connections^
		       ((reference-unit/sig "connect.ss") 
			constants (core function@))]
	  [exn : mred:exn^ ((reference-unit/sig "exn.ss") constants)]
	  [container : mred:container^
		     ((reference-unit/sig "containr.ss")
		      constants connections (core function@))]
	  [exit : mred:exit^ ((reference-unit/sig "exit.ss")
			      constants preferences gui-utils)]
	  [preferences : mred:preferences^
		       ((reference-unit/sig "prefs.ss")
			constants
			exn container exit gui-utils
			edit (core pretty-print@)
			(core function@))]
	  [autoload : mred:autoload^
		    ((reference-unit/sig "autoload.ss")
		     constants preferences (core file@))]
	  [autosave : mred:autosave^
		    ((reference-unit/sig "autosave.ss")
		     constants exit preferences)]
	  [mode : mred:mode^
		((reference-unit/sig "mode.ss")
		 constants keymap)]
	  [handler : mred:handler^
		   ((reference-unit/sig "handler.ss")
		    constants group gui-utils 
		    editor-frame finder hyper-frame
		    canvas container edit preferences
		    (core file@))] 
	  [keymap : mred:keymap^
		  ((reference-unit/sig "keys.ss")
		   constants preferences exit finder handler
		   find-string scheme-paren gui-utils)]
	  [match-cache : mred:match-cache^ 
		       ((reference-unit/sig "mcache.ss") constants)]
	  [scheme-paren : mred:scheme-paren^
			((reference-unit/sig "sparen.ss") 
			 constants paren)]
	  [paren : mred:paren^ ((reference-unit/sig "paren.ss") constants)]
	  [path-utils : mred:path-utils^ ((reference-unit/sig "fileutil.ss")
					  constants)]
	  [icon : mred:icon^ ((reference-unit/sig "icon.ss") constants)]
	  [menu : mred:menu^ ((reference-unit/sig "menu.ss") 
			      constants (core function@))]
	  [edit : mred:edit^ 
		((reference-unit/sig "edit.ss")
		 constants connections finder path-utils mode
		 scheme-paren keymap icon preferences gui-utils
		 (core function@))]
	  [gui-utils : mred:gui-utils^
		     ((reference-unit/sig "guiutils.ss")
		      constants frame container canvas edit
		      (core function@) trigger)]
	  [finder : mred:finder^
		  ((reference-unit/sig "finder.ss")
		   constants container preferences
		   gui-utils edit canvas
		   (core string@) (core function@) (core file@))]
	  [group : mred:group^ 
		 ((reference-unit/sig "group.ss")
		  constants preferences editor-frame gui-utils
		  exit autosave handler application
		  (core function@))]
	  [canvas : mred:canvas^ 
		  ((reference-unit/sig "canvas.ss")
		   constants container edit preferences (core file@))]
	  [panel : mred:panel^ ((reference-unit/sig "panel.ss")
				constants
				container canvas (core function@))]
	  [frame : mred:frame^ 
		 ((reference-unit/sig "frame.ss")
		  constants
		  preferences edit container canvas icon
		  menu group finder find-string handler exit autosave
		  panel gui-utils application
		  (core function@) (core file@))]
	  [find-string : mred:find-string^ 
		       ((reference-unit/sig "findstr.ss")
			constants
			container canvas edit frame)]
	  [editor-frame : mred:editor-frame^ 
			((reference-unit/sig "edframe.ss")
			 constants
			 preferences edit frame container
			 canvas find-string icon menu group
			 finder handler exit autosave gui-utils
			 (core function@) (core file@))]
	  [project : mred:project^ 
		   ((reference-unit/sig "project.ss")
		    constants
		    group container gui-utils exit finder
		    frame handler (core file@) (core function@))]
	  [console : mred:console^ 
		   ((reference-unit/sig "console.ss")
		    constants
		    preferences edit frame canvas find-string
		    exit finder handler gui-utils scheme-mode
		    scheme-paren icon hyper-frame version application
		    (core function@) (core string@) (core pretty-print@)
		    trigger)]
	  [scheme-mode : mred:scheme-mode^ 
		       ((reference-unit/sig "ssmode.ss")
			constants
			preferences application container
			mode match-cache paren scheme-paren icon
			handler keymap (core string@))]
	  [url : mred:url^ ((reference-unit/sig "url.ss")
			    constants (core function@) gui-utils
			    exn (core file@))]
	  [hyper-dialog : mred:hyper-dialog^ 
			((reference-unit/sig "hyprdial.ss")
			 constants hyper-edit (core file@))]
	  [hyper-edit : mred:hyper-edit^ 
		      ((reference-unit/sig "hypredit.ss")
		       constants edit hyper-dialog html url
		       (core file@) (core string@))]
	  [hyper-frame : mred:hyper-frame^ 
		       ((reference-unit/sig "hyprfram.ss")
			constants
			hyper-edit hyper-dialog container
			frame canvas group find-string
			preferences handler)]
	  [html : mred:html^ ((reference-unit/sig "html.ss")
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
