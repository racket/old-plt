;;
;; $Id: frameworkr.ss,v 1.7 1998/09/14 03:13:40 robby Exp $
;;

(compound-unit/sig (import [core : mzlib:core^]
			   [mred : mred-interfaces^])
  (link [date : mzlib:date^ ((require-library-unit/sig "dater.ss")
			     (core function))]
	[application : framework:application^ ((require-unit/sig "app.ss"))]
	[version : framework:version^ ((require-unit/sig "version.ss") (core string) (core function))]
	[exn : framework:exn^ ((require-unit/sig "exn.ss"))]
	[exit : framework:exit^ ((require-unit/sig "exit.ss") preferences gui-utils)]
	[preferences : framework:preferences^
		     ((require-unit/sig "prefs.ss")
		      mred
		      exn exit (core pretty-print) (core function))]
	[autosave : framework:autosave^
		  ((require-unit/sig "autosave.ss") exit preferences)]
	[handler : framework:handler^
		 ((require-unit/sig "handler.ss")
		  mred
		  gui-utils finder group  text preferences
		  (core file))] 
	[keymap : framework:keymap^
		((require-unit/sig "keys.ss") mred preferences finder handler scheme-paren)]
	[match-cache : framework:match-cache^ ((require-unit/sig "mcache.ss"))]
	[paren : framework:paren^ ((require-unit/sig "paren.ss"))]
	[scheme-paren : framework:scheme-paren^
		      ((require-unit/sig "sparen.ss") paren)]
	[path-utils : framework:path-utils^ ((require-unit/sig "fileutil.ss"))]
	[icon : framework:icon^ ((require-unit/sig "icon.ss"))]

	[editor : framework:editor^ ((require-library "editor.ss")
				     mred
				     autosave finder path-utils keymap icon preferences gui-utils)]
	[pasteboard : framework:pasteboard^ ((require-library "pasteboard.ss")
					     mred editor)]
	[text : framework:text^ ((require-library "text.ss") mred editor preferences keymap)]

	[gui-utils : framework:gui-utils^ ((require-unit/sig "guiutils.ss") mred)]

	[finder : framework:finder^
		((require-unit/sig "finder.ss")
		 mred
		 preferences gui-utils
		 (core string) (core function) (core file))]

	[group : framework:group^ 
	       ((require-unit/sig "group.ss")
		mred exit
		(core function) (core file))]

	[canvas : framework:canvas^ ((require-unit/sig "canvas.ss") mred preferences)]

	[panel : framework:panel^ ((require-unit/sig "panel.ss") mred)]

	[frame : framework:frame^ 
	       ((require-unit/sig "frame.ss")
		mred
		group preferences icon handler
		application panel gui-utils
		(core function))]
	[scheme-mode : framework:scheme-mode^ 
		     ((require-unit/sig "scheme.ss")
		      mred preferences match-cache paren
		      scheme-paren icon keymap)]
	[main : () ((require-library "main.ss") preferences exit)])
  (export
   (unit application)
   (unit version)
   (unit exn)
   (unit exit)
   (unit preferences)
   (unit autosave)
   (unit handler) 
   (unit keymap)
   (unit match-cache)
   (unit paren)
   (unit scheme-paren)
   (unit path-utils)
   (unit icon)
   (unit editor)
   (unit pasteboard)
   (unit text)
   (unit gui-utils)
   (unit finder)
   (unit group)
   (unit canvas)
   (unit panel)
   (unit frame)
   (unit scheme-mode)))