;;
;; $Id: frameworkc.ss,v 1.3 1998/12/08 01:03:03 robby Exp $
;;

(compound-unit/sig (import [core:string : mzlib:string^]
			   [core:function : mzlib:function^]
			   [core:pretty-print : mzlib:pretty-print^]
			   [core:file : mzlib:file^]
			   [core:thread : mzlib:thread^]
			   [mred : mred-interfaces^]
			   [keys : framework:keys^]
			   [test : framework:test^])
  (link [application : framework:application^ ((require-relative-library-unit/sig "app.ss"))]
	[version : framework:version^ ((require-relative-library-unit/sig "version.ss") core:string core:function)]
	[exn : framework:exn^ ((require-relative-library-unit/sig "exn.ss"))]
	[exit : framework:exit^ ((require-relative-library-unit/sig "exit.ss") preferences gui-utils)]
	[preferences : framework:preferences^
		     ((require-relative-library-unit/sig "prefs.ss")
		      mred
		      exn exit panel core:pretty-print core:function)]
	[autosave : framework:autosave^
		  ((require-relative-library-unit/sig "autosave.ss") mred exit preferences)]
	[handler : framework:handler^
		 ((require-relative-library-unit/sig "handler.ss")
		  mred
		  gui-utils finder group  text preferences frame
		  core:file)] 
	[keymap : framework:keymap^
		((require-relative-library-unit/sig "keymap.ss")
		 mred keys preferences finder handler scheme-paren frame)]
	[match-cache : framework:match-cache^ ((require-relative-library-unit/sig "mcache.ss"))]
	[paren : framework:paren^ ((require-relative-library-unit/sig "paren.ss"))]
	[scheme-paren : framework:scheme-paren^
		      ((require-relative-library-unit/sig "sparen.ss") paren)]
	[path-utils : framework:path-utils^ ((require-relative-library-unit/sig "fileutil.ss"))]
	[icon : framework:icon^ ((require-relative-library-unit/sig "icon.ss") mred)]

	[editor : framework:editor^ ((require-relative-library-unit/sig "editor.ss")
				     mred
				     autosave finder path-utils keymap icon preferences text pasteboard)]
	[pasteboard : framework:pasteboard^ ((require-relative-library-unit/sig "pasteboard.ss")
					     mred editor)]
	[text : framework:text^ ((require-relative-library-unit/sig "text.ss")
				 mred editor preferences keymap gui-utils
				 core:function)]

	[gui-utils : framework:gui-utils^ ((require-relative-library-unit/sig "guiutils.ss") mred)]

	[finder : framework:finder^
		((require-relative-library-unit/sig "finder.ss")
		 mred
		 preferences gui-utils
		 core:string core:function core:file)]

	[group : framework:group^ 
	       ((require-relative-library-unit/sig "group.ss")
		mred exit frame
		core:function core:file)]

	[canvas : framework:canvas^ ((require-relative-library-unit/sig "canvas.ss")
				     mred preferences)]

	[panel : framework:panel^ ((require-relative-library-unit/sig "panel.ss")
				   mred core:function)]

	[frame : framework:frame^ 
	       ((require-relative-library-unit/sig "frame.ss")
		mred
		group preferences icon handler application
		panel gui-utils exit finder keymap text editor
		core:function)]
	[scheme : framework:scheme^ 
		((require-relative-library-unit/sig "scheme.ss")
		 mred preferences match-cache paren
		 scheme-paren icon keymap text frame
		 core:thread)]
	[main : framework:main^ ((require-relative-library-unit/sig "main.ss")
				 mred
				 preferences exit group
				 core:function)])
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
   (unit scheme)
   (unit main)))