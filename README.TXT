
This is the PLT software package directory. 

Executables
-----------

The following are the executables:

  plt\DrScheme.exe -- PLT's main Scheme development environment
  plt\Drscheme Jr.exe -- A simple, text-based DrScheme
  plt\MrEd.exe -- a Scheme GUI engine
  plt\MzScheme.exe -- a text-based Scheme interpreter
  plt\mzc.exe -- the MzScheme compiler

DrScheme relies on MrEd, and DrScheme Jr and mzc rely on MzScheme. You
may not have some of these executables, depending on which
distribution you downloaded.

Compiling
---------

If you downloaded the source code distribution instead of the standard
distribution, you must compile MzScheme and MrEd before using the PLT
software (including DrScheme). 

For compilation instructions, see plt/src/worksp/README. 

If there is no plt\src\mzscheme directory, you did not download
the source code distribution. In this case, you should be ready to
run.

Documentation
-------------

Some documentation may be included in plt\collects\doc, but some of
the PLT documentation must be downloaded separately from the
executables.

HTML documentation for PLT software should be installed into
plt\collects\doc. For example, the DrScheme documenation should be in
plt\collects\doc\drscheme, with button icons in plt\collects\doc\icons.

DrScheme and MrEd will automatically add documentation installed in
plt\collects\doc to the Help menu.

Release Notes
-------------

The release notes for this version are in plt\notes\release\51.html.
More specific notes can be found in sub-directories of plt\notes.

More Information
----------------

For more information, see
  http://www.cs.rice.edu/CS/PLT/packages/
