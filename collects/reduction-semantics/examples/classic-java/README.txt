ClassicJava in PLT Redex
Richard Cobbe
January 2005
$Id$
======================================================================

This directory contains an implementation of most of ClassicJava (Flatt,
Krishnamurthi, and Felleisen, '99) in the PLT Redex system (Matthews et al,
'04).  The implementation currently runs in DrScheme 209; it will not
compile in DrScheme 299/300.

Language Differences
====================
There are several differences between the language implemented here and the
language described in the paper.  

  1) This implementation does not support interfaces or abstract methods.  

  2) I have added boolean and integer types, with associated primitives, to
     make it easier to write interesting test cases without having to
     Church-encode everything.  

  3) There is a minor change to the dynamic semantics.  In the paper, the
     expression
        view t null
    would produce, at run-time, "error: dereferenced null".  In this
    implementation, the equivalent expression 
        (cast t null)
    evaluates to null.  The implementation's behavior is consistent with
    Java.

Overview of the files:
======================
- grammar.txt : defines the surface syntax of this implementation of
  ClassicJava and explains how it relates to the more traditional Java
  syntax.

- parser.ss: implements a parser for the language defined in grammar.txt.

- ast.ss: data type definitions for the abstract syntax tree and related
  types.

- program.ss: defines functions for operating on Program objects.  A
  Program object represents a complete ClassicJava program.

- elaboration.ss: defines the type-checking and static elaboration logic.

- reduction.ss: defines the reduction semantics and the second grammar
  as required by PLT Redex's pattern matching system.

- utils.ss: miscellaneous utilities.

- store.ss: abbreviation of a functional store data type, indexed by
  natural numbers.

- environment.ss: implementation of a standard rib-cage environment.

- classic-java.ss: front-end; allows for easy evaluation of ClassicJava
  programs (either graphically or textually).  See the comments at the
  start of classic-java.ss for details.

- *-tests.ss: all of my test cases.  You will not be able to load these, as
  they depend on SchemeUnit and a couple of other libraries that I've
  hacked up but not distributed.  I've included these primarily for
  documentation purposes.  If you should want to run these, let me know,
  and I can send you the necessary libraries.
