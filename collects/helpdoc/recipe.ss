
(
("Data Analysis and Definition"

"to understand what kinds of information are to be processed and
how to represent the information as data"

("What form of information must be represented?  What kind of objects
 must be represented?" "Identify all those things (objects, points in
 space, measurements, names, and also information aggregates) that the
 program must represent as data.")

("Is it possible to represent the information with Scheme's basic forms
 of data?" "For example, a simple temperature measurement can be
 represented with a plain number. A symbol is a good representation for a
 name.")

("If some information is represented as a Scheme number, is it possible to
 describe the proper range of numbers via (unions of) intervals?"
 "Formulate a data definition to describe the intervals. If the number
 represents a dollar amount, it can only be positive. If it is an absolute
 temperature, it can only be a number like $-272^oC$ or larger.")

("Does an object in the problem statement have several properties?" "Use
 structures to represent things that have more than one property. Define a
 structure, using \\scheme{define-struct}, and formulate a data definition
 that describes what kind of data each field of the structure contains.
 For example, a colored game ball may have two properties: a color and a
 position. Its representation could be a structure with two fields; the
 first one contains a symbol, the second one (the representation of) a
 point in space.

 Note: It is also possible to represent information aggregates as Scheme
 functions or vectors.")

("Does some {\\em information\\/} collection consist of disjoint subsets?"
 "Formulate a data definition with several clauses for ``mixed''
 collections of information or data. For example, a problem might mention
 many different kinds of shapes: triangle, squares, rectangles, etc. Each
 of them may be represented by a structure.")

("Does some {\\em data\\/} collection consist of disjoint subsest?" "For
 example, to represent list of things, a Scheme programmer must consider
 two distinct sets of data: \\scheme{empty} and \\scheme{cons}tructed
 lists.")

("Does the problem require the representation of an arbitrarily large
 information aggregate?" "Formulate a recursive (self-referential) data
 definition to do so. Mark the self-references in the data definition.
 (Note: The data definitions must have several clauses.) Examples of such
 data abound. Here are two. A list of temperature measurements might
 contain 100 items now and 500 later; it is best thought of as arbitrarily
 large. The known portion of some paternal family tree is 300+ years deep;
 amother one might be only 100+ years deep. Again, these trees are best
 thought of as arbitrarily large.")

("When all the relevant classes of information are defined, \\ldots" "
 \\ldots\\ make up examples for all data definitions.")) 

;; ------------------------------------------------------------------------

("Contract and Header"

"to formulate examples of the program's input-output behavior
(using the data definitions) and a program header"

("What are the ``givens'' in the problem statement?  What are the
 ``unknowns''?" "All but one of the unknowns are ``inputs''; the remaining
 one is the desired ``output.''")

("For each unknown, \\ldots" "\\ldots\\ pick a (meaningful) name that stands
 for its value. Optional: use an English phrase to describe a variable's
 purpose.")

("To what class of data does each of the program's inputs belong?  To
 what class of data does the program's output belong?" "Use the data
 definitions from ``Data Analysis and Definition'' to formulate the
 answers; if they are insufficient, improve them and the set of
 examples. Write down a contract.")

("Formulate the header, using the chosen variable names." "The name for
 the unknown output is the program's name; the remaining names are those of
 the inputs.")
)

;; ------------------------------------------------------------------------

("Program Examples"

"to understand what the program produces for a variety of inputs"

("What are some examples that describe what the program computes?" "Make up
 examples for the inputs and determine the output by hand. Show the work
 because this may help in determining the full definition of the program.
 Use the examples from ``Data Analysis and Definition.''")

("Assume the program has one input." "\\ldots") 
;; {For more than one input, see below.}

("Does the data definition for the input consist of several clauses?"
 "Include at least one example per clause.")

("Does the data definition describe a small finite set of values?"  "Use
 every element.")

("Does the data definition describe an interval?" "Use all ``special''
 points (boundaries), but also include some plain interior points.") 
)

;; ------------------------------------------------------------------------

("Template"

"to translate the input data definition(s) into a program template"

("Assume the program has one input" "\\ldots")

("Does the data definition for the input specify separate clauses?"
 "Formulate a \\scheme{cond}-expression with one \\scheme{cond}-clause per
 clause in the data definition.")

("If we need a \\scheme{cond}-expression, what Scheme conditions distinguish
 the various kinds of inputs?" "Write down the conditions.  Use the
 examples from ``Program Examples'' to ensure that the chosen input(s) are
 recognized by the matching conditions.")

("Are the inputs structures?" "Write down one selector expression per field
 of the structure. If the template consists of a \\scheme{cond}-expression,
 deal with each \\scheme{cond}-clause separately. Understand its condition
 and keep in mind that it is true when the answer expression is
 evaluated.")

("Is the data definition of the input self-referential?" "The template of
 the program should refer to itself in an analogous manner. Circle the
 corresponding access expressions and draw an arrow back to the program's
 name. Alternatively, add the matching recursive calls.")  ) 

;;  ------------------------------------------------------------------------

("Body"

"to produce a complete program by filling the gaps in the template"

("What is the program supposed to compute?" "Re-read the program
examples.")

("What does each expression in the template compute?" "If an expression is
 difficult, first determine the kind of data that the expression computes;
 then describe the result more precisely.")

("How can the program compute the final result from all these values?"
 "Form an expression that involves some or all of the following: the input
 variables, the expressions in the template, primitive operations, and
 other programs.")

("Does the template consist of a \\scheme{cond}-expression?" "Deal with one
 \\scheme{cond}-clause at a time. Understand its condition and keep in mind
 that it is true when the answer expression is evaluated.")

("For recursive programs, \\ldots" "\\ldots\\ deal with non-recursive
 \\scheme{cond}-clauses first.")

("For the recursive \\scheme{cond}-clauses, what does the natural
 recursion compute?" "Remember the purpose of the program and remember
 that, for the natural recursion, the program is only applied to a {\\em
 part\\/} of the original input (e.g. the rest of a list of numbers, the
 left part of a family tree, the predecessor of a natural number, etc).")

("When stuck, walk through the program with examples." "Use the examples
 from ``Program Examples'' to understand what the answer should be. Use
 examples to determine what kind of data is available, what sub-expressions
 compute, what the natural recursion computes.")
)

;; ------------------------------------------------------------------------

("Test"
"to test the program's behavior on the specified examples"

("Use the examples from ``Program Examples'' to test the program." "If
an actual answer differs from a corresponding expected one, it is possible
that
\\begin{enumerate}
  \\item the original caculation or
  \\item the program formulation
\\end{enumerate}
 contains a mistake. In rare cases, it is also possible that both answers
 are wrong because both parts contain mistakes.
")
)
)
