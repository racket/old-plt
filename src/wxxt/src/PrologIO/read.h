/*								-*- C++ -*-
 * File:		read.h
 * Purpose:	Prolog subset reading/manipulation
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef hy_readh
#define hy_readh

#ifndef wx_xt

    // wxWindows standard include mechanism
    /* sccsid[] = "%W% %G%" */
#   include <stdio.h>
#   include <iostream.h>
#   include <wx_list.h>
#   include <wx_hash.h>

#else // wx_xt

    // The Xt port uses another include mechanism
#   pragma interface

#endif

// Error types
#define PROIO_ERROR_GENERAL 1
#define PROIO_ERROR_SYNTAX  2

#ifndef wx_xt
// The Xt port collects all global data in one file (sharability of library)
    // Error handler function definition. If app returns TRUE,
    // carry on processing.
    typedef Bool (*proioErrorHandler) (int errorType, char *msg);
    extern proioErrorHandler currentProioErrorHandler;
    // Temporary variable for communicating between read.cc and YACC/LEX
    class PrologDatabase;
    extern PrologDatabase *hyPrologDatabase;
#endif

typedef enum {
    PrologNull,
    PrologInteger,
    PrologReal,
    PrologWord,
    PrologString,
    PrologList
} PrologType;

class PrologDatabase;
class ClipsTemplate;
class ClipsTemplateSlot;

class PrologExpr
{
 public:
  wxObject *client_data;
  PrologType type;
  union {
    long integer;
    char *word;
    char *string;
    float real;
    PrologExpr *first;  // If is a list expr, points to the first node
    } value;

  PrologExpr *next;     // If this is a node in a list, points to the next node
  PrologExpr *last;     // If is a list expr, points to the last node

  PrologExpr(char *functor);      // Assume this is a new clause - pass functor
  PrologExpr(PrologType the_type, char *word_or_string = NULL, Bool allocate = TRUE);
  PrologExpr(long the_integer);
  PrologExpr(float the_real);
  PrologExpr(wxList *the_list);
  ~PrologExpr(void);

  inline PrologType Type(void) { return type; }
  inline long IntegerValue(void)
  {
    if (type == PrologInteger)
      return value.integer;
    else if (type == PrologReal)
      return (long)value.real;
    else return 0;
  }

  inline float RealValue(void) {
    if (type == PrologReal)
      return value.real;
    else if (type == PrologInteger)
      return (float)value.integer;
    else return 0.0;
  }

  inline char *WordValue(void) {
    if (type == PrologWord)
      return value.word;
    else if (type == PrologString)
      return value.string;
    else return NULL;
  }

  inline char *StringValue(void) {
    if (type == PrologString)
      return value.string;
    else if (type == PrologWord)
      return value.word;
    else return NULL;
  }

  // Get nth arg of clause (starting from 1)
  PrologExpr *Arg(PrologType type, int arg);

  // Return nth argument of a list expression (starting from zero)
  PrologExpr *Nth(int arg);
  // Returns the number of elements in a list expression
  int Number(void);

  PrologExpr *Copy(void);

  PrologExpr *GetAttributeValueNode(char *word);  // Use only for a clause or list
  PrologExpr *AttributeValue(char *word);  // Use only for a clause
  char *Functor(void);                     // Only for a clause
  Bool IsFunctor(char *f);                     // Only for a clause
  void WritePrologClause(ostream& stream);  // Write this expression as a top-level clause
  void WritePrologExpr(ostream& stream);    // Write as any other subexpression
  void WriteLispExpr(ostream& stream);
  void WriteClipsClause(ostream& stream, Bool filtering = FALSE,
                        PrologDatabase *database = NULL);
  void WriteClipsSlot(ostream& stream, ClipsTemplate *temp);
  void WriteClipsList(ostream& stream);

  // Debugging purposes - write to std output
  void WriteClause(void);
  void WriteExpr(void);

  // Append an expression to a list
  void Append(PrologExpr *expr);
  // Insert at beginning of list
  void Insert(PrologExpr *expr);

  // Get first expr in list
  inline PrologExpr *GetFirst(void)
	{ return ((type == PrologList) ? value.first : (PrologExpr*)NULL); }

  // Get next expr if this is a node in a list
  inline PrologExpr *GetNext(void) { return next; }

  // Get last expr in list
  inline PrologExpr *GetLast(void)
	{ return ((type == PrologList) ? last : (PrologExpr*)NULL); }

  // This should really be called SetAttributeValue since any existing
  // attribute-value is deleted first.
  void AddAttributeValue(char *attribute, long value);
  void AddAttributeValue(char *attribute, float value);
  void AddAttributeValueWord(char *attribute, char *value);
  void AddAttributeValueString(char *attribute, char *value);
  void AddAttributeValue(char *attribute, wxList *value);
  void AddAttributeValue(char *attribute, PrologExpr *value);
  void AddAttributeValueStringList(char *attribute, wxList *string_list);

  void DeleteAttributeValue(char *attribute);

  // Retrieving values - only assigns values if they were found, and of right type
  // Usage: e.g.
  // int id = 0;
  // clause->AssignAttributeValue("id", &id);

  void AssignAttributeValue(char *att, int *var);
  void AssignAttributeValue(char *att, long *var);
  void AssignAttributeValue(char *att, float *var);
  void AssignAttributeValue(char *att, char **var);  // Word OR string -> string
  void AssignAttributeValue(char *att, PrologExpr **var);

  // Add string items to list if the list attribute exists
  void AssignAttributeValueStringList(char *att, wxList *var);

  // Associate other data with this expression, e.g. when reading in a
  // number of linked items - store C++ object pointer with the expression
  // so we can index into the Prolog database and fish out the pointer.
  inline void SetClientData(wxObject *data) { client_data = data; }
  inline wxObject *GetClientData(void) { return client_data; }
};

class PrologDatabase: public wxList
{
  DECLARE_DYNAMIC_CLASS(PrologDatabase)
 private:
  wxNode *position;              // Where we are in a search
  wxHashTable *hash_table;
  char *attribute_to_hash;
  wxList templates;
 public:
  int noErrors;

  PrologDatabase(proioErrorHandler handler = 0);

  // Use hashing on both the functor, and the attribute of
  // specified type (PrologString or PrologInteger) and name.
  // So to find node 45
  // (i.e. match the clause node(id=45, ...))
  // it usually requires 1 look-up: the keys for functor and attribute
  // are added together.
  // Obviously if the attribute was missing in a clause, it would
  // fail to be found by this method, but could be retrieved by a
  // linear search using BeginFind and FindClauseByFunctor,
  // or just searching through the list as per usual.

  PrologDatabase(PrologType type, char *attribute, int size = 500,
                 proioErrorHandler handler = 0);

  ~PrologDatabase(void);

  void BeginFind(void);          // Initialise a search
  PrologExpr *FindClause(long id);  // Find a term based on an integer id attribute
                                 // e.g. node(id=23, type=rectangle, ....).

  // Find on basis of attribute/value pairs, e.g. type=rectangle
  // This doesn't use hashing; it's a linear search.
  PrologExpr *FindClause(char *word, char *value);
  PrologExpr *FindClause(char *word, long value);
  PrologExpr *FindClause(char *word, float value);
  PrologExpr *FindClauseByFunctor(char *functor);

  PrologExpr *HashFind(char *functor, char *value);
  PrologExpr *HashFind(char *functor, long value);

  void Append(PrologExpr *expr);  // Does cleverer things if hashing is on
  void ClearDatabase(void);
  inline int GetErrorCount() { return noErrors; }
  Bool ReadProlog(char *filename);
  Bool ReadPrologFromString(char *buffer);
  void WriteProlog(ostream& stream);
  void WriteLisp(ostream& stream);

  // Write as a set of CLIPS facts (have to define templates manually elsewhere)
  void WriteClips(ostream& stream);

  // Writes templates AND only writes slots mentioned in the appropriate
  // template.
  void WriteClipsFiltering(ostream& stream);

  // Using template info
  void AddTemplate(ClipsTemplate *temp);
  ClipsTemplate *FindTemplate(char *template_name);
};

// Clips helper stuff

class ClipsTemplate: public wxObject
{
 public:
  char *name;
  wxList slots;
  ClipsTemplate(char *the_name);
  ~ClipsTemplate(void);
  void AddSlot(char *slot_name, char *def = NULL, Bool multi = FALSE);
  ClipsTemplateSlot *SlotExists(char *slot_name);
  void Write(ostream& stream);
};

class ClipsTemplateSlot: public wxObject
{
 public:
  char *slot_name;
  char *default_name;
  Bool multi;
  ClipsTemplateSlot(char *sname, char *def = NULL, Bool mult = FALSE);
  ~ClipsTemplateSlot(void);
  void Write(ostream& stream);
};

// Function call-style interface - some more convenience wrappers/unwrappers

// Make a call
PrologExpr *wxMakeCall(char *functor ...);

#define wxMakeInteger(x) (new PrologExpr((long)x))
#define wxMakeReal(x) (new PrologExpr((float)x))
#define wxMakeString(x) (new PrologExpr(PrologString, x))
#define wxMakeWord(x)   (new PrologExpr(PrologWord, x))
#define wxMake(x)       (new PrologExpr(x))

// Check types of arguments - return NULL if no error, an error
// string if an error.
char *wxCheckTypes(PrologExpr *expr ...);
char *wxCheckClauseTypes(PrologExpr *expr, wxList *type_list);

// Checks functor
Bool wxIsFunctor(PrologExpr *expr, char *functor);

#endif // hy_readh

