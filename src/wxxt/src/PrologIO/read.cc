/*
 * File:		read.cc
 * Purpose:	Prolog subset reading and manipulation
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wx_xt

    // wxWindows standard include mechanism
    static const char sccsid[] = "%W% %G%";
    // For compilers that support precompilation, includes "wx.h".
#   include "wx_prec.h"
#   ifdef __BORLANDC__
#       pragma hdrstop
#   endif
#   ifndef WX_PRECOMP
#       include <common.h>
#       include <wx_utils.h>
#   endif
#   include "read.h"

#else // wx_xt

    // The Xt port uses another include mechanism
#   ifdef __GNUG__
#       pragma implementation "read.h"
#   endif
#   define  Uses_wxPrologIO
#   include "wx.h"

#endif // #ifndef wx_xt

#include <stdarg.h>
#include <ctype.h>
#include <math.h>

// functions imported from/exported to wxPrologParser
extern "C" {
    void PROIO_LexFromFile(FILE *fd);
    void PROIO_LexFromString(char *buf);
    int  PROIO_yyparse(void);

    void  add_expr(char *);
    char *make_exp(char *str1, char *str2);
    char *make_exp2(char *str1, char *str2, char *str3);
    char *make_integer(char *str);
    char *make_real(char *str1, char *str2);
    char *make_string(char *str);
    char *make_word(char *str);
    char *proio_cons(char * ccar, char * ccdr);
    void  process_command(char * cexpr);
    void  syntax_error(char *s);
}

#ifndef wx_xt
    // The Xt port collects all global data in one file (sharability of library)
    PrologDatabase *hyPrologDatabase = NULL;
    proioErrorHandler currentProioErrorHandler;
#endif

IMPLEMENT_DYNAMIC_CLASS(PrologDatabase, wxList)

PrologExpr::PrologExpr(char *functor)
{
  type = PrologList;
  next = NULL;
  last = NULL;
  value.first = NULL;

  PrologExpr *pfunctor = new PrologExpr(PrologWord, functor);
  Append(pfunctor);
  client_data = NULL;
}

PrologExpr::PrologExpr(PrologType the_type, char *word_or_string, Bool allocate)
{
  type = the_type;

  switch (the_type)
  {
   case PrologWord:
    value.word = allocate ? copystring(word_or_string) : word_or_string;
    break;
   case PrologString:
    value.string = allocate ? copystring(word_or_string) : word_or_string;
    break;
   case PrologList:
    last = NULL;
    value.first = NULL;
    break;
   case PrologReal:
   case PrologInteger:
   case PrologNull: 
#ifdef wx_x
    cerr << "Should never get here!\n";
#endif
    break;
  }
  client_data = NULL;
  next = NULL;
}

PrologExpr::PrologExpr(long the_integer)
{
  type = PrologInteger;
  value.integer = the_integer;
  client_data = NULL;
  next = NULL;
}

PrologExpr::PrologExpr(float the_real)
{
  type = PrologReal;
  value.real = the_real;
  client_data = NULL;
  next = NULL;
}

PrologExpr::PrologExpr(wxList *the_list)
{
  type = PrologList;
  client_data = NULL;
  last = NULL;
  value.first = NULL;

  PrologExpr *listExpr = new PrologExpr(PrologList);

  wxNode *node = the_list->First();
  while (node)
  {
    PrologExpr *expr = (PrologExpr *)node->Data();
    listExpr->Append(expr);
    node = node->Next();
  }
  Append(listExpr);

  delete the_list;
}

PrologExpr::~PrologExpr(void)
{
  switch (type)
  {
    case PrologInteger:
    case PrologReal:
    {
     break;
    }
   case PrologString:
   {
     delete[] value.string;
     break;
   }
   case PrologWord:
   {
     delete[] value.word;
     break;
   }
   case PrologList:
   {
     PrologExpr *expr = value.first;
     while (expr)
     {
       PrologExpr *expr1 = expr->next;

       delete expr;
       expr = expr1;
     }
     break;
   }
   case PrologNull: break;
  }
}

void PrologExpr::Append(PrologExpr *expr)
{
  if (!value.first)
    value.first = expr;

  if (last)
    last->next = expr;
  last = expr;
}

void PrologExpr::Insert(PrologExpr *expr)
{
  expr->next = value.first;
  value.first = expr;

  if (!last)
    last = expr;
}

PrologExpr *PrologExpr::Copy(void)
{
  // This seems to get round an optimizer bug when
  // using Watcom C++ 10a in WIN32 compilation mode.
  // If these lines not present, the type seems to be
  // interpreted wrongly as an integer.
  // I don't want to turn optimization off since it's needed
  // for reading in files quickly.
#if defined(__WATCOMC__)
  char buf[2];
  sprintf(buf, "");
#endif

  switch (type)
  {
    case PrologInteger:
      return new PrologExpr(value.integer);
    case PrologReal:
      return new PrologExpr(value.real);
    case PrologString:
      return new PrologExpr(PrologString, value.string);
    case PrologWord:
      return new PrologExpr(PrologWord, value.word);
    case PrologList:
    {
      PrologExpr *expr = value.first;
      PrologExpr *new_list = new PrologExpr(PrologList);
      while (expr)
      {
        PrologExpr *expr2 = expr->Copy();
        new_list->Append(expr2);
        expr = expr->next;
      }
      return new_list;
    }
   case PrologNull: 
#ifdef wx_x
    cerr << "Should never get here!\n";
#endif
    break;
  }
  return NULL;
}


// Get the PrologExpr (containing (= PrologExpr Value) form) for the given word
//  or string, assuming that we have Attribute=Value, ...
PrologExpr *PrologExpr::GetAttributeValueNode(char *word)  // Use only for a clause or list
{
  if (type != PrologList)
    return NULL;

  PrologExpr *expr = value.first;
  while (expr)
  {
    if (expr->type == PrologList)
    {
      PrologExpr *firstNode = expr->value.first;
      if ((firstNode->type == PrologWord) && (firstNode->value.word[0] == '='))
      {
        PrologExpr *secondNode = firstNode->next;
        if ((secondNode->type == PrologWord) && 
            (strcmp(word, secondNode->value.word) == 0))
        {
          return expr;
        }
      }
    }
    expr = expr->next;
  }
  return NULL;
}

// Get the value (in PrologExpr form) for the given word or string, assuming
// that we have Attribute=Value, ...
PrologExpr *PrologExpr::AttributeValue(char *word)  // Use only for a clause or list
{
  if (type != PrologList)
    return NULL;

  PrologExpr *attExpr = GetAttributeValueNode(word);
  if (attExpr && attExpr->value.first && attExpr->value.first->next)
    return attExpr->value.first->next->next;
  else return NULL;
}

char *PrologExpr::Functor(void)  // Use only for a clause
{
  if ((type != PrologList) || !value.first)
    return NULL;

  if (value.first->type == PrologWord)
    return value.first->value.word;
  else
    return NULL;
}

Bool PrologExpr::IsFunctor(char *f)  // Use only for a clause
{
  if ((type != PrologList) || !value.first)
    return FALSE;

  return (value.first->type == PrologWord && 
          (strcmp(f, value.first->value.word) == 0));
}

// Return nth argument of a clause (starting from 1)
PrologExpr *PrologExpr::Arg(PrologType theType, int arg)
{
  PrologExpr *expr = value.first;
  int i;
  for (i = 1; i < arg; i++)
    if (expr)
      expr = expr->next;

  if (expr && (expr->type == theType))
    return expr;
  else
    return NULL;
}

// Return nth argument of a list expression (starting from zero)
PrologExpr *PrologExpr::Nth(int arg)
{
  if (type != PrologList)
    return NULL;

  PrologExpr *expr = value.first;
  int i;
  for (i = 0; i < arg; i++)
    if (expr)
      expr = expr->next;
    else return NULL;

  if (expr)
    return expr;
  else
    return NULL;
}

  // Returns the number of elements in a list expression
int PrologExpr::Number(void)
{
  if (type != PrologList)
    return 0;

  int i = 0;
  PrologExpr *expr = value.first;
  while (expr)
  {
    expr = expr->next;
    i ++;
  }
  return i;
}

void PrologExpr::DeleteAttributeValue(char *attribute)
{
  if (type != PrologList)
    return;

  PrologExpr *expr = value.first;
  PrologExpr *lastExpr = this;
  while (expr)
  {
    if (expr->type == PrologList)
    {
      PrologExpr *firstNode = expr->value.first;
      if ((firstNode->type == PrologWord) && (firstNode->value.word[0] == '='))
      {
        PrologExpr *secondNode = firstNode->next;
        if ((secondNode->type == PrologWord) && 
            (strcmp(attribute, secondNode->value.word) == 0))
        {
          PrologExpr *nextExpr = expr->next;
          delete expr;

          lastExpr->next = nextExpr;

          if (last == expr)
            last = lastExpr;

          return;
        }
      }
    }
    lastExpr = expr;
    expr = expr->next;
  }
  return;
}

void PrologExpr::AddAttributeValue(char *attribute, PrologExpr *val)
{
  if (type != PrologList)
  {
//    cout << "Error! tried to add an attribute-value pair to a nonlist Prolog expression\n";
    return;
  }
  // Warning - existing code may assume that any existing value
  // is deleted first. For efficiency, we leave this to the application.
//  DeleteAttributeValue(attribute);

  PrologExpr *patt = new PrologExpr(PrologWord, attribute);
  PrologExpr *pequals = new PrologExpr(PrologWord, "=");

  PrologExpr *listExpr = new PrologExpr(PrologList);

  listExpr->Append(pequals);
  listExpr->Append(patt);
  listExpr->Append(val);

  Append(listExpr);
}

void PrologExpr::AddAttributeValue(char *attribute, long val)
{
  if (type != PrologList)
  {
//    cout << "Error! tried to add an attribute-value pair to a nonlist Prolog expression\n";
    return;
  }
  // Warning - existing code may assume that any existing value
  // is deleted first. For efficiency, we leave this to the application.
//  DeleteAttributeValue(attribute);

  PrologExpr *patt = new PrologExpr(PrologWord, attribute);
  PrologExpr *pval = new PrologExpr(val);
  PrologExpr *pequals = new PrologExpr(PrologWord, "=");

  PrologExpr *listExpr = new PrologExpr(PrologList);

  listExpr->Append(pequals);
  listExpr->Append(patt);
  listExpr->Append(pval);

  Append(listExpr);
}

void PrologExpr::AddAttributeValue(char *attribute, float val)
{
  if (type != PrologList)
  {
//    cout << "Error! tried to add an attribute-value pair to a nonlist Prolog expression\n";
    return;
  }

//  DeleteAttributeValue(attribute);
  PrologExpr *patt = new PrologExpr(PrologWord, attribute);
  PrologExpr *pval = new PrologExpr(val);
  PrologExpr *pequals = new PrologExpr(PrologWord, "=");

  PrologExpr *listExpr = new PrologExpr(PrologList);

  listExpr->Append(pequals);
  listExpr->Append(patt);
  listExpr->Append(pval);

  Append(listExpr);
}

void PrologExpr::AddAttributeValueString(char *attribute, char *val)
{
  if (type != PrologList)
  {
//    cout << "Error! tried to add an attribute-value pair to a nonlist Prolog expression\n";
    return;
  }
  if (!val)
    return;

//  DeleteAttributeValue(attribute);

  PrologExpr *patt = new PrologExpr(PrologWord, attribute);
  PrologExpr *pval = new PrologExpr(PrologString, val);
  PrologExpr *pequals = new PrologExpr(PrologWord, "=");

  PrologExpr *listExpr = new PrologExpr(PrologList);

  listExpr->Append(pequals);
  listExpr->Append(patt);
  listExpr->Append(pval);

  Append(listExpr);
}

void PrologExpr::AddAttributeValueWord(char *attribute, char *val)
{
  if (type != PrologList)
  {
//    cout << "Error! tried to add an attribute-value pair to a nonlist Prolog expression\n";
    return;
  }
  if (!val)
    return;

//  DeleteAttributeValue(attribute);

  PrologExpr *patt = new PrologExpr(PrologWord, attribute);
  PrologExpr *pval = new PrologExpr(PrologWord, val);
  PrologExpr *pequals = new PrologExpr(PrologWord, "=");

  PrologExpr *listExpr = new PrologExpr(PrologList);

  listExpr->Append(pequals);
  listExpr->Append(patt);
  listExpr->Append(pval);

  Append(listExpr);
}

void PrologExpr::AddAttributeValue(char *attribute, wxList *val)
{
  if (type != PrologList)
  {
//    cout << "Error! tried to add an attribute-value pair to a nonlist Prolog expression\n";
    return;
  }
  if (!val)
    return;

//  DeleteAttributeValue(attribute);

  PrologExpr *patt = new PrologExpr(PrologWord, attribute);
  PrologExpr *pval = new PrologExpr(val);
  PrologExpr *pequals = new PrologExpr(PrologWord, "=");

  PrologExpr *listExpr = new PrologExpr(PrologList);

  listExpr->Append(pequals);
  listExpr->Append(patt);
  listExpr->Append(pval);

  Append(listExpr);
}

void PrologExpr::AddAttributeValueStringList(char *attribute, wxList *string_list)
{
  if (type != PrologList)
  {
//    cout << "Error! tried to add an attribute-value pair to a nonlist Prolog expression\n";
    return;
  }
  if (!string_list)
    return;

//  DeleteAttributeValue(attribute);

  // First make a list of PrologExpr strings
  PrologExpr *listExpr = new PrologExpr(PrologList);
  wxNode *node = string_list->First();
  while (node)
  {
    char *string = (char *)node->Data();
    PrologExpr *expr = new PrologExpr(PrologString, string);
    listExpr->Append(expr);
    node = node->Next();
  }

  // Now make an (=, Att, Value) triple
  PrologExpr *patt = new PrologExpr(PrologWord, attribute);
  PrologExpr *pequals = new PrologExpr(PrologWord, "=");

  PrologExpr *listExpr2 = new PrologExpr(PrologList);

  listExpr2->Append(pequals);
  listExpr2->Append(patt);
  listExpr2->Append(listExpr);

  Append(listExpr2);
}

void PrologExpr::AssignAttributeValue(char *att, int *var)
{
  PrologExpr *expr = AttributeValue(att);

  if (expr && (expr->Type() == PrologInteger || expr->Type() == PrologReal))
    *var = (int)(expr->IntegerValue());
}

void PrologExpr::AssignAttributeValue(char *att, long *var)
{
  PrologExpr *expr = AttributeValue(att);

  if (expr && (expr->Type() == PrologInteger || expr->Type() == PrologReal))
    *var = expr->IntegerValue();
}

void PrologExpr::AssignAttributeValue(char *att, float *var)
{
  PrologExpr *expr = AttributeValue(att);
  if (expr && (expr->Type() == PrologInteger || expr->Type() == PrologReal))
    *var = expr->RealValue();
}

void PrologExpr::AssignAttributeValue(char *att, char **var)  // Word OR string -> string
{
  PrologExpr *expr = AttributeValue(att);
  if (expr && expr->Type() == PrologWord)
  {
    if (*var) delete[] *var;
    *var = copystring(expr->WordValue());
  }
  else if (expr && expr->Type() == PrologString)
  {
    if (*var) delete[] *var;
    *var = copystring(expr->StringValue());
  }
}

/*
void PrologExpr::AssignAttributeValue(char *att, wxList **var)
{
  PrologExpr *expr = AttributeValue(att);
  if (expr && expr->Type() == PrologList)
    *var = expr->ListValue();
}
*/

void PrologExpr::AssignAttributeValue(char *att, PrologExpr **var)
{
  PrologExpr *expr = AttributeValue(att);
  if (expr)
    *var = expr;
}

void PrologExpr::AssignAttributeValueStringList(char *att, wxList *var)
{
  PrologExpr *expr = AttributeValue(att);
  if (expr && expr->Type() == PrologList)
  {
    PrologExpr *string_expr = expr->value.first;
    while (string_expr)
    {
      if (string_expr->Type() == PrologString)
        var->Append((wxObject *)copystring(string_expr->StringValue()));

      string_expr = string_expr->next;
    }
  }
}

void PrologExpr::WritePrologClause(ostream& stream)  // Write this expression as a top-level clause
{
  if (type != PrologList)
    return;

  PrologExpr *node = value.first;
  if (node)
  {
    node->WritePrologExpr(stream);
    stream << "(";
    node = node->next;
    Bool first = TRUE;
    while (node)
    {
      if (!first)
        stream << "  ";
      node->WritePrologExpr(stream);
      node = node->next;
      if (node) stream << ",\n";
      first = FALSE;
    }
    stream << ").\n\n";
  }
}

void PrologExpr::WritePrologExpr(ostream& stream)    // Write as any other subexpression
{
  // This seems to get round an optimizer bug when
  // using Watcom C++ 10a in WIN32 compilation mode.
  // If these lines not present, the type seems to be
  // interpreted wrongly as an integer.
  // I don't want to turn optimization off since it's needed
  // for reading in files quickly.
#if defined(__WATCOMC__)
  char buf[2];
  sprintf(buf, "");
#endif

  switch (type)
  {
    case PrologInteger:
    {
      stream << value.integer;
      break;
    }
    case PrologReal:
    {
      float f = value.real;
/* Now the parser can cope with this.
      // Prevent printing in 'e' notation. Any better way?
      if (fabs(f) < 0.00001)
        f = 0.0;
*/
      sprintf(wxBuffer, "%.6g", f);
      stream << wxBuffer;
      break;
    }
    case PrologString:
    {
      stream << "\"";
      int i;
      int len = strlen(value.string);
      for (i = 0; i < len; i++)
      {
        char ch = value.string[i];
        if (ch == '"' || ch == '\\')
          stream << "\\";
        stream << ch;
      }

      stream << "\"";
      break;
    }
    case PrologWord:
    {
      Bool quote_it = FALSE;
      int len = strlen(value.word);
      if ((len == 0) || (len > 0 && (value.word[0] > 64 && value.word[0] < 91)))
        quote_it = TRUE;
      else
      {
        int i;
        for (i = 0; i < len; i++)
          if ((!isalpha(value.word[i])) && (!isdigit(value.word[i])) &&
              (value.word[i] != '_'))
            { quote_it = TRUE; i = len; }
      }

      if (quote_it)
        stream << "'";

      stream << value.word;

      if (quote_it)
        stream << "'";

      break;
    }
    case PrologList:
    {
      if (!value.first)
        stream << "[]";
      else
      {
        PrologExpr *expr = value.first;

        if ((expr->Type() == PrologWord) && (strcmp(expr->WordValue(), "=") == 0))
        {
          PrologExpr *arg1 = expr->next;
          PrologExpr *arg2 = arg1->next;
          arg1->WritePrologExpr(stream);
          stream << " = ";
          arg2->WritePrologExpr(stream);
        }
        else
        {
          stream << "[";
          while (expr)
          {
            expr->WritePrologExpr(stream);
            expr = expr->next;
            if (expr) stream << ", ";
          }
          stream << "]";
        }
      }
      break;
    }
   case PrologNull: break;
  }
}

void PrologExpr::WriteClause(void)
{
#ifdef wx_x
  WritePrologClause(cout);
  cout.flush();
#endif
}

void PrologExpr::WriteExpr(void)
{
#ifdef wx_x
  WritePrologExpr(cout);
  cout.flush();
#endif
}

void PrologExpr::WriteLispExpr(ostream& stream)
{
  switch (type)
  {
    case PrologInteger:
    {
      stream << value.integer;
      break;
    }
    case PrologReal:
    {
      stream << value.real;
      break;
    }
    case PrologString:
    {
      stream << "\"" << value.string << "\"";
      break;
    }
    case PrologWord:
    {
      stream << value.word;
      break;
    }
    case PrologList:
    {
      PrologExpr *expr = value.first;

      stream << "(";
      while (expr)
      {
        expr->WriteLispExpr(stream);
        expr = expr->next;
        if (expr) stream << " ";
      }

      stream << ")";
      break;
    }
   case PrologNull: break;
  }
}

// CLIPS format. If filtering, don't write the clause at all if the
// relevant template is not found in database template list
void PrologExpr::WriteClipsClause(ostream& stream, Bool filtering, PrologDatabase *database)
{
  if (type != PrologList)
    return;

  ClipsTemplate *temp = NULL;
  PrologExpr *node = value.first;
  if (node)
  {
    PrologExpr *functor = node;
    if (filtering)
      temp = database->FindTemplate(functor->WordValue());

    if (filtering && !temp)
      return;

    stream << "(deffacts fact-" << NewId() << "\n";
    stream << " (";
    functor->WritePrologExpr(stream);
    stream << "\n";
    node = node->next;
    while (node)
    {
      node->WriteClipsSlot(stream, temp);
      node = node->next;
    }
    stream << " ))\n\n";
  }
}

// Assume that this is a slot of the form (= slot_name value).
// Therefore ignore first element of list.
void PrologExpr::WriteClipsSlot(ostream& stream, ClipsTemplate *temp)
{
  if (type != PrologList)
    return;

  PrologExpr *firstExpr = value.first;
  PrologExpr *slot_name_expr = firstExpr ? firstExpr->next : (PrologExpr*)NULL;
  PrologExpr *slot_value = slot_name_expr ? slot_name_expr->next : (PrologExpr*)NULL;

  if (slot_name_expr && slot_value)
  {
      Bool do_it = TRUE;
      ClipsTemplateSlot *slot = NULL;

      // If the template doesn't have this slot, ignore it - don't
      // print it out.
      if (temp && (slot_name_expr->Type() == PrologWord))
      {
        slot = temp->SlotExists(slot_name_expr->WordValue());
        if (!slot)
          do_it = FALSE;
      }

      if (do_it)
      {
        stream << "   (";
        slot_name_expr->WriteLispExpr(stream);
        stream << " ";
        if (slot && slot->multi)
          slot_value->WriteClipsList(stream);
        else slot_value->WriteLispExpr(stream);
        stream << ")\n";
      }
  }
}

void PrologExpr::WriteClipsList(ostream& stream)
{
  switch (type)
  {
    case PrologInteger:
    {
      stream << value.integer;
      break;
    }
    case PrologReal:
    {
      stream << value.real;
      break;
    }
    case PrologString:
    {
      stream << "\"" << value.string << "\"";
      break;
    }
    case PrologWord:
    {
      stream << value.word;
      break;
    }
    case PrologList:
    {
      PrologExpr *expr = value.first;

      while (expr)
      {
        expr->WriteLispExpr(stream);
        expr = expr->next;
        if (expr) stream << " ";
      }

      break;
    }
   case PrologNull: break;
  }
}

// Prolog 'database' (list of expressions)
PrologDatabase::PrologDatabase(proioErrorHandler handler)
{
  position = NULL;
  hash_table = NULL;
  attribute_to_hash = NULL;
  currentProioErrorHandler = handler;
  noErrors = 0;
}

PrologDatabase::PrologDatabase(PrologType type, char *attribute, int size,
                               proioErrorHandler handler)
{
  position = NULL;
  attribute_to_hash = copystring(attribute);
  if (type == PrologString)
    hash_table = new wxHashTable(wxKEY_STRING, size);
  else if (type == PrologInteger)
    hash_table = new wxHashTable(wxKEY_INTEGER, size);
  else hash_table = NULL;

  currentProioErrorHandler = handler;
  noErrors = 0;
}

PrologDatabase::~PrologDatabase(void)
{
  ClearDatabase();
  if (hash_table)
    delete hash_table;
  if (attribute_to_hash)
    delete attribute_to_hash;
}

void PrologDatabase::BeginFind(void)          // Initialise a search
{
  position = First();
}

PrologExpr *PrologDatabase::FindClause(long id)  // Find a term based on an integer id attribute
                                 // e.g. node(id=23, type=rectangle, ....).
{
  PrologExpr *found = NULL;
  while (position && !found)
  {
    PrologExpr *term = (PrologExpr *)position->Data();
    
    if (term->Type() == PrologList)
    {
      PrologExpr *value = term->AttributeValue("id");
      if (value->Type() == PrologInteger && value->IntegerValue() == id)
        found = term;
    }
    position = position->Next();
  }
  return found;
}

// Find on basis of attribute/value pairs, e.g. type=rectangle
PrologExpr *PrologDatabase::FindClause(char *word, char *val)
{
  PrologExpr *found = NULL;
  while (position && !found)
  {
    PrologExpr *term = (PrologExpr *)position->Data();
    
    if (term->Type() == PrologList)
    {
      PrologExpr *value = term->AttributeValue(word);
      if ((value->Type() == PrologWord && strcmp(value->WordValue(), val) == 0) ||
          (value->Type() == PrologString && strcmp(value->StringValue(), val) == 0))
        found = term;
    }
    position = position->Next();
  }
  return found;
}

PrologExpr *PrologDatabase::FindClause(char *word, long val)
{
  PrologExpr *found = NULL;
  while (position && !found)
  {
    PrologExpr *term = (PrologExpr *)position->Data();
    
    if (term->Type() == PrologList)
    {
      PrologExpr *value = term->AttributeValue(word);
      if ((value->Type() == PrologInteger) && (value->IntegerValue() == val))
        found = term;
    }
    position = position->Next();
  }
  return found;
}

PrologExpr *PrologDatabase::FindClause(char *word, float val)
{
  PrologExpr *found = NULL;
  while (position && !found)
  {
    PrologExpr *term = (PrologExpr *)position->Data();
    
    if (term->Type() == PrologList)
    {
      PrologExpr *value = term->AttributeValue(word);
      if ((value->Type() == PrologReal) && (value->RealValue() == val))
        found = term;
    }
    position = position->Next();
  }
  return found;
}

PrologExpr *PrologDatabase::FindClauseByFunctor(char *functor)
{
  PrologExpr *found = NULL;
  while (position && !found)
  {
    PrologExpr *term = (PrologExpr *)position->Data();
    
    if (term->Type() == PrologList)
    {
      char *value = term->Functor();
      if (strcmp(value, functor) == 0)
        found = term;
    }
    position = position->Next();
  }
  return found;
}

// If hashing is on, must store in hash table too
void PrologDatabase::Append(PrologExpr *clause)
{
  wxList::Append((wxObject *)clause);
  if (hash_table)
  {
    char *functor = clause->Functor();
    PrologExpr *expr = clause->AttributeValue(attribute_to_hash);
    if (expr)
    {
      long functor_key = hash_table->MakeKey(functor);
      long value_key = 0;
//      PrologExpr *value_expr = (PrologExpr *)(expr->ListValue()->Nth(2)->Data());
      if (expr && expr->Type() == PrologString)
      {
        value_key = hash_table->MakeKey(expr->StringValue());
        hash_table->Put(functor_key + value_key, expr->StringValue(), (wxObject *)clause);
      }
      else if (expr && expr->Type() == PrologInteger)
      {
        value_key = expr->IntegerValue();
        hash_table->Put(functor_key + value_key, expr->IntegerValue(), (wxObject *)clause);
      }

    }
  }
}

// Clips compatibility
void PrologDatabase::AddTemplate(ClipsTemplate *temp)
{
  templates.Append(temp);
}

// Find the template corresponding to the given name, and put a temp.
// pointer in the PrologDatabase structure
ClipsTemplate *PrologDatabase::FindTemplate(char *template_name)
{
  wxNode *node = templates.First();
  ClipsTemplate *temp = NULL;
  while (node && !temp)
  {
    ClipsTemplate *t = (ClipsTemplate *)node->Data();
    if (strcmp(template_name, t->name) == 0)
      temp = t;
    node = node->Next();
  }
  return temp;
}

PrologExpr *PrologDatabase::HashFind(char *functor, long value)
{
  long key = hash_table->MakeKey(functor) + value;

  // The key alone isn't guaranteed to be unique:
  // must supply value too. Let's assume the value of the
  // id is going to be reasonably unique.
  return (PrologExpr *)hash_table->Get(key, value);
}

PrologExpr *PrologDatabase::HashFind(char *functor, char *value)
{
  long key = hash_table->MakeKey(functor) + hash_table->MakeKey(value);
  return (PrologExpr *)hash_table->Get(key, value);
}

void PrologDatabase::ClearDatabase(void)
{
  noErrors = 0;
  wxNode *node = First();
  while (node)
  {
    PrologExpr *expr = (PrologExpr *)node->Data();
    delete expr;
    delete node;
    node = First();
  }
  node = templates.First();
  while (node)
  {
    ClipsTemplate *temp = (ClipsTemplate *)node->Data();
    delete temp;
    delete node;
    node = First();
  }

  if (hash_table)
    hash_table->Clear();
}

Bool PrologDatabase::ReadProlog(char *filename)
{
  noErrors = 0;
  char *actual_file = copystring(filename);
#ifdef wx_msw
  Unix2DosFilename(actual_file);
#endif

  FILE *f = fopen(actual_file, "r");
  if (f)
  {
    hyPrologDatabase = this;

    PROIO_LexFromFile(f);
    PROIO_yyparse();
    fclose(f);
    delete actual_file;
    return (noErrors == 0);
  }
  else
  {
    delete actual_file;
    return FALSE;
  }
}

Bool PrologDatabase::ReadPrologFromString(char *buffer)
{
  noErrors = 0;
  if (buffer)
  {
    hyPrologDatabase = this;

    PROIO_LexFromString(buffer);
    PROIO_yyparse();
    return (noErrors == 0);
  }
  else
  {
    return FALSE;
  }
}


void PrologDatabase::WriteProlog(ostream& stream)
{
  noErrors = 0;
  wxNode *node = First();
  while (node)
  {
    PrologExpr *expr = (PrologExpr *)node->Data();
    expr->WritePrologClause(stream);
    node = node->Next();
  }
}

void PrologDatabase::WriteLisp(ostream& stream)
{
  noErrors = 0;
  wxNode *node = First();
  while (node)
  {
    PrologExpr *expr = (PrologExpr *)node->Data();
    expr->WriteLispExpr(stream);
    stream << "\n\n";
    node = node->Next();
  }
}

// Write deftemplates and then facts
void PrologDatabase::WriteClips(ostream& stream)
{
  noErrors = 0;
  wxNode *node = templates.First();
  while (node)
  {
    ClipsTemplate *temp = (ClipsTemplate *)(node->Data());
    temp->Write(stream);
    node = node->Next();
  }

  node = First();
  while (node)
  {
    PrologExpr *expr = (PrologExpr *)node->Data();
    expr->WriteClipsClause(stream);
    node = node->Next();
  }
}

// Write templates, and filter out unknown slots when writing facts
void PrologDatabase::WriteClipsFiltering(ostream& stream)
{
  noErrors = 0;
  wxNode *node = templates.First();
  while (node)
  {
    ClipsTemplate *temp = (ClipsTemplate *)(node->Data());
    temp->Write(stream);
    node = node->Next();
  }

  node = First();
  while (node)
  {
    PrologExpr *expr = (PrologExpr *)node->Data();
    expr->WriteClipsClause(stream, TRUE, this);
    node = node->Next();
  }
}

void add_expr(PrologExpr * expr)
{
  hyPrologDatabase->Append(expr);
}


// Useful Clips Deftemplate construction stuff for use in conjunction
// with writing out a Clips file

ClipsTemplate::ClipsTemplate(char *the_name)
{
  name = copystring(the_name);
}

ClipsTemplate::~ClipsTemplate(void)
{
  delete name;
  wxNode *node = slots.First();
  while (node)
  {
    ClipsTemplateSlot *slot = (ClipsTemplateSlot *)node->Data();
    delete slot;
    delete node;
    node = slots.First();
  }
}

void ClipsTemplate::AddSlot(char *slot_name, char *def, Bool multi)
{
  ClipsTemplateSlot *slot = new ClipsTemplateSlot(slot_name, def, multi);
  slots.Append(slot);
}

ClipsTemplateSlot *ClipsTemplate::SlotExists(char *slot_name)
{
  wxNode *node = slots.First();
  ClipsTemplateSlot *found = NULL;
  while (node && !found)
  {
    ClipsTemplateSlot *slot = (ClipsTemplateSlot *)(node->Data());
    if (strcmp(slot->slot_name, slot_name) == 0)
      found = slot;
    node = node->Next();
  }
  return found;
}

void ClipsTemplate::Write(ostream& stream)
{
  stream << "(deftemplate " << name << "\n";
  wxNode *node = slots.First();
  while (node)
  {
    ClipsTemplateSlot *slot = (ClipsTemplateSlot *)(node->Data());
    slot->Write(stream);
    node = node->Next();
  }
  stream << ")\n\n";
}

ClipsTemplateSlot::ClipsTemplateSlot(char *sname, char *def, Bool mult)
{
  slot_name = copystring(sname);
  if (def)
    default_name = copystring(def);
  else default_name = NULL;
  multi = mult;
}

ClipsTemplateSlot::~ClipsTemplateSlot(void)
{
  delete slot_name;
  if (default_name)
    delete default_name;
}

void ClipsTemplateSlot::Write(ostream& stream)
{
  stream << "  (";
  if (multi)
    stream << "multi-";

  stream << "field " << slot_name;
  if (default_name)
    stream << "\n   (default " << default_name << ")";
  stream << ")\n";
}

// Procedure call helpers
PrologExpr *wxMakeCall(char *functor ...)
{
  va_list ap;
  PrologExpr *clause = new PrologExpr(functor);

  va_start(ap, functor);

  for (;;)
  {
    PrologExpr *object = va_arg(ap, PrologExpr *);
    if (((int)object) == 0)
      break;
    else
      clause->Append(object);
  }
  va_end(ap);
  return clause;
}

char *wxCheckTypes(PrologExpr *expr ...)
{
  char buf[200];
  va_list ap;

  va_start(ap, expr);

  if (expr && (expr->Type() == PrologList) && expr->value.first)
  {
    PrologExpr *argument = expr->value.first->next;
    while (argument)
    {
      PrologType object = va_arg(ap, PrologType);

      if (((int)object) == 0)
      {
        va_end(ap);
        return copystring("Too many arguments");
      }

      if (argument->Type() != object)
      {
        char *found_type = "unknown";
        char *expected_type = "unknown";
        switch (argument->Type())
	{
	  case PrologString:
            found_type = "string";
            break;
	  case PrologWord:
            found_type = "word";
            break;
	  case PrologInteger:
            found_type = "integer";
            break;
	  case PrologReal:
            found_type = "real";
            break;
	  case PrologList:
            found_type = "list";
            break;
          case PrologNull: break;
	}

        switch (object)
	{
	  case PrologString:
            expected_type = "string";
            break;
	  case PrologWord:
            expected_type = "word";
            break;
	  case PrologInteger:
            expected_type = "integer";
            break;
	  case PrologReal:
            expected_type = "real";
            break;
	  case PrologList:
            expected_type = "list";
            break;
          case PrologNull: break;
	}
        sprintf(buf, "%s found where %s expected", found_type, expected_type);

        va_end(ap);
        return copystring(buf);
      }

      argument = argument->next;
    }
  } else
  { va_end(ap); return copystring("Ill-formed procedure call"); }
  va_end(ap);
  return NULL;
}

char *wxCheckClauseTypes(PrologExpr *expr, wxList *type_list)
{
  char buf[200];

  if (expr && (expr->Type() == PrologList) && expr->value.first)
  {
    PrologExpr *argument = expr->value.first;
    wxNode *type_node = type_list->First();;
    while (argument)
    {
      if (!type_node)
      {
        return copystring("Too many arguments");
      }

      int object_int = (int)(type_node->Data());
      PrologType object = (PrologType)object_int;

      if (argument->Type() != object)
      {
        char *found_type = "unknown";
        char *expected_type = "unknown";
        switch (argument->Type())
	{
	  case PrologString:
            found_type = "string";
            break;
	  case PrologWord:
            found_type = "word";
            break;
	  case PrologInteger:
            found_type = "integer";
            break;
	  case PrologReal:
            found_type = "real";
            break;
	  case PrologList:
            found_type = "list";
            break;
          case PrologNull: break;
	}

        switch (object)
	{
	  case PrologString:
            expected_type = "string";
            break;
	  case PrologWord:
            expected_type = "word";
            break;
	  case PrologInteger:
            expected_type = "integer";
            break;
	  case PrologReal:
            expected_type = "real";
            break;
	  case PrologList:
            expected_type = "list";
            break;
          case PrologNull: break;
	}
        sprintf(buf, "%s found where %s expected", found_type, expected_type);

        return copystring(buf);
      }

      argument = argument->next;
      type_node = type_node->Next();
    }
    if (type_node)
    {
      return copystring("Too few arguments");
    }
  } else
  { return copystring("Ill-formed procedure call"); }
  return NULL;
}

// Checks functor
Bool wxIsFunctor(PrologExpr *expr, char *functor)
{
  if (expr && (expr->Type() == PrologList))
  {
    PrologExpr *first_expr = expr->value.first;

    if (first_expr && (first_expr->Type() == PrologWord) &&
       (strcmp(first_expr->WordValue(), functor) == 0))
      return TRUE;
    else 
      return FALSE;
  } 
  else 
    return FALSE;
}

#ifdef wx_x
void DebugPrintClause(PrologExpr *expr)
{
  expr->WritePrologClause(cout);
}
void DebugPrintExpr(PrologExpr *expr)
{
  expr->WritePrologExpr(cout);
}
#endif

/*
 * Called from parser
 *
 */

char *make_integer(char *str)
{
  PrologExpr *x = new PrologExpr(atol(str));

  return (char *)x;
}

char *make_real(char *str1, char *str2)
{
  char buf[50];

  sprintf(buf, "%s.%s", str1, str2);
  float f = (float)atof(buf);
  PrologExpr *x = new PrologExpr(f);

  return (char *)x;
}

// extern "C" double exp10(double);

char *make_exp(char *str1, char *str2)
{
  double mantissa = (double)atoi(str1);
  double exponent = (double)atoi(str2);

  double d = mantissa * pow(10.0, exponent);

  PrologExpr *x = new PrologExpr((float)d);

  return (char *)x;
}

char *make_exp2(char *str1, char *str2, char *str3)
{
  char buf[50];

  sprintf(buf, "%s.%s", str1, str2);
  double mantissa = (double)atof(buf);
  double exponent = (double)atoi(str3);

  double d = mantissa * pow(10.0, exponent);

  PrologExpr *x = new PrologExpr((float)d);

  return (char *)x;
}

char *make_word(char *str)
{
  PrologExpr *x = new PrologExpr(PrologWord, str);
  return (char *)x;
}

char *make_string(char *str)
{
  char *s, *t;
  int len, i;

  str++;			/* skip leading quote */
  len = strlen(str) - 1;	/* ignore trailing quote */
    
  s = new char[len + 1];
    
  t = s;
  for(i=0; i<len; i++)
  {
    if (str[i] == '\\' && str[i+1] == '"')
    {
      *t++ = '"';
      i ++;
    }
    else if (str[i] == '\\' && str[i+1] == '\\')
    {
      *t++ = '\\';
      i ++;
    }
    else
      *t++ = str[i];
  }

  *t = '\0';

  PrologExpr *x = new PrologExpr(PrologString, s, FALSE);
  return (char *)x;
}

char *proio_cons(char * ccar, char * ccdr)
{
  PrologExpr *car = (PrologExpr *)ccar;
  PrologExpr *cdr = (PrologExpr *)ccdr;

  if (cdr == NULL)
  {
    cdr = new PrologExpr(PrologList);
  }
  if (car)
    cdr->Insert(car);
  return (char *)cdr;
}

void process_command(char * cexpr)
{
  PrologExpr *expr = (PrologExpr *)cexpr;
  add_expr(expr);
}

void syntax_error(char *s)
{
  wxDebugMsg("YACC: %s", s);
  if (currentProioErrorHandler)
    (void)(*(currentProioErrorHandler))(PROIO_ERROR_SYNTAX, "syntax error");
  if (hyPrologDatabase) hyPrologDatabase->noErrors += 1;
}

#ifdef _WINDLL
char *__cdecl strdup(const char *s)
{
  int len = strlen(s);
  char *new_s = (char *)malloc(sizeof(char)*(len+1));
  strcpy(new_s, s);
  return new_s;
}
#endif
