/*								-*- C++ -*-
 * File:		wx_hash.h
 * Purpose:	Basic hash table implementation
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_hashh
#define wxb_hashh

#ifdef __GNUG__
#pragma interface
#endif

#ifndef wx_xt
#   include "wx_obj.h"
#   include "wx_list.h"
#endif

/*
 * A hash table is an array of user-definable size with lists
 * of data items hanging off the array positions.  Usually there'll
 * be a hit, so no search is required; otherwise we'll have to run down
 * the list to find the desired item.
*/

#ifdef IN_CPROTO
typedef       void    *wxHashTable ;
#else

class wxHashTable: public wxObject
{

 public:
  int n;
  int current_position;
  wxNode *current_node;

  wxList **hash_table;

  wxHashTable(int the_key_type = 0, int size = 1000);
  ~wxHashTable(void);

  // Note that there are 2 forms of Put, Get.
  // With a key and a value, the *value* will be checked
  // when a collision is detected. Otherwise, if there are
  // 2 items with a different value but the same key,
  // we'll retrieve the WRONG ONE. So where possible,
  // supply the required value along with the key.
  // In fact, the value-only versions make a key, and still store
  // the value. The use of an explicit key might be required
  // e.g. when combining several values into one key.
  // When doing that, it's highly likely we'll get a collision,
  // e.g. 1 + 2 = 3, 2 + 1 = 3.

  // key and value are NOT necessarily the same
  void Put(long key, long value, wxObject *object);
  void Put(long key, char *value, wxObject *object);

  // key and value are the same
  void Put(long value, wxObject *object);
  void Put(const char *value, wxObject *object);

  // key and value not the same
  wxObject *Get(long key, long value);
  wxObject *Get(long key, char *value);

  // key and value are the same
  wxObject *Get(long value);
  wxObject *Get(const char *value);

  // Deletes entry and returns data if found
  wxObject *Delete(long key);
  wxObject *Delete(const char *key);

  wxObject *Delete(long key, int value);
  wxObject *Delete(long key, char *value);

  // Construct your own integer key from a string, e.g. in case
  // you need to combine it with something
  long MakeKey(const char *string);

  // Way of iterating through whole hash table (e.g. to delete everything)
  // Not necessary, of course, if you're only storing pointers to
  // objects maintained separately

  void BeginFind(void);
  wxNode *Next(void);

  void DeleteContents(Bool flag);
  void Clear(void);

};

/* Special hash table implementation for widgets. */
class wxNonlockingHashTable
{
  struct Bucket *buckets;
  long numbuckets, numwidgets;
 public:
  wxNonlockingHashTable(void);
  ~wxNonlockingHashTable();
  void Put(long widget, wxObject *object);
  wxObject *Get(long widget);
  void Delete(long widget);
  void DeleteObject(wxObject *object);
  inline void Append(long w, wxObject *o) { Put(w, o); }
  inline wxObject *Find(long w) { return Get(w); }
};

#endif // IN_CPROTO
#endif // wxb_hashh
