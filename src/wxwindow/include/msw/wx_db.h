/*
 * File:	wx_db.h
 * Purpose:	wxWindows database connectivity classes
 * Author:	Julian Smart
 *              Olaf Klein (oklein@smallo.ruhr.de)
 *              Patrick Halke (patrick@zaphod.ruhr.de)
 * Created:	1995
 * Updated:
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#include "wx_setup.h"

#if USE_ODBC

#ifndef wx_dbh
#define wx_dbh

#include "common.h"
#include "sqlext.h"
#include "wx_list.h"

typedef RETCODE wxRETCODE;

// Recordset open types
#define wxOPEN_TYPE_DYNASET         1
#define wxOPEN_TYPE_SNAPSHOT        2
#define wxOPEN_TYPE_FORWARD_ONLY    3

// Recordset open options
#define wxOPTION_DEFAULT            1
#define wxOPTION_APPEND_ONLY        2
#define wxOPTION_READ_ONLY          3

// Data types
class wxRecordSet;

class wxDatabase: public wxObject
{
  // JACS
  DECLARE_DYNAMIC_CLASS(wxDatabase)
 private:
 protected:
  static HENV hEnv;
  static int refCount;
  
  HDBC hDBC;
  char* username;
  char* password;
  char* datasource;
  char* dbname;
  char* connectstring;
  Bool isOpen;
  
  // error-handling variables
  wxRETCODE retcode;
  char sqlstate[SQL_SQLSTATE_SIZE+1];  // error class and subclass
  char errmsg[SQL_MAX_MESSAGE_LENGTH]; // error message
  long nat_err;                        // error number by ODBC driver
  Bool err_occured;

  wxList recordSets; // Record sets: Added by JACS

 public:
  wxDatabase(void);
  ~wxDatabase(void);
  
  Bool Open(char *, Bool exclusive =FALSE, Bool readOnly =TRUE, char *username ="ODBC", char *password ="");
  Bool Close(void);

  // Cleanup operations, added by JACS
  void DeleteRecordSets(void); // Called when the database is deleted
  void ResetRecordSets(void); // Required if the database is closed
  inline wxList& GetRecordSets(void) { return recordSets; }
  
  inline char *GetUsername(void) { return username; }
  inline char *GetPassword(void) { return password; }
  inline char *GetDataSource(void) { return datasource; }
  inline Bool IsOpen(void) { return isOpen; }
  inline wxRETCODE GetErrorCode(void) { return retcode; }
  inline HDBC GetHDBC(void) { return hDBC; }
  inline HENV GetHENV(void) { return hEnv; }
  
  void SetPassword(char *s);
  void SetUsername(char *s);
  void SetDataSource(char *s);
  
  // Database attributes
  char *GetDatabaseName(void);
  Bool CanUpdate(void);
  Bool CanTransact(void);
  Bool InWaitForDataSource(void);
  void SetLoginTimeout(long seconds);
  void SetQueryTimeout(long seconds);
  void SetSynchronousMode(Bool synchronous);
  
  // Database operations
  Bool BeginTrans(void);
  Bool CommitTrans(void);
  Bool RollbackTrans(void);
  void Cancel(void);

  // Error handling
  Bool ErrorOccured(void);
  char* GetErrorMessage(void);
  long  GetErrorNumber(void);
  char* GetErrorClass(void);
  inline void ErrorSnapshot(HSTMT =SQL_NULL_HSTMT);

  // Overridables
  virtual void OnSetOptions(wxRecordSet *recordSet);
  virtual void OnWaitForDataSource(Bool stillExecuting);
};

// Represents a data row
class wxQueryField: public wxObject
{
  // JACS
  DECLARE_DYNAMIC_CLASS(wxQueryField)
 private:
  void *data;
  short type;
  long size;
  Bool dirty;

  Bool AllocData(void);
  
  public:
  wxQueryField(void);
  ~wxQueryField(void);
  
  Bool SetData(void*, long);
  void SetDirty(Bool =TRUE);
  void ClearData(void);
  void SetType(short);
  void SetSize(long);
  
  void* GetData(void);
  short GetType(void);
  long GetSize(void);
  
  Bool IsDirty(void);
};

// Represents a column description
class wxQueryCol: public wxObject
{
  // JACS
  DECLARE_DYNAMIC_CLASS(wxQueryCol)
 private:
  short type;
  char *name;
  Bool nullable;
  long varsize;
  void* var;
  
  public:
  wxList fields;
  
  wxQueryCol(void);
  ~wxQueryCol(void);
  
  void* BindVar(void*, long);
  void FillVar(int);
  void AppendField(void*, long);
  Bool SetData(int, void*, long);
  void SetName(char*);
  void SetNullable(Bool);
  void SetFieldDirty(int, Bool =TRUE);
  void SetType(short);
  
  char* GetName(void);
  short GetType(void);
  Bool IsNullable(void);
  void* GetData(int);
  long GetSize(int);

  Bool IsFieldDirty(int);
};

class wxRecordSet: public wxObject
{
  // JACS
  DECLARE_DYNAMIC_CLASS(wxRecordSet)
 private:
  int cursor;
  int type;
  int options;
  
  protected:
  HSTMT hStmt;
  int nFields;
  int nParams;
  int nRecords;
  short nCols;
  char *recordFilter;
  char *sortString;
  char *defaultSQL;
  char* tablename;
  wxDatabase *parentdb;
  wxRETCODE retcode;
  wxList cols;
  wxList fetchbuf;
  
  void FillVars(int);

  public:
  // JACS gave parent a default value for benefit of IMPLEMENT_DYNAMIC_CLASS
  wxRecordSet(wxDatabase *parent = NULL, int =wxOPEN_TYPE_DYNASET, int =wxOPTION_DEFAULT);
  ~wxRecordSet(void);
  
  // My own, lower-level functions.
  Bool BeginQuery(int openType, char *sql = NULL, int options = wxOPTION_DEFAULT);
  Bool EndQuery(void);
  Bool Query(char* columns, char* table =NULL, char *filter =NULL);
  
  // Attributes
  inline int GetNumberFields(void) { return nFields; }
  inline int GetNumberParams(void) { return nParams; }
  long GetNumberRecords(void);
  long GetNumberCols(void);
  inline char *GetFilter(void) { return recordFilter; }
  inline char *GetSortString(void) { return sortString; }
  inline wxDatabase *GetDatabase(void) { return parentdb; }
  inline wxRETCODE GetErrorCode(void) { return retcode; }
  Bool CanAppend(void);
  Bool CanRestart(void);
  Bool CanScroll(void);
  Bool CanTransact(void);
  Bool CanUpdate(void);
  long GetCurrentRecord(void);
  Bool RecordCountFinal(void);
  Bool GetResultSet(void);
  Bool ExecuteSQL(char*);
  Bool GetTables(void);
  Bool GetColumns(char* =NULL);
  char *GetTableName(void);
  void SetTableName(char*);
  char *GetSQL(void);
  Bool IsOpen(void);
  Bool IsBOF(void);
  Bool IsEOF(void);
  Bool IsDeleted(void);
  
  Bool GetFieldData(int colPos, int dataType, void *dataPtr);
  Bool GetFieldData(const char*, int dataType, void *dataPtr);
  void* GetFieldDataPtr(int, int);
  void* GetFieldDataPtr(const char*, int);
  char* GetColName(int);
  short GetColType(int);
  short GetColType(const char*);
  void* BindVar(int, void*, long);
  void* BindVar(const char*, void*, long);

  void SetType(int);
  int GetType(void);
  void SetOptions(int);
  int GetOptions(void);
    
  // Update operations
  void AddNew(void);
  Bool Delete(void);
  void Edit(void);
  Bool Update(void);
  
  // Record navigation
  virtual Bool Move(long rows);
  virtual Bool MoveFirst(void);
  virtual Bool MoveLast(void);
  virtual Bool MoveNext(void);
  virtual Bool MovePrev(void);
  virtual Bool GoTo(long);
  
  // Others
  Bool GetDataSources(void);
  
  // Associate a column name/position with a data location
  //   Bool BindColumn(int colPos, int dataType, void *dataPtr);
  
  void Cancel(void);
  Bool IsFieldDirty(int);
  Bool IsFieldDirty(const char*);
  Bool IsFieldNull(int);
  Bool IsFieldNull(const char*);
  Bool IsColNullable(int);
  Bool IsColNullable(const char*);
  virtual Bool Requery(void);
  virtual void SetFieldDirty(int, Bool dirty = TRUE);
  virtual void SetFieldDirty(const char*, Bool dirty = TRUE);
  void SetFieldNull(void *p, Bool isNull = TRUE);
  
  // Overridables
  virtual char *GetDefaultConnect(void);
  virtual char *GetDefaultSQL(void);
  
  // Internal
  
  // Build SQL query from column specification
  Bool ConstructDefaultSQL(void);
  void SetDefaultSQL(char *s);
  Bool ReleaseHandle(void); // Added JACS
};

#endif

#endif  // USE_ODBC
