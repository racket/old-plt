/*
 * File:	wx_db.cc
 * Purpose:	ODBC database implementation
 * Author:	Julian Smart
 *              Olaf Klein (oklein@smallo.ruhr.de)
 *              Patrick Halke (patrick@zaphod.ruhr.de)
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else


#include "common.h"
#endif

#if USE_ODBC
#include "wxstring.h"
#include "wx_utils.h"
#include "wx_dialg.h"

#include "wx_db.h"

#ifndef WIN32
#include <print.h>
#endif

HENV wxDatabase::hEnv = 0;
int wxDatabase::refCount = 0;

IMPLEMENT_DYNAMIC_CLASS(wxDatabase, wxObject)
IMPLEMENT_DYNAMIC_CLASS(wxQueryCol, wxObject)
IMPLEMENT_DYNAMIC_CLASS(wxQueryField, wxObject)
IMPLEMENT_DYNAMIC_CLASS(wxRecordSet, wxObject)

wxDatabase::wxDatabase(void)
{
  hDBC = 0;
  username = NULL;
  password = NULL;
  datasource = NULL;
  dbname = NULL;
  connectstring = NULL;
  isOpen = FALSE;
  refCount ++;
  retcode = 0;
  username = NULL;
  password = NULL;
  err_occured = FALSE;

  memset(sqlstate, 0, sizeof sqlstate);
  memset(errmsg, 0, sizeof errmsg);
  
  if (hEnv == 0)
  {
    retcode = SQLAllocEnv(&hEnv);

    if (retcode != SQL_SUCCESS)
      hEnv = 0;
  }
}

wxDatabase::~wxDatabase(void)
{
  Close();
  DeleteRecordSets(); // Added JACS

  if (connectstring)
    delete[] connectstring;
  if (datasource)
    delete[] datasource;
  if (username)
    delete[] username;
  if (password)
    delete[] password;
  if (dbname)
    delete[] dbname;
  
  refCount --;
  if (!refCount && hEnv)
  {
    retcode = SQLFreeEnv(hEnv);
    hEnv = 0; // JACS 17/6
  }
}

void wxDatabase::ErrorSnapshot(HSTMT hstmt)
{
  SWORD len = 0; // JACS: sometimes doesn't get filled in by SQLError.
  
  err_occured = TRUE;
  SQLError(hEnv, hDBC, hstmt, (unsigned char *)sqlstate, &nat_err, (unsigned char *)errmsg, SQL_MAX_MESSAGE_LENGTH-1, &len);
  errmsg[len] = '\0';
}

Bool wxDatabase::ErrorOccured(void) 
{
  return err_occured;
}

char* wxDatabase::GetErrorMessage(void)
{
  return errmsg;
}

long wxDatabase::GetErrorNumber(void) 
{
  return nat_err;
}

char* wxDatabase::GetErrorClass(void) {
  return sqlstate;
}

Bool wxDatabase::Open(char *thedatasource, Bool exclusive, Bool readOnly, char *username, char *password)
{
  err_occured = FALSE;
  
  if (isOpen)
    return FALSE;
  
  SetUsername(username);
  SetPassword(password);
  SetDataSource(thedatasource);
  
  if (!hEnv)
    return FALSE;

  retcode = SQLAllocConnect(hEnv, &hDBC);
  if (retcode != SQL_SUCCESS) {
    hDBC = 0;
    return FALSE;
  }
  
  retcode = SQLConnect(hDBC, (UCHAR FAR*)thedatasource, strlen(thedatasource), (UCHAR FAR*)username, strlen(username),
		       (UCHAR FAR*)password, strlen(password));
  
  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
    ErrorSnapshot();
    return FALSE;
  }

  isOpen = TRUE;
  
  return TRUE;
}

Bool wxDatabase::Close(void)
{
  // JACS: make sure the record set statements are all released.
  ResetRecordSets();
  
  err_occured = FALSE;
  if (hDBC != 0)
  {
    retcode = SQLDisconnect(hDBC);

    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
      ErrorSnapshot();      
      return FALSE;
    }

    retcode = SQLFreeConnect(hDBC);

    hDBC = 0;
    isOpen = FALSE;
    
    return TRUE;
  }
  
  return FALSE;
}

// Database attributes
char *wxDatabase::GetDatabaseName(void)
{
  err_occured = FALSE;
  if (hDBC == 0)
    return NULL;
    
  char nameBuf[400];
  int nameSize = 0;

  retcode = SQLGetInfo(hDBC, SQL_DATABASE_NAME, nameBuf, sizeof(nameBuf), (short *)&nameSize);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
    return NULL;

  delete[] dbname;
  dbname = NULL;
  
  if (nameSize > 0)
  {
    dbname = copystring(nameBuf);
    return dbname;
  }
  else
    return NULL;
}
  
Bool wxDatabase::CanUpdate(void)
{
  return FALSE;
}

Bool wxDatabase::CanTransact(void)
{
  return FALSE;
}

Bool wxDatabase::InWaitForDataSource(void)
{
  return FALSE;
}

void wxDatabase::SetLoginTimeout(long seconds)
{
}

void wxDatabase::SetQueryTimeout(long seconds)
{
}

void wxDatabase::SetSynchronousMode(Bool synchronous)
{
}

// Database operations
Bool wxDatabase::BeginTrans(void)
{
  return FALSE;
}

Bool wxDatabase::CommitTrans(void)
{
  return FALSE;
}

Bool wxDatabase::RollbackTrans(void)
{
  return FALSE;
}

void wxDatabase::Cancel(void)
{
}

// Overridables
void wxDatabase::OnSetOptions(wxRecordSet *recordSet)
{
}

void wxDatabase::OnWaitForDataSource(Bool stillExecuting)
{
}

void wxDatabase::SetPassword(char *s)
{
  if (password)
    delete[] password;
  if (s)
  {
    password = copystring(s);
  }
  else
    password = NULL;
}

void wxDatabase::SetUsername(char *s)
{
  delete[] username;
  
  if (s)
    username = copystring(s);
  else
    username = NULL;
}

void wxDatabase::SetDataSource(char *s)
{
  delete[] datasource;
  
  if (s)
    datasource = copystring(s);
  else
    datasource = NULL;
}

/*
 * Added by JACS
 */

void wxDatabase::DeleteRecordSets(void)
{
  wxNode *node = recordSets.First();
  while (node)
  {
    wxNode *next = node->Next();
    wxRecordSet *rec = (wxRecordSet *)node->Data();
    delete rec;
    // The node is implicitly deleted by ~wxRecordSet
    node = next;
  }
}

void wxDatabase::ResetRecordSets(void)
{
  wxNode *node = recordSets.First();
  while (node)
  {
    wxRecordSet *rec = (wxRecordSet *)node->Data();
    rec->ReleaseHandle();

    node = node->Next();
  }
}

/*
 * wxRecordSet
 */
 
wxRecordSet::wxRecordSet(wxDatabase *db, int typ, int opt)
{
  parentdb = db;
  hStmt = 0;
  nFields = 0;
  nParams = 0;
  recordFilter = NULL;
  sortString = NULL;
  retcode = 0;
  cursor = 0;
  tablename = NULL;
  nCols = 0;
  nRecords = 0;
  
  type = typ;
  options = opt;

  // Added JACS
  if (parentdb)
    parentdb->GetRecordSets().Append(this);
}

wxRecordSet::~wxRecordSet(void)
{
  ReleaseHandle();
  
  // JACS
  if (parentdb)
    parentdb->GetRecordSets().DeleteObject(this);
    
  if (recordFilter)
    delete[] recordFilter;
  if (sortString)
    delete[] sortString;
  if (tablename)
    delete[] tablename;
}

// If SQL is non-NULL, table and columns can be NULL.
Bool wxRecordSet::BeginQuery(int openType, char *sql, int options)
{
  // Needs to construct an appropriate SQL statement. By default
  // (i.e. if table and columns are provided) then
  // SELECT <columns> FROM <table>
  // will be used.
  if (!parentdb)
    return FALSE;
  if (!parentdb->GetHDBC())
    return FALSE;
    
  if (hStmt)
  {
    retcode = SQLFreeStmt(hStmt, SQL_DROP);
    if (retcode == SQL_ERROR)
    {
      parentdb->ErrorSnapshot(hStmt);
      return FALSE;
    }
    hStmt = 0;
  }

  retcode = SQLAllocStmt(parentdb->GetHDBC(), &hStmt);
  if (retcode != SQL_SUCCESS)
    return FALSE;

  return TRUE;
}

Bool wxRecordSet::Query(char *columns, char *table, char *filter)
{
  // Needs to construct an appropriate SQL statement. By default
  // (i.e. if table and columns are provided) then
  // SELECT <columns> FROM <table>
  // will be used.

  char* thetable = table ? table : tablename;
  
  if (!thetable)
    return FALSE;
  if (!columns)
    return FALSE;
    
  wxString query;

  query += "SELECT ";
  query += columns;
  query += " FROM ";
  query += thetable;

  if (filter)  {
    query += " WHERE ";
    query += filter;
  }
  retcode = SQLPrepare(hStmt, (UCHAR FAR *)query.GetData(), strlen(query.GetData()));
  if (retcode != SQL_SUCCESS) {
    parentdb->ErrorSnapshot(hStmt);
    return FALSE;
  }

  retcode = SQLExecute(hStmt);

  if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
    parentdb->ErrorSnapshot(hStmt);
    return FALSE;
  }

  return TRUE;
}

Bool wxRecordSet::EndQuery(void)
{
  return TRUE;
}

void wxRecordSet::FillVars(int recnum) {
  wxNode* node = cols.First();
  
  do {
    ((wxQueryCol*)node->Data())->FillVar(recnum);
  } while (node = node->Next());
}

Bool wxRecordSet::GetResultSet(void)
{
//  long trash = SQL_NULL_DATA; // value added by JACS
  long trash;
               // contains the number of bytes transferred by SQLFetch()
               // who needs this ?
  wxNode *currow, *fetch, *curcol;
  
  retcode = SQLNumResultCols(hStmt, &nCols);

  if (!nCols)
    return TRUE;
  
  // delete old data first
  cols.DeleteContents(TRUE);
  cols.Clear();
  fetchbuf.DeleteContents(TRUE);
  fetchbuf.Clear();
  
  nRecords = 0;
  cursor = 0;

  int i;
  for (i=0; i<nCols; i++) {
    char name[512];
    short type, scale, nullable, namelen;
    unsigned long len;
    
    retcode = SQLDescribeCol(hStmt, i+1, (unsigned char *)name, 511, &namelen, &type, &len, &scale, &nullable);
    if (SQL_SUCCESS != retcode && SQL_SUCCESS_WITH_INFO != retcode)  {
      parentdb->ErrorSnapshot(hStmt);
      return FALSE;
    }

    wxQueryCol *col1 = new wxQueryCol;
    curcol = cols.Append(name, col1);
    col1->SetName(name);
    col1->SetType(type);
    col1->SetNullable(nullable);

    wxQueryField *field1 = new wxQueryField;
    fetch = fetchbuf.Append(field1);
    field1->SetType(type);
    field1->SetSize(len);
    
    SQLBindCol(hStmt, i+1, SQL_C_BINARY, field1->GetData(), field1->GetSize(), &trash);
  }
  
  switch (type) {
    case wxOPEN_TYPE_SNAPSHOT:
    // get it all !
    // After we've done an SQLFetch, copy the data in the fetch buffer into
    // new fields, for each column.
    while (SQL_SUCCESS == (retcode = SQLFetch(hStmt)) || SQL_SUCCESS_WITH_INFO == retcode) {
      nRecords++;
      
      curcol = cols.First();
      fetch = fetchbuf.First();
      for (i=0; i<nCols; i++) {

        wxQueryField *fetchField = (wxQueryField *)fetch->Data();
        wxQueryCol *col = (wxQueryCol *)curcol->Data();
        wxQueryField *field = new wxQueryField;

	currow = col->fields.Append(field);
	
	field->SetType(fetchField->GetType());
	field->SetData(fetchField->GetData(), fetchField->GetSize());
        curcol = curcol->Next();
        fetchField->ClearData(); // Runs ok if this commented out and SetData commented out
	fetch = fetch->Next();
      }
    }
    // while loop should only be leaved, when no more data was found;
    // otherwise it seems, that there was an error
    if (SQL_NO_DATA_FOUND != retcode) {
      parentdb->ErrorSnapshot(hStmt);
      return FALSE;
    }
    break;
    case wxOPEN_TYPE_DYNASET:
    // get first record only
    if (SQL_SUCCESS == (retcode = SQLFetch(hStmt)) || retcode == SQL_SUCCESS_WITH_INFO) {
      nRecords = 1;  // TO DO! # of records in the ODBC result set should be set here.
      
      curcol = cols.First();
      fetch = fetchbuf.First();
      for (i=0; i<nCols; i++) {
	currow = ((wxQueryCol*)curcol->Data())->fields.Append(new wxQueryField);
	
	((wxQueryField*)currow->Data())->SetType(((wxQueryField*)fetch->Data())->GetType());
	((wxQueryField*)currow->Data())->SetData(((wxQueryField*)fetch->Data())->GetData(), ((wxQueryField*)fetch->Data())->GetSize());
        curcol = curcol->Next();
        ((wxQueryField*)fetch->Data())->ClearData();
	fetch = fetch->Next();
      }
    }
    if (SQL_NO_DATA_FOUND != retcode) {
      parentdb->ErrorSnapshot(hStmt);
      return FALSE;
    }
    break;
    default:
    return FALSE;
  }

  FillVars(0);
  
  return TRUE;
}

Bool wxRecordSet::ExecuteSQL(char *sql)
{
  if (hStmt)
  {
    retcode = SQLFreeStmt(hStmt, SQL_DROP);
    if (retcode == SQL_ERROR)
    {
      parentdb->ErrorSnapshot(hStmt);
      return FALSE;
    }
    hStmt = NULL;
  }
  
  retcode = SQLAllocStmt(parentdb->GetHDBC(), &hStmt);
  
  if (SQL_SUCCESS != retcode) {
    parentdb->ErrorSnapshot(hStmt);
    return FALSE;
  }

  retcode = SQLExecDirect(hStmt, (UCHAR FAR*)sql, SQL_NTS);
  
  if (SQL_SUCCESS != retcode && SQL_SUCCESS_WITH_INFO != retcode)  {
    parentdb->ErrorSnapshot(hStmt);
    return FALSE;
  }

  return GetResultSet();
}

Bool wxRecordSet::GetDataSources(void) {
  
  char *dsname = "Name", *dsdesc = "Description";
  char namebuf[64];
  char descbuf[512];
  short namelen, desclen;
  
  cursor = 0;
  
  // delete old data first
  cols.DeleteContents(TRUE);
  cols.Clear();
  nRecords = 0;

  // JACS This is a snapshot, not a dynaset.
  type = wxOPEN_TYPE_SNAPSHOT;
  
  wxNode *namecol, *desccol;
  
  namecol = cols.Append(dsname, new wxQueryCol);
  ((wxQueryCol*)namecol->Data())->SetName(dsname);
  ((wxQueryCol*)namecol->Data())->SetType(SQL_CHAR);
  desccol = cols.Append(dsdesc, new wxQueryCol);
  ((wxQueryCol*)desccol->Data())->SetName(dsdesc);
  ((wxQueryCol*)desccol->Data())->SetType(SQL_CHAR);
  
  retcode = SQLDataSources(parentdb->GetHENV(), SQL_FETCH_FIRST, (unsigned char *)namebuf, 63, &namelen, (unsigned char *)descbuf, 511, &desclen);
  while (SQL_SUCCESS == retcode || SQL_SUCCESS_WITH_INFO == retcode) {
    nRecords++;
    ((wxQueryCol*)namecol->Data())->AppendField(namebuf, namelen);
    ((wxQueryCol*)desccol->Data())->AppendField(descbuf, desclen);
    retcode = SQLDataSources(parentdb->GetHENV(), SQL_FETCH_NEXT, (unsigned char *)namebuf, 63, &namelen, (unsigned char *)descbuf, 511, &desclen);
  }

  if (SQL_SUCCESS != retcode && SQL_SUCCESS_WITH_INFO != retcode && SQL_NO_DATA_FOUND != retcode)  {
    parentdb->ErrorSnapshot();
    return FALSE;
  }

  cursor = 0;
  
  return TRUE;
}

// Attributes
void wxRecordSet::SetTableName(char* name) {
  delete[] tablename;
  tablename = NULL;
  
  if (name)
    tablename = copystring(name);
}

Bool wxRecordSet::GetTables(void)
{
  if (hStmt)
  {
    retcode = SQLFreeStmt(hStmt, SQL_DROP);
    if (retcode == SQL_ERROR)
    {
      parentdb->ErrorSnapshot(hStmt);
      return FALSE;
    }
      
    hStmt = NULL;
  }
  
  retcode = SQLAllocStmt(parentdb->GetHDBC(), &hStmt);
  
  if (SQL_SUCCESS != retcode) {
    parentdb->ErrorSnapshot();
    return FALSE;
  }

  retcode = SQLTables(hStmt, NULL, 0, NULL, 0, NULL, 0, NULL, 0);
  
  if (SQL_SUCCESS != retcode && SQL_SUCCESS_WITH_INFO != retcode)  {
    parentdb->ErrorSnapshot(hStmt);
    return FALSE;
  }

  return GetResultSet();
}

Bool wxRecordSet::GetColumns(char* table)
{
  char* name=NULL;
  char* wildcard = "%";

  name = table ? table : tablename;
  
  if (!name)
    return FALSE;

  if (hStmt)
  {
    retcode = SQLFreeStmt(hStmt, SQL_DROP);
    if (retcode == SQL_ERROR)
    {
      parentdb->ErrorSnapshot(hStmt);
      return FALSE;
    }
    hStmt = NULL;
  }
  
  retcode = SQLAllocStmt(parentdb->GetHDBC(), &hStmt);
  
  if (SQL_SUCCESS != retcode) {
    parentdb->ErrorSnapshot();
    return FALSE;
  }

  //retcode = SQLColumns(hstmt, (unsigned char*)parentdb->GetDataSource(), strlen(parentdb->GetDataSource()), wildcard, 1, name, strlen(name), wildcard, 1);
  retcode = SQLColumns(hStmt, NULL, 0, NULL, 0, (unsigned char *)name, strlen(name), NULL, 0);
  
  if (SQL_SUCCESS != retcode && SQL_SUCCESS_WITH_INFO != retcode)  {
    parentdb->ErrorSnapshot(hStmt);
    return FALSE;
  }

  return GetResultSet();
}

long wxRecordSet::GetNumberRecords(void) 
{
  return nRecords;
}

long wxRecordSet::GetNumberCols(void)
{
  return nCols;
}

char* wxRecordSet::GetColName(int col)
{
  wxNode* node = cols.Nth(col);
    
  if (!node)
    return NULL;
    
  return ((wxQueryCol*)node->Data())->GetName();
}

short wxRecordSet::GetColType(int col)
{
  wxNode* node = cols.Nth(col);
    
  if (!node)
    return SQL_TYPE_NULL;
    
  return ((wxQueryCol*)node->Data())->GetType();
}

short wxRecordSet::GetColType(const char *col)
{
  wxNode* node = cols.Find(col);
    
  if (!node)
    return SQL_TYPE_NULL;
    
  return ((wxQueryCol*)node->Data())->GetType();
}

Bool wxRecordSet::GetFieldData(int col, int type, void* data)
{
  wxNode* node = cols.Nth(col);

  if (!node)
    return FALSE;
  
  if (((wxQueryCol*)node->Data())->GetType() != type)
    return FALSE;

  void* src = ((wxQueryCol*)node->Data())->GetData(cursor);

  if (!src)
    return FALSE;
  
  memcpy(data, src, ((wxQueryCol*)node->Data())->GetSize(cursor));
  
  return TRUE;
}

Bool wxRecordSet::GetFieldData(const char* name, int type, void *data)
{
  wxNode* node = cols.Find(name);

  if (!node)
    return FALSE;
  
  if (((wxQueryCol*)node->Data())->GetType() != type)
    return FALSE;

  void* src = ((wxQueryCol*)node->Data())->GetData(cursor);

  if (!src)
    return FALSE;
  
  memcpy(data, src, ((wxQueryCol*)node->Data())->GetSize(cursor));
  
  return TRUE;
}

void* wxRecordSet::GetFieldDataPtr(int col, int type)
{
  wxNode* node = cols.Nth(col);

  if (!node)
    return NULL;
  
  if (((wxQueryCol*)node->Data())->GetType() != type)
    return NULL;

  return ((wxQueryCol*)node->Data())->GetData(cursor);
}

void* wxRecordSet::GetFieldDataPtr(const char* name, int type)
{
  wxNode* node = cols.Find(name);

  if (!node)
    return NULL;
  
  if (((wxQueryCol*)node->Data())->GetType() != type)
    return NULL;

  return ((wxQueryCol*)node->Data())->GetData(cursor);
}

void* wxRecordSet::BindVar(int col, void* var, long size) {
  wxNode* node = cols.Nth(col);
  
  if (!node)
    return NULL;
  
  return ((wxQueryCol*)node->Data())->BindVar(var, size);
}

void* wxRecordSet::BindVar(const char* name, void* var, long size) {
  wxNode* node = cols.Find(name);
  
  if (!node)
    return NULL;
  
  return ((wxQueryCol*)node->Data())->BindVar(var, size);
}

void wxRecordSet::SetType(int typ) {
  type = typ;
}

int wxRecordSet::GetType(void) {
  return type;
}

void wxRecordSet::SetOptions(int opts) {
  options = opts;
}

int wxRecordSet::GetOptions(void) {
  return options;
}

Bool wxRecordSet::CanAppend(void)
{
  return FALSE;
}

Bool wxRecordSet::CanRestart(void)
{
  return FALSE;
}

Bool wxRecordSet::CanScroll(void)
{
  return FALSE;
}

Bool wxRecordSet::CanTransact(void)
{
  return FALSE;
}

Bool wxRecordSet::CanUpdate(void)
{
  return FALSE;
}

long wxRecordSet::GetCurrentRecord(void)
{
  return -1L;
}

Bool wxRecordSet::RecordCountFinal(void)
{
  return FALSE;
}

char* wxRecordSet::GetTableName(void)
{
  return tablename;
}

char *wxRecordSet::GetSQL(void)
{
  return NULL;
}

Bool wxRecordSet::IsOpen(void)
{
  return parentdb->IsOpen();
}

Bool wxRecordSet::IsBOF(void)
{
  return cursor < 0;
}

Bool wxRecordSet::IsEOF(void)
{
  return cursor >= nRecords;
}

Bool wxRecordSet::IsDeleted(void)
{
  return FALSE;
}

// Update operations
void wxRecordSet::AddNew(void)
{
}

Bool wxRecordSet::Delete(void)
{
  return FALSE;
}

void wxRecordSet::Edit(void)
{
}

Bool wxRecordSet::Update(void)
{
  return FALSE;
}

// Record navigation
Bool wxRecordSet::Move(long rows)
{
  if (!nRecords) {
    cursor = -1;
    return FALSE;
  }
  
  switch (type) {
    case wxOPEN_TYPE_SNAPSHOT:
    cursor += (int)rows;
    if (cursor < 0) {
      cursor = -1;
      return FALSE;
    }
    if (cursor > nRecords-1) {
      cursor = nRecords;
      return FALSE;
    }
    return TRUE;
      
    case wxOPEN_TYPE_DYNASET:
    return FALSE;
    default:
    return FALSE;
  }
}

Bool wxRecordSet::GoTo(long row)
{
  if (!nRecords) {
    cursor = -1;
    return FALSE;
  }
  
  switch (type) {
    case wxOPEN_TYPE_SNAPSHOT:
    cursor = (int)row;
    if (cursor < 0) {
      cursor = -1;
      return FALSE;
    }
    if (cursor > nRecords-1) {
      cursor = nRecords;
      return FALSE;
    }
    return TRUE;
      
    case wxOPEN_TYPE_DYNASET:
    return FALSE;
    default:
    return FALSE;
  }
}

Bool wxRecordSet::MoveFirst(void)
{
  if (!nRecords) {
    cursor = -1;
    return FALSE;
  }
  
  switch (type) {
    case wxOPEN_TYPE_SNAPSHOT:
    cursor = 0;
    return TRUE;
    
    case wxOPEN_TYPE_DYNASET:
    return FALSE;
    default:
    return FALSE;
  }
}

Bool wxRecordSet::MoveLast(void)
{
  if (!nRecords) {
    cursor = -1;
    return FALSE;
  }
  
  switch (type) {
    case wxOPEN_TYPE_SNAPSHOT:
    cursor = nRecords-1;
    return TRUE;
    
    case wxOPEN_TYPE_DYNASET:
    return FALSE;
    default:
    return FALSE;
  }
}

Bool wxRecordSet::MoveNext(void)
{
  if (!nRecords) {
    cursor = -1;
    return FALSE;
  }
  
  switch (type) {
    case wxOPEN_TYPE_SNAPSHOT:
    cursor++;
    if (cursor >= nRecords) {
      cursor = nRecords;
      return FALSE;
    }
    return TRUE;
    
    case wxOPEN_TYPE_DYNASET:
    return FALSE;
    default:
    return FALSE;
  }
}

Bool wxRecordSet::MovePrev(void)
{
  if (!nRecords) {
    cursor = -1;
    return FALSE;
  }
  
  switch (type) {
    case wxOPEN_TYPE_SNAPSHOT:
    cursor--;
    if (cursor < 0) {
      cursor = 0;
      return FALSE;
    }
    return TRUE;

    case wxOPEN_TYPE_DYNASET:
    return FALSE;
    default:
    return FALSE;
  }
}

// Others
void wxRecordSet::Cancel(void)
{
}

Bool wxRecordSet::IsFieldDirty(int col)
{
  wxNode* node = cols.Nth(col);
    
  if (!node)
    return FALSE;
    
  return ((wxQueryCol*)node->Data())->IsFieldDirty(cursor);
}

Bool wxRecordSet::IsFieldDirty(const char* name)
{
  wxNode* node = cols.Find(name);
    
  if (!node)
    return FALSE;
    
  return ((wxQueryCol*)node->Data())->IsFieldDirty(cursor);
}

Bool wxRecordSet::IsFieldNull(int col)
{
  wxNode* node = cols.Nth(col);
    
  if (!node)
    return TRUE;
    
  return NULL != ((wxQueryCol*)node->Data())->GetData(cursor);
}

Bool wxRecordSet::IsFieldNull(const char* name)
{
  wxNode* node = cols.Find(name);
    
  if (!node)
    return TRUE;
    
  return NULL != ((wxQueryCol*)node->Data())->GetData(cursor);
}

Bool wxRecordSet::IsColNullable(int col)
{
  wxNode* node = cols.Nth(col);
    
  if (!node)
    return FALSE;
    
  return ((wxQueryCol*)node->Data())->IsNullable();
}

Bool wxRecordSet::IsColNullable(const char* name)
{
  wxNode* node = cols.Find(name);
    
  if (!node)
    return FALSE;
    
  return ((wxQueryCol*)node->Data())->IsNullable();
}

Bool wxRecordSet::Requery(void)
{
  return FALSE;
}

void wxRecordSet::SetFieldDirty(int col, Bool dirty)
{
  wxNode* node = cols.Nth(col);
    
  if (!node)
    return;
    
  ((wxQueryCol*)node->Data())->SetFieldDirty(cursor, dirty);
}

void wxRecordSet::SetFieldDirty(const char* name, Bool dirty)
{
  wxNode* node = cols.Find(name);
    
  if (!node)
    return;
    
  ((wxQueryCol*)node->Data())->SetFieldDirty(cursor, dirty);
}

void wxRecordSet::SetFieldNull(void *p, Bool isNull)
{
}
   
// Overridables
char *wxRecordSet::GetDefaultConnect(void)
{
  return NULL;
}

char *wxRecordSet::GetDefaultSQL(void)
{
  return NULL;
}

void wxRecordSet::SetDefaultSQL(char *s)
{
  delete[] defaultSQL;
  
  if (s)
    defaultSQL = copystring(s);
  else
    defaultSQL = NULL;
}

// Build SQL query from column specification
Bool wxRecordSet::ConstructDefaultSQL(void)
{
//  if (queryCols.Number() == 0)
    return FALSE;
}

Bool wxRecordSet::ReleaseHandle(void)
{
  if (hStmt)
  {
    retcode = SQLFreeStmt(hStmt, SQL_DROP);
    if (retcode == SQL_ERROR)
    {
      if (parentdb)
        parentdb->ErrorSnapshot(hStmt);
      return FALSE;
    }
    hStmt = 0;
  }
  return TRUE;
}

wxQueryCol::wxQueryCol(void) {
//  __type = wxTYPE_QUERYCOL;
  name = NULL;
  type = SQL_TYPE_NULL;
  nullable = FALSE;
  var = NULL;
  varsize = 0;
}

wxQueryCol::~wxQueryCol(void) {
  // delete all data
  fields.DeleteContents(TRUE);
  fields.Clear();

  if (name)
    delete[] name;
}

void wxQueryCol::SetName(char* n) {
  name = new char[strlen(n)+1];
  strcpy(name, n);
}

Bool wxQueryCol::SetData(int row, void* buf, long len) {
  wxNode* node = fields.Nth(row);
  
  if (!node)
    return FALSE;
  
  return ((wxQueryField*)node->Data())->SetData(buf, len);
}

void wxQueryCol::SetFieldDirty(int row, Bool dirty) {
  wxNode* node = fields.Nth(row);
  
  if (node)
    return;
  
  ((wxQueryField*)node->Data())->SetDirty(dirty);
}

void wxQueryCol::AppendField(void* buf, long len) {
  wxNode* node = fields.Append(new wxQueryField);
  ((wxQueryField*)node->Data())->SetType(type);
  ((wxQueryField*)node->Data())->SetData(buf, len);
}

void wxQueryCol::SetType(short t) {
  type = t;
}

void* wxQueryCol::BindVar(void* v, long s) {
  void* oldvar = var;
  
  var = v;
  varsize = s;
  
  return oldvar;
}

void wxQueryCol::FillVar(int recnum) {
  if (!var)
    return;
  
  wxNode* node = fields.Nth(recnum);
  
  if (!node)
    return;

  long actsize = ((wxQueryField*)node->Data())->GetSize();
  if (actsize > varsize)
    actsize = varsize;

  memcpy(var, ((wxQueryField*)node->Data())->GetData(), actsize);
}

void wxQueryCol::SetNullable(Bool n) {
  nullable = n;
}
  
char* wxQueryCol::GetName(void) {
  return name;
}

short wxQueryCol::GetType(void)  {
  return type;
}

Bool wxQueryCol::IsNullable(void) {
  return nullable;
}


Bool wxQueryCol::IsFieldDirty(int row) {
  wxNode* node = fields.Nth(row);
  
  if (!node)
    return FALSE;
  
  return ((wxQueryField*)node->Data())->IsDirty();
}

void* wxQueryCol::GetData(int row) {
  wxNode* node = fields.Nth(row);
  
  if (!node)
    return NULL;
  
  return ((wxQueryField*)node->Data())->GetData();
}

long wxQueryCol::GetSize(int row) {
  wxNode* node = fields.Nth(row);
  
  if (!node)
    return NULL;
  
  return ((wxQueryField*)node->Data())->GetSize();
}

wxQueryField::wxQueryField(void) {
//  __type = wxTYPE_QUERYROW;
  data = NULL;
  type = SQL_TYPE_NULL;
  size = 0;
  dirty = FALSE;
}

wxQueryField::~wxQueryField(void) {
  switch (type)
  {
    case SQL_NUMERIC:
    case SQL_DECIMAL:
    case SQL_CHAR:
    case SQL_VARCHAR:
      if (data) // JACS
        delete[] (char*)data;
      break;
    case SQL_INTEGER:
      if (data) // JACS
        delete (long*)data;
      break;
    case SQL_SMALLINT:
      if (data)
        delete (short*)data;
      break;
    case SQL_FLOAT:
    case SQL_DOUBLE:
      if (data)
        delete (double*)data;
      break;
    case SQL_REAL:
      if (data)
        delete (float*)data;
      break;
  }
}

Bool wxQueryField::AllocData(void) {
  switch (type)
  {
    case SQL_NUMERIC:
    case SQL_DECIMAL:
    case SQL_CHAR:
    case SQL_VARCHAR:
    {
      if (data) // JACS
        delete[] (char*)data;
      if (data = new char[size+1])
      {
        char *str = (char *)data;
        int i;
        for (i = 0; i < size; i++)
          str[i] = 0;
//      memset(data, 0, size+1);
      }
      break;
    }
    case SQL_INTEGER:
    {
      if (data) // JACS
        delete (long*)data;
      if (data = new long)
        *(long*)data = 0L;
      break;
    }
    case SQL_SMALLINT:
    {
      if (data)
        delete (short*)data;
      if (data = new short)
        *(short*)data = 0;
      break;
    }
    case SQL_FLOAT:
    case SQL_DOUBLE:
    {
      if (data)
        delete (double*)data;
      if (data = new double)
        *(double*)data = 0;
      break;
    }
    case SQL_REAL:
    {
      if (data)
        delete (float*)data;
      if (data = new float)
        *(float*)data = 0;
      break;
    }
    default:
    return FALSE;
  }
  
  return TRUE;
}

Bool wxQueryField::SetData(void* d, long s) {
  size = s;
  if (AllocData() && d)
  {
//    memcpy(data, d, s);
    switch (type)
    {
      case SQL_NUMERIC:
      case SQL_DECIMAL:
      case SQL_CHAR:
      case SQL_VARCHAR:
      {
        char *str = (char *)data;
        int i;
        for (i = 0; i < size; i++)
          str[i] = 0;
          
        strncpy(str, (char *)d, (int)size);
        str[size] = 0;
        break;
      }
      case SQL_INTEGER:
      {
        *(long*)data = *((long *)d);
        break;
      }
      case SQL_SMALLINT:
      {
        *(short*)data = *((short*)d);
        break;
      }
      case SQL_FLOAT:
      case SQL_DOUBLE:
      {
        *(double*)data = *((double*)d);
        break;
      }
      case SQL_REAL:
      {
        *(float*)data = *((float*)d);
        break;
      }
      default:
        return FALSE;
    }
    return TRUE;
  }
  return FALSE;
}

void wxQueryField::ClearData(void) {
  if (data)
  {
  //    memset(data, 0, size);
    switch (type)
    {
      case SQL_NUMERIC:
      case SQL_DECIMAL:
      case SQL_CHAR:
      case SQL_VARCHAR:
      {
        char *str = (char *)data;
        int i;
        for (i = 0; i < size; i++)
          str[i] = 0;
        break;
      }
      case SQL_INTEGER:
      {
        *(long*)data = 0L;
        break;
      }
      case SQL_SMALLINT:
      {
        *(short*)data = 0;
        break;
      }
      case SQL_FLOAT:
      case SQL_DOUBLE:
      {
        *(double*)data = .0;
        break;
      }
      case SQL_REAL:
      {
        *(float*)data = .0;
        break;
      }
      default:
        return;
    }
  }
}

void wxQueryField::SetDirty(Bool d) {
  dirty = d;
}

void wxQueryField::SetType(short t) {
  type = t;
}

void wxQueryField::SetSize(long s) {
  size = s;
  AllocData();
}

void* wxQueryField::GetData(void) {
  return data;
}

short wxQueryField::GetType(void)  {
  return type;
}

long wxQueryField::GetSize(void) {
  return size;
}

Bool wxQueryField::IsDirty(void) {
  return dirty;
}

#endif // USE_ODBC
