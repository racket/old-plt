// mysterx.h

#define sizeray(x) (sizeof(x)/sizeof(*x))

#define MX_PRIM_DECL(f) Scheme_Object *f(int,Scheme_Object **)

#define MX_DEFAULT_WIDTH  (400)
#define MX_DEFAULT_HEIGHT (400)

#define DOCHWND_TRIES 40
#define DOCDISPATCH_TRIES 60

// temp !!! until dont_gc_ptr fixed

#ifdef scheme_dont_gc_ptr
#undef scheme_dont_gc_ptr
#endif

#ifdef scheme_gc_ptr_ok
#undef scheme_gc_ptr_ok
#endif

#define scheme_dont_gc_ptr(p)
#define scheme_gc_ptr_ok(p)

// end temp !!!!!!!!!!!!!!!!!!!!!

#define NORETVALINDEX (-1)
#define UNICODE_BUFFER_SIZE 256
#define TYPE_TBL_SIZE 1019

typedef struct _MX_prim_ {
  Scheme_Object *(*c_fun)(int argc,Scheme_Object **);
  char *name;
  short minargs;
  short maxargs;
} MX_PRIM;

typedef struct _scheme_com_obj_ { 
  Scheme_Type type;
  IDispatch *pIDispatch;
} MX_COM_Object;

typedef struct _scheme_mx_event_ { 
  Scheme_Type type;
  IEvent *pEvent;
} MX_Event;

typedef enum _mx_desckind_ {
  funcDesc,varDesc
} MX_DESCKIND;

typedef struct _method_desc_ {
  MEMBERID memID;
  MX_DESCKIND descKind;
  union {
    FUNCDESC *pFuncDesc;
    VARDESC *pVarDesc;
  };
} MX_TYPEDESC;

typedef struct _mx_com_data_ {
  Scheme_Type type;
  union { // MS representations
    DATE date; 
    CY cy;    
    SCODE scode;
    IUnknown *pIUnknown;
  }; 
} MX_COM_Data_Object;

typedef struct _com_document_ {
  Scheme_Type type;
  HWND hwnd;
  IHTMLDocument2 *pIHTMLDocument2;
  IEventQueue *pIEventQueue;
} MX_Document_Object;

typedef struct _date_ {
  Scheme_Type type;
} MX_Date_Object;

typedef struct _mx_type_tbl_entry_ {
  IDispatch *pIDispatch;
  char *name;
  INVOKEKIND invKind;
  MX_TYPEDESC *pTypeDesc;
  struct _mx_type_tbl_entry_ *next;
} MX_TYPE_TBL_ENTRY;

typedef enum _mx_html_where_ {
  insert,append
} MX_HTML_WHERE;
  
typedef struct _document_window_ { // parameters a la MrEd frame% class
  char *label;
  int width;
  int height;
  int x;
  int y;
  DWORD style;
} DOCUMENT_WINDOW;

typedef struct _document_window_init_ {
  DOCUMENT_WINDOW docWindow;
  IStream **ppIStream; // for passing COM interface back to main thread
} DOCUMENT_WINDOW_INIT;

typedef struct _document_window_style_option {
  char *name;
  DWORD bits;
  BOOL enable;
} DOCUMENT_WINDOW_STYLE_OPTION;

#define MX_COM_OBJP(o) (o->type == mx_com_object_type)
#define MX_COM_OBJ_VAL(o) (((MX_COM_Object *)o)->pIDispatch)

#define MX_DOCUMENTP(o) (o->type == mx_document_type)
#define MX_DOCUMENT_VAL(o) (((MX_Document_Object *)o)->pIHTMLDocument2)
#define MX_DOCUMENT_EVENTQUEUE(o) (((MX_Document_Object *)o)->pIEventQueue)

#define MX_EVENTP(o) (o->type == mx_event_type)
#define MX_EVENT_VAL(o) (((MX_Event *)o)->pEvent)

#define MX_CYP(o) (o->type == mx_com_cy_type)
#define MX_CY_VAL(o) (((MX_COM_Data_Object *)o)->cy)

#define MX_DATEP(o) (o->type == mx_com_date_type)
#define MX_DATE_VAL(o) (((MX_COM_Data_Object *)o)->date)

#define MX_SCODEP(o) (o->type == mx_com_scode_type)
#define MX_SCODE_VAL(o) (((MX_COM_Data_Object *)o)->scode)

#define MX_IUNKNOWNP(o) (o->type == mx_com_iunknown_type)
#define MX_IUNKNOWN_VAL(o) (((MX_COM_Data_Object *)o)->pIUnknown)

extern Scheme_Type mx_com_object_type; 
extern Scheme_Type mx_event_type;
extern Scheme_Type mx_document_type;
extern Scheme_Type mx_com_cy_type;
extern Scheme_Type mx_com_date_type;
extern Scheme_Type mx_com_boolean_type;
extern Scheme_Type mx_com_scode_type;
extern Scheme_Type mx_com_variant_type;
extern Scheme_Type mx_com_hresult_type;
extern Scheme_Type mx_com_iunknown_type;
extern Scheme_Type mx_com_pointer_type;
extern Scheme_Type mx_com_array_type;

extern Scheme_Object *hash_table_get;
extern Scheme_Object *hash_table_put;
extern Scheme_Object *hash_table_remove;
extern Scheme_Object *make_hash_table;

Scheme_Object *mx_make_cy(CY *);
Scheme_Object *mx_make_date(DATE *);
Scheme_Object *mx_make_bool(unsigned);
Scheme_Object *mx_make_scode(SCODE);
Scheme_Object *mx_make_idispatch(IDispatch *);
Scheme_Object *mx_make_iunknown(IUnknown *);

MX_PRIM_DECL(mx_com_invoke);
MX_PRIM_DECL(mx_com_set_property);
MX_PRIM_DECL(mx_com_get_property);
MX_PRIM_DECL(mx_com_methods);
MX_PRIM_DECL(mx_com_get_properties);
MX_PRIM_DECL(mx_com_set_properties);
MX_PRIM_DECL(mx_com_method_type);
MX_PRIM_DECL(mx_com_get_property_type);
MX_PRIM_DECL(mx_com_set_property_type);
MX_PRIM_DECL(mx_all_controls);
MX_PRIM_DECL(mx_all_com_classes);
MX_PRIM_DECL(mx_document_objects);
MX_PRIM_DECL(mx_coclass_to_html);
MX_PRIM_DECL(mx_insert_html);
MX_PRIM_DECL(mx_append_html);
MX_PRIM_DECL(mx_replace_html);
MX_PRIM_DECL(mx_get_event);
MX_PRIM_DECL(mx_document_pred);
MX_PRIM_DECL(mx_event_keypress_pred);
MX_PRIM_DECL(mx_event_keydown_pred);
MX_PRIM_DECL(mx_event_keyup_pred);
MX_PRIM_DECL(mx_event_mousedown_pred);
MX_PRIM_DECL(mx_event_mouseover_pred);
MX_PRIM_DECL(mx_event_mousemove_pred);
MX_PRIM_DECL(mx_event_mouseout_pred);
MX_PRIM_DECL(mx_event_mouseup_pred);
MX_PRIM_DECL(mx_event_click_pred);
MX_PRIM_DECL(mx_event_dblclick_pred);
MX_PRIM_DECL(mx_event_error_pred);
MX_PRIM_DECL(mx_event_tag);
MX_PRIM_DECL(mx_event_id);
MX_PRIM_DECL(mx_event_from_tag);
MX_PRIM_DECL(mx_event_from_id);
MX_PRIM_DECL(mx_event_to_tag);
MX_PRIM_DECL(mx_event_to_id);
MX_PRIM_DECL(mx_event_x);
MX_PRIM_DECL(mx_event_y);
MX_PRIM_DECL(mx_event_pred);
MX_PRIM_DECL(mx_event_keypress_pred);
MX_PRIM_DECL(mx_event_keydown_pred);
MX_PRIM_DECL(mx_event_keyup_pred);
MX_PRIM_DECL(mx_event_mousedown_pred);
MX_PRIM_DECL(mx_event_mouseover_pred);
MX_PRIM_DECL(mx_event_mouseout_pred);
MX_PRIM_DECL(mx_event_mouseup_pred);
MX_PRIM_DECL(mx_event_click_pred);
MX_PRIM_DECL(mx_event_dblclick_pred);
MX_PRIM_DECL(mx_event_error_pred);
MX_PRIM_DECL(mx_block_until_event);
MX_PRIM_DECL(mx_event_available);
MX_PRIM_DECL(mx_make_document);
MX_PRIM_DECL(mx_document_show);
  
void mx_register_com_object(Scheme_Object *,IUnknown *);
IHTMLElementCollection *getBodyObjects(IHTMLElement *);
IDispatch *getObjectInCollection(IHTMLElementCollection *,int);
Scheme_Object *BSTRToSchemeString(BSTR);
void initEventNames(void);


