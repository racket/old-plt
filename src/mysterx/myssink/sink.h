// Sink.h : Declaration of the CSink

#ifndef __SINK_H_
#define __SINK_H_

#include "resource.h"       // main symbols

#include "escheme.h"

/////////////////////////////////////////////////////////////////////////////
// CSink
class ATL_NO_VTABLE CSink : 
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CSink, &CLSID_Sink>,
	public IDispatchImpl<ISink, &IID_ISink, &LIBID_MYSSINKLib>
{

private:
  
  Scheme_Extension_Table *scheme_extension_table;

  Scheme_Object *click_proc;  
  Scheme_Object *dblclick_proc;  
  Scheme_Object *keydown_proc;
	Scheme_Object *keypress_proc;
	Scheme_Object *keyup_proc;	
	Scheme_Object *mousedown_proc;	
	Scheme_Object *mousemove_proc;	
	Scheme_Object *mouseup_proc;	
	Scheme_Object *error_proc;	
	Scheme_Object *readystatechange_proc;	

  unsigned click_dispid;  
  unsigned dblclick_dispid;  
  unsigned keydown_dispid;
	unsigned keypress_dispid;
	unsigned keyup_dispid;	
	unsigned mousedown_dispid;	
	unsigned mousemove_dispid;	
	unsigned mouseup_dispid;	
	unsigned error_dispid;	
	unsigned readystatechange_dispid;	
 
public:
	CSink()
	{
	  click_proc = NULL;
    dblclick_proc = NULL;
    keydown_proc = NULL;
	  keypress_proc = NULL;
	  keyup_proc = NULL;	
	  mousedown_proc = NULL;	
	  mousemove_proc = NULL;	
	  mouseup_proc = NULL;	
	  error_proc = NULL;	
	  readystatechange_proc = NULL;	

    click_dispid = 0xFFFFFDA8;
    dblclick_dispid = 0xFFFFFDA7; 
    keydown_dispid = 0xFFFFFDA6;
	  keypress_dispid = 0xFFFFFDA5;
	  keyup_dispid = 0xFFFFFDA4;	
	  mousedown_dispid = 0xFFFFFDA3;	
	  mousemove_dispid = 0xFFFFFDA2;	
	  mouseup_dispid = 0xFFFFFDA1;	
	  error_dispid = 0xFFFFFDA0;	
	  readystatechange_dispid = 0xFFFFFD9F;
    
    }

DECLARE_REGISTRY_RESOURCEID(IDR_SINK)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CSink)
	COM_INTERFACE_ENTRY(ISink)
	COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

// ISink
public:
	STDMETHOD(set_extension_table)(int);

  // stock event handlers
  
  STDMETHOD(Click)();
	STDMETHOD(DblClick)();
	STDMETHOD(KeyDown)(short *,short);
	STDMETHOD(KeyPress)(short *);
	STDMETHOD(KeyUp)(short *,short);
	STDMETHOD(MouseMove)(short,short,long,long);
	STDMETHOD(MouseDown)(short,short,long,long);
	STDMETHOD(MouseUp)(short,short,long,long);
	STDMETHOD(Error)();
	STDMETHOD(ReadyStateChange)(long);

  // stock event handler updaters

 	STDMETHOD(set_click_proc)(int);
 	STDMETHOD(set_dblclick_proc)(int);
 	STDMETHOD(set_keydown_proc)(int);
 	STDMETHOD(set_keypress_proc)(int);
 	STDMETHOD(set_keyup_proc)(int);
 	STDMETHOD(set_mousedown_proc)(int);
 	STDMETHOD(set_mousemove_proc)(int);
 	STDMETHOD(set_mouseup_proc)(int);
 	STDMETHOD(set_error_proc)(int);
 	STDMETHOD(set_readystatechange_proc)(int);
};

#endif //__SINK_H_
