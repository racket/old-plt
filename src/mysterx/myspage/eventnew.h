// EventNew.h : Declaration of the CEvent

#ifndef __EVENT_H_
#define __EVENT_H_

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CEvent
class ATL_NO_VTABLE CEvent : 
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CEvent, &CLSID_Event>,
	public IDispatchImpl<IEvent, &IID_IEvent, &LIBID_MYSPAGELib>
{
public:
	CEvent()
	{
	}

DECLARE_REGISTRY_RESOURCEID(IDR_EVENT2)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CEvent)
	COM_INTERFACE_ENTRY(IEvent)
	COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

// IEvent
public:
};

#endif //__EVENT_H_
