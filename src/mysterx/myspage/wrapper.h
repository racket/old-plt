#pragma once
#ifndef __WRAPPERS_H__
#define __WRAPPERS_H__

// based on Chris Sells' code at www.sellsbrothers.com/tools

class CWrapperDispatch :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CWrapperDispatch>,
    public IDispatch
{
public:
    void SetDispatch(IDispatch* pdisp) {
        m_spdisp = pdisp;
    }

DECLARE_NO_REGISTRY()
BEGIN_COM_MAP(CWrapperDispatch)
    COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

// IDispatch

    STDMETHODIMP GetTypeInfoCount(UINT* pctinfo) {
        if( !m_spdisp ) return E_UNEXPECTED;
        return m_spdisp->GetTypeInfoCount(pctinfo);
    }

	STDMETHODIMP GetTypeInfo(UINT itinfo, LCID lcid, ITypeInfo** pptinfo) {
        if( !pptinfo ) return E_POINTER;
        *pptinfo = 0;
        if( !m_spdisp ) return E_UNEXPECTED;
        return m_spdisp->GetTypeInfo(itinfo, lcid, pptinfo);
    }

	STDMETHODIMP GetIDsOfNames(REFIID riid, LPOLESTR* rgszNames, UINT cNames,
							   LCID lcid, DISPID* rgdispid) {
        if( !m_spdisp ) return E_UNEXPECTED;
        return m_spdisp->GetIDsOfNames(riid, rgszNames, cNames, lcid, rgdispid);
    }

	STDMETHODIMP Invoke(DISPID dispidMember, REFIID riid, LCID lcid,
						WORD wFlags, DISPPARAMS* pdispparams,
						VARIANT* pvarResult, EXCEPINFO* pexcepinfo,
						UINT* puArgErr) {
        if( !m_spdisp ) return E_UNEXPECTED;
        return m_spdisp->Invoke(dispidMember, riid, lcid, wFlags, pdispparams, pvarResult, pexcepinfo, puArgErr);
    }

private:
    CComPtr<IDispatch>  m_spdisp;
};

// this class override IDocHostUIHandler, to block context menus

class CWrapperUIHandler :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CWrapperUIHandler>,
    public IDocHostUIHandler
{
public:
    void SetUIHandler(IDocHostUIHandler* pHandler) {
        m_spHandler = pHandler;
    }

DECLARE_NO_REGISTRY()
BEGIN_COM_MAP(CWrapperUIHandler)
    COM_INTERFACE_ENTRY(IDocHostUIHandler)
END_COM_MAP()

// IDocHostUIHandler

  STDMETHODIMP ShowContextMenu(DWORD dwID,POINT *ppt,
			       IUnknown *pcmdtReserved,
			       IDispatch *pdispReserved) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }
  
  return S_OK; // overrides default menu
}
	
 STDMETHODIMP GetHostInfo(DOCHOSTUIINFO *pInfo) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->GetHostInfo(pInfo);
}
	
 STDMETHODIMP ShowUI(DWORD dwID,
		     IOleInPlaceActiveObject *pActiveObject,
		     IOleCommandTarget *pCommandTarget,
		     IOleInPlaceFrame *pFrame,
		     IOleInPlaceUIWindow *pDoc) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->ShowUI(dwID,pActiveObject,pCommandTarget,pFrame,pDoc);
 }
	
 STDMETHODIMP HideUI(void) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->HideUI();
 }
	
 STDMETHODIMP UpdateUI(void) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->UpdateUI();
 }
	
 STDMETHODIMP EnableModeless(BOOL fEnable) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->EnableModeless(fEnable);
 }
	
 STDMETHODIMP OnDocWindowActivate(BOOL fActivate) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->OnDocWindowActivate(fActivate);
 }
	
  STDMETHODIMP OnFrameWindowActivate(BOOL fActivate) {
    if (!m_spHandler) {
      return E_UNEXPECTED;
    }

  return m_spHandler->OnFrameWindowActivate(fActivate);
 }
	
  STDMETHODIMP ResizeBorder(LPCRECT prcBorder,
			    IOleInPlaceUIWindow *pUIWindow,BOOL fFrameWindow) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->ResizeBorder(prcBorder,pUIWindow,fFrameWindow);
 }
	
  STDMETHODIMP TranslateAccelerator(LPMSG lpMsg,
				    const GUID __RPC_FAR *pguidCmdGroup,
				    DWORD nCmdID) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->TranslateAccelerator(lpMsg,pguidCmdGroup,nCmdID);
 }
	
 STDMETHODIMP GetOptionKeyPath(LPOLESTR *pchKey,DWORD dw) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->GetOptionKeyPath(pchKey,dw);
 }
	
 STDMETHODIMP GetDropTarget(IDropTarget *pDropTarget,IDropTarget **ppDropTarget) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->GetDropTarget(pDropTarget,ppDropTarget);
 }
	
 STDMETHODIMP GetExternal(IDispatch **ppDispatch) {
   if (!m_spHandler) {
     return E_UNEXPECTED;
   }

   return m_spHandler->GetExternal(ppDispatch);
 }
	
 STDMETHODIMP TranslateUrl(DWORD dwTranslate,
			   OLECHAR  *pchURLIn,
			   OLECHAR  **ppchURLOut) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->TranslateUrl(dwTranslate,pchURLIn,ppchURLOut);
 }
	
 STDMETHODIMP FilterDataObject(IDataObject *pDO,
			       IDataObject **ppDORet) {
  if (!m_spHandler) {
    return E_UNEXPECTED;
  }

  return m_spHandler->FilterDataObject(pDO,ppDORet);
 }

private:
  CComPtr<IDocHostUIHandler> m_spHandler;
};

#endif	// __WRAPPERS_H__


