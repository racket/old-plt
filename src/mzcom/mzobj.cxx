// mzobj.cxx : Implementation of CMzObj

#include <resource.h>

#include "stdafx.h"
#include "mzcom.h"
#include "mzobj.h"

/////////////////////////////////////////////////////////////////////////////
// CMzObj

HANDLE exitSem;

void exitHandler(int) {
  ReleaseSemaphore(exitSem,1,NULL);
  ExitThread(0);
} 

DWORD WINAPI evalLoop(LPVOID args) {
  UINT len;
  char *narrowInput,*narrowOutput;
  Scheme_Object *outputObj;
  Scheme_Env *env;
  OLECHAR *outputBuffer;
  mz_jmp_buf saveBuff;
  THREAD_GLOBALS *pTg;
  HANDLE readSem;
  HANDLE writeSem;
  BSTR **ppInput;
  BSTR *pOutput;
  HRESULT *pHr;
  BOOL *pErrorState;

  // make sure all MzScheme calls in this thread

  scheme_exit = exitHandler;

  pTg = (THREAD_GLOBALS *)args;

  ppInput = pTg->ppInput;
  pOutput = pTg->pOutput; 
  pHr = pTg->pHr;
  readSem = pTg->readSem;
  writeSem = pTg->writeSem;
  pErrorState = pTg->pErrorState;

  env = scheme_basic_env();

  if (env == NULL) {
    ::MessageBox(NULL,"Can't create Scheme environment","MzCOM",MB_OK);
    ExitThread(0);
  }

  scheme_dont_gc_ptr(env); 

  while (1) {

    WaitForSingleObject(readSem,INFINITE);

    len = SysStringLen(**ppInput);

    narrowInput = (char *)scheme_malloc(len + 1);

    WideCharToMultiByte(CP_ACP,(DWORD)0,
			**ppInput,len,
			narrowInput,len + 1,
			NULL,NULL);

    narrowInput[len] = '\0';

    memcpy(&saveBuff,&scheme_error_buf,sizeof(mz_jmp_buf));
    if (scheme_setjmp(scheme_error_buf)) {
      if (scheme_jumping_to_continuation) {
	scheme_clear_escape();
      } 
      *pOutput = SysAllocString(L"");
      *pErrorState = TRUE;
      *pHr = E_FAIL;
    }
    else {

      outputObj = scheme_eval_string_all(narrowInput,env,TRUE);

      narrowOutput = scheme_format("~s",2,1,&outputObj,NULL);
  
      len = strlen(narrowOutput);

      outputBuffer = (OLECHAR *)scheme_malloc((len + 1) * sizeof(OLECHAR));

      MultiByteToWideChar(CP_ACP,(DWORD)0,
			  narrowOutput,len,
			  outputBuffer,len + 1);

      outputBuffer[len] = '\0';

      *pOutput = SysAllocString(outputBuffer);

      *pErrorState = FALSE;

      *pHr = S_OK;
    }
  
    memcpy(&scheme_error_buf,&saveBuff,sizeof(mz_jmp_buf));

    ReleaseSemaphore(writeSem,1,NULL);

  }

  return 0;
}

CMzObj::CMzObj(void) {
  static THREAD_GLOBALS tg;

  inputMutex = NULL;
  readSem = NULL;
  threadId = NULL;
  threadHandle = NULL;

  inputMutex = CreateSemaphore(NULL,1,1,NULL);
  if (inputMutex == NULL) {
    MessageBox(NULL,"Can't create mutex","MzCOM",MB_OK);
    return;
  }

  readSem = CreateSemaphore(NULL,0,1,NULL);

  if (readSem == NULL) {
    MessageBox(NULL,"Can't create read semaphore","MzCOM",MB_OK);
    return; 
  }

  writeSem = CreateSemaphore(NULL,0,1,NULL);

  if (writeSem == NULL) {
    MessageBox(NULL,"Can't create write semaphore","MzCOM",MB_OK);
    return; 
  }

  exitSem = CreateSemaphore(NULL,0,1,NULL);

  if (exitSem == NULL) {
    MessageBox(NULL,"Can't create exit semaphore","MzCOM",MB_OK);
    return; 
  }

  evalSems[0] = writeSem;
  evalSems[1] = exitSem;

  tg.pHr = &hr;
  tg.ppInput = &globInput;
  tg.pOutput = &globOutput;
  tg.readSem = readSem;
  tg.writeSem = writeSem;
  tg.pErrorState = &errorState;

  threadHandle = CreateThread(NULL,0,evalLoop,(LPVOID)&tg,0,&threadId);
}

CMzObj::~CMzObj(void) {
  if (readSem) {
    CloseHandle(readSem);
  }

  if (writeSem) {
    CloseHandle(readSem);
  }

  if (exitSem) {
    CloseHandle(readSem);
  }

  if (inputMutex) {
    CloseHandle(inputMutex);
  }

  if (threadHandle) {
    DWORD threadStatus;

    GetExitCodeThread(threadHandle,&threadStatus);

    if (threadStatus == STILL_ACTIVE) {
      TerminateThread(threadHandle,0);
    }
  }
}

void CMzObj::RaiseError(const OLECHAR *msg) {
  BSTR bstr;
  bstr = SysAllocString(msg);
  Fire_Error(bstr);
  SysFreeString(bstr);
}

BOOL CMzObj::testThread(void) {
  DWORD threadStatus;

  if (threadHandle == NULL) {
    RaiseError(L"No evaluator");
    return FALSE;
  }

  if (GetExitCodeThread(threadHandle,&threadStatus) == 0) { 
    RaiseError(L"Evaluator may be terminated");
  }

  if (threadStatus != STILL_ACTIVE) {
    RaiseError(L"Evaluator terminated");
    return FALSE;
  }

  return TRUE;
}

STDMETHODIMP CMzObj::Eval(BSTR input, LPBSTR output) {
  if (!testThread()) {
    return E_ABORT;
  }

  WaitForSingleObject(inputMutex,INFINITE);
  globInput = &input;
  // allow evaluator to read
  ReleaseSemaphore(readSem,1,NULL);
  // wait until evaluator done or eval thread terminated
  if (WaitForMultipleObjects(2,evalSems,FALSE,INFINITE) ==
      WAIT_OBJECT_0 + 1) {
    RaiseError(L"Scheme terminated evaluator");
    return E_FAIL;
  }
  *output = globOutput;
  ReleaseSemaphore(inputMutex,1,NULL);

  if (errorState) {
    RaiseError(L"Scheme evaluation error");
  }

  return hr;
}

BOOL WINAPI dlgProc(HWND hDlg,UINT msg,WPARAM wParam,LPARAM) {
  switch(msg) {
  case WM_INITDIALOG :
    SetActiveWindow(hDlg);
    return TRUE;
  case WM_COMMAND :
    switch (LOWORD(wParam)) { 
    case IDOK :
    case IDCANCEL :
      EndDialog(hDlg,0);
      return FALSE;
    }
  default :
    return FALSE;
  }
}

STDMETHODIMP CMzObj::About(void) {
  DialogBox(globHinst,MAKEINTRESOURCE(ABOUTBOX),
	    NULL,dlgProc);
  return S_OK;
}
