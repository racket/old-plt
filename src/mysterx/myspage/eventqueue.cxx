// EventQueue.cpp : Implementation of CEventQueue

#include "stdafx.h"
#include "myspage.h"
#include "DHTMLpage.h"
#include "eventqueue.h"

#include <stdio.h>
#include <limits.h>


/////////////////////////////////////////////////////////////////////////////
// CEventQueue

CEventQueue::CEventQueue(void) { 

    queueLength = 0;
    readerNdx = writerNdx = 0;
    
    stubHandle = OpenProcess(PROCESS_ALL_ACCESS,FALSE,GetCurrentProcessId());
  
    readSem = CreateSemaphore(NULL,0,LONG_MAX,NULL);  // using MAXQUEUELENGTH doesn't work
    mutex = CreateSemaphore(NULL,1,1,NULL);

    if (readSem == NULL || mutex == NULL) {
      ::failureBox("Error creating event semaphore(s)");
    }
}

STDMETHODIMP CEventQueue::QueueEvent(IEvent *pEvent) {
  BOOL signalReader;

  WaitForSingleObject(mutex,INFINITE); 

  if (queueLength < MAXQUEUELENGTH) {
    queueLength++;
    signalReader = TRUE;
  } 
  else {
    readerNdx = ++readerNdx % MAXQUEUELENGTH;
    signalReader = FALSE;
  }

  theQueue[writerNdx] = pEvent;

  writerNdx = ++writerNdx % MAXQUEUELENGTH;

  ReleaseSemaphore(mutex,1,NULL);

  if (signalReader) {
    ReleaseSemaphore(readSem,1,NULL);
  }

  return S_OK;

}


STDMETHODIMP CEventQueue::GetEvent(IEvent **ppEvent) {
  DWORD whyWait;
  MSG msg;

  while (1) {

    whyWait = MsgWaitForMultipleObjects(1,&proxyReadSem,FALSE,INFINITE,QS_ALLEVENTS);

    if (whyWait == WAIT_OBJECT_0) {
      break;
    }

    while (PeekMessage(&msg,NULL,0,0,PM_REMOVE)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
  }
    
  WaitForSingleObject(proxyMutex,INFINITE);

  *ppEvent = theQueue[readerNdx];

  readerNdx = ++readerNdx % MAXQUEUELENGTH;
  queueLength--;

  ReleaseSemaphore(proxyMutex,1,NULL);

	return S_OK;
}
 

STDMETHODIMP CEventQueue::SetProxySemaphores(void) {
  HANDLE proxyHandle;
  BOOL result;

  proxyHandle = OpenProcess(PROCESS_ALL_ACCESS,FALSE,GetCurrentProcessId());
  
  result = DuplicateHandle(stubHandle,mutex,(HANDLE)proxyHandle,&proxyMutex,SEMAPHORE_ALL_ACCESS,FALSE,0);

  if (result == FALSE) {
    ::failureBox("Can't create proxy mutex");
  }
 
  result = DuplicateHandle(stubHandle,readSem,(HANDLE)proxyHandle,&proxyReadSem,SEMAPHORE_ALL_ACCESS,FALSE,0);
  
  if (result == FALSE) {
    ::failureBox("Can't create proxy read semaphore");
  }

  CloseHandle(stubHandle);  
  CloseHandle(proxyHandle);  
  
	return S_OK;
}

STDMETHODIMP CEventQueue::get_EventAvailable(VARIANT_BOOL *pVal) {

  WaitForSingleObject(proxyMutex,INFINITE);
  
  *pVal = (queueLength == 0) ? 0 : -1; 

  ReleaseSemaphore(proxyMutex,1,NULL);

	return S_OK;
}
