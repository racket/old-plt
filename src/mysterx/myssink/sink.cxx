// Sink.cpp : Implementation of CSink

#include "stdafx.h"
#include <stdio.h>
#include "Myssink.h"
#include "Sink.h"
#include "comtypes.h"

/////////////////////////////////////////////////////////////////////////////
// CSink

// import Scheme extension table

STDMETHODIMP CSink::set_extension_table(int p)
{
  scheme_extension_table = (Scheme_Extension_Table *)p;

	return S_OK;
}

// stock events

STDMETHODIMP CSink::Click(void)
{
  if (click_proc == NULL) {
    return S_OK;  
  }

  scheme_apply(click_proc,0,NULL);

  return S_OK;
}

STDMETHODIMP CSink::DblClick(void)
{
  if (dblclick_proc == NULL) {
    return S_OK;  
  }

  scheme_apply(click_proc,0,NULL);

  return S_OK;
}

STDMETHODIMP CSink::KeyDown(short *keyCode,short shift)
{
  Scheme_Object *argv[2],*boxedVal;

  if (keydown_proc == NULL) {
    return S_OK;  
  }

  argv[0] = scheme_box(scheme_make_integer((long)*keyCode));
  argv[1] = scheme_make_integer((long)shift);

  scheme_apply(keydown_proc,2,argv);

  // reflect any changes made by handler
  
  boxedVal = SCHEME_BOX_VAL(argv[0]);
  
  if (isShortInt(boxedVal)) {
    *keyCode = (short)SCHEME_INT_VAL(boxedVal);
  }

  return S_OK;
}

STDMETHODIMP CSink::KeyPress(short *ascii)
{
  Scheme_Object *argv[1],*boxedVal;

  if (keypress_proc == NULL) {
    return S_OK;  
  }

  argv[0] = scheme_box(scheme_make_integer((long)*ascii));

  scheme_apply(keypress_proc,2,argv);

  // reflect any changes made by handler
  
  boxedVal = SCHEME_BOX_VAL(argv[0]);
  
  if (isShortInt(boxedVal)) {
    *ascii = (short)SCHEME_INT_VAL(boxedVal);
  }

  return S_OK;
}

STDMETHODIMP CSink::KeyUp(short *keyCode, short shift)
{
  Scheme_Object *argv[2],*boxedVal;

  if (keyup_proc == NULL) {
    return S_OK;  
  }

  argv[0] = scheme_box(scheme_make_integer((long)*keyCode));
  argv[1] = scheme_make_integer((long)shift);

  scheme_apply(keyup_proc,2,argv);

  // reflect any changes made by handler
  
  boxedVal = SCHEME_BOX_VAL(argv[0]);
  
  if (isShortInt(boxedVal)) {
    *keyCode = (short)SCHEME_INT_VAL(boxedVal);
  }

  return S_OK;
}

STDMETHODIMP CSink::MouseDown(short button, short shift, long x, long y)
{
  Scheme_Object *argv[4];

  if (mousedown_proc == NULL) {
    return S_OK;  
  }

  argv[0] = scheme_make_integer((long)button);
  argv[1] = scheme_make_integer((long)shift);
  argv[2] = scheme_make_integer_value(x);
  argv[3] = scheme_make_integer_value(y);

  scheme_apply(mousedown_proc,4,argv);

  return S_OK;
}

STDMETHODIMP CSink::MouseMove(short button, short shift, long x, long y)
{
  Scheme_Object *argv[4];

  if (mousemove_proc == NULL) {
    return S_OK;  
  }

  argv[0] = scheme_make_integer((long)button);
  argv[1] = scheme_make_integer((long)shift);
  argv[2] = scheme_make_integer_value(x);
  argv[3] = scheme_make_integer_value(y);

  scheme_apply(mousedown_proc,4,argv);

  return S_OK;
}

STDMETHODIMP CSink::MouseUp(short button, short shift, long x, long y)
{
  Scheme_Object *argv[4];

  if (mouseup_proc == NULL) {
    return S_OK;  
  }

  argv[0] = scheme_make_integer((long)button);
  argv[1] = scheme_make_integer((long)shift);
  argv[2] = scheme_make_integer_value(x);
  argv[3] = scheme_make_integer_value(y);

  scheme_apply(mousedown_proc,4,argv);

  return S_OK;
}

STDMETHODIMP CSink::Error(void)
{  

  if (error_proc == NULL) {
    return S_OK;  
  }

  scheme_apply(error_proc,0,NULL);

  return S_OK;
}

STDMETHODIMP CSink::ReadyStateChange(long readyState)
{
  Scheme_Object *argv[1];

  if (readystatechange_proc == NULL) {
    return S_OK;  
  }

  argv[0] = scheme_make_integer_value(readyState);

  scheme_apply(mousedown_proc,1,argv);
   
  return S_OK;
}


// stock event handler updaters

STDMETHODIMP CSink::set_click_proc(int p)
{
  click_proc = (Scheme_Object *)p;

	return S_OK;
}

STDMETHODIMP CSink::set_dblclick_proc(int p)
{
  dblclick_proc = (Scheme_Object *)p;

	return S_OK;
}

STDMETHODIMP CSink::set_keydown_proc(int p)
{
  keydown_proc = (Scheme_Object *)p;

	return S_OK;
}

STDMETHODIMP CSink::set_keypress_proc(int p)
{
  keypress_proc = (Scheme_Object *)p;

	return S_OK;
}

STDMETHODIMP CSink::set_keyup_proc(int p)
{
  keyup_proc = (Scheme_Object *)p;

	return S_OK;
}

STDMETHODIMP CSink::set_mousedown_proc(int p)
{
  mousedown_proc = (Scheme_Object *)p;

	return S_OK;
}

STDMETHODIMP CSink::set_mousemove_proc(int p)
{
  mousemove_proc = (Scheme_Object *)p;

	return S_OK;
}

STDMETHODIMP CSink::set_mouseup_proc(int p)
{
  mouseup_proc = (Scheme_Object *)p;

	return S_OK;
}

STDMETHODIMP CSink::set_error_proc(int p)
{
  error_proc = (Scheme_Object *)p;

	return S_OK;
}

STDMETHODIMP CSink::set_readystatechange_proc(int p)
{
  readystatechange_proc = (Scheme_Object *)p;

	return S_OK;
}


