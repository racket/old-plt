/* Link this file to gcc-compiled Win32 MzScheme extensions */

/* init.cc for WIN32.   Copyright 1996 Cygnus Solutions
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include <windows.h> 

extern "C" {
  int WINAPI dll_entry (HANDLE h, DWORD reason, void *ptr);
};

/* Magic that's needed if we use the Cygwin API */
/* This is a global variable. */
struct _reent *_impure_ptr;
extern struct _reent *__imp_reent_data;

int WINAPI dll_entry (HANDLE, DWORD reason, void *){
  _impure_ptr = __imp_reent_data; /* last bit of magic */
  switch (reason) {
  case DLL_PROCESS_ATTACH:
    break;
  case DLL_PROCESS_DETACH:
    break;
  case DLL_THREAD_ATTACH:
    break;
  case DLL_THREAD_DETACH:
    break;
  }
  return 1;
}
