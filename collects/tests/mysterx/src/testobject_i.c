/* this file contains the actual definitions of */
/* the IIDs and CLSIDs */

/* link this file in with the server and any clients */


/* File created by MIDL compiler version 5.01.0164 */
/* at Thu Jun 17 16:29:01 1999
 */
/* Compiler settings for testobject.idl:
    Oicf (OptLev=i2), W1, Zp8, env=Win32, ms_ext, c_ext
    error checks: allocation ref bounds_check enum stub_data 
*/
//@@MIDL_FILE_HEADING(  )
#ifdef __cplusplus
extern "C"{
#endif 


#ifndef __IID_DEFINED__
#define __IID_DEFINED__

typedef struct _IID
{
    unsigned long x;
    unsigned short s1;
    unsigned short s2;
    unsigned char  c[8];
} IID;

#endif // __IID_DEFINED__

#ifndef CLSID_DEFINED
#define CLSID_DEFINED
typedef IID CLSID;
#endif // CLSID_DEFINED

const IID IID_ITestControl = {0x07B31FFC,0x19EE,0x11D3,{0xB5,0xDB,0x00,0x60,0x08,0x90,0x02,0xFE}};


const IID LIBID_TESTOBJECTLib = {0x07B31FF0,0x19EE,0x11D3,{0xB5,0xDB,0x00,0x60,0x08,0x90,0x02,0xFE}};


const IID DIID__ITestControlEvents = {0x07B31FFD,0x19EE,0x11D3,{0xB5,0xDB,0x00,0x60,0x08,0x90,0x02,0xFE}};


const CLSID CLSID_TestControl = {0xFED8FE26,0x19CA,0x11D3,{0xB5,0xDB,0x00,0x60,0x08,0x90,0x02,0xFE}};


#ifdef __cplusplus
}
#endif

