#ifndef __GIFDECOD_INC__
#define __GIFDECOD_INC__

#ifdef __cplusplus
extern "C"{       
#endif

	HANDLE 	FAR PASCAL _export Gif_File_To_DIB(const char *);
//	BOOL 	FAR PASCAL _export GIFEncode(const char* FName,LPSTR lpDIB);
	BOOL 	GIFEncode(const char* FName,LPSTR lpDIB);
#ifdef __cplusplus		
}		          
#endif

        
        
#endif //__GIFSAVE_INC__
