/* I can't find PreCarbon.o, so I'm just going to write CopyPascalStringToC myself, to have
   the same interface that the Carbon version has.  This way, the same source code will 
   work in carbon; just take out this file. */
   
void CopyPascalStringToC(ConstStr255Param src, char *dst)
{
	int string_len = (short)(*((char *)src));
	int i;
	/* I can't believe there's no !@#$ strncpy defined in the libs MrEd uses. */
	for (i=0;i<string_len;i++) {
		dst[i] = ((char *)src)[i+1];
	} 
	dst[string_len] = (char)0;
}