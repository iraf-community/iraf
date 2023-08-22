/* Copyright 2006-2009 Chisato Yamauchi (C-SODA/ISAS/JAXA)
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* I32TO64 - Convert big endian 32bit integer array into 64bit.
 */
int
I32TO64 (void *a, void *b, XINT *nelems)
{
	XINT  i, j, k;
	char  *ip = (char *) a, 
	      *op = (char *) b;


	j = *nelems * 8;
	k = *nelems * 4;

	if ( ip < op ) {
	    for ( i = k ; 0 < i ; i-- )
		op[i-1] = ip[i-1];
	}
	else if ( op < ip ) {
	    for ( i = 0 ; i < k ; i++ )
		op[i] = ip[i];
	}

	for ( i=0 ; i < *nelems ; i++ ) {
	    char pad;
	    op[--j] = op[--k];
	    op[--j] = op[--k];
	    op[--j] = op[--k];
	    op[--j] = op[--k];
	    if ( (op[k] & 0x080) != 0 ) pad = 0x0ff;
	    else pad = 0;
	    op[--j] = pad;
	    op[--j] = pad;
	    op[--j] = pad;
	    op[--j] = pad;
	}

	return 0;
}
