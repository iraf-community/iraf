/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>

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
	int fill;


	/* Move the input data to the output array so we can
	 * do an in-place conversion.
	 */
	memmove ((void *)op, (void *)ip, *nelems * 8);

	j = (*nelems) * 8 - 4;
	k = (*nelems) * 4 - 4;
	for (i=(*nelems - 1); i >= 0; i--) {
	    memmove ((void *)&op[j], (void *)&ip[k], sizeof (int));
	    fill = ((ip[k] & 0x080) != 0) ? 0x0ff : 0;
	    memset ((void *)&op[j-4], (int)fill, 4);
	    k -= 4, j -= 8;
	}

	return 0;
}
