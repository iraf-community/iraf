/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>
#include <stdlib.h>

#define import_spp
#define import_knames
#include <iraf.h>

/* I64TO32 - Convert big endian 64bit integer array into 32bit.
 */
int
I64TO32 (void *a, void *b, XINT *nelems)
{
	XINT *ip = (XINT *)a;
	int *op = (int *) calloc (*nelems, sizeof (int));
	int *tmp = (int *)NULL, i;

	tmp = op;
	for (i=0 ; i < *nelems ; i++, ip++) {
	    *tmp++ = (int) (*ip >> 32);
	}
	memmove (b, op, *nelems * sizeof(int));

	free ((void *) op);
	return 0;
}
