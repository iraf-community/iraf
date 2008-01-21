/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */
#include <string.h>

#define import_spp
#define import_knames
#include <iraf.h>

/* AMOVP -- Copy a block of memory.
 */
int AMOVP ( XPOINTER *a, XPOINTER *b, XINT *n )
{
	if (a != b)
	    memmove ((char *)b, (char *)a, *n * sizeof(*a));
	return 0;
}
