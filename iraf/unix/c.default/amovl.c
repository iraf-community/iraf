/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */
#include <string.h>

#define import_spp
#define import_knames
#include <iraf.h>

/* AMOVL -- Copy a block of memory.
 */
int AMOVL ( XLONG *a, XLONG *b, XINT *n )
{
	if (a != b)
	    memmove ((char *)b, (char *)a, *n * sizeof(*a));
	return 0;
}
