/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */
#include <string.h>

#define import_spp
#define import_knames
#include <iraf.h>

/* AMOVI -- Copy a block of memory.
 */
void AMOVI ( XINT *a, XINT *b, XINT *n )
{
	if (a != b)
	    memmove ((char *)b, (char *)a, *n * sizeof(*a));
}
