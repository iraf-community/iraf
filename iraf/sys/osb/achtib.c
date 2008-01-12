/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHT_B -- Pack SPP array into an unsigned byte array.
 * [MACHDEP]: The underscore appended to the procedure name is OS dependent.
 */
int ACHTIB ( XINT *a, XCHAR *b, XINT *npix )
{
	XINT *ip;
	XUBYTE *op, *maxop;

	maxop = (XUBYTE *)b + *npix -1;
	for ( ip=(XINT *)a, op=(XUBYTE *)b ; op <= maxop ; )
		*op++ = *ip++;

	return 0;
}
