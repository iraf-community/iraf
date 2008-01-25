/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHT_B -- Pack SPP array into an unsigned byte array.
 * [MACHDEP]: The underscore appended to the procedure name is OS dependent.
 */
int ACHTXB ( XCOMPLEX *a, XUBYTE *b, XINT *npix )
{
	XCOMPLEX *ip;
	XUBYTE *op, *maxop;

	maxop = b + *npix -1;
	for ( ip=a, op=b ; op <= maxop ; op++, ip++ ) {
	    *op = (XUBYTE) ip->r;
	}

	return 0;
}
