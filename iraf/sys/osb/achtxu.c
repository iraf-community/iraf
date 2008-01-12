/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHT_U -- Pack an SPP datatype array into an unsigned short integer.
 * [MACHDEP]: The underscore appended to the procedure name is OS dependent.
 */
int ACHTXU ( XCOMPLEX *a, XUSHORT *b, XINT *npix )
{
	XCOMPLEX *ip;
	XUSHORT *op, *maxop;

	if (sizeof(*op) > sizeof(*ip)) {
	    for ( ip = &a[*npix], op = &b[*npix] ; a < ip ; )
		    *--op = (int) (--ip)->r;
	} else {
	    maxop = b + *npix -1;
	    for ( ip=a, op=b ; op <= maxop ; )
		    *op++ = (int) (ip++)->r;
	}

	return 0;
}
