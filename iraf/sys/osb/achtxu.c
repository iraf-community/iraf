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
	XUSHORT *op;

	if ( sizeof(*ip) < sizeof(*op) ) {
	    for ( ip = a + *npix, op = b + *npix ; b < op ; ) {
		--op; --ip;
		*op = (XUSHORT) (ip->r);
	    }
	} else {
	    XUSHORT *maxop = b + *npix -1;
	    for ( ip=a, op=b ; op <= maxop ; op++, ip++ ) {
		*op = (XUSHORT) (ip->r);
	    }
	}

	return 0;
}
