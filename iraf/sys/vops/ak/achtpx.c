/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHTP_ -- Pack an SPP datatype array.
 */
int ACHTPX ( XPOINTER *a, XCOMPLEX *b, XINT *npix )
{
	XPOINTER *ip;
	XCOMPLEX *op;

	if ( sizeof(*ip) < sizeof(*op) ) {
	    for ( ip = a + *npix, op = b + *npix ; b < op ; ) {
		--op; --ip;
		op->r = (float)(*ip);
		op->i = 0;
	    }
	} else {
	    XCOMPLEX *maxop = b + *npix -1;
	    for ( ip=a, op=b ; op <= maxop ; op++, ip++ ) {
		op->r = (float)(*ip);
		op->i = 0;
	    }
	}

	return 0;
}
