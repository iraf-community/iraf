/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHT_P -- Pack an SPP datatype array.
 */
int ACHTXP ( XCOMPLEX *a, XPOINTER *b, XSIZE_T *npix )
{
	XCOMPLEX *ip;
	XPOINTER *op;

	if ( sizeof(*ip) < sizeof(*op) ) {
	    for ( ip = a + *npix, op = b + *npix ; b < op ; ) {
		--op; --ip;
		*op = (XPOINTER) (ip->r);
	    }
	} else {
	    XPOINTER *maxop = b + *npix -1;
	    for ( ip=a, op=b ; op <= maxop ; op++, ip++ ) {
		*op = (XPOINTER) (ip->r);
	    }
	}

	return 0;
}
