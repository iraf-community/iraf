/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <stdlib.h>
#include <iraf.h>


/* ISCL64 - Scale a pixel array stored as SPP chars to the desired type.
 */
ISCL64 (a, b, npix, bscale, bzero)
XCHAR	*a;			/* input array			*/
XCHAR	*b;			/* output array			*/
XINT	*npix;			/* number of bytes to swap	*/
XDOUBLE *bscale, *bzero;	/* scaling factors		*/
{
    int   i, pix;
    int   *ip = (int *) a;
    double *dp = (double *) calloc (*npix, sizeof (double));
    double *tmp;

    tmp = dp;
    for (i=0; i < *npix; i++) {
	pix = *ip;
	*tmp = (double) (pix * (*bscale) + (*bzero));
	tmp++, ip++;
    }

    memmove (b, dp, (*npix * sizeof (double)));
}
