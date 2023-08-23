/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>
#include <stdlib.h>

#define	import_spp
#define import_knames
#include <stdlib.h>
#include <iraf.h>


/* ISCL64 - Scale a pixel array stored as SPP chars to the desired type.
 */
void
ISCL64 (
    XCHAR     *a,		/* input array			*/
    XCHAR     *b,		/* output array			*/
    XINT     *npix,		/* number of bytes to swap	*/
    XDOUBLE  *bscale,
    XDOUBLE  *bzero 	        /* scaling factors		*/
)
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
