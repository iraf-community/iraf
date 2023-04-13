/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>
#include <stdlib.h>

#define	import_spp
#define import_knames
#include <iraf.h>


/* ISCL32 - Scale a pixel array stored as SPP chars to the desired type.
 */
void
ISCL32 (
    XCHAR	*a,		/* input array			*/
    XCHAR	*b,		/* output array			*/
    XINT	*npix,		/* number of bytes to swap	*/
    XDOUBLE *bscale, 
    XDOUBLE *bzero		/* scaling factors		*/
)
{
    int   i, pix;
    int   *ip = (int *) a;
    float *rp = (float *) calloc (*npix, sizeof (float));
    float *tmp;

    tmp = rp;
    for (i=0; i < *npix; i++) {
	pix = *ip;
	*tmp = (float) (pix * (*bscale) + (*bzero));
	tmp++, ip++;
    }

    memmove (b, rp, (*npix * sizeof (float)));
}
