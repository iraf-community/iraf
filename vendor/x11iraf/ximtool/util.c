/*
 * UTIL.C -- Utility routines for the package.
 *
 *	   bswap2  (a, b, nbytes)
 *	   bswap4  (a, aoff, b, boff, nbytes)
 *	   bswap8  (a, aoff, b, boff, nbytes)
 *	     flip  (buffer, nx, ny)
 *     is_swapped  ()
 *	  min_max  (a, npts, bitpix, min, max)
 *	   strpak  (in, out, len)
 *
 *
 */


#include <stdio.h>
#ifdef ULTRIX
#include <sys/types.h>
#endif
#include <unistd.h>

#ifndef AIXV3
#ifndef OSF1
typedef unsigned char uchar;
#endif
#endif



/* BSWAP2 - Move bytes from array "a" to array "b", swapping successive
 * pairs of bytes.  The two arrays may be the same but may not be offset
 * and overlapping.
 */
bswap2 (a, b, nbytes)
char    *a, *b;         	/* input array                  */
int     nbytes;         	/* number of bytes to swap      */
{
        register char *ip=a, *op=b, *otop;
        register unsigned temp;

        /* Swap successive pairs of bytes.
         */
        for (otop = op + (nbytes & ~1);  op < otop;  ) {
            temp  = *ip++;
            *op++ = *ip++;
            *op++ = temp;
        }

        /* If there is an odd byte left, move it to the output array.
         */
        if (nbytes & 1)
            *op = *ip;
}


/* BSWAP4 - Move bytes from array "a" to array "b", swapping the four bytes
 * in each successive 4 byte group, i.e., 12345678 becomes 43218765.
 * The input and output arrays may be the same but may not partially overlap.
 */
bswap4 (a, aoff, b, boff, nbytes)
char	*a;			/* input array			*/
int	aoff;			/* first byte in input array	*/
char	*b;			/* output array			*/
int	boff;			/* first byte in output array	*/
int	nbytes;			/* number of bytes to swap	*/
{
	register char	*ip, *op, *tp;
	register int	n;
	static	char temp[4];

	tp = temp;
	ip = (char *)a + aoff - 1;
	op = (char *)b + boff - 1;

	/* Swap successive four byte groups.
	 */
	for (n = nbytes >> 2;  --n >= 0;  ) {
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	}

	/* If there are any odd bytes left, move them to the output array.
	 * Do not bother to swap as it is unclear how to swap a partial
	 * group, and really incorrect if the data is not modulus 4.
	 */
	for (n = nbytes & 03;  --n >= 0;  )
	    *op++ = *ip++;
}


/* BSWAP8 - Move bytes from array "a" to array "b", swapping the eight bytes
 * in each successive 8 byte group, i.e., 12345678 becomes 87654321.
 * The input and output arrays may be the same but may not partially overlap.
 */
bswap8 (a, aoff, b, boff, nbytes)
char	*a;			/* input array			*/
int	aoff;			/* first byte in input array	*/
char	*b;			/* output array			*/
int	boff;			/* first byte in output array	*/
int	nbytes;		/* number of bytes to swap	*/
{
	register char	*ip, *op, *tp;
	register int	n;
	static	char temp[8];

	tp = temp;
	ip = (char *)a + aoff - 1;
	op = (char *)b + boff - 1;

	/* Swap successive eight byte groups.
	 */
	for (n = nbytes >> 3;  --n >= 0;  ) {
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	}

	/* If there are any odd bytes left, move them to the output array.
	 * Do not bother to swap as it is unclear how to swap a partial
	 * group, and really incorrect if the data is not modulus 8.
	 */
	for (n = nbytes & 03;  --n >= 0;  )
	    *op++ = *ip++;
}


/* IS_SWAPPED -- See if this is a byte-swapped (relative to Sun) machine.
 */

is_swapped ()
{
        union {
            char ch[4];
            int  i;
        } u;

        u.i = 1;
        return ((int) u.ch[0]);
}


/* MIN_MAX -- Get the min and max values of an array.
 */

min_max (a, npts, bitpix, min, max)
char	*a;
int	npts;
int	bitpix;
float	*min, *max;
{
	register int i;

	*min =  32768.0, *max = -32768.0;

	if (bitpix ==   8) {
	    char *buf = (char *)a;
	    for (i=0; i < npts; i++) {
	        if (buf[i] < *min) *min = buf[i];
	        if (buf[i] > *max) *max = buf[i];
	    }
	} else if (bitpix ==  16) {
	    short *buf = (short *)a;
	    for (i=0; i < npts; i++) {
	        if (buf[i] < *min) *min = buf[i];
	        if (buf[i] > *max) *max = buf[i];
	    }
	} else if (bitpix ==  32) {
	    int *buf = (int *)a;
	    for (i=0; i < npts; i++) {
	        if (buf[i] < *min) *min = buf[i];
	        if (buf[i] > *max) *max = buf[i];
	    }
	} else if (bitpix == -32) {
	    float *buf = (float *)a;
	    for (i=0; i < npts; i++) {
	        if (buf[i] < *min) *min = buf[i];
	        if (buf[i] > *max) *max = buf[i];
	    }
	} else if (bitpix == -64) {
	    double *buf = (double *)a;
	    for (i=0; i < npts; i++) {
	        if (buf[i] < *min) *min = buf[i];
	        if (buf[i] > *max) *max = buf[i];
	    }
	}
}


/* STRPAK -- Convert ASCII string from SPP char per short to C char per byte
 */

strpak (in, out, len)
char	*in, *out;
int	len;
{
	int	i, j;

	/* adaptive byte selection (whichever byte order) chars alternate 
	 * with \0 
	 */
	if ( in[0] == '\0' )
	    j = 1;
	else
	    j = 0;
	for ( i = 0; i < len; i++, j += 2 )
	    out[i] = in[j];
	out[i] = '\0';
}


/* FLIP -- Reverse order of lines in image.  We do this as a separate step
 * rather than when reading so the sampling grid for zscale is the same as
 * for DISPLAY, even though the pixels are stored "flipped" in the pixfile.
 */
flip (buffer, nx, ny)
uchar *buffer;
int  nx, ny;
{
        int     i;
        register int    j, v;
        register uchar *buff1, *buff2;

        for (i = 0; i < ny / 2; i++) {
            buff1 = &buffer[i*nx];
            buff2 = &buffer[(ny-1-i)*nx];
            for (j = 0; j < nx; j++) {
                v = *buff1;
                *(buff1++) = *buff2;
                *(buff2++) = v;
            }
        }
}    
