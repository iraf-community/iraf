/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */
#include <string.h>

#define import_spp
#define import_knames
#include <iraf.h>

/* BYTMOV -- Byte move from array "a" to array "b".  The move must be
 * nondestructive, allowing a byte array to be shifted left or right a
 * few bytes, hence comparison of the addresses of the arrays is necessary
 * to determine if they overlap.
 */
/* a      : input byte array			*/
/* aoff   : first byte in A to be moved		*/
/* b      : output byte array			*/
/* boff   : first byte in B to be written	*/
/* nbytes : number of bytes to move		*/
int BYTMOV ( XCHAR *a, XINT *aoff, XCHAR *b, XINT *boff, XINT *nbytes )
{
	if ((a + *aoff) != (b + *boff))
	    memmove ((char *)b + (*boff-1), (char *)a + (*aoff-1), *nbytes);
	return 0;
}
