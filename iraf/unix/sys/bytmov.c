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
/* arg_a  : input byte array			*/
/* aoff   : first byte in A to be moved		*/
/* arg_b  : output byte array			*/
/* boff   : first byte in B to be written	*/
/* nbytes : number of bytes to move		*/
int BYTMOV ( void *arg_a, XSIZE_T *aoff, void *arg_b, XSIZE_T *boff, XSIZE_T *nbytes )
{
	char *a = (char *)arg_a;
	char *b = (char *)arg_b;
	if ((a + *aoff) != (b + *boff))
	    memmove (b + (*boff-1), a + (*aoff-1), *nbytes);
	return 0;
}
