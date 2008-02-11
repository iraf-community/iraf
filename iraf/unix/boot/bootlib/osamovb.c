/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <unistd.h>

#define import_spp
#define import_knames
#include <iraf.h>

/* OS_AMOVB -- Memory to memory copy using BYTMOV.
 */
void os_amovb ( const char *a, char *b, size_t nbytes )
{
	XCHAR	*a_wp, *b_wp;
	XSIZE_T	a_off, b_off, x_nbytes = nbytes;

	a_wp = (XCHAR *)a;
	b_wp = (XCHAR *)b;

	/* The following offsets can be something other than one if the
	 * buffers are not word aligned.
	 */
	a_off = a - (const char *)a_wp + 1;
	b_off = b - (char *)b_wp + 1;
	
	BYTMOV (a_wp, &a_off, b_wp, &b_off, &x_nbytes);
}
