/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>


/* OS_AMOVB -- Memory to memory copy using BYTMOV.
 */
void
os_amovb (
  char	*a,
  char	*b,
  int	nbytes
)
{
	XCHAR	*a_wp, *b_wp;
	XINT	a_off, b_off;
	XINT	n_wp;

	extern void BYTMOV(XCHAR *a, XINT *aoff, XCHAR *b, XINT *boff, XINT *nbytes);


	a_wp = (XCHAR *)a;
	b_wp = (XCHAR *)b;
	n_wp = nbytes;

	/* The following offsets can be something other than one if the
	 * buffers are not word aligned.
	 */
	a_off = a - (char *)a_wp + 1;
	b_off = b - (char *)b_wp + 1;
	
	BYTMOV (a_wp, &a_off, b_wp, &b_off, &n_wp);
}
