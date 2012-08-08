/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* ACHTB_ -- Unpack an unsigned byte array into an SPP array.
 * The loop runs in the reverse direction so that the unpack can be
 * performed in place (a and b can be the same array).
 */
void
ACHTBD (
  XCHAR		*a,
  XDOUBLE	*b,
  XINT		*npix
)
{
	register XUBYTE	*ip, *first = (XUBYTE *)a;
	register XDOUBLE	*op;

	for (ip = &first[*npix], op = &((XDOUBLE *)b)[*npix];  ip > first;  )
		*--op = *--ip;
}
