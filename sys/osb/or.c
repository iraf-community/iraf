/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* ORI -- Bitwise boolean OR of two integer variables.
 */
XINT
ORI (XINT *a, XINT *b)
{
	return (*a | *b);
}


/* ORS -- Bitwise boolean OR of two short integer variables.
 */
XSHORT
ORS (XSHORT *a, XSHORT *b)
{
	return (*a | *b);
}


/* ORL -- Bitwise boolean OR of two long integer variables.
 */
XLONG
ORL (XLONG *a, XLONG *b)
{
	return (*a | *b);
}
