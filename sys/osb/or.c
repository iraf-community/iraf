/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* ORI -- Bitwise boolean OR of two integer variables.
 */
XINT
ORI (a, b)
XINT	*a, *b;
{
	return (*a | *b);
}


/* ORS -- Bitwise boolean OR of two short integer variables.
 */
XSHORT
ORS (a, b)
XSHORT	*a, *b;
{
	return (*a | *b);
}


/* ORL -- Bitwise boolean OR of two long integer variables.
 */
XLONG
ORL (a, b)
XLONG	*a, *b;
{
	return (*a | *b);
}
