/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* ANDI -- Bitwise boolean AND of two integer variables.
 */
XINT
ANDI (a, b)
XINT	*a, *b;
{
	return (*a & *b);
}


/* ANDS -- Bitwise boolean AND of two short integer variables.
 */
XSHORT
ANDS (a, b)
XSHORT	*a, *b;
{
	return (*a & *b);
}


/* ANDL -- Bitwise boolean AND of two long integer variables.
 */
XLONG
ANDL (a, b)
XLONG	*a, *b;
{
	return (*a & *b);
}
