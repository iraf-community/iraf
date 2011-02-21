/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* ANDI -- Bitwise boolean AND of two integer variables.
 */
XINT
ANDI (XINT *a, XINT *b)
{
	return (*a & *b);
}


/* ANDS -- Bitwise boolean AND of two short integer variables.
 */
XSHORT
ANDS (XSHORT *a, XSHORT *b)
{
	return (*a & *b);
}


/* ANDL -- Bitwise boolean AND of two long integer variables.
 */
XLONG
ANDL (XLONG *a, XLONG *b)
{
	return (*a & *b);
}
