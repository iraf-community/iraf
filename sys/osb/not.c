/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* NOTI -- Bitwise boolean NOT of an integer variable.
 */
XINT
NOTI (XINT *a)
{
	return (~(*a));
}


/* NOTS -- Bitwise boolean NOT of a short integer variable.
 */
XSHORT
NOTS (XSHORT *a)
{
	return (~(*a));
}


/* NOTL -- Bitwise boolean NOT of a long integer variable.
 */
XLONG
NOTL (XLONG *a)
{
	return (~(*a));
}
