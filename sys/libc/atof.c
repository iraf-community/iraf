/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#define	import_ctype
#include <iraf.h>


/* ATOF -- Ascii to double floating.  Convert any legal floating point number
** into a binary double precision floating value.
*/
double
atof (char *str)
{
	XINT	ip = 1;
	XDOUBLE dval;

	if (CTOD (c_sppstr(str), &ip, &dval) == 0)
	    return ( (double) 0);
	else
	    return ( (double) dval);
}
