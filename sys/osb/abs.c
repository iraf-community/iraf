/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */
#include <stdlib.h>

#define	import_spp
#include <iraf.h>

/* ABS -- Integer absolute value.
 */
XINT
abs_ (XINT *a)
{
	return (abs(*a));
}
