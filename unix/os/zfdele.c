/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

#include "osproto.h"

/* ZFDELE -- Delete a file.
 */
int
ZFDELE (
  PKCHAR  *fname,
  XINT	  *status
)
{
	if (unlink ((char *)fname) == ERR)
	    *status = XERR;
	else
	    *status = XOK;

	return (*status);
}
