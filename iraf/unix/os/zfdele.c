/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <unistd.h>

#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

#include "zos.h"

/* ZFDELE -- Delete a file.
 */
int ZFDELE ( PKCHAR *fname, XINT *status )
{
	vm_delete ((const char *)fname, 0);
	if (unlink ((const char *)fname) == ERR)
	    *status = XERR;
	else
	    *status = XOK;

	return *status;
}
