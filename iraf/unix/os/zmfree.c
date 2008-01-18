/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

/* ZMFREE -- Return heap space previously allocated by ZMALOC or ZRALOC.
 * The manual page for FREE says nothing about error checking, so we do
 * not look at the return value.
 */
int ZMFREE ( XPOINTER *buf, XINT *status )
{

	free (LOC_TO_ADDR (*buf, char));
	*status = XOK;

	return *status;
}
