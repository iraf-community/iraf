/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

/* ZLOCVA -- Return the address of a variable or array element in XCHAR units.
 * Must be able to do signed arithmetic on the integer value returned.
 * We ASSUME that XCHAR through XDOUBLE are addressed in the same units.
 * The transformation from a machine address into a "location" is machine
 * dependent, and is given by the macro ADDR_TO_LOC defined in kernel.h.
 */
int ZLOCVA ( void *variable, XPOINTER *location )
{

	*location = ADDR_TO_LOC (variable);

	return XOK;
}
