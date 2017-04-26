/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZGTPID -- Get process id number (used for process control and to make
 * unique file names).
 */
int
ZGTPID (XINT *pid)
{
	*pid = (XINT) getpid();
	return (XOK);
}
