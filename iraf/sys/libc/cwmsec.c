/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_knames
#include <iraf.h>

/* C_WMSEC -- Delay for so may milliseconds.
 */
/* msec : milliseconds to delay */
void c_wmsec ( unsigned long msec )
{
	XINT x_msec = msec;
	ZWMSEC (&x_msec);
}
