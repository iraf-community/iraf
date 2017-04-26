/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_knames
#include <iraf.h>


/* C_WMSEC -- Delay for so may milliseconds.
*/
void
c_wmsec (
  int	msec			/* milliseconds to delay */
)
{
	XINT  x_msec = msec;

	ZWMSEC (&x_msec);
}
