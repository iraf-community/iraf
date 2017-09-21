/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <time.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>


/* ZGMTCO -- Return the correction, in seconds, from local standard time
 * (clock time) to GMT.   GMT = LST + gmtco (seconds), or gmtco = GMT-LST.
 */
int
ZGMTCO (
  XINT	*gmtcor				/* seconds */
)
{
	time_t now = time(NULL);
	struct tm ptm;
	localtime_r(&now, &ptm);
	*gmtcor = -ptm.tm_gmtoff;
	return (XOK);
}
