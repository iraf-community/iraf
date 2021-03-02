/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <time.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

#define SECONDS_1970_TO_1980    315532800L

/* ZGMTCO -- Return the correction, in seconds, from local standard time
 * (clock time) to GMT.   GMT = LST + gmtco (seconds), or gmtco = GMT-LST.
 */
int
ZGMTCO (
  XINT	*gmtcor				/* seconds */
)
{
	time_t gmt_to_lst(), ltime;

	/* Given an input value of zero (biased by SECONDS_1970_TO_1980)
	 * gmt_to_lst will return a negative value in seconds for a location
	 * in the US (as an limiting test case).  We want to return the
	 * correction to LST to get GMT, a positive value for the US, so
	 * we need to negate this value.  gmt_to_lst will already have taken
	 * daylight savings time into account.  Although we talk about the
	 * US (as a test case) this relation will hold both east and west
	 * of Greenwich.
	 */

	*gmtcor = -((XINT) gmt_to_lst ((time_t) SECONDS_1970_TO_1980));
	return (XOK);
}
