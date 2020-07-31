/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */
#include <stdio.h>
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
    time_t gmt = 315532800; /* The value doesn't matter here; we take 1980-01-01 */
    *gmtcor = gmt - gmt_to_lst(gmt);
    return (XOK);
}
