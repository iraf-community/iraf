/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifdef SYSV
#include <time.h>
#else
#include <sys/time.h>
#include <sys/timeb.h>
#endif
#include <utime.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

#define SECONDS_1970_TO_1980    315532800L


/* ZFUTIM -- Set the file access/modification times.  Times are set in
 * in units of seconds since 00:00:00 01-Jan-80, local time, as returned
 * by ZFINFO.  A NULL time value will not modify the field.
 */
ZFUTIM (fname, atime, mtime, status)
PKCHAR	*fname;
XLONG	*atime;
XLONG	*mtime;
XINT	*status;
{
	struct	stat osfile;
	struct	utimbuf time;
	time_t	gmt = 0, time_var;
	int	offset = 0;
	int	stat(), utime();

	/* Get UNIX file info.
	 */
	if (stat ((char *)fname, &osfile) == ERR) {
	    *status = XERR;
	    return;
	}

	/* Get the timezone offset.  Correct for daylight savings time,
	 * if in effect.
	 */
	ZGMTCO (&offset);
	offset += SECONDS_1970_TO_1980;

	/* Set file access times.  If time is NULL use the current value.
	 */
	time.actime  = ((*atime == NULL) ? osfile.st_atime : (*atime+offset));
	time.modtime = ((*mtime == NULL) ? osfile.st_mtime : (*mtime+offset));

	if (utime ((char *)fname, &time) == ERR) {
	    *status = XERR;
	    return;
	}
	*status = XOK;
}
