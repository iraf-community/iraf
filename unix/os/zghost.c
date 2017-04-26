/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

/* ZGHOST -- Get the network name of the host computer.
 */
int
ZGHOST (
  PKCHAR  *outstr,		/* receives host name		*/
  XINT	  *maxch 
)
{
	char namebuf[SZ_FNAME];

	gethostname (namebuf, SZ_FNAME);
	strncpy ((char *)outstr, namebuf, *maxch);
	((char *)outstr)[*maxch] = EOS;

	return (XOK);
}
