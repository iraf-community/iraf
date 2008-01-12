/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#define import_spp
#define import_kernel
#define import_knames
#include <iraf.h>

#include "zos.h"

/* ZGHOST -- Get the network name of the host computer.
 */
/* outstr : receives host name		*/
int ZGHOST ( PKCHAR *outstr, XINT *maxch )
{
	char namebuf[SZ_FNAME];
	size_t bufsize = *maxch + 1;

	gethostname (namebuf, SZ_FNAME);
	namebuf[SZ_FNAME-1]='\0';
	safe_strcpy ((char *)outstr, bufsize, namebuf);

	return XOK;
}
