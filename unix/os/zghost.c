/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

/* ZGHOST -- Get the network name of the host computer.
 */
ZGHOST (outstr, maxch)
PKCHAR	*outstr;		/* receives host name		*/
XINT	*maxch;
{
	gethostname ((char *)outstr, *maxch);
	((char *)outstr)[*maxch] = EOS;
}
