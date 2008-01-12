/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>


/* C_CNVDATE -- Convert long integer time as returned by CLKTIME into a short
 * format date string.
 */
/* clktime : seconds since jan.1,1980 */
/* outstr  : encoded time string      */
char *c_cnvdate ( long clktime, char *outstr, size_t bufsize )
{
	XCHAR	buf[SZ_LINE+1];
	XINT	x_maxch = SZ_LINE;

	CNVDATE (&clktime, buf, &x_maxch);
	return (c_strpak (buf, outstr, bufsize));
}
