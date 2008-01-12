/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_RDUKEY -- Read a user keystroke object from the terminal in raw mode.
 */
/* obuf    : output buffer       */
/* bufsize : buffer size of obuf */
int c_rdukey ( char *obuf, size_t bufsize )
{
	XCHAR	buf[SZ_LINE+1];
	XINT	x_maxch = SZ_LINE;
	XINT	status;

	obuf[0] = EOS;
	if ((status = RDUKEY (buf, &x_maxch)) > 0)
	    c_strpak (buf, obuf, bufsize);

	return (status);
}
