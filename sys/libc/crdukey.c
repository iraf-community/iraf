/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_RDUKEY -- Read a user keystroke object from the terminal in raw mode.
 */
c_rdukey (obuf, maxch)
char	*obuf;			/* output buffer	*/
int	maxch;			/* maxc chars out	*/
{
	XCHAR	buf[SZ_LINE+1];
	XINT	x_maxch = SZ_LINE;
	int	status;

	obuf[0] = EOS;
	if ((status = RDUKEY (buf, &x_maxch)) > 0)
	    c_strpak (buf, obuf, maxch);

	return (status);
}
