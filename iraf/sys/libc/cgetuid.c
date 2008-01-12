/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_GETUID -- Get the user identification string (user name).
 */
/* outstr  : user name, C string   */
/* bufsize : buffer size of outstr */
char *c_getuid ( char *outstr, size_t bufsize )
{
	XCHAR	spp_uid[SZ_FNAME+1];
	XINT	x_maxch = SZ_FNAME;

	GETUID (spp_uid, &x_maxch);
	return (c_strpak (spp_uid, outstr, bufsize));
}
