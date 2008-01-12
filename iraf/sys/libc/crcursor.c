/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_RCURSOR -- Read a cursor.
 */
/* fd      : FIO file descriptor   */
/* bufsize : buffer size of outstr */
int c_rcursor ( int fd, char *outstr, size_t bufsize )
{
	XCHAR	buf[SZ_LINE+1];
	XINT	key;
	XINT	x_fd = fd;
	XINT	x_maxch = SZ_LINE;

	key = RCURSOR (&x_fd, buf, &x_maxch);
	c_strpak (buf, outstr, bufsize);

	return (key == XEOF ? EOF : key);
}
