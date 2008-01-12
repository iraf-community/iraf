/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#define import_error
#include <iraf.h>

/* C_ERRGET -- Get the error code and error message string of the most
 * recent error.
 */
/* outstr  : error message string  */
/* bufsize : buffer size of outstr */
int c_errget ( char *outstr, size_t bufsize )
{
	XCHAR	buf[SZ_LINE+1];
	XINT	szbuf = SZ_LINE;
	int	errcode;

	errcode = ERRGET (buf, &szbuf);
	c_strpak (buf, outstr, bufsize);

	return (errcode);
}
