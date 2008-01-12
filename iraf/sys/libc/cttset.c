/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_STTYCO -- Set terminal driver options via a command string.
 */
void c_sttyco ( const char *args, int ttin, int ttout, int outfd )
{
	XCHAR x_args[SZ_COMMAND];
	XINT x_ttin = ttin;
	XINT x_ttout = ttout;
	XINT x_outfd = outfd;

	c_strupk (args, x_args, SZ_COMMAND);
	STTYCO (x_args, &x_ttin, &x_ttout, &x_outfd);
}


/* C_TTSETI -- FIO set integer terminal driver parameter.
 */
/* fd    : FIO file descriptor */
/* param : param to be set     */
/* value : new value           */
void c_ttseti ( int fd, int param, int value )
{
	XINT x_fd = fd;
	XINT x_param = param;
	XINT x_value = value;

	TTSETI (&x_fd, &x_param, &x_value);
}


/* C_TTSTATI -- FIO stat integer terminal driver parameter.
 */
/* fd    : FIO file descriptor */
/* param : param to be set     */
int c_ttstati ( int fd, int param )
{
	XINT x_fd = fd;
	XINT x_param = param;

	return (TTSTATI (&x_fd, &x_param));
}

/* C_TTSETS -- FIO set string terminal driver parameter.
 */
/* fd    : FIO file descriptor */
/* param : param to be set     */
/* value : new value           */
void c_ttsets ( int fd, int param, const char *value )
{
	XINT x_fd = fd;
	XINT x_param = param;

	TTSETS (&x_fd, &x_param, c_sppstr (value));
}


/* C_TTSTATS -- FIO stat string terminal driver parameter.
 */
/* fd     : FIO file descriptor   */
/* param  : param to be set       */
/* outstr : receives string value */
ssize_t c_ttstats ( int fd, int param, char *outstr, size_t bufsize )
{
	XCHAR	x_sval[SZ_LINE+1];
	XINT	x_maxch = SZ_LINE;
	XINT	x_fd = fd;
	XINT	x_param = param;
	XINT	nchars;

	nchars = TTSTATS (&x_fd, &x_param, x_sval, &x_maxch);
	c_strpak (x_sval, outstr, bufsize);
	return (bufsize-1 < nchars ? bufsize-1 : nchars);
}
