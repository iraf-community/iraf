/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_STTYCO -- Set terminal driver options via a command string.
 */
c_sttyco (args, ttin, ttout, outfd)
char	*args;
int	ttin, ttout;
int	outfd;
{
	XCHAR	x_args[SZ_COMMAND];

	c_strupk (args, x_args, SZ_COMMAND);
	STTYCO (x_args, &ttin, &ttout, &outfd);
}


/* C_TTSETI -- FIO set integer terminal driver parameter.
 */
c_ttseti (fd, param, value)
int	fd;			/* FIO file descriptor		*/
int	param;			/* param to be set		*/
int	value;			/* new value			*/
{
	TTSETI (&fd, &param, &value);
}


/* C_TTSTATI -- FIO stat integer terminal driver parameter.
 */
c_ttstati (fd, param)
int	fd;			/* FIO file descriptor		*/
int	param;			/* param to be set		*/
{
	return (TTSTATI (&fd, &param));
}

/* C_TTSETS -- FIO set string terminal driver parameter.
 */
c_ttsets (fd, param, value)
int	fd;			/* FIO file descriptor		*/
int	param;			/* param to be set		*/
char	*value;			/* new value			*/
{
	TTSETS (&fd, &param, c_sppstr (value));
}


/* C_TTSTATS -- FIO stat string terminal driver parameter.
 */
c_ttstats (fd, param, outstr, maxch)
int	fd;			/* FIO file descriptor		*/
int	param;			/* param to be set		*/
char	*outstr;		/* receives string value	*/
int	maxch;
{
	XCHAR	x_sval[SZ_LINE+1];
	XINT	x_maxch = SZ_LINE;
	int	nchars;

	nchars = TTSTATS (&fd, &param, x_sval, &x_maxch);
	c_strpak (x_sval, outstr, maxch);
	return (maxch < nchars ? maxch : nchars);
}
