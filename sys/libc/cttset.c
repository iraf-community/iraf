/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_STTYCO -- Set terminal driver options via a command string.
*/
void
c_sttyco (
  char	*args,
  XINT	ttin, 
  XINT  ttout,
  XINT	outfd
)
{
	XCHAR	x_args[SZ_COMMAND];
	XINT    x_ttin = ttin, x_ttout = ttout, x_outfd = outfd;

	c_strupk (args, x_args, SZ_COMMAND);
	STTYCO (x_args, &x_ttin, &x_ttout, &x_outfd);
}


/* C_TTSETI -- FIO set integer terminal driver parameter.
*/
void
c_ttseti (
  XINT	fd,			/* FIO file descriptor		*/
  int	param,			/* param to be set		*/
  int	value			/* new value			*/
)
{
	XINT  x_fd = fd, x_param = param, x_value = value;

	TTSETI (&x_fd, &x_param, &x_value);
}


/* C_TTSTATI -- FIO stat integer terminal driver parameter.
*/
int
c_ttstati (
  XINT	fd,			/* FIO file descriptor		*/
  int	param			/* param to be set		*/
)
{
	XINT  x_fd = fd, x_param = param;

	return (TTSTATI (&x_fd, &x_param));
}

/* C_TTSETS -- FIO set string terminal driver parameter.
*/
void
c_ttsets (
  XINT	fd,			/* FIO file descriptor		*/
  int	param,			/* param to be set		*/
  char	*value			/* new value			*/
)
{
	XINT  x_fd = fd, x_param = param;

	TTSETS (&x_fd, &x_param, c_sppstr (value));
}


/* C_TTSTATS -- FIO stat string terminal driver parameter.
*/
int
c_ttstats (
  XINT	fd,			/* FIO file descriptor		*/
  int	param,			/* param to be set		*/
  char	*outstr,		/* receives string value	*/
  int	maxch
)
{
	XCHAR	x_sval[SZ_LINE+1];
	XINT	x_fd = fd, x_param = param, x_maxch = SZ_LINE;
	int	nchars;

	nchars = TTSTATS (&x_fd, &x_param, x_sval, &x_maxch);
	c_strpak (x_sval, outstr, maxch);
	return (maxch < nchars ? maxch : nchars);
}
