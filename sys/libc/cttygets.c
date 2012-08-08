/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

#define	SZ_CAPSTR	128
static	XCHAR	buf[SZ_CAPSTR];
static	XINT	szbuf = 128;


/* C_TTYGETS -- Get the value of a termcap capability as a character string,
** suitable for subsequent output to the device with TTYPUTS (assuming the
** capability is a control function).  The number of characters in the output
** string is returned as the function value.
*/
int
c_ttygets (
  XINT	tty,			/* tty descriptor		*/
  char	*cap,			/* two char capability name	*/
  char	*outstr,		/* output string		*/
  int	maxch			/* max chars out, excl EOS	*/
)
{
	XINT  x_tty = tty;
	int	nchars;

	nchars = TTYGETS (&x_tty, c_sppstr(cap), buf, &szbuf);
	c_strpak (buf, outstr, maxch);

	return (nchars);
}
