/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_ERRGET -- Get the error code and error message string of the most
** recent error.
*/
int
c_errget (
  char	*outstr,		/* error message string		*/
  int	maxch			/* max chars out, incl EOS	*/
)
{
	XCHAR	buf[SZ_LINE+1];
	XINT	szbuf = SZ_LINE;
	int	errcode;

	errcode = (int) ERRGET (buf, &szbuf);
	c_strpak (buf, outstr, maxch);

	return (errcode);
}
