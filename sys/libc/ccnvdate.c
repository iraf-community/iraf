/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>


/* C_CNVDATE -- Convert long integer time as returned by CLKTIME into a short
** format date string.
*/
char *
c_cnvdate (
  long	clktime,		/* seconds since jan.1,1980	*/
  char	*outstr,		/* encoded time string		*/
  int	maxch 
)
{
	XCHAR	buf[SZ_LINE];
	XINT	x_maxch = SZ_LINE;

	CNVDATE (&clktime, buf, &x_maxch);
	return (c_strpak (buf, outstr, maxch));
}
