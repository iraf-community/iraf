/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_RCURSOR -- Read a cursor.
*/
int
c_rcursor (
  int	fd,			/* FIO file descriptor		*/
  char	*outstr,		/* output string		*/
  int	maxch
)
{
	XCHAR	buf[SZ_LINE];
	XINT x_fd = fd, x_maxch = maxch;
	int	key;


	key = (int) RCURSOR (&x_fd, buf, &x_maxch);
	c_strpak (buf, outstr, maxch);

	return (key == XEOF ? EOF : key);
}
