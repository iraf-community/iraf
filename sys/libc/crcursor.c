/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_RCURSOR -- Read a cursor.
 */
c_rcursor (fd, outstr, maxch)
int	fd;			/* FIO file descriptor		*/
char	*outstr;		/* output string		*/
int	maxch;
{
	XCHAR	buf[SZ_LINE];
	int	key;

	key = RCURSOR (&fd, buf, &maxch);
	c_strpak (buf, outstr, maxch);

	return (key == XEOF ? EOF : key);
}
