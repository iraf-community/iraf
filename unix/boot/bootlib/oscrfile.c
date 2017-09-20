/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <fcntl.h>
#include "bootlib.h"


/* OS_CREATEFILE -- Open a new file for writing.  Create the file with the
 * given mode bits.
 */
int
os_createfile (
  char	*fname,
  int	mode,
  int	type
)
{
	static	XINT xmode = NEW_FILE;
	PKCHAR	*osfn = (PKCHAR *) vfn2osfn (fname, 1);
	XINT	chan;
	extern  int ZOPNTX(PKCHAR *osfn, XINT *mode, XINT *chan);


	if (bdebug)
	    fprintf (stderr, "create %s file `%s' -> `%s'\n",
		type == TEXT_FILE ? "text" : "binary", fname, (char *)osfn);
	osfiletype = type;

	if (type == BINARY_FILE)
	    return (creat ((char *)osfn, mode));
	else {
	    ZOPNTX (osfn, &xmode, &chan);
	    txop = text;
	    return (chan == XERR ? ERR : chan);
	}
}
