/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include "bootlib.h"

extern	int  os_access (char *fname, int mode, int type);


/* OS_FCOPY -- Copy a file.  Used by RTAR to resolve links.
 */
int
os_fcopy (
  char	*oldfile,
  char	*newfile
)
{
	XCHAR	buf[SZ_FBUF];
	XINT	status,	junk, maxch = SZ_FBUF, mode = 0, in, out, n, nw;

	extern  int ZOPNTX(PKCHAR *osfn, XINT *mode, XINT *chan);
	extern  int ZGETTX(XINT *fd, XCHAR *buf, XINT *maxchars, XINT *status);
	extern	int ZCLSTX(XINT *fd, XINT *status);
	extern	int ZPUTTX(XINT *fd, XCHAR *buf, XINT *nchars, XINT *status);


	if (os_access (oldfile,0,0) == NO)
	    return (ERR);

	if (os_access (oldfile, 0, TEXT_FILE) == YES) {
	    if (bdebug)
		fprintf (stderr, "copy text file '%s' -> '%s'\n",
		    oldfile, newfile);

	    mode = READ_ONLY;
	    ZOPNTX ((PKCHAR *)vfn2osfn(oldfile,0), &mode, &in);
	    if (in == XERR)
		return (ERR);

	    mode = NEW_FILE;
	    ZOPNTX ((PKCHAR *)vfn2osfn(newfile,1), &mode, &out);
	    if (out == XERR) {
		ZCLSTX (&in, &status);
		return (ERR);
	    }

	    while (ZGETTX (&in, buf, &maxch, &n), n != XEOF) {
		if (n != XERR)
		    ZPUTTX (&out, buf, &n, &status);
		if (n == XERR || status == XERR) {
		    ZCLSTX (&in, &junk);
		    ZCLSTX (&out, &junk);
		    return (ERR);
		}
	    }

	    ZCLSTX (&in, &status);
	    ZCLSTX (&out, &status);

	    return (status);

	} else {
	    if (bdebug)
		fprintf (stderr, "copy binary file `%s' -> `%s'\n",
		    oldfile, newfile);

	    if ((in = open (vfn2osfn(oldfile,0), O_RDONLY)) == ERR)
		return (ERR);
	    if ((out = creat (vfn2osfn(newfile,1), 0644)) == ERR) {
		close (in);
		return (ERR);
	    }

	    while ((n = read (in, (char *)buf, SZ_FBUF)) > 0)
		nw = write (out, (char *)buf, n);

	    close (in);
	    close (out);
	    if (n < 0)
		return (ERR);
	}

	return (ERR);
}
