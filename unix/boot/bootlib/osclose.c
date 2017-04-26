/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <unistd.h>		/* for close()	*/
#include "bootlib.h"


/* OS_CLOSE -- Close a file created (opened) by OSCREATE.  If writing to a
 * text file flush any incomplete (non newline terminated) output line.
 */
void
os_close (int fd)
{
	XINT	junk, xfd=fd;
	XINT	nchars;

	extern  int ZPUTTX(), ZCLSTX();


	if (osfiletype == BINARY_FILE)
	    close (fd);
	else {
	    if (txop > text) {
		nchars = txop - text;
		ZPUTTX (&xfd, text, &nchars, &junk);
	    }
	    ZCLSTX (&xfd, &junk);
	}
}
