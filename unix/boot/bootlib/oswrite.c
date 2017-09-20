/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <unistd.h>
#include "bootlib.h"

/* OS_WRITE -- Write to the output file.  If the output file is a text file
 * we must convert the binary input (text) stream to the record structured
 * stream required by the host.
 */
int
os_write (
  int	fd,			/* output file			*/
  char	*buf,			/* data to be written		*/
  int	nbytes 			/* num bytes to be written	*/
)
{
	register char	*ip;
	register XCHAR	*op, *otop;
	register int	ch, n;
	XINT	nchars, status, xfd=fd;
	extern  int ZPUTTX(XINT *fd, XCHAR *buf, XINT *nchars, XINT *status);


	if (osfiletype == BINARY_FILE)
	    return (write (fd, buf, nbytes));

	n    = nbytes;
	ip   = buf;
	op   = txop;
	otop = &text[SZ_FBUF];

	/* Accumulate an output line of text and pass it on to the system when
	 * newline is seen or when the output buffer fills (unlikely).
	 */
	while (--n >= 0) {
	    *op++ = ch = *ip++;
	    if (ch == '\n' || op >= otop) {
		nchars = op - text;
		ZPUTTX (&xfd, text, &nchars, &status);
		op = txop = text;
		if (status == XERR)
		    return (ERR);
	    }
	}

	txop = op;
	return (nbytes);
}
