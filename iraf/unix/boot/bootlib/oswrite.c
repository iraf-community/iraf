/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <unistd.h>
#include "bootlib.h"

/* OS_WRITE -- Write to the output file.  If the output file is a text file
 * we must convert the binary input (text) stream to the record structured
 * stream required by the host.
 */
/* fd     : output file			*/
/* buf    : data to be written		*/
/* nbytes : num bytes to be written	*/
ssize_t os_write ( int fd, const char *buf, size_t nbytes )
{
	const char *ip;
	XCHAR *op, *otop;
	int ch;
	size_t n;
	XINT nchars, status, x_fd;

	if (osfiletype == BINARY_FILE)
	    return (write (fd, buf, nbytes));

	n    = nbytes;
	ip   = buf;
	op   = txop;
	otop = &text[SZ_FBUF];

	/* Accumulate an output line of text and pass it on to the system when
	 * newline is seen or when the output buffer fills (unlikely).
	 */
	for ( ; 0 < n ; n-- ) {
	    *op++ = ch = *ip++;
	    if (ch == '\n' || op >= otop) {
		nchars = op - text;
		x_fd = fd;
		ZPUTTX (&x_fd, text, &nchars, &status);
		op = txop = text;
		if (status == XERR)
		    return (ERR);
	    }
	}

	txop = op;
	return (nbytes);
}
