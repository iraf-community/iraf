/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "types.h"

extern	int errno;
extern	int tcperrno;

/* TCP_WRITE -- Write to a socket.
 */
tcp_write (s, buf, nbytes)
u_sock	s;			/* output socket	*/
char	*buf;			/* input buffer		*/
int	nbytes;			/* num bytes to write	*/
{
	/* MACHDEP */
eprintf ("write %d bytes\n", nbytes);
	nbytes = write (s, buf, nbytes);
eprintf ("%d bytes written\n", nbytes);

	tcperrno = errno;
	return (nbytes);
}
