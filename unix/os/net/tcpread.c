/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "types.h"

extern	int errno;
extern	int tcperrno;

/* TCP_READ -- Read from a socket.
 */
tcp_read (s, buf, maxbytes)
u_sock	s;			/* input socket		*/
char	*buf;			/* output buffer	*/
int	maxbytes;		/* max bytes to read	*/
{
	int	nbytes;

	/* MACHDEP */
eprintf ("read %d bytes\n", maxbytes);

	nbytes = read (s, buf, maxbytes);
eprintf ("\t%d bytes read\n", nbytes);

	tcperrno = errno;
	return (nbytes);
}
