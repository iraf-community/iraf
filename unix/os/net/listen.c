/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "types.h"

extern	int errno;
extern	int tcperrno;

/* TCP_LISTEN -- Listen for connections on a socket.  Returns immediately,
 * i.e., listen does not block the calling process.
 */
tcp_listen (s, backlog)
u_sock	s;			/* the socket			*/
int	backlog;		/* max queued connects		*/
{
	int	status;

	/* MACHDEP */
	status = listen (s, backlog);
	tcperrno = errno;
	return (status);
}
