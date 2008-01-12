/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "types.h"

extern	int errno;
int	tcperrno;

/* TCP_CONNECT -- Initiate a connection on a socket.  Returns when the server
 * accepts the connection and a full duplex connection has been established.
 * Zero is returned if the connection succeeds; -1 is returned if the connection
 * fails.  The sockaddr argument is necessary because a socket may be used to
 * talk to multiple endpoints.
 */
tcp_connect (s, name, namelen)
u_sock	s;			/* the socket			*/
struct	sockaddr *name;		/* endpoint of communications	*/
int	namelen;		/* sizeof(name)			*/
{
	int	status;
eprintf("connect\n");

	/* MACHDEP */
	status = connect (s, name, namelen);
	tcperrno = errno;
	return (status);
}
