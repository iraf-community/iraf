/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "types.h"

extern	int errno;
extern	int tcperrno;

/* TCP_ACCEPT -- Accept a connection on a socket.  Accept extracts the first
 * connection from the queue of pending connections (set up with LISTEN),
 * creates a new socket with the same properties as S and allocates a new
 * file descriptor NS for the socket.
 */
u_sock
tcp_accept (s, addr, addrlen)
u_sock	s;			/* the socket			*/
struct	sockaddr *addr;		/* endpoint of communications	*/
int	*addrlen;		/* sizeof (addr)		*/
{
	u_sock	ns;

	/* MACHDEP */
	ns = accept (s, addr, addrlen);
	tcperrno = errno;
	return (ns);
}
