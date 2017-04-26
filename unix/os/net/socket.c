/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "types.h"

extern	int errno;
extern	int tcperrno;

/* TCP_SOCKET -- Create an endpoint for communications (a socket) and bind the
 * socket to an i/o descriptor, returning the descriptor as the function value.
 */
u_sock
tcp_socket (af, type, protocol)
int	af;		/* address format, e.g, AF_INET		*/
int	type;		/* socket type, e.g., SOCK_STREAM	*/
int	protocol;	/* communications protocol, if used	*/
{
	u_sock	s;

	/* MACHDEP */
eprintf ("socket\n");
	s = socket (af, type, protocol);
	tcperrno = errno;
	return (s);
}
