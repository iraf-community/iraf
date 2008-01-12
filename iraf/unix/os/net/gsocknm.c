/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "types.h"

extern	int errno;
extern	int tcperrno;

/* TCP_GSOCKNAME -- Get socket name.  Return the current network name for the
 * indicated socket.
 */
tcp_gsockname (s, name, namelen)
u_sock	s;			/* the socket			*/
struct	sockaddr *name;		/* endpoint of communications	*/
int	namelen;		/* maxlen in; actual len out	*/
{
	int	status;

	/* MACHDEP */
	status = getsockname (s, name, namelen);
	tcperrno = errno;
	return (status);
}
