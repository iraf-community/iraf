/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "types.h"

extern	int errno;
extern	int tcperrno;

/* TCP_CLOSE -- Close a socket.
 */
tcp_close (s)
u_sock	s;			/* the socket	*/
{
	/* MACHDEP */
	return (close (s));
}
