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
