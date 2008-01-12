/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include "netdb.h"

#define	import_kernel
#define	import_knames
#define	import_spp
#include <iraf.h>


/* TCP_GETHOSTBYNAME -- Scan the host name table to get the internet address
 * of the named host.
 */
struct hostent *
tcp_gethostbyname (name)
register char *name;
{
	register struct hostent *p;
	register char **cp;
	struct	hostent *tcp_ghostent();

eprintf("gethostbyname %s\n", name);
	tcp_ophnt();

	while (p = tcp_ghostent()) {
	    if (strcmp (p->h_name, name) == 0)
		break;
	    for (cp = p->h_aliases; *cp != 0; cp++)
		if (strcmp (*cp, name) == 0)
		    goto found;
	}
found:
	tcp_clhnt();
	return (p);
}
