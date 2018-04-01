/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include "types.h"
#include "netdb.h"
#include "socket.h"

#define	import_kernel
#define	import_knames
#define	import_spp
#include <iraf.h>

#define MAXALIASES	35
#define MAXADDRSIZE	14
#define	LINSIZ		80

static int	hostf = NULL;
static char	line[LINSIZ+1];
static char	hostaddr[MAXADDRSIZE];
static struct	hostent host;
static char	*host_aliases[MAXALIASES];
static char	*tcp_locate();


/* TCP_GHOSTENT -- Return the next entry (line) in the host name table
 * decoded into a hostent structure.
 *
 * The format of an entry in the host name table (e.g., /etc/hosts on a UNIX
 * system) is as follows:
 *
 *	ddd.ddd		alias1 alias2 ... aliasN
 */
struct hostent *
tcp_ghostent (void)
{
	register char *cp, **q;
	u_long	tcp_inetaddr();
	char	*p, *tcp_hostdb();
	char	*ku_fgets();

	if (hostf == NULL && (hostf = ku_fopen (tcp_hostdb(), "r" )) == NULL)
	    return (NULL);

again:
	if ((p = ku_fgets (line, LINSIZ, hostf)) == NULL)
	    return (NULL);
eprintf("..%s", line);

	if (*p == '#')
	    goto again;
	cp = tcp_locate (p, "#\n");
	if (cp == NULL)
	    goto again;

	*cp = '\0';
	cp = tcp_locate (p, " \t");
	if (cp == NULL)
	    goto again;
	*cp++ = '\0';

	/* THIS STUFF IS INTERNET SPECIFIC.
	 */
	host.h_addr = hostaddr;
	*((u_long *)host.h_addr) = tcp_inetaddr (p);
	host.h_length = sizeof (u_long);
	host.h_addrtype = AF_INET;

	while (*cp == ' ' || *cp == '\t')
	    cp++;
	host.h_name = cp;

	q = host.h_aliases = host_aliases;
	cp = tcp_locate (cp, " \t");
	if (cp != NULL) 
	    *cp++ = '\0';

	while (cp && *cp) {
	    if (*cp == ' ' || *cp == '\t') {
		cp++;
		continue;
	    }
	    if (q < &host_aliases[MAXALIASES - 1])
		*q++ = cp;
	    cp = tcp_locate (cp, " \t");
	    if (cp != NULL)
		*cp++ = '\0';
	}

	*q = NULL;

	return (&host);
}


/* TCP_OPHNT -- Open the host name table, a text file.
 */
int 
tcp_ophnt (void)
{
	char	*tcp_hostdb();

eprintf ("ophnt %s\n", tcp_hostdb);
	if (hostf == NULL)
	    hostf = ku_fopen (tcp_hostdb(), "r");
}


/* TCP_CLHNT -- Close the host name table file.
 */
int 
tcp_clhnt (void)
{
	if (hostf) {
	    ku_fclose (hostf);
	    hostf = NULL;
	}
}


/* TCP_LOCATE -- Return a pointer to the first character in the indicated
 * character class.
 */
static char *
tcp_locate (register char *cp, char *match)
{
	register char *mp, c;

	while (c = *cp) {
	    for (mp = match;  *mp;  mp++)
		if (*mp == c)
		    return (cp);
	    cp++;
	}

	return ((char *)0);
}
