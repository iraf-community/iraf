/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>

#define	import_kernel
#define	import_knames
#define	import_spp
#include <iraf.h>

/* MACHDEP */
#define	HOSTDB	"/etc/hosts"		/* change to "" if not UNIX	*/


/* TCP_HOSTDB -- Return the machine dependent pathname of the host name table
 * file.  On a Berkeley UNIX host system this is "/etc/hosts", but to avoid
 * hidden machine pathnames in the code we reference "iraf$dev/uhosts" instead.
 */
char *
tcp_hostdb()
{
	static	char hostdb[SZ_FNAME+1] = HOSTDB;
	PKCHAR	osfn[SZ_FNAME+1];

	/* If HOSTDB is explicitly defined, use it, else return OSFN of the
	 * the file "dev$uhosts".  If the filename generation fails (e.g.,
	 * because IRAF is not defined in the host environment) return
	 * anything.  In this case anything is the pathname of the Berkeley
	 * UNIX hosts file, which will cause a file open failure on most
	 * systems.
	 */
	if (hostdb[0] == '\0') {
	    if (ku_mkfname ("iraf", "dev", "uhosts", osfn, SZ_FNAME) == ERR)
		strcpy ((char *)osfn, "/etc/hosts");
	    strcpy (hostdb, (char *)osfn);
	}

	return (hostdb);
}
