/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

#define	SZ_ULIBSTR	512
#define	ULIB		"IRAFULIB"


/* IRAFPATH -- Determine the pathname of the given IRAF library file.  If the
 * file is found the full pathname is returned, else the given filename is
 * returned.  A list of user directories is first searched if defined, followed
 * by the IRAF system directories, allowing users to have custom versions of
 * the system files, e.g., for testing purposes.
 */
char *
irafpath (char *fname)
            			/* simple filename, no dirs */
{
	static	char pathname[SZ_PATHNAME+1];
	PKCHAR	ulibs[SZ_ULIBSTR+1];
	PKCHAR	hostdir[SZ_LINE+1];
	PKCHAR	irafdir[SZ_LINE+1];
	PKCHAR	ldir[SZ_FNAME+1];
	XINT	sz_ulibs=SZ_ULIBSTR;
	XINT	x_maxch=SZ_LINE, x_status;
	char	*ip, *op, *irafarch;

	extern  int  ZGTENV(PKCHAR *envvar, PKCHAR *outstr, XINT *maxch, XINT *status);


	/* Search any user libraries first. */
	strcpy ((char *)ldir, ULIB);
	(void) ZGTENV (ldir, ulibs, &sz_ulibs, &x_status);
	if (x_status > 0)
	    for (ip=(char *)ulibs;  *ip;  ) {
		/* Get next user directory pathname. */
		while (isspace (*ip))
		    ip++;
		if (!*ip)
		    break;
		for (op=pathname;  *ip && !isspace(*ip);  )
		    *op++ = *ip++;
		if (*(op-1) != '/')
		    *op++ = '/';
		*op = '\0';

		strcat (pathname, fname);
		if (access (pathname, 0) == 0)
		    return (pathname);
	    }

	/* Get the root pathnames.  */
	strcpy ((char *)ldir, "host");
	ZGTENV (ldir, hostdir, &x_maxch, &x_status);
	if (x_status <= 0)
	    return (fname);
	strcpy ((char *)ldir, "iraf");
	ZGTENV (ldir, irafdir, &x_maxch, &x_status);
	if (x_status <= 0)
	    return (fname);

	/* Look first in HBIN.
	 * Use IRAFARCH if defined.
	 */
	strcpy (pathname, (char *)hostdir);
	strcat (pathname, "bin");
	if ( (irafarch = getenv("IRAFARCH")) ) {
	    strcat (pathname, ".");
	    strcat (pathname, irafarch);
	}
	strcat (pathname, "/");
	strcat (pathname, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	/* Try HLIB */
	strcpy (pathname, (char *)hostdir);
	strcat (pathname, "hlib/");
	strcat (pathname, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	/* Try BIN - use IRAFARCH if defined. */
	strcpy (pathname, (char *)irafdir);
	strcat (pathname, "bin");
	if ( (irafarch = getenv("IRAFARCH")) ) {
	    strcat (pathname, ".");
	    strcat (pathname, irafarch);
	}
	strcat (pathname, "/");
	strcat (pathname, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	/* Try LIB */
	strcpy (pathname, (char *)irafdir);
	strcat (pathname, "lib/");
	strcat (pathname, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	return (fname);
}
