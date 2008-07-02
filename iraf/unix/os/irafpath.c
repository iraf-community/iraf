/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>

#define import_spp
#define import_kernel
#define import_knames
#include <iraf.h>

#define safe_strcat(dest,size,src) strncat(dest,src,(strlen(dest) < (size)) ? (size)-1-strlen(dest) : 0)

#define SZ_ULIBSTR 512
#define ULIB "IRAFULIB"

/* IRAFPATH -- Determine the pathname of the given IRAF library file.  If the
 * file is found the full pathname is returned, else the given filename is
 * returned.  A list of user directories is first searched if defined, followed
 * by the IRAF system directories, allowing users to have custom versions of
 * the system files, e.g., for testing purposes.
 */
/* fname : simple filename, no dirs */
const char *irafpath ( const char *fname )
{
	static char pathname[SZ_PATHNAME+1];
	PKCHAR ulibs[SZ_ULIBSTR+1];
	PKCHAR hostdir[SZ_LINE+1];
	PKCHAR irafdir[SZ_LINE+1];
	PKCHAR ldir[SZ_FNAME+1];
	XINT sz_ulibs=SZ_ULIBSTR;
	XINT x_maxch=SZ_LINE, x_status;
	const char *ip, *irafarch;
	char *op, *maxop;

	/* Search any user libraries first. */
	strcpy ((char *)ldir, ULIB);
	ZGTENV (ldir, ulibs, &sz_ulibs, &x_status);
	if (x_status > 0)
	    for ( ip=(const char *)ulibs ; *ip ; ) {
		/* Get next user directory pathname. */
		while (isspace (*ip))
		    ip++;
		if (!*ip)
		    break;
		maxop = pathname + SZ_PATHNAME+1 -1;
		for ( op=pathname ; op < maxop && *ip && !isspace(*ip) ; op++, ip++ )
		    *op = *ip;
		if ( pathname < op && *(op-1) != '/' && op < maxop )
		    *op++ = '/';
		*op = '\0';

		safe_strcat (pathname, SZ_PATHNAME+1, fname);
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

	/* Try HBIN */
	strcpy (pathname, (const char *)hostdir);
	safe_strcat (pathname, SZ_PATHNAME+1, "bin/");
	safe_strcat (pathname, SZ_PATHNAME+1, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	/* Try HCONFIG */
	strcpy (pathname, (const char *)hostdir);
	safe_strcat (pathname, SZ_PATHNAME+1, "config/");
	safe_strcat (pathname, SZ_PATHNAME+1, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	/* Try HLIB */
	strcpy (pathname, (const char *)hostdir);
	safe_strcat (pathname, SZ_PATHNAME+1, "lib/");
	safe_strcat (pathname, SZ_PATHNAME+1, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	/* Try BIN - use IRAFARCH if defined. */
	if ( (irafarch = getenv("IRAFARCH")) != NULL ) {
	    strcpy (pathname, (const char *)irafdir);
	    safe_strcat (pathname, SZ_PATHNAME+1, "bin.");
	    safe_strcat (pathname, SZ_PATHNAME+1, irafarch);
	    safe_strcat (pathname, SZ_PATHNAME+1, "/");
	    safe_strcat (pathname, SZ_PATHNAME+1, fname);
	    if (access (pathname, 0) == 0)
		return (pathname);
	}

	/* Try BIN */
	strcpy (pathname, (const char *)irafdir);
	safe_strcat (pathname, SZ_PATHNAME+1, "bin/");
	safe_strcat (pathname, SZ_PATHNAME+1, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	/* Try BASE */
	strcpy (pathname, (const char *)irafdir);
	safe_strcat (pathname, SZ_PATHNAME+1, "base/");
	safe_strcat (pathname, SZ_PATHNAME+1, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	/* Try CONFIG */
	strcpy (pathname, (const char *)irafdir);
	safe_strcat (pathname, SZ_PATHNAME+1, "config/");
	safe_strcat (pathname, SZ_PATHNAME+1, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	/* Try LIB */
	strcpy (pathname, (const char *)irafdir);
	safe_strcat (pathname, SZ_PATHNAME+1, "lib/");
	safe_strcat (pathname, SZ_PATHNAME+1, fname);
	if (access (pathname, 0) == 0)
	    return (pathname);

	return (fname);
}
