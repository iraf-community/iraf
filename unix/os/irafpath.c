/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>

#define import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

#define	SZ_ULIBSTR	512
#define	ULIB		"IRAFULIB"

extern	char *getenv();


/* IRAFPATH -- Determine the pathname of the given IRAF library file.  If the
 * file is found the full pathname is returned, else the given filename is
 * returned.  A list of user directories is first searched if defined, followed
 * by the IRAF system directories, allowing users to have custom versions of
 * the system files, e.g., for testing purposes.
 */
char *
irafpath (fname)
char *fname;			/* simple filename, no dirs */
{
	static	char pathname[SZ_PATHNAME+1];
	PKCHAR	ulibs[SZ_ULIBSTR+1];
	PKCHAR	hostdir[SZ_LINE+1];
	PKCHAR	irafdir[SZ_LINE+1];
	PKCHAR	ldir[SZ_FNAME+1];
	XINT	sz_ulibs=SZ_ULIBSTR;
	XINT	x_maxch=SZ_LINE, x_status;
	char	*ip, *op, *irafarch;

	extern  int  ZGTENV();


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
	 */
	strcpy (pathname, (char *)hostdir);
	strcat (pathname, "bin.");

#ifdef LINUXPPC
	strcat (pathname, "linuxppc");
#else
#ifdef CYGWIN
	strcat (pathname, "cygwin");
#else
#ifdef LINUX64
	strcat (pathname, "linux64");
#else
#ifdef REDHAT
	strcat (pathname, "redhat");
#else
#ifdef LINUX
	strcat (pathname, "linux");
#else
#ifdef BSD
	strcat (pathname, "freebsd");
#else
#ifdef IPAD
	strcat (pathname, "ipad");
#else
#ifdef MACOSX
	/* Setup for cross-compilation, default to 'macintel'.
	 */
        if ((irafarch = getenv("IRAFARCH"))) {
            if (strcmp (irafarch, "macosx") == 0) 
		strcat (pathname, "macosx");
            else if (strcmp (irafarch, "macintel") == 0) 
		strcat (pathname, "macintel");
            else 
		strcat (pathname, "macosx");
        } else
	    strcat (pathname, "macintel");
#else
#ifdef SOLARIS
#ifdef X86
	strcat (pathname, "sunos");
#else
	strcat (pathname, "ssol");
#endif
#else
#ifdef sparc
	strcat (pathname, "sparc");
#else
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif

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
	if ( (irafarch = getenv("IRAFARCH")) ) {
	    strcpy (pathname, (char *)irafdir);
	    strcat (pathname, "bin.");
	    strcat (pathname, irafarch);
	    strcat (pathname, "/");
	} else {
	    strcpy (pathname, (char *)irafdir);
	    strcat (pathname, "bin/");
	}
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
