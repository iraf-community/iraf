/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define	import_spp
#define	import_error
#define	import_kernel
#define	import_knames
#include <iraf.h>


/*
 * SGIDEBUG.C -- Determine pathname to the executable for the named SGI
 * translator, and execute the translator.  Pass all command line arguments
 * to the child, which also inherits stdin, stdout, and stderr.
 *
 * NOTE:  This is the debug version of the SGIDISPATCH command.
 *
 * Usage: 	sgidebug translator [args]
 */

#define	DEF_HOST	"unix"		/* default host system        */
#define	F_OK		0		/* access mode `file exists'  */
#ifndef X_OK
#define	X_OK		1		/* access mode `executable'   */
#endif

FILE    *dbg = (FILE *) NULL;

char	*irafpath (char *path);


/* MAIN -- Main entry point for SGIDISPATCH.
 */
int
main (int argc, char *argv[])
{
	char	tpath[SZ_PATHNAME+1];
	char	translator[SZ_PATHNAME+1];
	int	ip;

	/* Do nothing if called with no arguments.
	 */
	if (argc < 2)
	    exit (OSOK);

        if ((dbg = fopen ("/tmp/sgidbg", "w")) == (FILE *) NULL)
            dbg = stderr;
            
	/* Construct pathname to translator.
	 */
	strcpy (translator, argv[1]);
	ip = strlen (translator);
	if (strcmp (&translator[ip], ".e") != 0)
	    strcat (translator, ".e");
	sprintf (tpath, "%s", irafpath(translator));
        fprintf (dbg, "sgidispatch -- translator path: %s\n", tpath);

	if (access (tpath, X_OK) == ERR) {
	    fprintf (stderr, "Fatal (sgidispatch): unable to access SGI");
	    fprintf (stderr, " translator `%s'\n", tpath);
	    fflush (stderr);
	    exit (OSOK+1);
	} else {
	    fprintf (dbg, "Translator `%s' exists as execitable\n", tpath);
	}

	/* Set up i/o for translator and attempt to fork.
	 */
        fprintf (dbg, "Calling translator....\n");
	fflush (stderr);
        fclose (dbg);

	argv[argc] = 0;
	execv (tpath, &argv[1]);
	fprintf (stderr, "Fatal (sgidispatch): unable to execv(%s, ...)\n",
	    tpath);
	fflush (stderr);
	exit (OSOK+1);
}


#define	SZ_ULIBSTR	512
#define	ULIB		"IRAFULIB"

/* VOS Prototypes.
 */
extern int ZGTENV (PKCHAR *envvar, PKCHAR *outstr, XINT *maxch, XINT *status);


/* IRAFPATH -- Determine the pathname of the given IRAF library file.  If the
 * file is found the full pathname is returned, else the given filename is
 * returned.  A list of user directories is first searched if defined, followed
 * by the IRAF system directories, allowing users to have custom versions of
 * the system files, e.g., for testing purposes.
 */
char *
irafpath (
    char  *fname		/* simple filename, no dirs */
)
{
	static	char pathname[SZ_PATHNAME+1];
	PKCHAR	ulibs[SZ_ULIBSTR+1];
	PKCHAR	hostdir[SZ_LINE+1];
	PKCHAR	irafdir[SZ_LINE+1];
	PKCHAR	ldir[SZ_FNAME+1];
	XINT	sz_ulibs=SZ_ULIBSTR;
	XINT	x_maxch=SZ_LINE, x_status;
	char	*ip, *op, *irafarch;


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
                fprintf (dbg, "irafpath IRAFULIB: %s\n", pathname);
		if (access (pathname, 0) == 0)
		    return (pathname);
	    }

	/* Get the root pathnames.  */
	strcpy ((char *)ldir, "host");
	ZGTENV (ldir, hostdir, &x_maxch, &x_status);
        fprintf (dbg, "irafpath host(%ld): %s\n", x_status, (char *)hostdir);
	if (x_status <= 0)
	    return (fname);
	strcpy ((char *)ldir, "iraf");
	ZGTENV (ldir, irafdir, &x_maxch, &x_status);
        fprintf (dbg, "irafpath iraf(%ld): %s\n", x_status, (char *)irafdir);
	if (x_status <= 0)
	    return (fname);

	/* Look first in HBIN.
	 */
	strcpy (pathname, (char *)hostdir);
	strcat (pathname, "bin.");

#ifdef LINUX64
	strcat (pathname, "linux64");
#else
#ifdef LINUX
	strcat (pathname, "linux");
#else
#ifdef MACOSX
	/* Setup for cross-compilation, default to 'macintel'.
	 */
        if ((irafarch = getenv("IRAFARCH"))) {
            fprintf (dbg, "irafpath IRAFARCH: %s\n", irafarch);
            if (strcmp (irafarch, "macosx") == 0 ||
                strcmp (irafarch, "macos64") == 0) 
		    strcat (pathname, "macosx");
            else if (strcmp (irafarch, "macintel") == 0) 
		strcat (pathname, "macintel");
            else 
		strcat (pathname, "macintel");
        } else {
            fprintf (dbg, "irafpath IRAFARCH: macintel (default)s\n");
	    strcat (pathname, "macintel");
        }
#endif
#endif
#endif
        fprintf (dbg, "irafpath HBIN: %s\n", pathname);

	strcat (pathname, "/");
	strcat (pathname, fname);
	if (access (pathname, 0) == 0) {
            fprintf (dbg, "irafpath returning: %s\n", pathname);
	    return (pathname);
        }

	/* Try HLIB */
	strcpy (pathname, (char *)hostdir);
	strcat (pathname, "hlib/");
	strcat (pathname, fname);
        fprintf (dbg, "irafpath HLIB: %s\n", pathname);
	if (access (pathname, 0) == 0) {
            fprintf (dbg, "irafpath returning: %s\n", pathname);
	    return (pathname);
        }

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
        fprintf (dbg, "irafpath BIN: %s\n", pathname);
	if (access (pathname, 0) == 0) {
            fprintf (dbg, "irafpath returning: %s\n", pathname);
	    return (pathname);
        }

	/* Try LIB */
	strcpy (pathname, (char *)irafdir);
	strcat (pathname, "lib/");
	strcat (pathname, fname);
        fprintf (dbg, "irafpath LIB: %s\n", pathname);
	if (access (pathname, 0) == 0) {
            fprintf (dbg, "irafpath returning: %s\n", pathname);
	    return (pathname);
        }

        fprintf (dbg, "irafpath default returning: %s\n", fname);
	return (fname);
}
