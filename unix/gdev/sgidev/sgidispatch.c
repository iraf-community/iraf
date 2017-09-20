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
 * SGIDISPATCH.C -- Determine pathname to the executable for the named SGI
 * translator, and execute the translator.  Pass all command line arguments
 * to the child, which also inherits stdin, stdout, and stderr.
 *
 * Usage: 	sgidispatch translator [args]
 */

#define	DEF_HOST	"unix"		/* default host system        */
#define	F_OK		0		/* access mode `file exists'  */
#ifndef X_OK
#define	X_OK		1		/* access mode `executable'   */
#endif

char	*irafpath(char *sysfile);


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

	/* Construct pathname to translator.
	 */
	strcpy (translator, argv[1]);
	ip = strlen (translator);
	if (strcmp (&translator[ip], ".e") != 0)
	    strcat (translator, ".e");
	sprintf (tpath, "%s", irafpath(translator));

	if (access (tpath, X_OK) == ERR) {
	    fprintf (stderr, "Fatal (sgidispatch): unable to access SGI");
	    fprintf (stderr, " translator `%s'\n", tpath);
	    fflush (stderr);
	    exit (OSOK+1);
	}

	/* Set up i/o for translator and attempt to fork.
	 */
	argv[argc] = 0;
	execv (tpath, &argv[1]);
	fprintf (stderr, "Fatal (sgidispatch): unable to execv(%s, ...)\n",
	    tpath);
	fflush (stderr);
	exit (OSOK+1);
}
