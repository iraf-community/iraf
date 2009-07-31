/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define import_spp
#define import_knames
#include <iraf.h>

#include "../../bootlib/bootlib.h"
#include "xpp.h"


/*
 * Main routine for the XPP preprocessor (first pass of the SPP compiler).
 */

#define MAX_IRAFDEFS	64
#define IRAFCONFIGDIR	"host$config/"
#define IRAFDEFS	"host$config/iraf.h"

int	errflag;
int	hbindefs = 0;

static int n_irafdefs = 0;
static char irafdefs[MAX_IRAFDEFS][SZ_PATHNAME];
static const char *pkgenv = NULL;
static char v_pkgenv[SZ_FNAME];

static int isxfile ( const char * );
static const char *dottor ( const char * );

int main ( int argc, char *argv[] )
{
	int i, rfflag, nfiles;
	FILE *fp_defs, *source;

	ZZSTRT();

	errflag = XPP_OK;
	linenum[0] = 1;
	rfflag = NO;
	nfiles = 0;

	/* Process flags and count the number of files.
	 */
	for (i=1;  argv[i] != NULL;  i++) {
	    if (argv[i][0] == '-') {
		const char *p;
		switch (argv[i][1]) {
		case 'R':
		    /* Write .r file. */
		    rfflag = YES;
		    break;
		case 'r':
		    /* Not used anymore */
		    if ((p = argv[++i]) == NULL)
			--i;
		    break;
		case 'h':
		    /* Use custom irafdefs file. */
		    if ((p = argv[++i]) == NULL)
			--i;
		    else {
			if ( n_irafdefs < MAX_IRAFDEFS ) {
			    if ( p[0] == '/' ) {
				snprintf (irafdefs[n_irafdefs], SZ_PATHNAME,
					  "%s", p);
				n_irafdefs++;
			    }
			    else {
				snprintf (irafdefs[n_irafdefs], SZ_PATHNAME,
					  "%s%s",
					  vfn2osfn (IRAFCONFIGDIR,0), p);
				n_irafdefs++;
			    }
			}
		    }
		    break;
		case 'A':
		    /* Use architecture-specific include file. */
		    hbindefs++;
		    break;
		case 'p':
		    /* Load the environment for the named package. */
		    if ((pkgenv = argv[++i]) == NULL)
			--i;
		    else
			loadpkgenv (pkgenv);
		    break;
		default:
		    fprintf (stderr, "unknown option '%s'\n", argv[i]);
		    fflush (stderr);
		}
	    } else if (isxfile (argv[i]))
		nfiles++;
	}

	/* If no package environment was specified on the command line,
	 * check if the user has a default package set in their environment.
	 */
	if (!pkgenv)
	    if (pkgenv = os_getenv("PKGENV")) {
		snprintf (v_pkgenv, SZ_FNAME, "%s", pkgenv);
		loadpkgenv (pkgenv = v_pkgenv);
	    }

	/* Generate pathname of <iraf.h>.
	 */
	if ( n_irafdefs == 0 ) {
	    snprintf (irafdefs[0], SZ_PATHNAME, "%s", vfn2osfn (IRAFDEFS,0));
	    n_irafdefs++;
	}

	/* Process either the standard input or a list of files.
	 */
	if (nfiles == 0) {
	    yyin = stdin;
	    yyout = stdout;
	    strcpy (fname[0], "STDIN");
	    yylex();

	} else {
	    /* Preprocess each file.
	     */
	    for (i=1;  argv[i] != NULL;  i++)
		if (isxfile (argv[i])) {
		    if (nfiles > 1) {
			fprintf (stderr, "%s:\n", argv[i]);
			fflush (stderr);
		    }

		    /* Open source file.
		     */
		    if ((source = fopen (vfn2osfn(argv[i],0), "r")) == NULL) {
			fprintf (stderr, "cannot read file %s\n", argv[i]); 
			fflush (stderr);
			errflag |= XPP_BADXFILE;
		    } else {
			int j;
			/* Open output file.
			 */
			if (rfflag) {
			    const char *osfn;
			    osfn = vfn2osfn (dottor (argv[i]), 0);
			    if ((yyout = fopen (osfn, "w")) == NULL) {
				fprintf (stderr,
				    "cannot write output file %s\n", osfn);
				fflush (stderr);
				errflag |= XPP_BADXFILE;
				fclose (yyin);
			        continue;
			    }
			} else
			    yyout = stdout;

			/* Open and process hconfig$iraf.h, etc.
			 */
			for ( j=0 ; j < n_irafdefs ; j++ ) {
			    if ((fp_defs = fopen (irafdefs[j], "r")) == NULL) {
				fprintf (stderr, "cannot open %s\n",
					 irafdefs[j]);
				ZZSTOP();
				exit (XPP_COMPERR);
			    }
			    yyin = fp_defs;
			    yylex();
			    linenum[0] = 1;
			    fclose (fp_defs);
			}

			/* Process the source file.
			 */
			snprintf (fname[0], SZ_PATHNAME, "%s", argv[i]);
			yyin = source;
			yylex();
			fclose (source);

			if (rfflag)
			    fclose (yyout);
		    }
		}
	}

	ZZSTOP();
	exit (errflag);
}


/* ISXFILE -- Does the named file have a ".x" extension.
 */
static int isxfile ( const char *fname )
{
	const char *p;

	if (fname[0] != '-') {
	    for (p=fname;  *p++ != EOS; )
		;
	    while (*--p != '.' && p >= fname)
		;
	    if (*p == '.' && *(p+1) == 'x')
		return (YES);
	}
	return (NO);
}


/* DOTTOR -- Change the extension of the named file to ".r".
 */
static const char *dottor ( const char *fname )
{
	static char rfname[SZ_PATHNAME+1];
	char *op, *maxop, *lastdot;
	const char *ip;

	lastdot = NULL;
	maxop = rfname + SZ_PATHNAME+1 -1;
	for ( ip=fname, op=rfname ; op < maxop && (*ip) ; op++, ip++ ) {
	    *op = *ip;
	    if (*op == '.')
		lastdot = op;
	}
	*op = EOS;

	if (lastdot) {
	    *(lastdot+1) = 'r';
	    *(lastdot+2) = EOS;
	}

	return (rfname);
}
