/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#define import_spp
#define import_xnames
#include <iraf.h>
#include "bootlib.h"

#define my_isspace(c)	((c)==' '||(c)=='\t'||(c)=='\n')
#define SETENV		"zzsetenv.def"
#define SZ_VALUE	SZ_COMMAND
#define MAXLEV		8
#define SPPINCLUDES		"sppincludes"
#define IRAFARCH	"IRAFARCH"
#define ARCH		"arch"

static void loadenv ( const char * );

/* LOADPKGENV -- Load the environment definitions for the named package.
 * [e.g., loadpkgenv ("noao")].  This assumes that the root directory of
 * the named package is already defined, and that this directory contains
 * a subdirectory lib containing the file zzsetenv.def.  If none of these
 * assumptions are true, call loadenv(osfn) with the host filename of the
 * file to be loaded.
 */
void loadpkgenv ( const char *pkg )
{
	char	vfn[SZ_PATHNAME+1];
	char	sppincludes[SZ_COMMAND+1];
	char	newincludes[SZ_COMMAND+1];

	/* Initialize the default IRAF environment. */
	_envinit();

	/* If no package name is given or the IRAF environment is being
	 * loaded we are done.
	 */
	if (!pkg || strcmp(pkg,"iraf")==0)
	    return;

	safe_strcpy (vfn, SZ_PATHNAME+1, pkg);
	safe_strcat (vfn, SZ_PATHNAME+1, "$config/");
	safe_strcat (vfn, SZ_PATHNAME+1, SETENV);

	/* Load the package environment.  The new values are added to the
	 * environment in the conventional way except for the value of
	 * "sppincludes".  As each package environment is loaded we want to
	 * add the newly defined package libraries to the current list
	 * of package libraries, otherwise the most recent package environment
	 * overrides the earlier ones.  It is still possible that user
	 * defined environment variables will be redefined but there is
	 * little we can do about that; "sppincludes" is special though since
	 * it is a part of the loadpkgenv facility.
	 */
	_os_getenv (SPPINCLUDES, sppincludes, SZ_COMMAND+1);
	loadenv (vfn2osfn (vfn, 0));
	_os_getenv (SPPINCLUDES, newincludes, SZ_COMMAND+1);

	if (strlen(newincludes) > 0 && strcmp (newincludes, sppincludes)) {
	    char *ip, *op, *otop;

	    /* Find the end of the current sppincludes file list. */
	    for (ip=op=sppincludes;  *ip;  ip++)
		if (!my_isspace(*ip))
		    op = ip + 1;

	    /* Concatenate the new files list segment. */
	    if (op > sppincludes)
		*op++ = ',';
	    for (ip=newincludes, otop=sppincludes+SZ_COMMAND;  *ip && op < otop;  ip++)
		if (!my_isspace(*ip))
		    *op++ = *ip;

	    /* Blank fill to the next SZ_LINE increment to optimize resets. */
	    while (op < otop && ((op-sppincludes) % SZ_LINE))
		*op++ = ' ';
	    *op++ = EOS;
		
	    /* Reset the stored value in the environment. */
	    os_putenv (SPPINCLUDES, sppincludes);
	}
}


#ifdef NOVOS
void _envinit( void )
{
}
static void loadenv ( const char *osfn )
{
	printf ("HSI is compiled NOVOS\n");
}
#else

/* ENVINIT -- Initialize the VOS environment list by scanning the file
 * hconfig$zzsetenv.def.  HCONFIG is defined in terms of HOST which is sufficiently
 * well known to have a value before the environment list is loaded.
 */
void _envinit( void )
{
	static int initialized = 0;
	const char *hconfig;
	char osfn[SZ_PATHNAME+1];
	char irafarch[SZ_PATHNAME+1];

	if (initialized++)
	    return;

	if (hconfig = os_getenv ("hconfig")) {
	    safe_strcpy (osfn, SZ_PATHNAME+1, hconfig);
	    safe_strcat (osfn, SZ_PATHNAME+1, SETENV);
	} else {
	    fprintf (stderr, "cannot translate logical name `hconfig'");
	    fflush (stderr);
	}

	ENVINIT();
	loadenv (osfn);

	/* If the variable "IRAFARCH" is defined and "arch" is not, add
	 * a definition for the latter.  "arch" is used to construct
	 * pathnames but the HSI architecture support requires only that
	 * IRAFARCH be predefined.
	 */
	if (_os_getenv (IRAFARCH, irafarch, SZ_PATHNAME+1))
	    if (!_os_getenv (ARCH, osfn, SZ_PATHNAME+1)) {
		XCHAR   x_name[SZ_PATHNAME+1];
		XCHAR   x_value[SZ_PATHNAME+1];

		snprintf (osfn, SZ_PATHNAME+1, ".%s", irafarch);
		os_strupk (ARCH, x_name, SZ_PATHNAME+1);
		os_strupk (osfn, x_value, SZ_PATHNAME+1);
		ENVRESET (x_name, x_value);
	    }
}


/* LOADENV -- Load environment definitions from the named host file.
 */
static void loadenv ( const char *osfn )
{
	char *ip;
	XCHAR *op, *maxop;
	char lbuf[SZ_LINE+1];
	char pkname[SZ_FNAME+1], old_value[SZ_VALUE+1];
	XCHAR name[SZ_FNAME+1], value[SZ_VALUE+1];
	FILE *fp, *sv_fp[MAXLEV];
	int lev=0;

	if ((fp = fopen (osfn, "r")) == NULL) {
	    printf ("envinit: cannot open `%s'\n", osfn);
	    fflush (stdout);
	    return;
	}

	for (;;) {
	    /* Get next line from input file. */
	    if (fgets (lbuf, SZ_LINE+1, fp) == NULL) {
		/* End of file. */
		if (lev > 0) {
		    fclose (fp);
		    fp = sv_fp[--lev];
		    continue;
		} else
		    break;

	    } else {
		/* Skip comments and blank lines. */
		for (ip=lbuf;  my_isspace(*ip);  ip++)
		    ;
		if (strncmp (lbuf, "set", 3) != 0) {
		    if (strncmp (lbuf, "reset", 5) != 0)
			continue;
		    else
			ip += 5;
		} else
		    ip += 3;

		/* Check for @file inclusion. */
		while (my_isspace(*ip))
		    ip++;

		if (*ip == '@') {
		    sv_fp[lev++] = fp;
		    if (lev >= MAXLEV) {
			printf ("envinit: nesting too deep\n");
			fflush (stdout);
			break;

		    } else {
			const char *fname;
			fname = ++ip;

			while (*ip)
			    if (my_isspace(*ip)) {
				*ip = '\0';
				break;
			    } else
				ip++;

			if ((fp = fopen (vfn2osfn(fname,0), "r")) == NULL) {
			    printf ("envinit: cannot open `%s'\n", fname);
			    fflush (stdout);
			    break;
			}
		    }
		    continue;
		}

		/* fall through */
	    }

	    /* Extract name field. */
	    maxop = name + SZ_FNAME+1 -1;
	    for ( op=name ; (*ip) && *ip != '=' && !my_isspace(*ip) ; ip++ ) {
		if ( op < maxop ) {
		    *op = *ip;
		    op++;
		}
	    }
	    *op = XEOS;

	    /* Extract value field; may be quoted.  Newline may be escaped
	     * to break a long value string over several lines of the input
	     * file.
	     */
	    for (; ( *ip && *ip == '=' ) || *ip == '"' || my_isspace (*ip);  ip++)
		;
	    maxop = value + SZ_VALUE+1 -1;
	    for ( op=value ; (*ip) && *ip != '"' && *ip != '\n' ; ) {
		if (*ip == '\\' && *(ip+1) == '\n') {
		    while ( 1 ) {
			if (fgets (lbuf, SZ_LINE+1, fp) == NULL)
			    goto loop_quit;
			for (ip=lbuf;  my_isspace(*ip);  ip++)
			    ;
			if (*ip != '#') break;
		    }
		} else {
		    if ( op < maxop ) {
			*op = *ip;
			op++;
		    }
		    ip++;
		}
	    }
	loop_quit:
	    *op = XEOS;

	    /* Allow the user to override the values of environment variables
	     * by defining them in their host environment.  Once again,
	     * "sppincludes" requires special treatment as we want to permit
	     * redefinitions to allow concatenation in loadpkgenv().
	     */
	    os_strpak (name, pkname, SZ_FNAME+1);
	    if (strcmp (pkname, SPPINCLUDES) &&
		_os_getenv (pkname, old_value, SZ_VALUE+1)) {
		if (bdebug)
		    printf ("%s = %s\n", pkname, old_value);
	    } else
		ENVRESET (name, value);
	}

	fclose (fp);
}
#endif
