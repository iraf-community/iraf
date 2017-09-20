/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#define	import_spp
#define	import_xnames
#include <iraf.h>

#define	isspace(c)	((c)==' '||(c)=='\t'||(c)=='\n')
#define	SETENV		"zzsetenv.def"
#define	SZ_VALUE	SZ_COMMAND
#define	MAXLEV		8
#define	PKGLIBS		"pkglibs"
#define	IRAFARCH	"IRAFARCH"
#define	ARCH		"arch"

extern	char  *_os_getenv (char *envvar, char *outstr, int maxch);
extern	char  *os_getenv (char *envvar);
extern	char  *os_strpak (XCHAR *sppstr, char *cstr, int maxch);
extern	char  *vfn2osfn (char *vfn, int new);
extern  XCHAR *os_strupk (char *str, XCHAR *outstr, int maxch);
extern	void   os_putenv (char *name, char *value);
extern	int bdebug;

void 	_envinit (void);
void 	loadenv (char *osfn);



/* LOADPKGENV -- Load the environment definitions for the named package.
 * [e.g., loadpkgenv ("noao")].  This assumes that the root directory of
 * the named package is already defined, and that this directory contains
 * a subdirectory lib containing the file zzsetenv.def.  If none of these
 * assumptions are true, call loadenv(osfn) with the host filename of the
 * file to be loaded.
 */
void
loadpkgenv (char *pkg)
{
	char	vfn[SZ_PATHNAME+1];
	char	pkglibs[SZ_COMMAND+1];
	char	newlibs[SZ_COMMAND+1];

	/* Initialize the default IRAF environment. */
	_envinit();

	/* If no package name is given or the IRAF environment is being
	 * loaded we are done.
	 */
	if (!pkg || strcmp(pkg,"iraf")==0)
	    return;

	strcpy (vfn, pkg);
	strcat (vfn, "$lib/");
	strcat (vfn, SETENV);

	/* Load the package environment.  The new values are added to the
	 * environment in the conventional way except for the value of
	 * "pkglibs".  As each package environment is loaded we want to
	 * add the newly defined package libraries to the current list
	 * of package libraries, otherwise the most recent package environment
	 * overrides the earlier ones.  It is still possible that user
	 * defined environment variables will be redefined but there is
	 * little we can do about that; "pkglibs" is special though since
	 * it is a part of the loadpkgenv facility.
	 */
	_os_getenv (PKGLIBS, pkglibs, SZ_COMMAND);
	loadenv (vfn2osfn (vfn, 0));
	_os_getenv (PKGLIBS, newlibs, SZ_COMMAND);

	if (strlen(newlibs) > 0 && strcmp (newlibs, pkglibs)) {
	    char    *ip, *op;
	    char    *otop;

	    /* Find the end of the current pkglibs file list. */
	    for (ip=op=pkglibs;  *ip;  ip++)
		if (!isspace(*ip))
		    op = ip + 1;

	    /* Concatenate the new files list segment. */
	    if (op > pkglibs)
		*op++ = ',';
	    for (ip=newlibs, otop=pkglibs+SZ_COMMAND;  *ip && op < otop;  ip++)
		if (!isspace(*ip))
		    *op++ = *ip;

	    /* Blank fill to the next SZ_LINE increment to optimize resets. */
	    while (op < otop && ((op-pkglibs) % SZ_LINE))
		*op++ = ' ';
	    *op++ = EOS;
		
	    /* Reset the stored value in the environment. */
	    os_putenv (PKGLIBS, pkglibs);
	}
}


#ifdef NOVOS
void _envinit (void) {}
void loadenv (char *osfn) { printf ("HSI is compiled NOVOS\n"); }
#else

/* ENVINIT -- Initialize the VOS environment list by scanning the file
 * hlib$zzsetenv.def.  HLIB is defined in terms of HOST which is sufficiently
 * well known to have a value before the environment list is loaded.
 */
void
_envinit (void)
{
	static	int initialized = 0;
	char	osfn[SZ_PATHNAME+1], *hlib;
	char	irafarch[SZ_PATHNAME+1];

	extern  void ENVINIT(void), ENVRESET(XCHAR *key, XCHAR *value);


	if (initialized++)
	    return;

	if ( (hlib = os_getenv ("hlib")) ) {
	    strcpy (osfn, hlib);
	    strcat (osfn, SETENV);
	} else {
	    fprintf (stderr, "cannot translate logical name `hlib'");
	    fflush (stderr);
	}

	ENVINIT();
	loadenv (osfn);

	/* If the variable "IRAFARCH" is defined and "arch" is not, add
	 * a definition for the latter.  "arch" is used to construct
	 * pathnames but the HSI architecture support requires only that
	 * IRAFARCH be predefined.
	 */
	if (_os_getenv (IRAFARCH, irafarch, SZ_PATHNAME))
	    if (!_os_getenv (ARCH, osfn, SZ_PATHNAME)) {
		XCHAR   x_name[SZ_PATHNAME+1];
		XCHAR   x_value[SZ_PATHNAME+1];

		sprintf (osfn, ".%s", irafarch);
		os_strupk (ARCH, x_name, SZ_PATHNAME);
		os_strupk (osfn, x_value, SZ_PATHNAME);
		ENVRESET (x_name, x_value);
	    }
}


/* LOADENV -- Load environment definitions from the named host file.
 */
void
loadenv (char *osfn)
{
	register char	*ip;
	register XCHAR	*op;

	char	lbuf[SZ_LINE+1];
	char	pkname[SZ_FNAME+1], old_value[SZ_VALUE+1];
	XCHAR	name[SZ_FNAME+1], value[SZ_VALUE+1];
	FILE	*fp, *sv_fp[MAXLEV];
	int	lev=0;

	extern  void ENVRESET(XCHAR *key, XCHAR *value);


	if ((fp = fopen (osfn, "r")) == NULL) {
	    printf ("envinit: cannot open `%s'\n", osfn);
	    fflush (stdout);
	    return;
	}

	for (;;) {
	    /* Get next line from input file. */
	    if (fgets (lbuf, SZ_LINE, fp) == NULL) {
		/* End of file. */
		if (lev > 0) {
		    fclose (fp);
		    fp = sv_fp[--lev];
		    continue;
		} else
		    break;

	    } else {
		/* Skip comments and blank lines. */
		for (ip=lbuf;  isspace(*ip);  ip++)
		    ;
		if (strncmp (lbuf, "set", 3) != 0) {
		    if (strncmp (lbuf, "reset", 5) != 0)
			continue;
		    else
			ip += 5;
		} else
		    ip += 3;

		/* Check for @file inclusion. */
		while (isspace(*ip))
		    ip++;

		if (*ip == '@') {
		    sv_fp[lev++] = fp;
		    if (lev >= MAXLEV) {
			printf ("envinit: nesting too deep\n");
			fflush (stdout);
			break;

		    } else {
			char *fname;
			fname = ++ip;

			while (*ip)
			    if (isspace(*ip)) {
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
	    for (op=name;  *ip && *ip != '=' && !isspace(*ip);  op++)
		*op = *ip++;
	    *op = XEOS;

	    /* Extract value field; may be quoted.  Newline may be escaped
	     * to break a long value string over several lines of the input
	     * file.
	     */
	    for (; *ip && (*ip == '=' || *ip == '"' || isspace (*ip));  ip++)
		;
	    for (op=value;  *ip && *ip != '"' && *ip != '\n';  op++)
		if (*ip == '\\' && *(ip+1) == '\n') {
again:		    if (fgets (lbuf, SZ_LINE, fp) == NULL)
			break;
		    for (ip=lbuf;  isspace(*ip);  ip++)
			;
		    if (*ip == '#')
			goto again;
		} else
		    *op = *ip++;
	    *op = XEOS;

	    /* Allow the user to override the values of environment variables
	     * by defining them in their host environment.  Once again,
	     * "pkglibs" requires special treatment as we want to permit
	     * redefinitions to allow concatenation in loadpkgenv().
	     */
	    os_strpak (name, pkname, SZ_FNAME);
	    if (strcmp (pkname, PKGLIBS) &&
		_os_getenv (pkname, old_value, SZ_VALUE)) {
		if (bdebug)
		    printf ("%s = %s\n", pkname, old_value);
	    } else
		ENVRESET (name, value);
	}

	if (fp != NULL) {
	    fclose (fp);
	}
}
#endif
