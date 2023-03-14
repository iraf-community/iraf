/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <libgen.h>
#define import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

static char *_ev_scaniraf (char *envvar);
static char *_ev_guess_irafdir (char *fname);


/* ZGTENV -- Get the value of a host system environment variable.  Look first
 * in the process environment.  If no entry is found there and the variable is
 * one of the standard named variables, get the system wide default value from
 * the file <iraf.h>, which is assumed to be located in /usr/include.
 */
int
ZGTENV (
  PKCHAR  *envvar,		/* name of variable to be fetched	*/
  PKCHAR  *outstr,		/* output string			*/
  XINT	  *maxch, 
  XINT    *status
)
{
	char	*ip, *op;
	int	n;

	if ((ip = getenv ((char *)envvar)) == NULL) {
	    ip = _ev_scaniraf ((char *)envvar);
	}

	op = (char *)outstr;
	if (ip == NULL) {
	    *op = EOS;
	    *status = XERR;
	} else {
	    *status = 0;
	    op[*maxch] = EOS;
	    for (n = *maxch;  --n >= 0 && (*op++ = *ip++);  )
		(*status)++;
	}

	return (XOK);
}


/*
 * Code to bootstrap the IRAF environment list for UNIX.
 */

#define	TABLE		"/usr/include/iraf.h"	/* table file		*/
#define	SZ_NAME		10
#define	SZ_VALUE	80


/* SCANIRAF -- If the referenced environment variable is a well known standard
 * variable, scan the file <iraf.h> for its system wide default value.  This
 * is done at run time rather than compile time to make it possible to make
 * changes to these variables (e.g., relocate iraf to a different root
 * directory) without recompiling major parts of the system.
 *
 * Virtually all IRAF environment variables are defined in the source code and
 * are portable.  In particular, virtually all source directories are defined
 * relative to the IRAF root directory "iraf$".  Only those definitions which
 * are both necessarily machine dependent and required for operation of the
 * bootstrap C programs (e.g., the CL, XC, etc.) are satisfied at this level.
 * These variables are the following.
 *
 *	iraf		The root directory of IRAF; if this is incorrect,
 *			    bootstrap programs like the CL will not be able
 *			    to find IRAF files.
 *
 *	host		The machine dependent subdirectory of iraf$.  The
 *			    actual name of this directory varies from system
 *			    to system (to avoid name collisions on tar tapes),
 *		 	    hence we cannot use "iraf$host/".
 *			    Examples: iraf$unix/, iraf$vms/, iraf$sun/, etc.
 *
 *	tmp		The place where IRAF is to put its temporary files.
 *			    This is normally /tmp/ for a UNIX system.  TMP
 *			    also serves as the default IMDIR.
 *	
 * The iraf and host names are derived from the origin of the symbolic link
 * to /usr/include/iraf.h, if not given as environment variable.
 *
 * Although the definitions are entered as standard C #defines, they should not
 * be directly referenced in C programs.
 */
static char *
_ev_scaniraf (char *envvar)
{
	char	*ip = NULL;

	if (strcmp(envvar, "tmp") == 0) {
	    if ((ip = getenv("TMPDIR")) == NULL) {
		ip = P_tmpdir;
	    }
	    ip = strdup(ip);
	    if (ip[strlen(ip)-1] != '/') {
		ip = realloc(ip, strlen(ip) + 2);
		strcat(ip, "/");
	    }
	} else if (strcmp(envvar, "host") == 0) {
	    if ((ip = getenv("iraf")) != NULL) {
	        ip = strdup(ip);
	    } else if ((ip = _ev_scaniraf("iraf")) == NULL) {
	        return (NULL);
	    }
	    ip = realloc(ip, strlen(ip) + 7);
	    strcat(ip, "/unix/");
	} else if (strcmp(envvar, "iraf") == 0) {
 	    if ((ip = _ev_guess_irafdir(TABLE)) == NULL) {
		return (NULL);
	    }
	    ip = realloc(ip, strlen(ip) + 2);
	    strcat(ip, "/");
	}

	if (ip != NULL) {
	    setenv(envvar, ip, 1);
	}

	return (ip);
}


/* _EV_LOADCACHE -- Follow the link <iraf.h> to the path of the IRAF
 * installation.  Cache these in case we are called again (they do not
 * change so often that we cannot cache them in memory).  Any errors
 * in accessing the table probably indicate an error in installing
 * IRAF hence should be reported immediately.
 */
static char*
_ev_guess_irafdir (char *fname)
{
  static  char	 *home, hpath[SZ_PATHNAME+1], *rpath, *path0, *path1, *path2;

	rpath = malloc(SZ_PATHNAME+1);
	if ((home = getenv ("HOME"))) {
	    sprintf (hpath, "%s/.iraf/iraf.h", home);
	    if ((realpath(hpath, rpath)) == NULL) {
		if ((realpath(fname, rpath)) == NULL) {
		    fprintf (stderr, "os.zgtenv: cannot follow link `%s'\n", fname);
		    free(rpath);
		    return (NULL);
		}
	    }
	} else {
	    /*	We should always have a $HOME, but try this to be safe.
	     */
	  if ((realpath(fname, rpath)) == NULL) {
		fprintf (stderr, "os.zgtenv: cannot follow link `%s'\n", fname);
		free(rpath);
		return (NULL);
	    }
	}

	path0 = strdup(dirname(rpath));
	free(rpath);
	path1 = strdup(dirname(path0));
	free(path0);
	path2 = strdup(dirname(path1));
	free(path1);
	return (path2);
}
