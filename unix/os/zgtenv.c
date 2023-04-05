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

#include "osproto.h"

static char *_ev_scaniraf (char *envvar);
static char *_ev_irafroot (void);


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
 *			    actual name of this directory is nowadays
 *			    usually "unix", but can be overwritten by an
 *			    environment variable.
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
	    ip = realloc(ip, strlen(ip) + 6);
	    strcat(ip, "unix/");
	} else if (strcmp(envvar, "iraf") == 0) {
 	    if ((ip = _ev_irafroot()) == NULL) {
		return (NULL);
	    }
	    if (ip[strlen(ip)-1] != '/') {
		ip = realloc(ip, strlen(ip) + 2);
		strcat(ip, "/");
	    }
	}

	if (ip != NULL) {
	    setenv(envvar, ip, 1);
	}

	return (ip);
}


/*
 * Code to bootstrap the IRAF environment list for UNIX.
 */

#define	IRAFROOT	"/etc/iraf/irafroot"


/* _EV_IRAFROOT -- Read one line of the irafroot file and return it
   (without the trailing newline).
 */
static char*
_ev_irafroot (void)
{
	char	*home, hpath[SZ_PATHNAME+1], *lbuf;
	FILE	*fp = NULL;

	if ((home = getenv ("HOME"))) {
	    sprintf (hpath, "%s/.iraf/irafroot", home);
	    fp = fopen (hpath, "r");
	}
	if (fp == NULL) {
	    if ((fp = fopen (IRAFROOT, "r")) == NULL) {
	        fprintf (stderr, "os.zgtenv: cannot open `%s'\n", IRAFROOT);
	        return (NULL);
	    }
	}

	lbuf = malloc(SZ_LINE+1);
	if (fgets (lbuf, SZ_LINE, fp) == NULL) {
	    fclose(fp);
	    return (NULL);
	} else {
	    fclose(fp);
	    if (lbuf[strlen(lbuf)-1] == '\n') {
	        lbuf[strlen(lbuf)-1] = 0;
	    }
	    return (lbuf);
	}
}
