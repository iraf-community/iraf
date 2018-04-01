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
static int   _ev_loadcache (char *fname);


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
	register char	*ip, *op;
	register int	n;


	op = (char *)outstr;
	if ((ip = getenv ((char *)envvar)) == NULL)
	    ip = _ev_scaniraf ((char *)envvar);

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
#define	NENV		3			/* n variables		*/
#define	SZ_NAME		10
#define	SZ_VALUE	80

struct	env {
	char	*ev_name;
	char	*ev_value;
};

int	ev_cacheloaded = 0;
struct	env ev_table[NENV] = {
	{ "host",		""},
	{ "iraf",		""},
	{ "tmp",		""}
};


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
 * The entries for these variables in the <iraf.h> must adhere to a standard
 * format, e.g. (substituting @ for *):
 *
 *	/@ ### Start of run time definitions @/
 *	#define	iraf		"/iraf/"
 *	#define	host		"/iraf/unix/"
 *	#define	tmp		"/tmp/"
 *	/@ ### End of run time definitions @/
 *
 * Although the definitions are entered as standard C #defines, they should not
 * be directly referenced in C programs.
 */
static char *
_ev_scaniraf (char *envvar)
{
	int	i;


	for (i=0;  i < NENV;  i++)
	    if (strcmp (ev_table[i].ev_name, envvar) == 0)
		break;

	if (i >= NENV)
	    return (NULL);

	if (!ev_cacheloaded) {
	    if (_ev_loadcache (TABLE) == ERR)
		return (NULL);
	    else
		ev_cacheloaded++;
	}

	return (ev_table[i].ev_value);
}


/* _EV_LOADCACHE -- Follow the link <iraf.h> to the path of the IRAF
 * installation.  Cache these in case we are called again (they do not
 * change so often that we cannot cache them in memory).  Any errors
 * in accessing the table probably indicate an error in installing
 * IRAF hence should be reported immediately.
 */
static int
_ev_loadcache (char *fname)
{
        static  char   *home, hpath[SZ_PATHNAME+1], *rpath, *lpath;

	rpath = malloc(SZ_PATHNAME+1);
	if ((home = getenv ("HOME"))) {
	    sprintf (hpath, "%s/.iraf/iraf.h", home);
	    if ((realpath(hpath, rpath)) == NULL) {
                if ((realpath(fname, rpath)) == NULL) {
		    fprintf (stderr, "os.zgtenv: cannot follow link `%s'\n", fname);
		    free(rpath);
		    return (ERR);
		}
	    }
	} else {
	    /*  We should always have a $HOME, but try this to be safe.
	     */
	  if ((realpath(fname, rpath)) == NULL) {
	        fprintf (stderr, "os.zgtenv: cannot follow link `%s'\n", fname);
		free(rpath);
		return (ERR);
	    }
	}

	/* host */
	lpath = strdup(dirname(rpath));
	free(rpath);
	rpath = strdup(dirname(lpath));
	free(lpath);
	ev_table[0].ev_value = strdup(dirname(rpath)); 
	free(rpath);
	ev_table[0].ev_value = realloc(ev_table[0].ev_value,
				       strlen(ev_table[0].ev_value) + 2);
	strcat(ev_table[0].ev_value, "/");

	/* iraf */
	rpath = strdup(ev_table[0].ev_value);
	ev_table[1].ev_value = strdup(dirname(rpath));
	free(rpath);
	ev_table[1].ev_value = realloc(ev_table[1].ev_value,
				       strlen(ev_table[1].ev_value) + 2);
	strcat(ev_table[1].ev_value, "/");

	/* tmp */
	ev_table[2].ev_value = getenv("TMPDIR");
	if (ev_table[2].ev_value == NULL) {
	  ev_table[2].ev_value = P_tmpdir;
	}
	ev_table[2].ev_value = strdup(ev_table[2].ev_value);
	if (ev_table[2].ev_value[strlen(ev_table[2].ev_value)-1] != '/') {
	  ev_table[2].ev_value = realloc(ev_table[2].ev_value,
					 strlen(ev_table[2].ev_value) + 2);
	  strcat(ev_table[2].ev_value, "/");
	}
	
	return (OK);
}
