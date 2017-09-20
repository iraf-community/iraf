/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>
#define	import_xnames
#include "bootlib.h"


char *_os_getenv(char *envvar, char *outstr, int maxch);


/* OS_GETENV -- Return the value of the named environment variable.  Null is
 * returned if the named variable is not found.
 */
char *
os_getenv (char *envvar)
{
	static	char	irafdir[SZ_PATHNAME+1] = "";
	static	char	hostdir[SZ_PATHNAME+1] = "";
	static	char	valstr[SZ_COMMAND+1];
	static	char	errmsg[] = "environment variable `%s' not found\n";
	extern	char	*os_subdir(char *dir, char *subdir);
	char	*vp;


	/* Try the standard environment first. */
	memset (valstr, 0, SZ_COMMAND+1);
	if ( (vp = _os_getenv (envvar, valstr, SZ_COMMAND)) )
	    return (vp);

	/* The following maps certain well-known IRAF logical directories
	 * even if there is no regular (VOS) environment facility.
	 */
	if (irafdir[0] == EOS)
	    if (_os_getenv ("iraf", irafdir, SZ_PATHNAME) == NULL) {
		fprintf (stderr, errmsg, "iraf");
		return (NULL);
	    }
	if (hostdir[0] == EOS)
	    if (_os_getenv ("host", hostdir, SZ_PATHNAME) == NULL) {
		fprintf (stderr, errmsg, "host");
		return (NULL);
	    }

	/* Map the names of the well known IRAF logical directories which
	 * are defined portably in terms of iraf$ or host$.
	 */
	if (       strcmp (envvar, "lib") == 0) 		/* iraf/. */
	    strcpy (valstr, os_subdir (irafdir, "lib"));
	else if (strcmp (envvar, "bin") == 0)
	    strcpy (valstr, os_subdir (irafdir, "bin"));
	else if (strcmp (envvar, "dev") == 0)
	    strcpy (valstr, os_subdir (irafdir, "dev"));
	else if (strcmp (envvar, "pkg") == 0)
	    strcpy (valstr, os_subdir (irafdir, "pkg"));
	else if (strcmp (envvar, "sys") == 0)
	    strcpy (valstr, os_subdir (irafdir, "sys"));
	else if (strcmp (envvar, "math") == 0)
	    strcpy (valstr, os_subdir (irafdir, "math"));
	else if (strcmp (envvar, "hlib") == 0)			/* host/. */
	    strcpy (valstr, os_subdir (hostdir, "hlib"));
	else if (strcmp (envvar, "as") == 0)
	    strcpy (valstr, os_subdir (hostdir, "as"));
	else
	    return (NULL);

	return (valstr);
}


#ifdef NOVOS
/* _OS_GETENV -- Fetch the value of the named environment variable from the
 * host environment.
 */
char *
_os_getenv (
  char	*envvar,		/* name of environment variable	*/
  char	*outstr,		/* receives value		*/
  int	maxch 
)
{
	PKCHAR	symbol[SZ_FNAME+1];
	PKCHAR	value[SZ_COMMAND+1];
	XINT	x_maxch = SZ_COMMAND, status=1;

	strcpy ((char *)symbol, envvar);
	ZGTENV (symbol, value, &x_maxch, &status);

	if (status < 0) {
	    outstr[0] = EOS;
	    return (NULL);
	} else {
	    strncpy (outstr, (char *)value, maxch);
	    outstr[maxch] = EOS;
	    return (outstr);
	}
}

#else
/* _OS_GETENV -- Fetch the value of the named environment variable from the
 * host environment.
 */
char *
_os_getenv (
  char	*envvar,		/* name of environment variable	*/
  char	*outstr,		/* receives value		*/
  int	maxch 
)
{
	XCHAR	x_symbol[SZ_FNAME+1];
	XCHAR	x_value[SZ_COMMAND+1];
	XINT	x_maxch = SZ_COMMAND, status=1;
	extern  XINT ENVFIND(XCHAR *key, XCHAR *value, XINT *maxch);


	os_strupk (envvar, x_symbol, SZ_FNAME);
	status = ENVFIND (x_symbol, x_value, &x_maxch);

	if (status <= 0) {
	    outstr[0] = EOS;
	    return (NULL);
	} else {
	    os_strpak (x_value, outstr, maxch);
	    return (outstr);
	}
}
#endif
