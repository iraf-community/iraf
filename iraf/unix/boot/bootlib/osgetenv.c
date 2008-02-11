/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>

#define import_xnames
#include "bootlib.h"


/* OS_GETENV -- Return the value of the named environment variable.  Null is
 * returned if the named variable is not found.
 */
const char *os_getenv ( const char *envvar )
{
	static char irafdir[SZ_PATHNAME+1] = "";
	static char hostdir[SZ_PATHNAME+1] = "";
	static char valstr[SZ_COMMAND+1];
	static const char *errmsg = "environment variable `%s' not found\n";
	char *vp;

	/* Try the standard environment first. */
	if (vp = _os_getenv (envvar, valstr, SZ_COMMAND+1))
	    return (vp);

	/* The following maps certain well-known IRAF logical directories
	 * even if there is no regular (VOS) environment facility.
	 */
	if (irafdir[0] == EOS)
	    if (_os_getenv ("iraf", irafdir, SZ_PATHNAME+1) == NULL) {
		fprintf (stderr, errmsg, "iraf");
		return (NULL);
	    }
	if (hostdir[0] == EOS)
	    if (_os_getenv ("host", hostdir, SZ_PATHNAME+1) == NULL) {
		fprintf (stderr, errmsg, "host");
		return (NULL);
	    }

	/* Map the names of the well known IRAF logical directories which
	 * are defined portably in terms of iraf$ or host$.
	 */
	if (     strcmp (envvar, "lib") == 0)			/* iraf/. */
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (irafdir, "lib"));
	else if (strcmp (envvar, "config") == 0)
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (irafdir, "config"));
	else if (strcmp (envvar, "base") == 0)
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (irafdir, "base"));
	else if (strcmp (envvar, "bin") == 0)
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (irafdir, "bin"));
	else if (strcmp (envvar, "dev") == 0)
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (irafdir, "dev"));
	else if (strcmp (envvar, "pkg") == 0)
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (irafdir, "pkg"));
	else if (strcmp (envvar, "sys") == 0)
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (irafdir, "sys"));
	else if (strcmp (envvar, "math") == 0)
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (irafdir, "math"));

	else if (strcmp (envvar, "hlib") == 0)			/* host/. */
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (hostdir, "lib"));
	else if (strcmp (envvar, "hconfig") == 0)
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (hostdir, "config"));
	else if (strcmp (envvar, "as") == 0)
	    safe_strcpy(valstr, SZ_COMMAND+1, os_subdir (hostdir, "as"));
	else
	    return (NULL);

	return (valstr);
}


#ifdef NOVOS
/* _OS_GETENV -- Fetch the value of the named environment variable from the
 * host environment.
 */
/* envvar : name of environment variable	*/
/* outstr : receives value			*/
char *_os_getenv ( const char *envvar, char *outstr, size_t bufsize )
{
	PKCHAR	symbol[SZ_FNAME+1];
	PKCHAR	value[SZ_COMMAND+1];
	XINT	x_maxch = SZ_COMMAND, status=1;

	safe_strcpy ((char *)symbol, SZ_FNAME+1, envvar);
	ZGTENV (symbol, value, &x_maxch, &status);

	if (status < 0) {
	    outstr[0] = EOS;
	    return (NULL);
	} else {
	    safe_strcpy (outstr, bufsize, (const char *)value);
	    return (outstr);
	}
}

#else
/* _OS_GETENV -- Fetch the value of the named environment variable from the
 * host environment.
 */
char *_os_getenv ( const char *envvar, char *outstr, size_t bufsize )
/* envvar : name of environment variable	*/
/* outstr : receives value			*/
{
	XCHAR	x_symbol[SZ_FNAME+1];
	XCHAR	x_value[SZ_COMMAND+1];
	XINT	x_maxch = SZ_COMMAND, status=1;

	os_strupk (envvar, x_symbol, SZ_FNAME+1);
	status = ENVFIND (x_symbol, x_value, &x_maxch);

	if (status <= 0) {
	    outstr[0] = EOS;
	    return (NULL);
	} else {
	    os_strpak (x_value, outstr, bufsize);
	    return (outstr);
	}
}
#endif
