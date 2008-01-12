/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>
#include "bootlib.h"

/*
 * OS_DIR -- A package for accessing a directory as a list of files.
 */

#ifndef NOVOS

/* OS_DIROPEN -- Open the directory.
 */
int os_diropen ( const char *dirname )
{
	PKCHAR	osfn[SZ_PATHNAME+1];
	XINT	chan;

	safe_strcpy ((char *)osfn, SZ_PATHNAME+1, dirname);
	ZOPDIR (osfn, &chan);

	return (chan);
}


/* OS_DIRCLOSE -- Close the directory.
 */
int os_dirclose ( int chan )
{
	XINT	status, x_chan;

	x_chan = chan;
	ZCLDIR (&x_chan, &status);
	return (status);
}


/* OS_GFDIR -- Get the next filename from the directory.
 */
int os_gfdir ( int chan, char *fname, size_t bufsize )
{
	PKCHAR	osfn[SZ_PATHNAME+1];
	XINT	x_maxch, status, x_chan;

	x_chan = chan;
	x_maxch = SZ_PATHNAME+1 - 1;
	for (;;) {
	    ZGFDIR (&x_chan, osfn, &x_maxch, &status);
	    if (status > 0) {
		/* Omit the self referential directory files "." and ".."
		 * or recursion may result.
		 */
		if (strcmp ((const char *)osfn, ".") == 0)
		    continue;
		if (strcmp ((const char *)osfn, "..") == 0)
		    continue;

		safe_strcpy (fname, bufsize, osfn2vfn ((const char *)osfn));
		return (status);

	    } else {
		/* End of directory.
		 */
		*fname = EOS;
		return (0);
	    }
	}
}

#else
/* NOVOS bootsrap.  Just stub these out until we re-boostrap using the
 * VOS libs, which provide zopdir.
 */

int os_dirclose ( int chan ) { return (-1); }
int os_diropen ( const char *dirname ) { return (-1); }
int os_gfdir ( int chan, char *fname, size_t bufsize ) { return (0); }

#endif
