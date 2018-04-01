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
int
os_diropen (char *dirname)
{
	PKCHAR	osfn[SZ_PATHNAME+1];
	XINT	chan;

	extern  int ZOPDIR(PKCHAR *fname, XINT *chan);


	strcpy ((char *)osfn, dirname);
	ZOPDIR (osfn, &chan);

	return (chan);
}


/* OS_DIRCLOSE -- Close the directory.
 */
int
os_dirclose (int chan)
{
	XINT	x_chan=chan, status;

	extern  int ZCLDIR(XINT *chan, XINT *status);


	ZCLDIR (&x_chan, &status);
	return (status);
}


/* OS_GFDIR -- Get the next filename from the directory.
 */
int
os_gfdir (
  int	chan,
  char	*fname,
  int	maxch
)
{
	PKCHAR	osfn[SZ_PATHNAME+1];
	XINT	x_chan=chan, x_maxch=maxch, status;

	extern  int ZGFDIR(XINT *chan, PKCHAR *outstr, XINT *maxch, XINT *status);

	for (;;) {
	    ZGFDIR (&x_chan, osfn, &x_maxch, &status);
	    if (status > 0) {
		/* Omit the self referential directory files "." and ".."
		 * or recursion may result.
		 */
		if (strcmp ((char *)osfn, ".") == 0)
		    continue;
		if (strcmp ((char *)osfn, "..") == 0)
		    continue;

		strncpy (fname, osfn2vfn ((char *)osfn), maxch);
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

int os_dirclose (int chan) { return (-1); }
int os_diropen (char *dirname) { return (-1); }
int os_gfdir (int chan, char *fname, int maxch) { return (0); }

#endif
