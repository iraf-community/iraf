/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/dir.h>
#include <stdio.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZOPDIR -- Open a directory file.  A directory file is interfaced to FIO
 * as a textfile, using a portable set of textfile driver subroutines.
 * The directory access primitives contained in this file are called by the
 * driver subroutines to read successive machine dependent filenames from
 * a directory.
 */
ZOPDIR (fname, chan)
PKCHAR	*fname;
XINT	*chan;
{
	char	osfn[SZ_PATHNAME+1];
	register char *ip, *op;

	/* The file name should have an "/" appended, if it is a proper
	 * directory prefix.  This must be removed to get the name of the
	 * directory file.
	*/
	for (ip=(char *)fname, op=osfn;  (*op = *ip++) != EOS;  op++)
	    ;
	if (*--op == '/')
	    *op = EOS;

	*chan = (XINT)opendir(osfn);
	if (*chan == NULL)
	    *chan = XERR;
}


/* ZCLDIR -- Close a directory file.
 */
ZCLDIR (chan, status)
XINT	*chan;
XINT	*status;
{
	closedir ((DIR *)*chan);
	*status = XOK;
}


/* ZGFDIR -- Get the next file name from an open directory file.  We are
 * called by the text file driver for a directory file, hence file names
 * are returned as simple packed strings.
 */
ZGFDIR (chan, outstr, maxch, status)
XINT	*chan, *maxch, *status;
PKCHAR	*outstr;
{
	register struct	direct *dp;
	register int	n;
	register char	*ip, *op;
	DIR	*dirp = (DIR *)*chan;

	for (dp = readdir(dirp);  dp != NULL;  dp = readdir(dirp))
	    if (dp->d_ino != 0) {
		n = (dp->d_namlen < *maxch) ? dp->d_namlen : *maxch;
		*status = n;
		for (ip=dp->d_name, op=(char *)outstr;  --n >= 0;  )
		    *op++ = *ip++;
		*op = EOS;
		return;
	    }

	*status = XEOF;
}
