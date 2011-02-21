/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <strings.h>
#define import_spp
#include <iraf.h>

/*
 * OS_FILETYPE -- Determine whether the named file is a text file, a binary
 * file, or a directory.  The filename extensions used to speed up the test
 * are portable provided osfn2vfn() is called to map the OSFN before we are
 * called.
 */

char *binextn[] = {		/* Known binary file extensions */
	".o",
	".e",
	".a",
	".so",
	".pyc",
	NULL
};

char *srcextn[] = {		/* Known source file extensions */
	".x",
	".h",
	".f",
	".f77",
	".f90",
	".s",
	".c",
	".cpp",
	".hlp",
	".mip",
	".imh",
	".pix",
	".gki",
	".vdm",
	".fits",
	".fit",
	".ftz",
	".pl",
	".gif",
	".jpeg",
	".jpg",
	".tiff",
	".tif",
	".png",
	".gz",
	".tar",
	".jar",
	".java",
	".py",
	".pdf",
	".ps",
	".hqx",
	".std",
	NULL
};

extern 	int  os_access (char *fname, int mode, int type);



/* OS_FILETYPE -- Determine the type of a file.  If the file has one of the
 * known source file extensions we assume it is a text file; if it has a well
 * known binary file extension we assume it is a binary file; otherwise we call
 * os_access to determine the file type.
 */
int
os_filetype (
  char	*fname			/* name of file to be examined	*/
)
{
	register char	*ip, *ep;
	register int	ch, i;
	char	*extn;


	/* Get filename extension.
	 */
	extn = NULL;
	for (ip=fname;  (ch = *ip);  ip++)
	    if (ch == '.')
		extn = ip;

	/* If the filename has a extension, check the list of known text and 
	 * binary file extensions to see if we can make a quick determination
	 * of the file type.
	 */
	if (extn) {
	    ch = *(extn + 1);

	    /* Known source file extension? */
	    for (i=0;  (ep = srcextn[i]);  i++)
		if (*(ep+1) == ch)
		    if (strcasecmp (ep, extn) == 0)
			return (TEXT_FILE);

	    /* Known binary file extension? */
	    for (i=0;  (ep = binextn[i]);  i++)
		if (*(ep+1) == ch)
		    if (strcasecmp (ep, extn) == 0)
			return (BINARY_FILE);
	}

	/* Call ACCESS to determine the file type.
	 */
	if (os_access (fname, READ_ONLY, DIRECTORY_FILE) == YES)
	    return (DIRECTORY_FILE);
	else if (os_access (fname, 0, TEXT_FILE) == YES)
	    return (TEXT_FILE);
	else
	    return (BINARY_FILE);
}
