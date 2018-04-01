/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>
#include "bootlib.h"

/* Uncomment the following if the kernel for this machine does not need
 * or provide its own custom irafpath function, used if it can not be easily
 * determine in advance what directories need to be searched.
 */
/* #define STANDALONE */

#ifdef STANDALONE
#define	irafpath os_irafpath
#endif

char	*irafpath(char *sysfile);
char	*os_getenv(char *envvar);
extern  int  os_access (char *fname, int mode, int type);


/* OS_SYSFILE -- Return the pathname of a system library file.  The library
 * search order is
 *
 *	IRAFULIB libraries, if any
 *	HSI system libraries (lib, hlib, hbin, etc.)
 *	pkglibs applications libraries, if any
 *
 * Hence, the IRAFULIB mechanism may be used to make use of custom copies
 * of system files (libraries or global include files), whereas the `pkglibs'
 * mechanism is provided to extend the system library search path to include
 * applications specified libraries.  These are intended to be the global
 * libraries of installed layered packages, rather than private user libraries
 * (the IRAFULIB mechanism is better for the latter).
 */
int
os_sysfile (
  char	*sysfile,		/* filename from include statement	*/
  char	*fname,			/* receives filename			*/
  int	maxch 
)
{
	register char *ip, *op;
	char	*files, *ip_save;


	/* Search the standard system libraries and exit if the named
	 * file is found.
	 */
	strncpy (fname, irafpath(sysfile), maxch);
	fname[maxch-1] = EOS;
	if (strcmp (fname, sysfile) != 0)
	    return (strlen (fname));

	/* Search the designated package libraries, if any.
	 */
	if ( (files = os_getenv ("pkglibs")) ) {
	    for (ip=files;  *ip;  ) {
		/* Get the next library name from the list. */
		while (isspace(*ip) || *ip == ',')
		    ip++;
		for (op=fname;  *ip && !isspace(*ip) && *ip != ',';  op++)
		    *op = *ip++;
		*op = EOS;

		/* Append the target filename. */
		for (ip_save=ip, (ip=sysfile);  (*op++ = *ip++);  )
		    ;
		ip = ip_save;

		/* Exit if the file exists. */
		if (os_access (fname, 0, 0))
		    return (strlen (fname));
	    }
	}

	return (ERR);
}


#ifdef STANDALONE
static	char *libs[] = { "iraf$lib/", "host$hlib/", "" };

/* OS_IRAFPATH -- Portable version of the kernel irafpath() function, used
 * if only the standard directories LIB and HLIB need to be searched.
 */
char *
os_irafpath (
    char *sysfile		/* filename from include statement	*/
)
{
	register char	*ip, *op;
	register int	n;
	static	char outfname[SZ_PATHNAME+1];
	char	fname[SZ_PATHNAME+1];
	int	i;

	strcpy (outfname, sysfile);

	for (i=0;  libs[i][0] != EOS;  i++) {
	    strcpy (fname, libs[i]);
	    strcat (fname, sysfile);
	    if (os_access (fname, 0,0) == YES) {
		n = SZ_PATHNAME;
		for (ip=fname, op=outfname;  --n >= 0 && (*op = *ip++);  op++)
		    ;
		*op = EOS;
		break;
	    }
	}

	return (outfname);
}
#endif
