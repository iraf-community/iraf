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
#define irafpath os_irafpath
const char *irafpath( const char * );
#else
/* os/irafpath.c */
extern const char *irafpath( const char * );
#endif

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
/* sysfile : filename from include statement	*/
/* envname : e.g. "sppincludes"			*/
/* fname   : receives filename			*/
int os_sysfile ( const char *sysfile, const char *envname, 
		 char *fname, size_t bufsize )
{
	const char *ip;
	char *op, *maxop;
	const char *files, *ip_save;
	const char *envnames[] = {envname ,NULL};
	int i;

	/* Search the standard system libraries and exit if the named
	 * file is found.
	 */
	safe_strcpy (fname, bufsize, irafpath(sysfile));
	if (strcmp (fname, sysfile) != 0)
	    return (strlen (fname));

	/* Search the designated package libraries, if any.
	 */
	for ( i=0 ; envnames[i] != NULL ; i++ ) {
	  files = os_getenv (envnames[i]);
	  if ( files != NULL ) {
	    for (ip=files;  *ip;  ) {
		/* Get the next library name from the list. */
		while (isspace(*ip) || *ip == ',')
		    ip++;
		maxop = fname + bufsize -1;
		for ( op=fname ; (*ip) && !isspace(*ip) && *ip != ',' ; ip++ ) {
		    if ( op < maxop ) {
			*op = *ip;
			op++;
		    }
		}
		if ( op <= maxop ) *op = EOS;

		/* Append the target filename. */
		ip_save=ip;
		for ( ip=sysfile ; op < maxop && (*ip) ; op++, ip++ )
		    *op = *ip;
		if ( op <= maxop ) *op = EOS;
		ip = ip_save;

		/* Exit if the file exists. */
		if (os_access (fname, 0, 0))
		    return (strlen (fname));
	    }
	  }
	}

	return (ERR);
}


#ifdef STANDALONE
static const char *libs[] = { "iraf$config/", "iraf$base/", "host$config/", "" };

/* OS_IRAFPATH -- Portable version of the kernel irafpath() function, used
 * if only the standard directories LIB and HLIB need to be searched.
 */
/* sysfile : filename from include statement	*/
const char *os_irafpath ( const char *sysfile )
{
	static char outfname[SZ_PATHNAME+1];
	char fname[SZ_PATHNAME+1];
	const char *ip;
	char *op;
	int i;

	safe_strcpy (outfname, SZ_PATHNAME+1, sysfile);

	for (i=0;  libs[i][0] != EOS;  i++) {
	    strcpy (fname, libs[i]);
	    safe_strcat (fname, SZ_PATHNAME+1, sysfile);
	    if (os_access (fname, 0,0) == YES) {
		for ( ip=fname, op=outfname ; (*ip) ; op++, ip++ )
		    *op = *ip;
		*op = EOS;
		break;
	    }
	}

	return (outfname);
}
#endif
