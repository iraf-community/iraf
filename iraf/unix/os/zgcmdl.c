#include <stdio.h>
#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

extern	char *environ[];

#ifdef LINUXPPC
#define xargc	f__xargc
#define xargv	f__xargv
#endif

#ifdef LINUX
extern	char **xargv;		/* defined in getarg(3f); requires libU77! */
extern	int xargc;
#else
static	char **xargv = NULL;
static	int xargc = 0;
#endif

/* ZGCMDL -- Get the host system command line used to invoke this process.
 * There does not seem to be any straightforward way to do this for UNIX,
 * but the argc,argv info is evidently pushed on the stack immediately before
 * the environment list, so we can locate the ARGV array by searching back
 * up the stack a bit.  This is very host OS dependent.
 */
/* cmd   : receives the command line	*/
/* maxch : maxch chars out		*/
int ZGCMDL ( PKCHAR *cmd, XINT *maxch, XINT *status )
{
	char **argv;
	const char *ip;
	char *op, *maxop;
	int n;
	unsigned int *ep;
	size_t bufsize = *maxch + 1;

	if (!(argv = xargv)) {
	    /* Locate the ARGV array.  This assumes that argc,argv are
	     * stored in memory immediately preceeding the environment
	     * list, i.e.,
	     *
	     *	argc
	     *	argv[0]
	     *	argv[1]
	     *	  ...
	     *	argv[argc-1]
	     *	NULL
	     *      env[0]			<- environ
	     *      env[1]
	     *	  ...
	     *
	     * !! NOTE !! - This is very system dependent!
	     */
	    ep = ((unsigned int *) *environ) - 1;
	    for (n=0;  *(ep-1) != (unsigned int)n;  n++)
		--ep;
	    argv = (char **)ep;
	}

	/* Reconstruct the argument list.
	 */
	maxop = (char *)cmd + bufsize -1;
	for ( op=(char *)cmd, argv++ ; op < maxop && *argv ; argv++ ) {
	    if ( (char *)cmd < op && op < maxop ) {
		*op++ = ' ';
	    }
	    for ( ip = *argv ; op < maxop && (*ip) ; op++, ip++ )
		*op = *ip;
	}

	if ( op <= maxop ) *op = EOS;
	*status = op - (char *)cmd;

	return *status;
}
