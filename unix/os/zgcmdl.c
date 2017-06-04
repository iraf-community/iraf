#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

extern	char *environ[];
#ifdef	__APPLE__
extern  char ***_NSGetArgv();
extern  int *_NSGetArgc();
#endif

#ifdef __linux__
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
int
ZGCMDL (
  PKCHAR  *cmd,				/* receives the command line	*/
  XINT	  *maxch,			/* maxch chars out		*/
  XINT	  *status
)
{
	register char *ip, *op;
	register int   n;
	char	**argv;

#ifdef	__APPLE__
	argv = *_NSGetArgv();
	xargc = *_NSGetArgc();
	xargv = argv;

#else
	unsigned int  *ep;
	register int  narg;


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
	    for (narg=0;  *(ep-1) != (unsigned int)narg;  narg++)
		--ep;
	    xargc = narg;
	    argv = (char **)ep;
	}
#endif

	/* Reconstruct the argument list.
	 */
	for (op=(char *)cmd, n = *maxch, argv++;  n >= 0 && *argv;  argv++) {
	    if (op > (char *)cmd && --n >= 0)
		*op++ = ' ';
	    for (ip = *argv;  --n >= 0 && (*op = *ip++);  op++)
		;
	}

	*op = EOS;
	*status = op - (char *)cmd;

	return (XOK);
}
