#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

extern	char *environ[];

/* ZGCMDL -- Get the host system command line used to invoke this process.
 * There does not seem to be any straightforward way to do this for UNIX,
 * but the argc,argv info is evidently pushed on the stack immediately before
 * the environment list, so we can locate the ARGV array by searching back
 * up the stack a bit.  This is very host OS dependent.
 */
ZGCMDL (cmd, maxch, status)
PKCHAR	*cmd;			/* receives the command line	*/
XINT	*maxch;			/* maxch chars out		*/
XINT	*status;
{
	register char	*ip, *op;
	unsigned int	*ep, ev;
	register int	n;
	char	**argv;

	/* Locate the ARGV array.  This assumes that argc,argv are stored in
	 * memory immediately preceeding the environment list, i.e.,
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
	 */
	ep = ((unsigned int *) *environ) - 1;
	for (n=0;  *(ep-1) != (unsigned int)n;  n++)
	    --ep;

	/* Reconstruct the argument list.
	 */
	argv = (char **)ep; argv++;
	for (op=(char *)cmd, n = *maxch;  n >= 0 && *argv;  argv++) {
	    if (op > (char *)cmd && --n >= 0)
		*op++ = ' ';
	    for (ip = *argv;  --n >= 0 && (*op = *ip++);  op++)
		;
	}

	*op = EOS;
	*status = op - (char *)cmd;
}
