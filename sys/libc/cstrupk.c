/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_libc
#define	import_spp
#include <iraf.h>

/* C_STRUPK -- Unpack a C string into an SPP string.  This procedure should
** be called from C in preference to the SPP procedure STRUPK because the
** input string is declared to be of type char, rather than as an XCHAR
** array containing packed chars as in STRUPK.  The output string is however
** of type XCHAR since it is expected to be passed to an SPP procedure.  A
** pointer to the output string is returned as the function value for use
** in argument lists.
*/
XCHAR *
c_strupk (
  char	*str,			/* C string			*/
  XCHAR	*outstr,		/* SPP string			*/
  int	maxch			/* max chars out, incl EOS	*/
)
{
	register char	*ip = str;
	register XCHAR	*op = outstr;
	register int	  n = maxch-1;


	/* Is is necessary to determine the length of the string in order to
	 * be able to unpack the string in place, i.e., from right to left.
	 */
	if (maxch)
	    if (sizeof(char) != sizeof(XCHAR) || str != (char *)outstr) {
		n = min (n, strlen(ip));
		op[n] = XEOS;

		for (n = n - 1;  n >= 0;  --n)
		    op[n] = ip[n];
	    }

	return (outstr);
}
