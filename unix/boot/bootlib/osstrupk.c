/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>

#include <string.h>
#define	import_spp
#include <iraf.h>

/* OS_STRUPK -- Unpack a C string into an SPP string.  This procedure should
 * be called from C in preference to the SPP procedure STRUPK because the
 * input string is declared to be of type char, rather than as an XCHAR
 * array containing packed chars as in STRUPK.  The output string is however
 * of type XCHAR since it is expected to be passed to an SPP procedure.  A
 * pointer to the output string is returned as the function value for use
 * in argument lists.
 */
XCHAR *
os_strupk (
  char	*str,			/* C string			*/
  XCHAR	*outstr,		/* SPP string			*/
  int	maxch 			/* max chars out, excl EOS	*/
)
{
	register char	*ip = str;
	register XCHAR	*op = outstr;
	register int	  n = maxch;


	/* Is is necessary to determine the length of the string in order to
	 * be able to unpack the string in place, i.e., from right to left.
	 */
	if (maxch) {
	    if (sizeof(char) != sizeof(XCHAR) || str != (char *)outstr) {
		n = min (n, strlen(ip));
		op[n] = XEOS;

		while (--n >= 0)
		    op[n] = ip[n];
	    }
	}

	return (outstr);
}
