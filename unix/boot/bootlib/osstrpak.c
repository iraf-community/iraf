/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#include <iraf.h>


/* OS_STRPAK -- Pack an SPP string (type XCHAR) into a C string in a user
 * supplied buffer.  Return a pointer to the output buffer.
 *
 * N.B.: This routine should be used in preference to STRPAK in C code
 * since the output string is of type char*, rather than XCHAR*.
 */
char *
os_strpak (
  XCHAR	*sppstr,		/* SPP string			*/
  char	*cstr,			/* C string			*/
  int	maxch 			/* max chars out, excl EOS	*/
)
{
	register XCHAR	*ip = sppstr;
	register char	*op = cstr;
	register int	n = maxch;


	while ( (*op++ = *ip++) ) {
	    if (--n <= 0) {
		*op = EOS;
		break;
	    }
	}

	return (cstr);
}
