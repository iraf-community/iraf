/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#include <iraf.h>


/* C_STRPAK -- Pack an SPP string (type XCHAR) into a C string in a user
** supplied buffer.  Return a pointer to the output buffer.
**
** N.B.: This routine should be used in preference to STRPAK in C code
** since the output string is of type char*, rather than XCHAR*.
*/
char *
c_strpak (
  XCHAR	*sppstr,		/* SPP string			*/
  char	*cstr,			/* C string			*/
  int	maxch			/* max chars out, incl EOS	*/
)
{
	register XCHAR	*ip = sppstr;
	register char	*op = cstr;
	register int	n = maxch-1;

	if (maxch) {
	    if (sizeof(XCHAR) != sizeof(char) || (char *)sppstr != cstr) {
		while (--n >= 0 && (*op++ = *ip++) != EOS)
		    ;
		cstr[maxch-1] = EOS;
	    }
	}

	return (cstr);
}
