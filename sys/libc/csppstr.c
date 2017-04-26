/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#include <iraf.h>

#define	SZ_UPKSTR	SZ_COMMAND
static	XCHAR u_upkstr[SZ_UPKSTR+1];

/* C_SPPSTR -- Unpack a C string into an SPP string.  This routine is offered
** as a convenient alternative to C_STRUPK for cases when the length of the
** string is known to be short and the value will be used before we are again
** called.  The unpacked string is left in a static internal buffer and a
** pointer to XCHAR is returned as the function value.
*/
XCHAR *
c_sppstr (
  char	*str
)
{
	register char	*ip = str;
	register XCHAR	*op = u_upkstr;
	register int	n = SZ_UPKSTR;

	while (--n >= 0 && (*op++ = *ip++) != XEOS)
	    ;
	u_upkstr[SZ_UPKSTR] = XEOS;

	return (u_upkstr);
}
