/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* STRPAK -- Pack an SPP character string into a C string, i.e., a sequence
 * of characters stored one per byte, delimited by EOS='\0'.  The operation
 * may be performed in place.  This version assumes that the host character
 * set is ASCII and hence no lookup table reference to map character sets is
 * needed.  If this is not the case, code must be added to convert to the host
 * character set.
 *
 * N.B.: If sizeof(XCHAR)=1, XEOS=EOS, and the host character set is ASCII,
 * and the operation is being performed in place, then this procedure should
 * do nothing.
 */
STRPAK (instr, outstr, maxch)
XCHAR	*instr;
PKCHAR	*outstr;
XINT	*maxch;
{
	register XCHAR	*ip = instr;
	register char	*op = (char *)outstr;
	register int	  n = *maxch;

	while ((*op++ = *ip++) != XEOS && --n >= 0)
	    ;
	*--op = EOS;
}
