/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#include <iraf.h>

/* C_STRPAK -- Pack an SPP string (type XCHAR) into a C string in a user
 * supplied buffer.  Return a pointer to the output buffer.
 *
 * N.B.: This routine should be used in preference to STRPAK in C code
 * since the output string is of type char*, rather than XCHAR*.
 */
/* sppstr  : SPP string          */
/* cstr    : output C string     */
/* bufsize : buffer size of cstr */
char *c_strpak ( const XCHAR *sppstr, char *cstr, iraf_size_t bufsize )
{
	const XCHAR *ip = sppstr;
	char *op, *maxop;

	if (sizeof(XCHAR) != sizeof(char) || (char *)sppstr != cstr) {
	    maxop = cstr + bufsize -1;
	    for ( op = cstr ; op < maxop && *ip != XEOS ; op++, ip++ )
		*op = *ip;
	    if ( op <= maxop ) *op = EOS;
	}

	return (cstr);
}
