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
/* sppstr : SPP string		*/
/* cstr   : C string		*/
char *os_strpak ( const XCHAR *sppstr, char *cstr, long bufsize )
{
	const XCHAR *ip;
	char *op, *maxop;

	maxop = cstr + bufsize -1;
	for ( op=cstr, ip=sppstr ; op < maxop && (*ip) ; op++, ip++ )
	    *op = *ip;
	if ( op <= maxop ) *op = EOS;

	return (cstr);
}
