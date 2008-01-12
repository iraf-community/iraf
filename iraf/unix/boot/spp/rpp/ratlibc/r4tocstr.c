/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

/* Convert a Ratfor string (one character per integer, terminated
 * by an EOS) to a C string (one character per 8-bit byte, terminated
 * by a byte of zero).
 */
void r4tocstr( RCHAR *rstr, char *cstr, size_t bufsize )
{
	char *maxptr = cstr + bufsize -1;
	while (*rstr != REOS) {
	    if (*rstr > 0177) {
		if ( cstr < maxptr ) *cstr++ = *((char *)rstr);
	    } else {
		if ( cstr < maxptr ) *cstr++ = *rstr;
	    }
	    rstr++;
	}
	if ( cstr <= maxptr ) *cstr = '\0';
}
