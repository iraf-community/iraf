/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

/* Convert a Ratfor string (one character per integer, terminated
 * by an EOS) to a C string (one character per 8-bit byte, terminated
 * by a byte of zero).
 */
void r4tocstr(register RCHAR *rstr, register char *cstr)
{
	while (*rstr != REOS) {
	    if (*rstr > 0177) {
		*cstr++ = *((char *)rstr);
		rstr++;
	    } else
		*cstr++ = *rstr++;
	}
	*cstr = '\0';
}
