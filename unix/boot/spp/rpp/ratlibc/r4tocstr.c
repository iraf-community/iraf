/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

/* Convert a Ratfor string (one character per 16-bit word, terminated
 * by an EOS) to a C string (one character per 8-bit byte, terminated
 * by a byte of zero).
 */
r4tocstr(rstr, cstr)
register RCHAR  *rstr;
register char   *cstr;
{
	while (*rstr != REOS)
		*cstr++ = *rstr++;
	*cstr = '\0';
}
