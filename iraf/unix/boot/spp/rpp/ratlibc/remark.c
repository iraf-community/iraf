/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

void REMARK ( int *strarg )
/* strarg: hollerith string is an integer array */
{
	const char *strin = (const char *)strarg;
	char c;

	while (((c = *strin++) != '.') && (c != '\0'))
	    if (c == '@') {
		switch (*strin) {
		case '.':
		    putc ('.', stderr);
		    strin++;
		    break;

		case 't':
		    putc ('\t', stderr);
		    strin++;
		    break;

		case 'b':
		    putc ('\b', stderr);
		    strin++;
		    break;

		case 'n':
		    putc ('\n', stderr);
		    strin++;
		    break;

		default:
		    putc ('@', stderr);
		    break;
		}
	    } else
		putc (c, stderr);

	putc ('\n', stderr);
}
