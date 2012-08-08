/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

FINT
GETLIN(line, fd)
RCHAR	*line;
FINT	*fd;
{
	register  c=0;
	register  count=0;
	register  RCHAR *cs;
	FILE	  *fp;

	fp = _fdtofile[*fd];
	cs = line;
	while (++count<MAXLINE && (c = getc(fp))>=0) {
	    *cs++ = c;
	    if (c == '\n') {
		*cs++ = REOS;
		return (count);		/* count includes newline, but does
				           not include the EOS */
	    }
	}

	if (c<0 && cs==line)
	    return(REOF);

	*cs++ = REOS;
	return(count);
}
