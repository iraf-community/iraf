# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"lroff.h"

# TEXTLEN -- Return the number of printable characters in a text string.

int procedure textlen (text_string)

char	text_string[ARB]
int	ip, nchars

begin
	nchars = 0
	for (ip=1;  text_string[ip] != EOS;  ip=ip+1)
	    if (!INVISIBLE (text_string[ip]))
		nchars = nchars + 1

	return (nchars)
end
