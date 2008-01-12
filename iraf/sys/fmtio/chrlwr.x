# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# CHRLWR -- Convert char to lower case

char procedure chrlwr (ch)

char	ch

begin
	if (IS_UPPER (ch))
	    return (TO_LOWER (ch))
	else
	    return (ch)
end
