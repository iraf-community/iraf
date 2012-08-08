# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# CHRUPR -- Convert char to upper case.

char procedure chrupr (ch)

char	ch

begin
	if (IS_LOWER (ch))
	    return (TO_UPPER (ch))
	else
	    return (ch)
end
