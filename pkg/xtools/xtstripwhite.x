# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# XT_STRIPWHITE -- Strip leading white space from a string.
# The string must have an EOS.

procedure xt_stripwhite (str)

char	str[ARB]		# String to be stripped

int	i

begin
	for (i=1; (str[i]!=EOS) && (IS_WHITE(str[i])); i=i+1)
	    ;
	call strcpy (str[i], str, ARB)
end
