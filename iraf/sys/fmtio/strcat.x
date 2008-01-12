# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRCAT -- String concatenation.  String STR is appended to OUTSTR.

procedure strcat (str, outstr, maxch)

char	str[ARB], outstr[ARB]
int	maxch, junk, gstrcat()

begin
	junk = gstrcat (str, outstr, maxch)
end
