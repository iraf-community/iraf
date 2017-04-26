# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/iminterp.h>

# CLGINTERP -- Select an interpolator from a CL input string.  The procedure
# is coded to be protected from changes in the values of the interpolator
# types in interpdef.h.

int procedure clginterp (param)

char	param[ARB]		# CL parameter prompt string
int	index, iicodes[5]
pointer	sp, word
int	clgwrd()
errchk	clgwrd
data	iicodes /II_NEAREST, II_LINEAR, II_POLY3, II_POLY5, II_SPLINE3/

begin
	call smark (sp)
	call salloc (word, SZ_FNAME, TY_CHAR)

	index = max (1, min (5, clgwrd (param, Memc[word], SZ_FNAME,
	    "|nearest|linear|poly3|poly5|spline3|")))

	call sfree (sp)
	return (iicodes[index])
end
