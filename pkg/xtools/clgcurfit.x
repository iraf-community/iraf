# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/curfit.h>

# CLGCURFIT -- Get the curve type and order for the curfit package.
#
# Prompt1 is issued for the curve type.  The curve type is entered
# as a minimum abbreviation string with clgwrd.  The allowed strings
# are legendre, chebyshev, and spline3.  Prompt2 is issued to get
# the order.

procedure clgcurfit (prompt1, prompt2, curve_type, order)

char	prompt1[ARB], prompt2[ARB]
int	curve_type
int	order

char	str[SZ_LINE]
int	i, curtypes[3], clgwrd(), clgeti()
errchk	clgwrd

data	curtypes/LEGENDRE, CHEBYSHEV, SPLINE3/

begin

	i = clgwrd (prompt1, str, SZ_LINE, ",legendre,chebyshev,spline3,")
	curve_type = curtypes[i]
	order = clgeti (prompt2)
end
