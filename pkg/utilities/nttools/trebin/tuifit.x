include "trebin.h"

# tuifit -- initialize or fit
# The input arrays of independent and dependent variable values are copied
# to output arrays, skipping indef values.  The number of good values
# (i.e. not indef) is checked to make sure there are enough for the type
# of interpolation specified.  In the case of spline interpolation, the
# array y2 of second derivatives is computed.
#
# Phil Hodge, 18-Apr-1988  Subroutine created
# Phil Hodge, 17-Jun-1993  Require at least two values for linear interpolation.

procedure tuifit (i_func, xin, yin, inrows,
		xa, ya, y2, n)

int	i_func			# i: interpolation function code
double	xin[ARB]		# i: array of independent-variable values
double	yin[ARB]		# i: array of dependent-variable values
int	inrows			# i: size of xin, yin arrays
double	xa[ARB]			# o: array of independent-variable values
double	ya[ARB]			# o: array of dependent-variable values
double	y2[ARB]			# o: used only by spline interpolation
int	n			# o: size of xa, ya, y2 arrays
#--
int	k			# loop index

begin
	n = 0					# initial value

	do k = 1, inrows {
	    if ( ! IS_INDEFD(xin[k]) && ! IS_INDEFD(yin[k])) {
		n = n + 1
		xa[n] = xin[k]
		ya[n] = yin[k]
	    }
	}

	switch (i_func) {
	case I_NEAREST:
	    if (n < 1)
		n = 0				# flag it as bad

	case I_LINEAR:
	    if (n < 2)
		n = 0

	case I_POLY3:
	    if (n < 4)
		n = 0

	case I_SPLINE:
	    if (n >= 4) {
		call tucspl (xa, ya, n, y2)
	    } else {
		n = 0
	    }
	}
end
