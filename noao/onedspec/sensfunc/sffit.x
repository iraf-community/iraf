include	<math/curfit.h>
include	"sensfunc.h"

define	FUNCTIONS	"|chebyshev|legendre|spline3|spline1|"

procedure sf_fit (stds, nstds, cv, function, order, xmin, xmax)

pointer	stds[nstds]
int	nstds
pointer	cv
char	function[ARB]
int	order
real	xmin
real	xmax

int	i, n, func, ord, strdic()
pointer	x, y, w

int	functions[4]
data	functions/CHEBYSHEV,LEGENDRE,SPLINE3,SPLINE1/

begin
	func = strdic (function, function, SZ_FNAME, FUNCTIONS)
	func = functions[max (1, func)]
	ord = order

	while (ord > 0) {
	    call cvfree (cv)
	    call cvinit (cv, func, ord, xmin, xmax)
	    do i = 1, nstds {
		if (STD_FLAG(stds[i]) == SF_INCLUDE) {
	            n = STD_NWAVES(stds[i])
	            x = STD_WAVES(stds[i])
	            y = STD_SENS(stds[i])
	            w = STD_WTS(stds[i])
	            call cvacpts (cv, Memr[x], Memr[y], Memr[w], n, WTS_USER)
		}
	    }
	    call cvsolve (cv, i)
	    if (i == OK)
		break

	    switch (func) {
	    case CHEBYSHEV, LEGENDRE:
		ord = ord - 1
	    case SPLINE3:
		ord = ord - 1
		if (ord == 0) {
		    func = CHEBYSHEV
		    ord = 2
		}
	    case SPLINE1:
		ord = ord - 1
		if (ord == 0) {
		    func = CHEBYSHEV
		    ord = 1
		}
	    }
	}

	switch (i) {
	case SINGULAR:
	    call error (0, "Singular solution")
	case NO_DEG_FREEDOM:
	    call error (0, "No degrees of freedom")
	}

	# Set fitted values
	do i = 1, nstds
	    if (STD_FLAG(stds[i]) != SF_EXCLUDE) {
	        n = STD_NWAVES(stds[i])
		if (n < 1)
		    next
	        x = STD_WAVES(stds[i])
		y = STD_FIT(stds[i])
		call cvvector (cv, Memr[x], Memr[y], n)
	    }
end
