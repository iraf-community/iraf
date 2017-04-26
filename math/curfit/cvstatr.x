# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/curfit.h>

include	"curfitdef.h"

# CVSTATI -- Return integer paramters from the curfit package

int procedure cvstati (cv, param)

pointer	cv			# Curfit pointer
int	param			# Parameter

begin
	switch (param) {
	case CVTYPE:
	    return (CV_TYPE(cv))
	case CVORDER:
	    switch (CV_TYPE(cv)) {
	    case LEGENDRE, CHEBYSHEV, USERFNC:
	        return (CV_ORDER(cv))
	    case SPLINE1, SPLINE3:
		return (CV_NPIECES(cv) + 1)
	    }
	case CVNSAVE:
	    if (CV_TYPE(cv) == USERFNC)
	        return (CV_SAVECOEFF + CV_NCOEFF(cv))
	    else
		return (CV_SAVECOEFF + CV_NCOEFF(cv) - 1)
	case CVNCOEFF:
	    return (CV_NCOEFF(cv))
	}
end

# CVSTATR -- Return real paramters from the curfit package

real procedure cvstatr (cv, param)

pointer	cv			# Curfit pointer
int	param			# Parameter

begin
	switch (param) {
	case CVXMIN:
	    return (CV_XMIN(cv))
	case CVXMAX:
	    return (CV_XMAX(cv))
	}
end
