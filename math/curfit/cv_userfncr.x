# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/curfit.h>

include	"curfitdef.h"

# Interface Routine for external user functions

# CV_B1USER - Evaluate basis functions at a single point with
# external user routine.

procedure rcv_b1user (cv, x)

pointer	cv
real	x

begin
	if (CV_USERFNC(cv) == NULL)
	    call error (0, "CV_USERFNC: Pointer not set")

	call zcall5 (CV_USERFNC(cv), x, CV_ORDER(cv), CV_MAXMIN(cv),
			CV_RANGE(cv), XBASIS(CV_XBASIS(cv)))
end

# CV_BUSER - Evaluate basis functions at a set of points with
# external user routine.

procedure rcv_buser (cv, x, npts)

pointer	cv
real   x[ARB]
int	npts

int	i, j

begin
	do i = 1, npts {
	    call rcv_b1user (cv, x[i])
	    do j = 1, CV_ORDER(cv)
		BASIS(CV_BASIS(cv)-1+i + npts*(j-1)) = 
			XBASIS(CV_XBASIS(cv)-1+j)
	}
end

# CV_EVUSER - Evaluate user function at a set of points using present
# coefficient values

procedure rcv_evuser (cv, x, yfit, npts)

pointer	cv
real	x[ARB],  yfit[ARB]
int	npts

int	i
real	adotr

begin
	do i = 1, npts {
	    call rcv_b1user (cv, x[i])
	    yfit[i] = adotr ( XBASIS(CV_XBASIS(cv)), COEFF(CV_COEFF(cv)),
				CV_ORDER(cv))
	}
end

# CVUSERFNC - Set external user function.

procedure cvuserfnc (cv, fnc)

pointer	cv
extern	fnc()

int	locpr()

begin
	CV_USERFNC(cv) = locpr (fnc)
end
