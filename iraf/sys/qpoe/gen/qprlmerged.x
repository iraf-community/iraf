# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "../qpex.h"

# QP_RLMERGE -- Merge (AND) two range lists.  Only ranges which are
# common to both range lists are output.  The number of ranges in the
# output range list is returned as the function value.

int procedure qp_rlmerged (os,oe,olen, xs,xe,nx, ys,ye,ny)

pointer	os, oe			#U output range list
int	olen			#U allocated length of OS, OE arrays

double	xs[ARB], xe[ARB]	#I range list to be merged with
int	nx			#I number of ranges in X list
double	ys[ARB], ye[ARB]	#I range list to be merged with X
int	ny			#I number of ranges in Y list

double	o1, o2
int	nx_out, xi, yi, i
double	qp_minvald(), qp_maxvald()
bool	qp_lessthand()
errchk	realloc

begin
	nx_out = 0
	if (nx <= 0 || ny <= 0)
	    return (0)

	xi = 1
	yi = 1

	do i = 1, ARB {
	    # Find a pair of ranges which intersect.
	    repeat {
		if (qp_lessthand (xe[xi], ys[yi])) {
		    if (xi >= nx)
			return (nx_out)
		    else
			xi = xi + 1
		} else if (qp_lessthand (ye[yi], xs[xi])) {
		    if (yi >= ny)
			return (nx_out)
		    else
			yi = yi + 1
		} else
		    break
	    }

	    # Compute the intersection.
	    o1 = qp_maxvald (xs[xi], ys[yi])
	    o2 = qp_minvald (xe[xi], ye[yi])

	    # Output the range.
	    if (nx_out + 1 > olen) {
		olen = max (DEF_XLEN, olen * 2)
		call realloc (os, olen, TY_DOUBLE)
		call realloc (oe, olen, TY_DOUBLE)
	    }

	    Memd[os+nx_out] = o1
	    Memd[oe+nx_out] = o2
	    nx_out = nx_out + 1

	    # Advance to the next range.
	    if (xi < nx && qp_lessthand (xe[xi], ye[yi]))
		xi = xi + 1
	    else if (yi < ny)
		yi = yi + 1
	    else
		break
	}

	return (nx_out)
end


# QP_MINVAL -- Return the lesser of two values, where either value can
# be an open range.

double procedure qp_minvald (x, y)

double	x			#I first value
double	y			#I second value

bool	qp_lessthand()

begin
	if (qp_lessthand (x, y))
	    return (x)
	else
	    return (y)
end


# QP_MAXVAL -- Return the greater of two values, where either value can
# be an open range.

double procedure qp_maxvald (x, y)

double	x			#I first value
double	y			#I second value

bool	qp_lessthand()

begin
	if (qp_lessthand (x, y))
	    return (y)
	else
	    return (x)
end


# QP_LESSTHAN -- Test if X is less than Y, where X and Y can be open
# range values.

bool procedure qp_lessthand (x, y)

double	x			#I first value
double	y			#I second value

begin
	if (IS_LEFTD(x))
	    return (!IS_LEFTD(y))
	else if (IS_RIGHTD(x))
	    return (false)
	else if (IS_LEFTD(y))
	    return (false)
	else if (IS_RIGHTD(y))
	    return (true)
	else
	    return (x < y)
end
