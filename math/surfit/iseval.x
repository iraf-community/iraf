# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/surfit.h>
include "surfitdef.h"

# ISEVAL -- Procedure to evaluate the fitted surface at a single point.
# The SF_NXCOEFF(sf) by SF_NYCOEFF(sf) coefficients are stored in the
# SF_NYCOEFF(sf) by SF_NXCOEFF(sf) matrix COEFF. The j-th element of the ith
# row of COEFF contains the coefficient of the i-th basis function in x and
# the j-th basis function in y.

real procedure iseval (sf, x, y)

pointer	sf		# pointer to surface descriptor structure
real	x		# x value
real	y		# y value

real	sum, accum
int	i, k, leftx, lefty, yorder
pointer	sp, xb, xzb, yb, yzb, czptr

begin
	# allocate space for the basis functions
	call smark (sp)
	call salloc (xb, SF_XORDER(sf), MEM_TYPE)
	xzb = xb - 1
	call salloc (yb, SF_YORDER(sf), MEM_TYPE)
	yzb = yb - 1

	# calculate the basis functions
	switch (SF_TYPE(sf)) {
	case SF_CHEBYSHEV:
	    leftx = 0
	    lefty = 0
	    czptr = SF_COEFF(sf) - 1
	    call sf_b1cheb (x, SF_XORDER(sf), SF_XMAXMIN(sf), SF_XRANGE(sf),
		XBS(xb))
	    call sf_b1cheb (y, SF_YORDER(sf), SF_YMAXMIN(sf), SF_YRANGE(sf),
		YBS(yb))

	case SF_LEGENDRE:
	    leftx = 0
	    lefty = 0
	    czptr = SF_COEFF(sf) - 1
	    call sf_b1leg (x, SF_XORDER(sf), SF_XMAXMIN(sf), SF_XRANGE(sf),
		XBS(xb))
	    call sf_b1leg (y, SF_YORDER(sf), SF_YMAXMIN(sf), SF_YRANGE(sf),
		YBS(yb))

	case SF_SPLINE3:
	    call sf_b1spline3 (x, SF_NXPIECES(sf), -SF_XMIN(sf),
	        SF_XSPACING(sf), XBS(xb), leftx)
	    call sf_b1spline3 (y, SF_NYPIECES(sf), -SF_YMIN(sf),
	        SF_YSPACING(sf), YBS(yb), lefty)
	    czptr = SF_COEFF(sf) - 1 + lefty + leftx * SF_NYCOEFF(sf)

	case SF_SPLINE1:
	    call sf_b1spline1 (x, SF_NXPIECES(sf), -SF_XMIN(sf),
	        SF_XSPACING(sf), XBS(xb), leftx)
	    call sf_b1spline1 (y, SF_NYPIECES(sf), -SF_YMIN(sf),
	        SF_YSPACING(sf), YBS(yb), lefty)
	    czptr = SF_COEFF(sf) - 1 + lefty + leftx * SF_NYCOEFF(sf)
	}

	# initialize accumulator
	# basis functions
	sum = 0.

	# loop over y basis functions
	yorder = SF_YORDER(sf)
	do i = 1, SF_XORDER(sf) {

	    # loop over the x basis functions
	    accum = 0.
	    do k = 1, yorder {
		accum = accum + COEFF(czptr+k) * YBS(yzb+k) 
	    }
	    accum = accum * XBS(xzb+i)
	    sum = sum + accum

	    # elements of COEFF where neither k = 1 or i = 1
	    # are not calculated if SF_XTERMS(sf) = NO
	    if (SF_XTERMS(sf) == NO)
		yorder = 1

	    czptr = czptr + SF_NYCOEFF(sf)
	}

	call sfree (sp)

	return (sum)
end
