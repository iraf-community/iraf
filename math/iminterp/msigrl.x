# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "im2interpdef.h"
include <math/iminterp.h>

# MSIGRL -- Procedure to integrate the 2D interpolant over a specified area.
# The x and y arrays are assumed to describe a polygon which is the domain over
# which the integration is to be performed. The x and y must describe a closed
# curve and npts must be >= 4 with the last vertex equal to the first vertex.
# The routine uses the technique of separation of variables. The restriction on
# the polygon is that horizontal lines have at most one segment in common with
# the domain of integration. Polygons which do not fit this restriction can be
# split into one or more polygons before calling msigrl and the results can
# then be summed.

real procedure msigrl (msi, x, y, npts)

pointer	msi		# pointer to the interpolant descriptor structure
real	x[npts]		# array of x values
real	y[npts]		# array of y values
int	npts		# number of points which describe the boundary

int	i, interp_type, nylmin, nylmax, offset
pointer	x1lim, x2lim, xintegrl, ptr
real	xmin, xmax, ymin, ymax, accum
real	ii_1dinteg()

begin
	# set up 1D interpolant type
	switch (MSI_TYPE(msi)) {
	case II_BINEAREST:
	    interp_type = II_NEAREST
	case II_BILINEAR:
	    interp_type = II_LINEAR
	case II_BIDRIZZLE:
	    interp_type = II_DRIZZLE
	case II_BIPOLY3:
	    interp_type = II_POLY3
	case II_BIPOLY5:
	    interp_type = II_POLY5
	case II_BISPLINE3:
	    interp_type = II_SPLINE3
	case II_BISINC:
	    interp_type = II_SINC
	case II_BILSINC:
	    interp_type = II_LSINC
	}

	# set up temporary storage for x limits and the x integrals
	call calloc (x1lim, MSI_NYCOEFF(msi), TY_REAL)
	call calloc (x2lim, MSI_NYCOEFF(msi), TY_REAL)
	call calloc (xintegrl, MSI_NYCOEFF(msi), TY_REAL)

	# offset of first data point from edge of coefficient array
	offset = mod (MSI_FSTPNT(msi), MSI_NXCOEFF(msi)) 

	# convert the (x,y) points which describe the polygon into
	# two arrays of x limits x1lim and x2lim and two y limits ymin and ymax
	call ii_find_limits (x, y, npts, 0, 0, MSI_NYCOEFF(msi),
	    Memr[x1lim+offset], Memr[x2lim+offset], ymin, ymax, nylmin, nylmax)
	nylmin = nylmin + offset
	nylmax = nylmax + offset

	# integrate in x
	ptr = MSI_COEFF(msi) + offset + (nylmin - 1) * MSI_NXCOEFF(msi)
	do i = nylmin, nylmax {
	    xmin = min (Memr[x1lim+i-1], Memr[x2lim+i-1])
	    xmax = max (Memr[x1lim+i-1], Memr[x2lim+i-1])
	    Memr[xintegrl+i-1] = ii_1dinteg (COEFF(ptr), MSI_NXCOEFF(msi),
	        xmin, xmax, interp_type, MSI_NSINC(msi), DX, MSI_XPIXFRAC(msi))
	    ptr = ptr + MSI_NXCOEFF(msi)
	}

	# integrate in y
	if (interp_type == II_SPLINE3) {
	    call amulkr (Memr[xintegrl], 6.0, Memr[xintegrl], MSI_NYCOEFF(msi))
	    accum = ii_1dinteg (Memr[xintegrl+offset], MSI_NYCOEFF(msi), ymin,
	        ymax, II_NEAREST, MSI_NSINC(msi), DY, MSI_YPIXFRAC(msi))
	} else {
	    accum = ii_1dinteg (Memr[xintegrl+offset], MSI_NYCOEFF(msi), ymin,
	        ymax, II_NEAREST, MSI_NSINC(msi), DY, MSI_YPIXFRAC(msi))
	}

	# free space
	call mfree (xintegrl, TY_REAL)
	call mfree (x1lim, TY_REAL)
	call mfree (x2lim, TY_REAL)

	return (accum)
end


# II_FIND_LIMITS -- Procedure to transform a set of (x,y)'s describing a
# polygon into a set of limits.

procedure ii_find_limits (x, y, npts, xboff, xeoff, max_nylines, x1lim, x2lim,
ymin, ymax, nylmin, nylmax)

real	x[npts]		# array of x values
real	y[npts]		# array of y values
int	npts		# number of data points
int	xboff, xeoff	# boundary extension limits
int	max_nylines	# max number of lines to integrate
real	x1lim[ARB]	# array of x1 limits
real	x2lim[ARB]	# array of x2 limits
real	ymin		# minimum y value for integration
real	ymax		# maximum y value for integration
int	nylmin		# minimum line number for x integration
int	nylmax		# maximum line number for x integration

int	i, ninter
pointer	sp, xintr, yintr
real	xmin, xmax, lx, ld
int	ii_pyclip()

begin
	call smark (sp)
	call salloc (xintr, npts, TY_REAL)
	call salloc (yintr, npts, TY_REAL)

	# find x and y limits and their indicess
	call alimr (x, npts, xmin, xmax)
	call alimr (y, npts, ymin, ymax)

	# calculate the line limits for integration 
	nylmin = max (1, min (int (ymin + 0.5) - xboff, max_nylines))
	nylmax = min (max_nylines, max (1, int (ymax + 0.5) + xeoff))

	# initialize
	lx = xmax - xmin

	# calculate the limits
	for (i = nylmin; i <= nylmax; i = i + 1) {

	    if (ymin > i)
	        ld = min (i + 0.5, ymax) * lx
	    else if (ymax < i)
	        ld = max (i - 0.5, ymin) * lx
	    else
	        ld = i * lx
	    ninter = ii_pyclip (x, y, Memr[xintr], Memr[yintr], npts, lx, ld)
	    if (ninter <= 0) {
		x1lim[i] = xmin
		x2lim[i] = xmin
	    } else {
		x1lim[i] = min (Memr[xintr], Memr[xintr+1])
		x2lim[i] = max (Memr[xintr], Memr[xintr+1])
	    }
	}

	call sfree (sp)
end


# II_YCLIP -- Procedure to determine the intersection points of a
# horizontal image line with an arbitrary polygon.

int procedure ii_pyclip (xver, yver, xintr, yintr, nver, lx, ld)

real	xver[ARB]		# x vertex coords
real	yver[ARB]		# y vertex coords
real	xintr[ARB]		# x intersection coords
real	yintr[ARB]		# y intersection coords
int	nver			# number of vertices
real	lx, ld 			# equation of image line

int	i, nintr
real	u1, u2, u1u2, dx, dy, dd, xa, ya, wa

begin
	nintr = 0
	u1 = - lx * yver[1] + ld
	do i = 2, nver {

	    u2 = - lx * yver[i] + ld
	    u1u2 = u1 * u2

	    # Test whether polygon line segment intersects image line or not.
	    if (u1u2 <= 0.0) {


		# Compute the intersection coords.
		if (u1 != 0.0 && u2 != 0.0) {

		    dy = yver[i-1] - yver[i]
		    dx = xver[i-1] - xver[i]
		    dd = xver[i-1] * yver[i] - yver[i-1] * xver[i]
		    xa = (dx * ld - lx * dd)
		    ya = dy * ld 
		    wa = dy * lx
		    nintr = nintr + 1
		    xintr[nintr] = xa / wa
		    yintr[nintr] = ya / wa

		# Test for collinearity.
		} else if (u1 == 0.0 && u2 == 0.0) {

		    nintr = nintr + 1
		    xintr[nintr] = xver[i-1]
		    yintr[nintr] = yver[i-1]
		    nintr = nintr + 1
		    xintr[nintr] = xver[i]
		    yintr[nintr] = yver[i]

		} else if (u1 != 0.0) {

		    if (i == 1) {
			dy = (yver[2] - yver[1])
			dd = (yver[nver-1] - yver[1])
		    } else if (i == nver) {
			dy = (yver[2] - yver[nver])
			dd = dy * (yver[nver-1] - yver[nver])
		    } else {
			dy = (yver[i+1] - yver[i])
			dd = dy * (yver[i-1] - yver[i])
		    }

		    if (dy != 0.0) {
			nintr = nintr + 1
			xintr[nintr] = xver[i]
			yintr[nintr] = yver[i]
		    }

		    if (dd > 0.0) {
			nintr = nintr + 1
			xintr[nintr] = xver[i]
			yintr[nintr] = yver[i]
		    }

		}
	    }

	    u1 = u2
	}

	return (nintr)
end
