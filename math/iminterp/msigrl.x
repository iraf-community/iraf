# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "im2interpdef.h"
include <math/iminterp.h>

# MSIGRL -- Procedure to integrate the 2D interpolant over a specified
# area. The x and y arrays are assumed to describe a polygon which is
# the domain over which the integration is to be performed. The x and y
# must describe a closed curve and npts must be >= 3. The routine uses the
# technique of separation of variables. The restriction on the polygon
# is that horizontal lines have at most one segment in common with the
# domain of integration. Polygons which do not fit this restriction can
# be split into one or more polygons before calling msigrl and the results
# can then be summed.

real procedure msigrl (msi, x, y, npts)

pointer	msi		# pointer to the interpolant descriptor structure
real	x[npts]		# array of x values
real	y[npts]		# array of y values
int	npts		# number of points which describe the boundary

int	interp_type, nylmin, nylmax, offset, i
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
	case II_BIPOLY3:
	    interp_type = II_POLY3
	case II_BIPOLY5:
	    interp_type = II_POLY5
	case II_BISPLINE3:
	    interp_type = II_SPLINE3
	}

	# set up temporary storage for x limits and the x integrals
	call calloc (x1lim, MSI_NYCOEFF(msi), TY_REAL)
	call calloc (x2lim, MSI_NYCOEFF(msi), TY_REAL)
	call calloc (xintegrl, MSI_NYCOEFF(msi), TY_REAL)

	# offset of first data point from edge of coefficient array
	offset = mod (MSI_FSTPNT(msi), MSI_NXCOEFF(msi)) 

	# convert the (x,y) points which describe the polygon into
	# two arrays of x limits x1lim and x2lim and two y limits ymin and ymax
	call find_limits (x, y, npts, Memr[x1lim+offset], Memr[x2lim+offset],
	    ymin, ymax, nylmin, nylmax)

	nylmin = nylmin + offset
	nylmax = nylmax + offset

	# integrate in x
	ptr = MSI_COEFF(msi) + offset + (nylmin - 1) * MSI_NXCOEFF(msi)
	do i = nylmin, nylmax {
	    xmin = min (Memr[x1lim+i-1], Memr[x2lim+i-1])
	    xmax = max (Memr[x1lim+i-1], Memr[x2lim+i-1])
	    Memr[xintegrl+i-1] = ii_1dinteg (COEFF(ptr), xmin, xmax,
	        interp_type)
	    ptr = ptr + MSI_NXCOEFF(msi)
	}

	# integrate in y
	accum = ii_1dinteg (Memr[xintegrl+offset], ymin, ymax, II_NEAREST)
	if (interp_type == II_SPLINE3)
	    accum = accum * 6.

	# free space
	call mfree (xintegrl, TY_REAL)
	call mfree (x1lim, TY_REAL)
	call mfree (x2lim, TY_REAL)

	return (accum)
end


# FIND_LIMITS -- Procedure to transform a set of (x,y)'s describing a
# polygon into a set of limits.

procedure find_limits (x, y, npts, x1lim, x2lim, ymin, ymax, nylmin, nylmax)

real	x[npts]		# array of x values
real	y[npts]		# array of y values
int	npts		# number of data points
real	x1lim[ARB]	# array of x1 limits
real	x2lim[ARB]	# array of x2 limits
real	ymin		# minimum y value for integration
real	ymax		# maximum y value for integration
int	nylmin		# minimum line number for x integration
int	nylmax		# maximum line number for x integration

int	i, j
int	nxmin, nxmax, nymin, nymax, nlines, ny1old, ny2old
pointer	sp, xwrap, ywrap
real	xmin, xmax, slope

begin
	# find x and y limits and their indicess
	call alimrix (x, npts, xmin, nxmin, xmax, nxmax)
	call alimrix (y, npts, ymin, nymin, ymax, nymax)

	# calculate the line limits for integration 
	nylmin = int (ymin + 0.5) 
	nylmax = int (ymax + 0.5) 

	# allocate working space space
	call smark (sp)
	call salloc (xwrap, npts, TY_REAL)
	call salloc (ywrap, npts, TY_REAL)

	# shift x and y arrays so that y[1] = ymin and reset endpoints
	call awrapr (x, Memr[xwrap], npts - 1, nymin - 1)
	call awrapr (y, Memr[ywrap], npts - 1, nymin - 1)
	Memr[xwrap+npts-1] = Memr[xwrap]
	Memr[ywrap+npts-1] = Memr[ywrap]

	nlines = abs (nymax - nymin) + 1 

	ny1old = 2
	ny2old = npts - 1

	# calculate the limits
	for (i = nylmin; i <= nylmax; i = i + 1) {

	    # find x1 limit
	    do j = ny1old, nlines {
		if (real (i) <= Memr[ywrap+j-1]) {
		    slope = (Memr[ywrap+j-1] - Memr[ywrap+j-2])
		    if (abs (slope) < EPSILON)
			x1lim[i] = Memr[xwrap+j-1]
		    else
			x1lim[i] = (real (i) - Memr[ywrap+j-2]) *
			   (Memr[xwrap+j-1] - Memr[xwrap+j-2]) /
			    slope + Memr[xwrap+j-2]
		    ny1old = j
		    break
		}
	    }

	    # find x2 limit
	    do j = ny2old, nlines, -1 {
		if (real (i) <= Memr[ywrap+j-1]) {
		    slope = (Memr[ywrap+j-1] - Memr[ywrap+j])
		    if (abs (slope) < EPSILON)
			x2lim[i] = Memr[xwrap+j]
		    else
			x2lim[i] = (real (i) - Memr[ywrap+j]) *
			    (Memr[xwrap+j-1] - Memr[xwrap+j]) /
			    slope + Memr[xwrap+j]
		    ny2old = j
		    break
		}
	    }
	}

	# finish the endpoints
	if (real (nylmin) < ymin) {
	    if (int (ymin) == int (ymax)) {
		x1lim[nylmin] = xmin
		x2lim[nylmin] = xmax
	    } else {
		x1lim[nylmin] = x1lim[nylmin+1]
		x2lim[nylmin] = x2lim[nylmin+1]
	    }
	}

	if (real (nylmax) > ymax) {
	    if (int (ymin) == int (ymax)) {
		x1lim[nylmax] = xmin
		x2lim[nylmax] = xmax
	    } else {
		x1lim[nylmax] = x1lim[nylmax-1]
		x2lim[nylmax] = x2lim[nylmax-1]
	    }
	}

	# free space
	call sfree (sp)
end


# ALIMRIX -- Procedure to find the maximum and minimum of a vector and
# the indices of these elements.

procedure alimrix (x, npts, xmin, nxmin, xmax, nxmax) 

real	x[npts]		# data
int	npts		# number of data points
real	xmin		# data minimum
int	nxmin		# index of data minimum
real	xmax		# data maximum
int	nxmax		# index of data maximum

int	i

begin
	nxmin = 1
	xmin = x[1]
	nxmax = 1
	xmax = x[1]

	do i = 2, npts {
	    if (x[i] < xmin) {
		nxmin = i
		xmin = x[i]
	    }
	    if (x[i] > xmax) {
		nxmax = i
		xmax = x[i]
	    }
	}
end


# AWRAPR -- Procedure to shift a vector with wrapping of the out of
# bounds pixels. a and b must be distinct vectors.

procedure awrapr (a, b, npts, shift)

real	a[npts]		# input vector
real	b[npts]		# output vector
int	npts		# number of data points
int	shift		# number of elements to shift

int	i, ishift, ashift

begin
	ishift = mod (shift, npts)
	ashift = abs (ishift)

	if (ishift > 0) {
	    do i = 1, npts - ashift
		b[i] = a[i + ashift]
	    do i = npts - ashift + 1, npts
		b[i] = a[i - npts + shift]
	} else if (ishift < 0) {
	    do i = ashift + 1, npts
		b[i] = a[i - ashift]
	    do i = 1, ashift
		b[i] = a[npts - ashift + i]
	} else
	    call amovr (a, b, npts)
end
