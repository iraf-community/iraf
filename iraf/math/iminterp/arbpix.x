# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math.h>
include <math/iminterp.h>
include "im1interpdef.h"

define MIN_BDX	0.05	# minimum distance from interpolation point for sinc

# ARBPIX -- Replace INDEF valued pixels with interpolated values. In order to
# replace bad points the spline interpolator uses a limited data array whose
# maximum total length is given by SPLPTS.

procedure arbpix (datain, dataout, npts, interp_type, boundary_type)

real	datain[ARB]		# input data array
real	dataout[ARB]		# output data array - cannot be same as datain
int	npts			# number of data points
int	interp_type		# interpolator type
int	boundary_type		# boundary type, at present must be BOUNDARY_EXT

int	i, badnc, k, ka, kb
real	ii_badpix()

begin
	if (interp_type < 1 || interp_type > II_NTYPES)
	    call error (0, "ARBPIX: Unknown interpolator type.")

	if (boundary_type < 1 || boundary_type > II_NBOUND)
	    call error (0, "ARBPIX: Unknown boundary type.")

	# Count bad points.
	badnc = 0
	do i = 1, npts
	    if (IS_INDEFR(datain[i]))
		badnc = badnc + 1

	# Return an array of INDEFS if all points bad.
	if (badnc == npts) {
	    call amovkr (INDEFR, dataout, npts)
	    return
	}

	# Copy input array to output array if all points good.
	if (badnc == 0) {
	    call amovr (datain, dataout, npts)
	    return
	}

	# If sinc interpolator use a special routine.
	if (interp_type == II_SINC || interp_type == II_LSINC) {
	    call ii_badsinc (datain, dataout, npts, NSINC, MIN_BDX)
	    return
	}
	
	# Find the first good point.
	for (ka = 1; IS_INDEFR (datain[ka]); ka = ka + 1)
	    ;

	# Bad points below first good point are set at first good value.
	do k = 1, ka - 1
	    dataout[k] = datain[ka]
	
	# Find last good point.
	for (kb = npts; IS_INDEFR (datain[kb]); kb = kb - 1)
	    ;

	# Bad points beyond last good point get set at good last value.
	do k = npts, kb + 1, -1
	    dataout[k] = datain[kb]

	# Load the other points interpolating the bad points as needed.
	do k = ka, kb {

	    if (IS_INDEFR(datain[k]))
		dataout[k] = ii_badpix (datain[ka], kb - ka + 1, k - ka + 1,
		    interp_type)
	    else
		dataout[k] = datain[k]

	}
end


# II_BADPIX -- This procedure fills a temporary array with good points that
# bracket the bad point and calls the interpolating routine.

real procedure ii_badpix (datain, npix, index, interp_type)

real	datain[ARB]	# datain array, y[1] and y[n] guaranteed to be good
int	npix		# length of y array
int	index		# index of bad point to replace
int	interp_type	# interpolator type

int	j, jj, pdown, pup, npts, ngood
real	tempdata[SPLPTS], tempx[SPLPTS]
real	ii_newpix()

begin
	# This code will work only if subroutines are implemented using
	# static storage - i.e. the old internal values survive. This avoids
	# reloading of temporary arrays if there are consequetive bad points.

	# The following test is done to improve speed.

	if (! IS_INDEFR(datain[index-1])) { 

	    # Set number of good points needed on each side of bad point.
	    switch (interp_type) {
	    case II_NEAREST:
		ngood = 1
	    case II_LINEAR:
		ngood = 1
	    case II_POLY3:
		ngood = 2
	    case II_POLY5:
		ngood = 3
	    case II_SPLINE3:
		ngood = SPLPTS / 2
	    case II_DRIZZLE:
		ngood = 1
	    }

	    # Search down.
	    pdown = 0
	    for (j = index - 1; j >= 1 && pdown < ngood; j = j - 1)
		if (! IS_INDEFR(datain[j]))
		    pdown = pdown + 1

	    # Load temporary arrays for values below our INDEF.
	    npts = 0
	    for(jj = j + 1; jj < index; jj = jj + 1)
		if (! IS_INDEFR(datain[jj])) {
		    npts = npts + 1
		    tempdata[npts] = datain[jj]
		    tempx[npts] = jj
		}

	     # Search and load up from INDEF.
	     pup = 0
	     for (j = index + 1; j <= npix && pup < ngood; j = j + 1)
		if (! IS_INDEFR(datain[j])) {
		     pup = pup + 1
		     npts = npts + 1
		     tempdata[npts] = datain[j]
		     tempx[npts] = j
		 }
	 }

	 # Return value interpolated from these arrays.
	 return (ii_newpix (real(index), tempx, tempdata,
	 	npts, pdown, interp_type))

end


# II_NEWPIX -- This procedure interpolates the temporary arrays. For the
# purposes of bad pixel replacement the drizzle replacement algorithm is
# equated with the linear interpolation replacement algorithm, an equation
# which is exact if the drizzle integration interval is exactly 1.0 pixels.
# II_NEWPIX does not represent a general puprpose routine because the
# previous routine has determined the proper indices.

real procedure ii_newpix (x, xarray, data, npts, index, interp_type)

real	x		# point to interpolate
real	xarray[ARB]	# x values
real	data[ARB]	# data values
int 	npts		# size of data array
int	index		# index such that xarray[index] < x < xarray[index+1]
int 	interp_type	# interpolator type

int	i, left, right
real	cc[SPLINE3_ORDER, SPLPTS], h
real	ii_polterp()

begin
	switch (interp_type) {

	case II_NEAREST:
	    if (x - xarray[1] > xarray[2] - x)
		return (data[2])
	     else
		return (data[1])

	case II_LINEAR, II_DRIZZLE:
	    return (data[1] + (x - xarray[1]) *
		    (data[2] - data[1]) / (xarray[2] - xarray[1]))

	case II_SPLINE3:
	    do i = 1, npts
		cc[1,i] = data[i]

	    cc[2,1] = 0.
	    cc[2,npts] = 0.

	     # Use spline routine from C. de Boor's book "A Practical Guide
	     # to Splines

	     call iicbsp (xarray, cc, npts, 2, 2)
	     h = x - xarray[index]

	     return (cc[1,index] + h * (cc[2,index] + h *
			       (cc[3,index] + h * cc[4,index]/3.)/2.))

	# One of the polynomial types.
	default:

	    # Allow lower order if not enough points on one side.
	    right = npts
	    left = 1

	    if (npts - index < index) {
		right = 2 * (npts - index)
		left = 2 * index - npts + 1
	    }

	    if (npts - index > index)
		right = 2 * index

	    # Finally polynomial interpolate.
	    return (ii_polterp (xarray[left], data[left], right, x))
	}
end


# II_BADSINC -- Procedure to evaluate bad pixels with a sinc interpolant
# This is the average of interpolation to points +-0.05 from the bad pixel.
# Sinc interpolation exactly at a pixel is undefined. Since this routine
# is intended to be a bad pixel replacement routine, no attempt has been
# made to optimize the routine by precomputing the sinc function.

procedure ii_badsinc (datain, dataout, npts, nsinc, min_bdx)

real	datain[ARB]	# input data including bad pixels with INDEF values
real	dataout[ARB]	# output data 
int	npts		# number of data values
int	nsinc		# sinc truncation length
real	min_bdx		# minimum  distance from interpolation point

int	i, j, k, xc
real	sconst, a2, a4, dx, dx2, dx4
real	w, d, z, w1, u1, v1

begin
	sconst = (HALFPI / nsinc) ** 2
	a2 = -0.49670
	a4 = 0.03705

	do i = 1, npts {

	    if (! IS_INDEFR(datain[i])) {
		dataout[i] = datain[i]
		next
	    }

	    # Initialize.
	    xc = i
	    w = 1.
	    u1 = 0.0; v1 = 0.0

	    do j = 1, nsinc {

		# Get the taper.
		w = -w

		# Sum the low side.
		k = xc - j
		if (k >= 1)
		    d = datain[k]
		else
		    d = datain[1]
		if (! IS_INDEFR(d)) {
		    dx = min_bdx + j
		    dx2 = sconst * j * j
		    dx4 = dx2 * dx2
		    z = 1. / dx
		    w1 = w * z * (1.0 + a2 * dx2 + a4 * dx4) ** 2
		    u1 = u1 + d * w1
		    v1 = v1 + w1
		    dx = -min_bdx + j
		    dx2 = sconst * j * j
		    dx4 = dx2 * dx2
		    z = 1. / dx
		    w1 = -w * z * (1.0 + a2 * dx2 + a4 * dx4) ** 2
		    u1 = u1 + d * w1
		    v1 = v1 + w1
		}

		# Sum the high side.
		k = xc + j
		if (k <= npts)
		    d = datain[k]
		else
		    d = datain[npts]
		if (! IS_INDEFR(d)) {
		    dx = min_bdx - j
		    dx2 = sconst * j * j
		    dx4 = dx2 * dx2
		    z = 1. / dx
		    w1 = w * z * (1.0 + a2 * dx2 + a4 * dx4) ** 2
		    u1 = u1 + d * w1
		    v1 = v1 + w1
		    dx = -min_bdx - j
		    dx2 = sconst * j * j
		    dx4 = dx2 * dx2
		    z = 1. / dx
		    w1 = -w * z * (1.0 + a2 * dx2 + a4 * dx4) ** 2
		    u1 = u1 + d * w1
		    v1 = v1 + w1
		}
	    }

	    # Compute the result.
	    if (v1 != 0.) {
		dataout[i] = u1 / v1
	    } else {
		do j = 1, npts {
		    k = xc - j
		    if (k >= 1)
			d = datain[k]
		    else
			d = datain[1]
		    if (!IS_INDEFR(d)) {
			dataout[i] = d
			break
		    }
		    k = xc + j
		    if (k <= npts)
			d = datain[k]
		    else
			d = datain[npts]
		    if (!IS_INDEFR(d)) {
			dataout[i] = d
			break
		    }
		}
	    }
	}
end
