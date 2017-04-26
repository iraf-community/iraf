# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.  

include <math.h>


# II_NEAREST -- Procedure to evaluate the nearest neighbour interpolant.

procedure ii_nearest (x, y, npts, data)

real	x[ARB]		# x values, must be within [1,npts]
real	y[ARB]		# interpolated values returned to user
int	npts		# number of x values
real	data[ARB]	# data to be interpolated

int	i

begin
	do i = 1, npts
	    y[i] = data[int(x[i] + 0.5)]
end


# II_LINEAR -- Procedure to evaluate the linear interpolant.

procedure ii_linear (x, y, npts, data)

real	x[ARB]		# x values, must be within [1,npts]
real	y[ARB]		# interpolated values returned to user
int	npts		# number of x values
real	data[ARB]	# data to be interpolated

int	i, nx

begin
	do i = 1, npts {
	    nx = x[i]
	    y[i] =  (x[i] - nx) * data[nx + 1] + (nx + 1 - x[i]) * data[nx]
	}
end


# II_POLY3 -- Procedure to evaluate the cubic polynomial interpolant.

procedure ii_poly3 (x, y, npts, data)

real	x[ARB]		# x values, must be within [1,npts]
real	y[ARB]		# interpolated values returned to user
int	npts		# number of x values
real	data[ARB]	# data to be interpolated from a[0] to a[npts+2]

int	i, nx, nxold
real	deltax, deltay, cd20, cd21

begin
	nxold = -1
	do i = 1, npts {
	    nx = x[i]
	    deltax = x[i] - nx
	    deltay = 1. - deltax

	    if (nx != nxold) {
		# second central differences:
		cd20 = 1./6. * (data[nx+1] - 2. * data[nx] + data[nx-1])
		cd21 = 1./6. * (data[nx+2] - 2. * data[nx+1] + data[nx])
		nxold = nx
	    }

	    y[i] = deltax * (data[nx+1] + (deltax * deltax - 1.) * cd21) +
		    deltay * (data[nx] + (deltay * deltay - 1.) * cd20)
	}
end


# II_POLY5 -- Procedure to evaluate the fifth order polynomial interpolant.

procedure ii_poly5 (x, y, npts, data)

real	x[ARB]		# x values, must be within [1,npts]
real	y[ARB]		# interpolated values returned to user
int	npts		# number of x values
real	data[ARB]	# data to be interpolated - from a[-1] to a[npts+3]

int	i, nx, nxold
real	deltax, deltay, cd20, cd21, cd40, cd41

begin
	nxold = -1
	do i = 1, npts {
	    nx = x[i]
	    deltax = x[i] - nx
	    deltay = 1. - deltax

	    if (nx != nxold) {
		cd20 = 1./6. * (data[nx+1] - 2. * data[nx] + data[nx-1])
		cd21 = 1./6. * (data[nx+2] - 2. * data[nx+1] + data[nx])
		# fourth central differences
		cd40 = 1./120. * (data[nx-2] - 4. * data[nx-1] +
			6. * data[nx] - 4. * data[nx+1] + data[nx+2])
		cd41 = 1./120. * (data[nx-1] - 4. * data[nx] +
			6. * data[nx+1] - 4. * data[nx+2] + data[nx+3])
		nxold = nx
	    }

	    y[i] = deltax * (data[nx+1] + (deltax * deltax - 1.) *
			 (cd21 + (deltax * deltax - 4.) * cd41)) +
		   deltay * (data[nx] + (deltay * deltay - 1.) *
			 (cd20 + (deltay * deltay - 4.) * cd40)) 
	}
end


# II_SPLINE3 -- Procedure to evaluate the cubic spline interpolant.

procedure ii_spline3 (x, y, npts, bcoeff)

real	x[ARB]		# x values, must be within [1,npts]
real	y[ARB]		# interpolated values returned to user
int	npts		# number of x values
real	bcoeff[ARB]	# basis spline coefficients - from a[0] to a[npts+1]

int	i, nx, nxold
real	deltax, c0, c1, c2, c3

begin
	nxold = -1
	do i = 1, npts {
	    nx = x[i]
	    deltax = x[i] - nx

	    if (nx != nxold) {
		# convert b-spline coeff's to poly. coeff's
		c0 = bcoeff[nx-1] + 4. * bcoeff[nx] + bcoeff[nx+1]
		c1 = 3. * (bcoeff[nx+1] - bcoeff[nx-1])
		c2 = 3. * (bcoeff[nx-1] - 2. * bcoeff[nx] + bcoeff[nx+1])
		c3 = -bcoeff[nx-1] + 3. * bcoeff[nx] - 3. * bcoeff[nx+1] +
		     bcoeff[nx+2]
		nxold = nx
	    }

	    y[i] = c0 + deltax * (c1 + deltax * (c2 + deltax * c3))
	}
end


# II_SINC -- Procedure to evaluate the sinc interpolant. The sinc
# truncation length is nsinc. The taper is a cosbell function which is
# approximated by a quartic polynomial which is valid for 0 <= x <= PI / 2
# (Abramowitz and Stegun, 1972, Dover Publications, p 76). If the point to
# be interpolated is less than mindx from a data point no interpolation is
# done and the data point itself is returned.

procedure ii_sinc (x, y, npts, data, npix, nsinc, mindx)

real	x[ARB]		# x values, must be within [1,npts]
real	y[ARB]		# interpolated values returned to user
int	npts		# number of x values
real	data[ARB]	# data to be interpolated
int	npix		# number of data pixels
int	nsinc		# sinc truncation length
real	mindx		# interpolation minimum

int	i, j, xc, minj, maxj, offj
pointer	sp, taper
real	dx, dxn, dx2, w1, sconst, a2, a4, sum, sumw

begin
	# Compute the constants for the cosine bell taper. 
	sconst = (HALFPI / nsinc) ** 2
	a2 = -0.49670
	a4 = 0.03705

	# Pre-compute the taper array. Incorporate the sign change portion
	# of the sinc interpolator into the taper array.
	call smark (sp)
	call salloc (taper, 2 * nsinc + 1, TY_REAL)
	if (mod (nsinc, 2) == 0)
	    w1 = 1.0
	else
	    w1 = -1.0
	do j = -nsinc,  nsinc {
	    dx2 = sconst * j * j
	    Memr[taper+j+nsinc] = w1 * (1.0 + a2 * dx2 + a4 * dx2 * dx2) ** 2
	    w1 = -w1
	}

	do i = 1, npts {

	    # Return zero outside of data.
	    xc = nint (x[i])
	    if (xc < 1 || xc > npix) {
		y[i] = 0.
		next
	    }

	    # Return the data value if x is too close to x[i].
	    dx = x[i] - xc
	    if (abs (dx) < mindx) {
		y[i] = data[xc]
		next
	    }

	    # Compute the limits of the true convolution.
	    minj = max (1, xc - nsinc)
	    maxj = min (npix, xc + nsinc)
	    offj = -xc + nsinc

	    # Do the convolution.
	    sum = 0.0
	    sumw = 0.0
	    dxn = dx + xc
	    do j = xc - nsinc, minj - 1 {
		w1 = Memr[taper+j+offj] / (dxn - j) 
		sum = sum + w1 * data[1]
		sumw = sumw + w1
	    }
	    do j = minj, maxj {
		w1 = Memr[taper+j+offj] / (dxn - j) 
		sum = sum + w1 * data[j]
		sumw = sumw + w1
	    }
	    do j = maxj + 1, xc + nsinc {
		w1 = Memr[taper+j+offj] / (dxn - j) 
		sum = sum + w1 * data[npix]
		sumw = sumw + w1
	    }

	    # Compute value.
	    y[i] = sum / sumw
	}

	call sfree (sp)
end


# II_LSINC -- Procedure to evaluate the sinc interpolant using a
# precomputed look-up table. The sinc truncation length is nsinc. The taper
# is a cosbell function which is  approximated by a quartic polynomial which
# is valid for 0 <= x <= PI / 2 (Abramowitz and Stegun, 1972, Dover
# Publications, p 76). If the point to be interpolated is less than mindx
# from a data point no interpolation is done and the data point itself is
# returned.

procedure ii_lsinc (x, y, npts, data, npix, ltable, nconv, nincr, mindx)

real	x[ARB]			# x values, must be within [1,npix]
real	y[ARB]			# interpolated values returned to user
int	npts			# number of x values
real	data[ARB]		# data to be interpolated
int	npix			# number of data pixels
real	ltable[nconv,nincr]	# the sinc look-up table
int	nconv			# sinc truncation length
int	nincr			# the number of look-up table entries
real	mindx			# interpolation minimum (don't use)

int	i, j, nsinc, xc, lut, minj, maxj, offj
real	dx, sum

begin
	nsinc = (nconv - 1) / 2
	do i = 1, npts {

	    # Return zero outside of data.
	    xc = nint (x[i])
	    if (xc < 1 || xc > npix) {
		y[i] = 0.
		next
	    }

	    # Return data point if dx is too small.
	    dx = x[i] - xc
	    if (abs (dx) < mindx) {
		y[i] = data[xc]
		next
	    }

	    # Find the correct look-up table entry.
	    if (nincr == 1)
		lut = 1
	    else 
		lut = nint ((-dx + 0.5) * (nincr - 1)) + 1
		#lut = int ((-dx + 0.5) * (nincr - 1) + 0.5) + 1

	    # Compute the convolution limits.
	    minj = max (1, xc - nsinc)
	    maxj = min (npix, xc + nsinc)
	    offj = -xc + nsinc + 1

	    # Do the convolution.
	    sum = 0.0
	    do j = xc - nsinc, minj - 1
		sum = sum + ltable[j+offj,lut] * data[1]
	    do j = minj, maxj
		sum = sum + ltable[j+offj,lut] * data[j]
	    do j = maxj + 1, xc + nsinc
		sum = sum + ltable[j+offj,lut] * data[npix]

	    # Compute the value.
	    y[i] = sum
	}
end


# II_DRIZ -- Procedure to evaluate the drizzle interpolant. 

procedure ii_driz (x, y, npts, data, pixfrac, badval)

real	x[ARB]		# x start and stop values, must be within [1,npts]
real	y[ARB]		# interpolated values returned to user
int	npts		# number of x values
real	data[ARB]	# data to be interpolated
real	pixfrac		# the drizzle pixel fraction
real	badval		# value for undefined pixels

int	i, j, neara, nearb
real	hpixfrac, xa, xb, dx, accum, waccum

begin
	hpixfrac = pixfrac / 2.0
	do i = 1, npts {

	    # Define the interval of integration.
	    xa = min (x[2*i-1], x[2*i])
	    xb = max (x[2*i-1], x[2*i])
	    neara = xa + 0.5
	    nearb = xb + 0.5

	    # Initialize the integration
	    accum = 0.0
	    waccum = 0.0
	    if (neara == nearb) {

	        dx = min (xb, nearb + hpixfrac) - max (xa, neara - hpixfrac)

		if (dx > 0.0) {
		    accum = accum + dx * data[neara]
		    waccum = waccum + dx
		}

	    } else {

		# first segement
		dx = neara + hpixfrac - max (xa, neara - hpixfrac)

		if (dx > 0.0) {
		    accum = accum + dx * data[neara]
		    waccum = waccum + dx
		}

		# interior segments.
		do j = neara + 1, nearb - 1 {
		    accum = accum + pixfrac * data[j]
		    waccum = waccum + pixfrac
		}

		# last segment
	        dx = min (xb, nearb + hpixfrac) - (nearb - hpixfrac)

		if (dx > 0.0) {
		    accum = accum + dx * data[nearb]
		    waccum = waccum + dx
		}
	    }

	    if (waccum == 0.0)
		y[i] = badval
	    else
		y[i] = accum / waccum
	}
end


# II_DRIZ1 -- Procedure to evaluate the drizzle interpolant in the case where
# pixfrac = 1.0. 

procedure ii_driz1 (x, y, npts, data, badval)

real	x[ARB]		# x start and stop values, must be within [1,npts]
real	y[ARB]		# interpolated values returned to user
int	npts		# number of x values
real	data[ARB]	# data to be interpolated
real	badval		# undefined pixel value

int	i, j, neara, nearb
real	xa, xb, deltaxa, deltaxb, dx, accum, waccum

begin
	do i = 1, npts {

	    # Define the interval of integration.
	    xa = min (x[2*i-1], x[2*i])
	    xb = max (x[2*i-1], x[2*i])
	    neara = xa + 0.5
	    nearb = xb + 0.5
	    deltaxa = xa - neara
	    deltaxb = xb - nearb

	    # Only one segment involved.
	    accum = 0.0
	    waccum = 0.0
	    if (neara == nearb) {

		dx = deltaxb - deltaxa
		accum = accum + dx * data[neara]
		waccum = waccum + dx

	    } else {

		# First segment.
		dx = 0.5 - deltaxa
		accum = accum + dx * data[neara]
		waccum = waccum + dx

		# Middle segment.
		do j = neara + 1, nearb - 1 {
		    accum = accum + data[j]
		    waccum = waccum + 1.0
		}

		# Last segment.
		dx = deltaxb + 0.5
		accum = accum + dx * data[nearb]
		waccum = waccum + dx
	    }

	    if (waccum == 0.0)
		y[i] = badval
	    else
		y[i] = accum / waccum
	}
end
