# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# II_NEAREST -- Procedure to evaluate the nearest neighbour interpolant

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


# II_LINEAR -- Procedure to evaluate a linear interpolant

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


# II_POLY3 -- Procedure to evaluate a cubic polynomial interpolant

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


# II_POLY5 -- Procedure to evaluate a fifth order polynomial interpolant

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


# II_SPLINE3 -- Procedure to evaluate a cubic spline interpolant

procedure ii_spline3 (x, y, npts, bcoeff)   # cubic spline evaluator

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


# II_SINC -- Evaluate the sinc interpolant at an array of points.
# The sinc truncation length is nsinc. The taper is a triangle function of
# slope staper which begins at ntaper. If the point to be interpolated
# is less than mindx from a data point no interpolation is done and the
# data point itself is returned.

procedure ii_sinc (x, y, npts, data, npix, nsinc, ntaper, staper, mindx)

real	x[ARB]		# x values, must be within [1,npts]
real	y[ARB]		# interpolated values returned to user
int	npts		# number of x values
real	data[ARB]	# data to be interpolated
int	npix		# number of data pixels
int	nsinc		# sinc truncation length
int	ntaper		# start of triangular taper
real	staper		# slope of triangular taper
real	mindx		# interpolation minimum

int	i, j, k, xc
real	dx, w, d, z
real	w1, u1, v1, u1a, v1a

begin
	do i = 1, npts {

	    # Return zero outside of data.
	    xc = nint (x[i])
	    if (xc < 1 || xc > npix) {
		y[i] = 0.
		next
	    }

	    # Return the data value if x is tool close to x[i].
	    dx = x[i] - xc
	    if (abs (dx) < mindx) {
		y[i] = data[xc]
		next
	    }

	    # Initialize.
	    w = 1.
	    d = data[xc]
	    z = 1. / dx
	    w1 = w * z; u1 = d * w1; v1 = w1

	    do j = 1, nsinc {

		# Get the taper.
		w = -w
		if (j > ntaper) {
		    if (w < 0.)
			w = min (0., w + staper)
		    else
			w = max (0., w - staper)
		    if (w == 0.)
			break
		}

		# Store previous value.
		u1a = u1; v1a = v1

		# Sum the low side.
		k = xc - j
		if (k >= 1) {
		    d = data[k]
		    z = 1. / (dx + j)
		    w1 = w * z; u1 = u1 + d * w1; v1 = v1 + w1
		}

		# Sum the high side.
		k = xc + j
		if (k <= npix) {
		    d = data[k]
		    z = 1. / (dx - j)
		    w1 = w * z; u1 = u1 + d * w1; v1 = v1 + w1
		}
	    }

	    # Average current and previous value.
	    y[i] = (u1 / v1 + u1a / v1a) / 2.
	}
end
