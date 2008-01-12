# BICUBIC -- Perform bicubic interpolation on an array. The point at which
# the function is to be estimated lies between the second and third columns
# and second and third rows of f, at a distance of (dx, dy), from the (2,2)
# element of f.

real procedure bicubic (f, nbox, dx, dy, dfdx, dfdy)

real	f[nbox,nbox]		# input real array to be interpolated
int	nbox			# size of the input array
real	dx, dy			# point at which array is to be interpolated
real	dfdx, dfdy		# output derivative of the interpolant

int	jy
real	c1, c2, c3, c4, temp[4], dfdxt[4], interp

begin
	# Interpolate first in x.
	do jy = 1, 4 {
	    c1 = 0.5 * (f[3,jy] - f[1,jy])
	    c4 = f[3,jy] - f[2,jy] - c1
	    c2 = 3.0 * c4 - 0.5 * (f[4,jy] - f[2,jy]) + c1
	    c3 = c4 - c2
	    c4 = dx * c3
	    temp[jy] = dx * (dx * (c4 + c2) + c1) + f[2,jy]
	    dfdxt[jy] = dx * (c4 * 3.0 + 2.0 * c2) + c1
	}

	# Interpolate next in y.
	c1 = 0.5 * (temp[3] - temp[1])
	c4 = temp[3] - temp[2] - c1
	c2 = 3.0 * c4 - 0.5 * (temp[4] - temp[2]) + c1
	c3 = c4 - c2
	c4 = dy * c3

	# Get the result.
	interp = dy * (dy * (c4 + c2) + c1) + temp[2]

	# Compute the derivatives.
	dfdy = dy * (c4 * 3.0 + 2.0 * c2) + c1
	c1 = 0.5 * (dfdxt[3] - dfdxt[1])
	c4 = dfdxt[3] - dfdxt[2] - c1
	c2 = 3.0 * c4 - 0.5 * (dfdxt[4] - dfdxt[2]) + c1
	c3 = c4 - c2
	dfdx = dy * (dy * (dy * c3 + c2) + c1) + dfdxt[2]

	return (interp)
end
