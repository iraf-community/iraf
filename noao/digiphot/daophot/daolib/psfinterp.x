# PSF_INTERP -- Interpolate in the PSF lookup table.

real procedure psf_interp (lookup, nx_psf, ny_psf, dx, dy, varpsf, x, y,
	dfdx, dfdy, status)

real	lookup[nx_psf,ny_psf,3]	# lookup table
int	nx_psf			# size of lookup table in x direction
int	ny_psf			# size of lookup table in y direction
real	dx, dy			# distance of star from the psf
int	varpsf			# variable psf across the image
real	x, y			# coordinates for interpolation
real	dfdx, dfdy		# first derivatives wrt x and y
int	status			# status return

real 	dummy, deltax, deltay
real	dgdx[4], g[4], df[4,4]
int	i, j, ii, jj, ix, iy
real	cubint()

begin
	# Check that we are in bounds
	if ((x < 2.0) || (x > real (nx_psf - 1))) {
	    status = ERR
	    dfdx = 0.0
	    dfdy = 0.0
	    return (0.0)
	}

	if ((y < 2.0) || (y > real (ny_psf - 1))) {
	    status = ERR
	    dfdx = 0.0
	    dfdy = 0.0
	    return (0.0)
	}

	status = OK
	ix = int (x)
	iy = int (y)
	deltax = x - ix
	deltay = y - iy

	if (varpsf == YES) {
	    jj = 1
	    do j = iy - 1, iy + 2 {
		ii = 1
		do i = ix - 1, ix + 2 {
		    df[ii,jj] = lookup[i,j,1] + dx * lookup[i,j,2] + dy *
			lookup[i,j,3]
		    ii = ii + 1
		}
		jj = jj + 1
	    }
	    do i = 1, 4
		g[i] = cubint (df[1,i], deltax, dgdx[i])
	} else {
	    j = iy - 2
	    do i = 1, 4 {
	        j = j + 1
	        g[i] = cubint (lookup[ix-1,j,1], deltax, dgdx[i])
	    }
	}

	dfdx = cubint (dgdx, deltay, dummy)
	return (cubint (g, deltay, dfdy))
end


# CUBINT -- Interpolate using a cubic interpolating polynomial.

real procedure cubint (f, x, dfdx)

real	f [4]			# Function to be interpolated
real	x			# Distance between 2nd element and point
real	dfdx			# Estimate of dF/dx at position X

real	c1, c2, c3
begin

	c1 = 0.5 * (f[3] - f[1])
	c2 = 2.0 * f[3] + f[1] - 0.5 * (5.0 * f[2] + f[4])
	c3 = 0.5 * (3.0 * (f[2] - f[3]) + f[4] - f[1])
	dfdx = x * (x * c3 * 3.0 + 2.0 * c2) + c1
	return (x * (x * (x * c3 + c2) + c1) + f[2])
end
