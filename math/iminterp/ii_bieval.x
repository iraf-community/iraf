# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# II_BINEAREST -- Procedure to evaluate the nearest neighbour interpolant.
# The real array coeff contains the coefficients of the 2D interpolant.
# The procedure assumes that 1 <= x <= nxpix and 1 <= y <= nypix and that
# coeff[1+first_point] = datain[1,1].
 
procedure ii_binearest (coeff, first_point, len_coeff, x, y, zfit, npts)

real	coeff[ARB]	# 1D coefficient array
int	first_point	# offset of first data point
int	len_coeff	# row length of coeff
real	x[npts]		# array of x values
real	y[npts]		# array of y values
real	zfit[npts]	# array of interpolated values
int	npts		# number of points to be evaluated

int	nx, ny
int	index
int	i

begin
	do i = 1, npts {

	    nx = x[i] + 0.5
	    ny = y[i] + 0.5

	    # define pointer to data[nx,ny]
	    index = first_point + (ny - 1) * len_coeff + nx

	    zfit[i] = coeff[index]

	}
end


# II_BILINEAR -- Procedure to evaluate the bilinear interpolant.
# The real array coeff contains the coefficients of the 2D interpolant.
# The procedure assumes that 1 <= x <= nxpix and 1 <= y <= nypix
# and that coeff[1+first_point] = datain[1,1].

procedure ii_bilinear (coeff, first_point, len_coeff, x, y, zfit, npts)

real	coeff[ARB]	# 1D array of coefficients
int	first_point	# offset of first data point
int	len_coeff	# row length of coeff
real	x[npts]		# array of x values
real	y[npts]		# array of y values
real	zfit[npts]	# array of interpolated values
int	npts		# number of data points

int	nx, ny
int	index
int	i
real	sx, sy, tx, ty

begin
	do i = 1, npts {

	    nx = x[i]
	    ny = y[i]

	    sx = x[i] - nx
	    tx = 1. - sx
	    sy = y[i] - ny
	    ty = 1. - sy

	    # define pointer to data[nx,ny]
	    index = first_point + (ny - 1) * len_coeff + nx

	    zfit[i] = tx * ty * coeff[index] + sx * ty * coeff[index + 1] +
		      sy * tx * coeff[index+len_coeff] +
		      sx * sy * coeff[index+len_coeff+1]
	}
end


# II_BIPOLY3 -- Procedure to evaluate the bicubic polynomial interpolant.
# The real array coeff contains the coefficients of the 2D interpolant.
# The procedure assumes that 1 <= x <= nxpix and  1 <= y <= nypix
# and that coeff[1+first_point] = datain[1,1]. The interpolant is
# evaluated using Everett's central difference formula.

procedure ii_bipoly3 (coeff, first_point, len_coeff, x, y, zfit, npts)

real	coeff[ARB]	# 1D array of coefficients
int	first_point	# offset first point
int	len_coeff	# row length of the coefficient array
real	x[npts]		# array of x values
real	y[npts]		# array of y values
real	zfit[npts]	# array of fitted values
int	npts		# number of points to be evaluated

int	nxold, nyold, nx, ny
int	first_row, index
int	i, j
real	sx, tx, sx2m1, tx2m1, sy, ty
real	cd20[4], cd21[4], ztemp[4], cd20y, cd21y

begin
	nxold = -1
	nyold = -1

	do i = 1, npts {

	    nx = x[i]
	    sx = x[i] - nx
	    tx = 1. - sx
	    sx2m1 = sx * sx - 1.
	    tx2m1 = tx * tx - 1.

	    ny = y[i]
	    sy = y[i] - ny
	    ty = 1. - sy

	    # define pointer to datain[nx,ny-1]
	    first_row = first_point + (ny - 2) * len_coeff + nx

	    # loop over the 4 surrounding rows of data
	    # calculate the  central differences at each value of y

	    # if new data point caculate the central differnences in x
	    # for each y

	    index = first_row
	    if (nx != nxold || ny != nyold) {
	        do j = 1, 4 {
		   cd20[j] = 1./6. * (coeff[index+1] - 2. * coeff[index] +
		   	     coeff[index-1])
		   cd21[j] = 1./6. * (coeff[index+2] - 2. * coeff[index+1] +
			     coeff[index])
		    index = index + len_coeff
	        }
	    }

	    # interpolate in x at each value of y
	    index = first_row
	    do j = 1, 4 {
		ztemp[j] = sx * (coeff[index+1] + sx2m1 * cd21[j]) +
			   tx * (coeff[index] + tx2m1 * cd20[j])
		index = index + len_coeff
	    }

	    # calculate y central differences
	    cd20y = 1./6. * (ztemp[3] - 2. * ztemp[2] + ztemp[1])
	    cd21y = 1./6. * (ztemp[4] - 2. * ztemp[3] + ztemp[2])

	    # interpolate in y
	    zfit[i] = sy * (ztemp[3] + (sy * sy - 1.) * cd21y) +
		      ty * (ztemp[2] + (ty * ty - 1.) * cd20y)

	    nxold = nx
	    nyold = ny

	}
end


# II_BIPOLY5 -- Procedure to evaluate a biquintic polynomial.
# The real array coeff contains the coefficents of the 2D interpolant.
# The routine assumes that 1 <= x <= nxpix and 1 <= y <= nypix
# and that coeff[1+first_point] = datain[1,1]. The interpolant is evaluated
# using Everett's central difference formula.

procedure ii_bipoly5 (coeff, first_point, len_coeff, x, y, zfit, npts)

real	coeff[ARB]	# 1D array of coefficients
int	first_point	# offset to first data point
int	len_coeff	# row length of coeff
real	x[npts]		# array of x values
real	y[npts]		# array of y values
real	zfit[npts]	# array of fitted values
int	npts		# number of points

int	nxold, nyold, nx, ny
int	first_row, index
int	i, j
real	sx, sx2, sx2m1, sx2m4, tx, tx2, tx2m1, tx2m4, sy, sy2, ty, ty2
real	cd20[6], cd21[6], cd40[6], cd41[6], ztemp[6]
real	cd20y, cd21y, cd40y, cd41y

begin
	nxold = -1
	nyold = -1

	do i = 1, npts {

	    nx = x[i]
	    sx = x[i] - nx
	    sx2 = sx * sx
	    sx2m1 = sx2 - 1.
	    sx2m4 = sx2 - 4.
	    tx = 1. - sx
	    tx2 = tx * tx
	    tx2m1 = tx2 - 1.
	    tx2m4 = tx2 - 4.

	    ny = y[i]
	    sy = y[i] - ny
	    sy2 = sy * sy
	    ty = 1. - sy
	    ty2 = ty * ty

	    # calculate value of pointer to data[nx,ny-2]
	    first_row = first_point + (ny - 3) * len_coeff + nx

	    # calculate the central differences in x at each value of y
	    index = first_row
	    if (nx != nxold || ny != nyold) {
		do j = 1, 6 {
		    cd20[j] = 1./6. * (coeff[index+1] - 2. * coeff[index] +
		    	      coeff[index-1])
		    cd21[j] = 1./6. * (coeff[index+2] - 2. * coeff[index+1] +
			      coeff[index])
		    cd40[j] = 1./120. * (coeff[index-2] - 4. * coeff[index-1] +
			      6. * coeff[index] - 4. * coeff[index+1] +
			      coeff[index+2])
		    cd41[j] = 1./120. * (coeff[index-1] - 4. * coeff[index] +
			      6. * coeff[index+1] - 4. * coeff[index+2] +
			      coeff[index+3])
		    index = index + len_coeff
		}
	    }

	    # interpolate in x at each value of y
	    index = first_row
	    do j = 1, 6 {
		ztemp[j] = sx * (coeff[index+1] + sx2m1 * (cd21[j] + sx2m4 *
			   cd41[j])) + tx * (coeff[index] + tx2m1 *
			   (cd20[j] + tx2m4 * cd40[j]))
		index = index + len_coeff
	    }

	    # central differences in y
	    cd20y = 1./6. * (ztemp[4] - 2. * ztemp[3] + ztemp[2])
	    cd21y = 1./6. * (ztemp[5] - 2. * ztemp[4] + ztemp[3])
	    cd40y = 1./120. * (ztemp[1] - 4. * ztemp[2] + 6. * ztemp[3] -
		    4. * ztemp[4] + ztemp[5])
	    cd41y = 1./120. * (ztemp[2] - 4. * ztemp[3] + 6. * ztemp[4] -
		    4. * ztemp[5] + ztemp[6])

	    # interpolate in y
	    zfit[i] = sy * (ztemp[4] + (sy2 - 1.) * (cd21y + (sy2 - 4.) *
		      cd41y)) + ty * (ztemp[3] + (ty2 - 1.) * (cd20y +
		      (ty2 - 4.) * cd40y))

	    nxold = nx
	    nyold = ny

	}
end


# II_BISPLINE3 -- Procedure to evaluate a bicubic spline.
# The real array coeff contains the B-spline coefficients.
# The procedure assumes that 1 <= x <= nxpix and 1 <= y <= nypix
# and that coeff[1+first_point] = B-spline[2].

procedure ii_bispline3 (coeff, first_point, len_coeff, x, y, zfit, npts)

real	coeff[ARB]	# 1D array of coefficients
int	first_point	# offset to first data point
int	len_coeff	# row length of coeff
real	x[npts]		# array of x values
real	y[npts]		# array of y values
real	zfit[npts]	# array of interpolated values
int	npts		# number of points to be evaluated

int	nx, ny
int	first_row, index
int	i, j
real	sx, tx, sy, ty
real	bx[4], by[4], accum, sum

begin
	do i = 1, npts {

	    nx = x[i]
	    sx = x[i] - nx
	    tx = 1. - sx

	    ny = y[i]
	    sy = y[i] - ny
	    ty = 1. - sy


	    # calculate the x B-splines
	    bx[1] = tx ** 3 
	    bx[2] = 1. + tx * (3. + tx * (3. - 3. * tx))
	    bx[3] = 1. + sx * (3. + sx * (3. - 3. * sx))
	    bx[4] = sx ** 3

	    # calculate the y B-splines
	    by[1] = ty ** 3
	    by[2] = 1. + ty * (3. + ty * (3. - 3. * ty))
	    by[3] = 1. + sy * (3. + sy * (3. - 3. * sy))
	    by[4] = sy ** 3

	    # calculate the pointer to data[nx,ny-1]
	    first_row = first_point + (ny - 2) * len_coeff + nx

	    # evaluate spline
	    accum = 0.
	    index = first_row
	    do j = 1, 4 {
		sum = coeff[index-1] * bx[1] + coeff[index] * bx[2] +
		      coeff[index+1] * bx[3] + coeff[index+2] * bx[4]	
		accum = accum + sum * by[j]
		index = index + len_coeff
	    }

	    zfit[i] = accum
	}
end
