# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math.h>

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


# II_BISINC -- Procedure to evaluate the 2D sinc function.  The real array
# coeff contains the data. The procedure assumes that 1 <= x <= nxpix and
# 1 <= y <= nypix and that coeff[1+first_point] = datain[1,1]. The since
# truncation length is nsinc. The taper is a cosbell function which is
# valid for 0 <= x <= PI / 2 (Abramowitz and Stegun, 1972, Dover Publications,
# p 76). If the point to be interpolated is less than mindx and mindy from
# a data point no interpolation is done and the data point is returned. This
# routine does not use precomputed arrays.

procedure ii_bisinc (coeff, first_point, len_coeff, len_array, x, y, zfit,
	npts, nsinc, mindx, mindy)

real	coeff[ARB]	# 1D array of coefficients
int	first_point	# offset to first data point
int	len_coeff	# row length of coeff
int	len_array	# column length of coeff
real	x[npts]		# array of x values
real	y[npts]		# array of y values
real	zfit[npts]	# array of interpolated values
int	npts		# the number of input points.
int	nsinc		# sinc truncation length
real	mindx		# interpolation mininmum in x
real	mindy		# interpolation mininmum in y

int	i, j, k, nconv, nx, ny, index, mink, maxk, offk, minj, maxj, offj
int	last_point
pointer	sp, taper, ac, ar
real	sconst, a2, a4, sdx, dx, dy, dxn, dyn, ax, ay, px, py, sumx, sumy, sum
real	dx2

begin
	# Compute the length of the convolution.
	nconv = 2 * nsinc + 1

	# Allocate working array space.
	call smark (sp)
	call salloc (taper, nconv, TY_REAL)
	call salloc (ac, nconv, TY_REAL)
	call salloc (ar, nconv, TY_REAL)

	# Compute the constants for the cosine bell taper.
        sconst = (HALFPI / nsinc) ** 2
        a2 = -0.49670
        a4 = 0.03705

	# Precompute the taper array. Incorporate the sign change portion
	# of the sinc interpolator into the taper array.
	if (mod (nsinc, 2) == 0)
	    sdx = 1.0
	else
	    sdx = -1.0
        do j = -nsinc,  nsinc {
	    dx2 = sconst * j * j
            Memr[taper+j+nsinc] = sdx * (1.0 + a2 * dx2 + a4 * dx2 * dx2) ** 2
            sdx = -sdx
        }

	do i = 1, npts {

	    # define the fractional pixel interpolation.
	    nx = nint (x[i])
	    ny = nint (y[i])
	    if (nx < 1 || nx > len_coeff || ny < 1 || ny > len_array) {
		zfit[i] = 0.0
		next
	    }
	    dx = x[i] - nx
	    dy = y[i] - ny

	    # define pointer to data[nx,ny]
	    if (abs (dx) < mindx && abs (dy) < mindy) {
	        index = first_point + (ny - 1) * len_coeff + nx
		zfit[i] = coeff[index]
		next
	    }

	    # initialize.
	    #dxn = -1-nsinc-dx
	    #dyn = -1-nsinc-dy
	    dxn = 1 + nsinc + dx
	    dyn = 1 + nsinc + dy

	    # Compute the x and y sinc arrays using a cosbell taper.
	    sumx = 0.0
	    sumy = 0.0
	    do j = 1, nconv {

		#ax = j + dxn
		#ay = j + dyn
		ax = dxn - j
		ay = dyn - j
		if (ax == 0.0)
		    px = 1.0
		else if (dx == 0.0)
		    px = 0.0
		else
		    px = Memr[taper+j-1] / ax 
		if (ay == 0.0)
		    py = 1.0
		else if (dy == 0.0)
		    py = 0.0
		else
		    py = Memr[taper+j-1] / ay 

		Memr[ac+j-1] = px
		Memr[ar+j-1] = py
		sumx = sumx + px
		sumy = sumy + py
	    }

	    # Compute the limits of the convolution.
	    minj = max (1, ny - nsinc)
	    maxj = min (len_array, ny + nsinc)
	    offj = ar - ny + nsinc
	    mink = max (1, nx - nsinc)
	    maxk = min (len_coeff, nx + nsinc)
	    offk = ac - nx + nsinc

	    # Initialize
	    zfit[i] = 0.0

	    # Do the convolution.
	    do j = ny - nsinc, minj - 1 {
		sum = 0.0
		do k = nx - nsinc, mink - 1
		    sum = sum + Memr[k+offk] * coeff[first_point+1] 
	        do k = mink, maxk
		    sum = sum + Memr[k+offk] * coeff[first_point+k] 
		do k = maxk + 1, nx + nsinc
		    sum = sum + Memr[k+offk] * coeff[first_point+len_coeff] 

		zfit[i] = zfit[i] + Memr[j+offj] * sum
	    }

	    do j = minj, maxj {
	    	index = first_point + (j - 1) * len_coeff
		sum = 0.0
		do k = nx - nsinc, mink - 1
		    sum = sum + Memr[k+offk] * coeff[index+1] 
	        do k = mink, maxk
		    sum = sum + Memr[k+offk] * coeff[index+k] 
		do k = maxk + 1, nx + nsinc
		    sum = sum + Memr[k+offk] * coeff[index+len_coeff] 

		zfit[i] = zfit[i] + Memr[j+offj] * sum
	    }

	    do j = maxj + 1, ny + nsinc {
	        last_point = first_point + (len_array - 1) * len_coeff
		sum = 0.0
		do k = nx - nsinc, mink - 1
		    sum = sum + Memr[k+offk] * coeff[last_point+1] 
	        do k = mink, maxk
		    sum = sum + Memr[k+offk] * coeff[last_point+k] 
		do k = maxk + 1, nx + nsinc
		    sum = sum + Memr[k+offk] * coeff[last_point+len_coeff] 

		zfit[i] = zfit[i] + Memr[j+offj] * sum
	    }

	    # Normalize.
	    zfit[i] = zfit[i] / sumx / sumy
	}

	call sfree (sp)
end


# II_BILSINC -- Procedure to evaluate the 2D sinc function.  The real array
# coeff contains the data. The procedure assumes that 1 <= x <= nxpix and
# 1 <= y <= nypix and that coeff[1+first_point] = datain[1,1]. The since
# truncation length is nsinc. The taper is a cosbell function which is
# valid for 0 <= x <= PI / 2 (Abramowitz and Stegun, 1972, Dover Publications,
# p 76). If the point to be interpolated is less than mindx and mindy from
# a data point no interpolation is done and the data point is returned. This
# routine does use precomputed arrays.

procedure ii_bilsinc (coeff, first_point, len_coeff, len_array, x, y, zfit,
	npts, ltable, nconv, nxincr, nyincr, mindx, mindy)

real	coeff[ARB]			   # 1D array of coefficients
int	first_point			   # offset to first data point
int	len_coeff			   # row length of coeff
int	len_array			   # column length of coeff
real	x[npts]				   # array of x values
real	y[npts]				   # array of y values
real	zfit[npts]			   # array of interpolated values
int	npts				   # the number of input points.
real	ltable[nconv,nconv,nxincr,nyincr]  # the pre-computed look-up table
int	nconv				   # the sinc truncation full width
int	nxincr				   # the interpolation resolution in x
int	nyincr				   # the interpolation resolution in y
real	mindx				   # interpolation mininmum in x
real	mindy				   # interpolation mininmum in y

int	i, j, k, nsinc, xc, yc, lutx, luty, minj, maxj, offj, mink, maxk, offk
int	index, last_point
real	dx, dy, sum

begin
	nsinc = (nconv - 1) / 2
	do i = 1, npts {

	    # Return zero outside of data.
	    xc = nint (x[i])
	    yc = nint (y[i])
	    if (xc < 1 || xc > len_coeff || yc < 1 || yc > len_array) {
		zfit[i] = 0.0
		next
	    }

	    dx = x[i] - xc
	    dy = y[i] - yc
	    if (abs(dx) < mindx && abs(dy) < mindy) {
		index = first_point + (yc - 1) * len_coeff + xc
		zfit[i] = coeff[index]
	    }

	    # Find the correct look-up table entry.
	    if (nxincr == 1)
		lutx = 1
	    else 
		lutx = nint ((-dx + 0.5) * (nxincr - 1)) + 1
		#lutx = int ((-dx + 0.5) * (nxincr - 1) + 0.5) + 1
	    if (nyincr == 1)
		luty = 1
	    else
		luty = nint ((-dy + 0.5) * (nyincr - 1)) + 1
		#luty = int ((-dy + 0.5) * (nyincr - 1) + 0.5) + 1

	    # Compute the convolution limits.
	    minj = max (1, yc - nsinc)
	    maxj = min (len_array, yc + nsinc)
	    offj = 1 - yc + nsinc
	    mink = max (1, xc - nsinc)
	    maxk = min (len_coeff, xc + nsinc)
	    offk = 1 - xc + nsinc

	    # Initialize
	    zfit[i] = 0.0

	    # Do the convolution.
	    do j = yc - nsinc, minj - 1 {
		sum = 0.0
		do k = xc - nsinc, mink - 1
		    sum = sum + ltable[k+offk,j+offj,lutx,luty] *
		        coeff[first_point+1] 
	        do k = mink, maxk
		    sum = sum + ltable[k+offk,j+offj,lutx,luty] *
		        coeff[first_point+k] 
		do k = maxk + 1, xc + nsinc
		    sum = sum + ltable[k+offk,j+offj,lutx,luty] *
		        coeff[first_point+len_coeff] 
		zfit[i] = zfit[i] +  sum
	    }

	    do j = minj, maxj {
	    	index = first_point + (j - 1) * len_coeff
		sum = 0.0
		do k = xc - nsinc, mink - 1
		    sum = sum + ltable[k+offk,j+offj,lutx,luty] * coeff[index+1]
	        do k = mink, maxk
		    sum = sum + ltable[k+offk,j+offj,lutx,luty] * coeff[index+k]
		do k = maxk + 1, xc + nsinc
		    sum = sum + ltable[k+offk,j+offj,lutx,luty] *
		        coeff[index+len_coeff] 
		zfit[i] = zfit[i] + sum
	    }

	    do j = maxj + 1, yc + nsinc {
	        last_point = first_point + (len_array - 1) * len_coeff
		sum = 0.0
		do k = xc - nsinc, mink - 1
		    sum = sum + ltable[k+offk,j+offj,lutx,luty] *
		        coeff[last_point+1] 
	        do k = mink, maxk
		    sum = sum + ltable[k+offk,j+offj,lutx,luty] *
		        coeff[last_point+k] 
		do k = maxk + 1, xc + nsinc
		    sum = sum + ltable[k+offk,j+offj,lutx,luty] *
		        coeff[last_point+len_coeff] 
		zfit[i] = zfit[i] + sum
	    }

	}
end


# II_BIDRIZ -- Procedure to evaluate the drizzle interpolant.
# The real array coeff contains the coefficients of the 2D interpolant.
# The procedure assumes that 1 <= x <= nxpix and 1 <= y <= nypix and that
# coeff[1+first_point] = datain[1,1]. Each x and y value is a set of 4
# values describing the vertices of a quadrilateral in the input data. The
# integration routine was adapted from the one developed by Bill Sparks at
# ST and used the DITHER / DRIZZLE software. The 4 points describing the
# corners of each quadrilateral integration region must be in order, e.g.
# describe the vertices of a polygon in either CW or CCW order.
 
procedure ii_bidriz (coeff, first_point, len_coeff, x, y, zfit, npts,
	xfrac, yfrac, badval)

real	coeff[ARB]	# 1D coefficient array
int	first_point	# offset of first data point
int	len_coeff	# row length of coeff
real	x[ARB]		# array of x values
real	y[ARB]		# array of y values
real	zfit[npts]	# array of interpolated values
int	npts		# number of points to be evaluated
real	xfrac, yfrac	# the x and y drizzle pixel fractions
real	badval		# undefined pixel value

int	i, ii, jj, kk, index, nearax, nearbx, nearay, nearby
real	px[5], py[5], dx, xmin, xmax, m, c, ymin, ymax, xtop
real	ovlap, accum, waccum, dxfrac, dyfrac, hxfrac, hyfrac, dhxfrac, dhyfrac
bool	negdx

begin
	dxfrac = max (0.0, min (1.0, 1.0 - xfrac))
	hxfrac = max (0.0, min (0.5, dxfrac / 2.0))
	dhxfrac = max (0.5, min (1.0, 1.0 - hxfrac))
	dyfrac = max (0.0, min (1.0, 1.0 - yfrac))
	hyfrac = max (0.0, min (0.5, dyfrac / 2.0))
	dhyfrac = max (0.5, min (1.0, 1.0 - hyfrac))

	do i = 1, npts {

	    # Compute the limits of the integration in x and y.
	    nearax = nint (min (x[4*i-3], x[4*i-2], x[4*i-1], x[4*i]))
	    nearbx = nint (max (x[4*i-3], x[4*i-2], x[4*i-1], x[4*i]))
	    nearay = nint (min (y[4*i-3], y[4*i-2], y[4*i-1], y[4*i]))
	    nearby = nint (max (y[4*i-3], y[4*i-2], y[4*i-1], y[4*i]))

	    # Initialize.
	    accum = 0.0
	    waccum = 0.0

	    # Loop over all pixels which contribute to the integral.
	    do jj = nearay, nearby {
		index = first_point + (jj - 1) * len_coeff
		do kk = 1, 4
		    py[kk] = y[4*i+kk-4] - jj + 0.5
		py[5] = py[1]
		do ii = nearax, nearbx {

		    # Transform the coordinates relative to a unit
		    # square centered at the origin of the pixel. We
		    # are going to approximate the new pixel area by
		    # a quadilateral. Close the quadilateral.

		    do kk = 1, 4
			px[kk] = x[4*i+kk-4] - ii + 0.5 
		    px[5] = px[1]

		    # Compute the area overlap of the output pixel with
		    # the input pixels. 
		    ovlap = 0.0
		    do kk = 1, 4 {

			# Check for vertical line segment.
			dx = px[kk+1] - px[kk]
			if (dx == 0.0)
			    next

			# Order the x integration limits.
			if (px[kk] < px[kk+1]) {
			    xmin = px[kk]
			    xmax = px[kk+1]
			} else {
			    xmin = px[kk+1]
			    xmax = px[kk]
			}

			# Check the x limits ignoring y for now.
			if ((xmin >= dhxfrac) || (xmax <= hxfrac))
			    next
			xmin = max (xmin, hxfrac)
			xmax = min (xmax, dhxfrac)

			# Get basic info about the line y = mx + c.
			if (dx < 0.0)
			    negdx = true
			else
			    negdx = false
			m = (py[kk+1] - py[kk]) / dx
			c = py[kk] - m * px[kk]
			ymin = m * xmin + c
			ymax = m * xmax + c

			# Trap segment entirely below axis.
			if (ymin <= hyfrac && ymax <= hyfrac)
			    next

			# Adjust bounds if segment crosses axis in order
			# to exclude anything below the axis.
			if (ymin < hyfrac) {
			    ymin = hyfrac
			    xmin = (hyfrac - c) / m
			}
			if (ymax < hyfrac) {
			    ymax = hyfrac
			    xmax = (hyfrac - c) / m
			}

			# There are four possibilities.

			# Both y above 1.0 - hyfrac. Line segment is entirely
			# above square.
			if ((ymin >= dhyfrac) && (ymax >= dhyfrac)) {

			    if (negdx)
				ovlap = ovlap + (xmin - xmax) * yfrac
			    else
				ovlap = ovlap + (xmax - xmin) * yfrac

			# Both y below 1.0 - hyfrac. Segment is entirely
			# within square.
			} else if ((ymin <= dhyfrac) && (ymax <= dhyfrac)) {

			    if (negdx)
				ovlap = ovlap + 0.5 * (xmin - xmax) *
				    (ymax + ymin - dyfrac)
			    else
				ovlap = ovlap + 0.5 * (xmax - xmin) *
				    (ymax + ymin - dyfrac)

			# One of each. Segment must cross top of square.
			} else {

			    xtop = (dhyfrac - c) / m

			    # insert precision check ?

			    if (ymin < dhyfrac) {
				if (negdx)
				    ovlap = ovlap - (0.5 * (xtop - xmin) *
				        (ymin + 1.0 - 3.0 * hyfrac) +
				        (xmax - xtop) * yfrac)
				else
				    ovlap = ovlap + (0.5 * (xtop - xmin) *
				        (ymin + 1.0 - 3.0 * hyfrac) +
				        (xmax - xtop) * yfrac)
			     } else {
				if (negdx)
				    ovlap = ovlap - (0.5 * (xmax - xtop) *
				        (ymax + 1.0 - 3.0 * hyfrac) +
					(xtop - xmin) * yfrac)
				else
				    ovlap = ovlap + (0.5 * (xmax - xtop) *
				        (ymax + 1.0 - 3.0 * hyfrac) +
					(xtop - xmin) * yfrac)
			     }

			}
		    }

		    accum = accum + coeff[index+ii] * ovlap
		    waccum = waccum + ovlap
		}
	    }

	    if (waccum == 0.0)
		zfit[i] = badval
	    else
		zfit[i] = accum / waccum
	}
end


# II_BIDRIZ1 -- Procedure to evaluate the drizzle interpolant when xfrac and
# yfrac are 1.0. The real array coeff contains the coefficients of the 2D
# interpolant. The procedure assumes that 1 <= x <= nxpix and 1 <= y <= nypix
# and that coeff[1+first_point] = datain[1,1]. Each x and y point is a set of 4
# values describing the vertices of a quadrilateral in the input data. The
# integration routine was adapted from the one developed by Bill Sparks at
# ST and used the DITHER / DRIZZLE software. The 4 points describing the
# corners of each quadrilateral integration region must be in order, e.g.
# describe the vertices of a polygon in either CW or CCW order.
 
procedure ii_bidriz1 (coeff, first_point, len_coeff, x, y, zfit, npts, badval)

real	coeff[ARB]	# 1D coefficient array
int	first_point	# offset of first data point
int	len_coeff	# row length of coeff
real	x[ARB]		# array of x values
real	y[ARB]		# array of y values
real	zfit[npts]	# array of interpolated values
int	npts		# number of points to be evaluated
real	badval		# undefined pixel value

int	i, ii, jj, kk, index, nearax, nearbx, nearay, nearby
real	px[5], py[5], dx, xmin, xmax, m, c, ymin, ymax, xtop
real	ovlap, accum, waccum
bool	negdx

begin
	do i = 1, npts {

	    # Compute the limits of the integration in x and y.
	    nearax = nint (min (x[4*i-3], x[4*i-2], x[4*i-1], x[4*i]))
	    nearbx = nint (max (x[4*i-3], x[4*i-2], x[4*i-1], x[4*i]))
	    nearay = nint (min (y[4*i-3], y[4*i-2], y[4*i-1], y[4*i]))
	    nearby = nint (max (y[4*i-3], y[4*i-2], y[4*i-1], y[4*i]))

	    # Initialize.
	    accum = 0.0
	    waccum = 0.0

	    # Loop over all pixels which contribute to the integral.
	    do jj = nearay, nearby {
		index = first_point + (jj - 1) * len_coeff
		do kk = 1, 4
		    py[kk] = y[4*i+kk-4] - jj + 0.5
		py[5] = py[1]
		do ii = nearax, nearbx {

		    # Transform the coordinates relative to a unit
		    # square centered at the origin of the pixel. We
		    # are going to approximate the new pixel area by
		    # a quadilateral. Close the polygon.

		    do kk = 1, 4
			px[kk] = x[4*i+kk-4] - ii + 0.5
		    px[5] = px[1]

		    # Compute the area overlap of the output pixel with
		    # the input pixels. 
		    ovlap = 0.0
		    do kk = 1, 4 {

			# Check for vertical line segment.
			dx = px[kk+1] - px[kk]
			if (dx == 0.0)
			    next

			# Order the x integration limits.
			if (px[kk] < px[kk+1]) {
			    xmin = px[kk]
			    xmax = px[kk+1]
			} else {
			    xmin = px[kk+1]
			    xmax = px[kk]
			}

			# Check the x limits ignoring y for now.
			if (xmin >= 1.0 || xmax <= 0.0)
			    next
			xmin = max (xmin, 0.0)
			xmax = min (xmax, 1.0)

			# Get basic info about the line y = mx + c.
			if (dx < 0.0)
			    negdx = true
			else
			    negdx = false
			m = (py[kk+1] - py[kk]) / dx
			c = py[kk] - m * px[kk]
			ymin = m * xmin + c
			ymax = m * xmax + c

			# Trap segment entirely below axis.
			if (ymin <= 0.0 && ymax <= 0.0)
			    next

			# Adjust bounds if segment crosses axis in order
			# to exclude anything below the axis.
			if (ymin < 0.0) {
			    ymin = 0.0
			    xmin = - c / m
			}
			if (ymax < 0.0) {
			    ymax = 0.0
			    xmax = - c / m
			}

			# There are four possibilities.

			# Both y above 1.0. Line segment is entirely above
			# square.
			if (ymin >= 1.0 && ymax >= 1.0) {

			    if (negdx)
				ovlap = ovlap + (xmin - xmax)
			    else
				ovlap = ovlap + (xmax - xmin)

			# Both y below 1.0. Segment is entirely within square.
			} else if (ymin <= 1.0 && ymax <= 1.0) {

			    if (negdx)
				ovlap = ovlap + 0.5 * (xmin - xmax) *
				    (ymax + ymin)
			    else
				ovlap = ovlap + 0.5 * (xmax - xmin) *
				    (ymax + ymin)

			# One of each. Segment must cross top of square.
			} else {

			    xtop = (1.0 - c) / m

			    # insert precision check, e.g. possible pixel
			    # overlap ? need to decide what action to take ...

			    if (ymin < 1.0) {
				if (negdx)
				    ovlap = ovlap - (0.5 * (xtop - xmin) *
				        (1.0 + ymin) + (xmax - xtop))
				else
				    ovlap = ovlap + (0.5 * (xtop - xmin) *
				        (1.0 + ymin) + (xmax - xtop))
			     } else {
				if (negdx)
				    ovlap = ovlap - (0.5 * (xmax - xtop) *
				        (1.0 + ymax) + (xtop - xmin))
				else
				    ovlap = ovlap + (0.5 * (xmax - xtop) *
				        (1.0 + ymax) + (xtop - xmin))
			     }

			}
		    }

		    accum = accum + coeff[index+ii] * ovlap
		    waccum = waccum + ovlap
		}
	    }

	    if (waccum == 0.0)
		zfit[i] = badval
	    else
		zfit[i] = accum / waccum
	}
end


# II_BIDRIZ0-- Procedure to evaluate the drizzle interpolant when xfrac and
# yfrac are 0.0. The real array coeff contains the coefficients of the 2D
# interpolant. The procedure assumes that 1 <= x <= nxpix and 1 <= y <= nypix
# and that coeff[1+first_point] = datain[1,1]. Each x and y point is a set of 4
# values describing the vertices of a quadrilateral in the input data. The
# integration routine determines whether a pixel is in, out, on the edge
# of or at a vertex of a polygon. The 4 points describing the corners of
# each quadrilateral integration region must be in order, e.g.  describe
# the vertices of a polygon in either CW or CCW order.
# THIS ROUTINE IS NOT CURRENTLY BEING USED.
 
procedure ii_bidriz0 (coeff, first_point, len_coeff, x, y, zfit, npts, badval)

real	coeff[ARB]	# 1D coefficient array
int	first_point	# offset of first data point
int	len_coeff	# row length of coeff
real	x[ARB]		# array of x values
real	y[ARB]		# array of y values
real	zfit[npts]	# array of interpolated values
int	npts		# number of points to be evaluated
real	badval		# the undefined pixel value

bool	boundary, vertex
int	i, jj, ii, kk, nearax, nearbx, nearay, nearby, ninter
real	accum, waccum, px[5], py[5], lx, ld, u1, u2, u1u2, dx, dy, dd
real	xi, ovlap, xmin, xmax

begin
	do i = 1, npts {

	    # Compute the limits of the integration in x and y.
	    nearax = nint (min (x[4*i-3], x[4*i-2], x[4*i-1], x[4*i]))
	    nearbx = nint (max (x[4*i-3], x[4*i-2], x[4*i-1], x[4*i]))
	    nearay = nint (min (y[4*i-3], y[4*i-2], y[4*i-1], y[4*i]))
	    nearby = nint (max (y[4*i-3], y[4*i-2], y[4*i-1], y[4*i]))

	    # Initialize.
	    accum = 0.0
	    waccum = 0.0

	    # Loop over all pixels which contribute to the integral.
	    do jj = nearay, nearby {
		do ii = nearax, nearbx {

		    # Transform the coordinates relative to a unit
		    # square centered at the origin of the pixel. We
		    # are going to approximate the new pixel area by
		    # a quadilateral. Close the quadrilateral.

		    do kk = 1, 4 {
			px[kk] = x[4*i+kk-4] - ii + 0.5
			py[kk] = y[4*i+kk-4] - jj + 0.5
		    }
		    px[5] = px[1]
		    py[5] = py[1]

		    # Initialize the integration.
		    ovlap = 0.0
		    ninter = 0

		    # Define a line segment which begins at the point x = 0.5
		    # y = 0.5 and runs parallel to the y axis.
		    call alimr (px, 5, xmin, xmax)
		    lx = xmax - xmin
		    ld = 0.5 * lx
		    u1 = -lx * py[1] + ld
		    boundary = false
		    vertex = false
		    do kk = 2, 5 {

		        u2 = -lx * py[kk] + ld
			u1u2 = u1 * u2

			# No intersection.
			if (u1*u2 > 0.0) {
			    ;

			# Intersection with polygon line segment.
			} else if (u1 != 0.0 && u2 != 0.0) {
			    dy = py[kk-1] - py[kk]
			    dx = px[kk-1] - px[kk]
			    dd = px[kk-1] * py[kk] - py[kk-1] * px[kk]
			    xi = (dx * ld - lx * dd) / (dy * lx)
			    if (xi > 0.5)
			        ninter = ninter + 1
			    if (xi == 0.5)
				boundary = true

			# Collinearity.
			} else if (u1 == 0.0 && u2 == 0.0) {
			    xmin = min (px[kk-1], px[kk])
			    xmax = max (px[kk-1], px[kk])
			    if (xmin == 0.5 || xmax == 0.5)
				vertex = true
			    else if (xmin < 0.5 && xmax > 0.5)
				boundary = true

			# Vertex.
			} else if (u1 != 0.0) {
			    if (px[kk] == 0.5)
				vertex = true
			}

			u1 = u2
		    }

		    if (vertex)
			ovlap = 0.25
		    else if (boundary)
			ovlap = 0.5
		    else if (mod (ninter, 2) == 0)
			ovlap = 0.0
		    else
		        ovlap = 1.0
		    waccum = waccum + ovlap
		    accum = accum + ovlap *
		        coeff[first_point+(jj-1)*len_coeff+ii]
		}
	    }

	    if (waccum == 0.0)
		zfit[i] = badval
	    else
		zfit[i] = accum / waccum
	}

end
