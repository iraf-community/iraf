# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# II_GRNEAREST -- Procedure to evaluate the nearest neighbour interpolant on
# a rectangular grid. The procedure assumes that 1 <= x <= nxpix and
# that 1 <= y <= nypix. The x and y vectors must be sorted in increasing
# value of x and y such that x[i] < x[i+1] and y[i] < y[i+1]. The routine
# outputs a grid of nxpix by nypix points using the coeff array where
# coeff[1+first_point] = datain[1,1]

procedure ii_grnearest (coeff, first_point, len_coeff, x, y, zfit, nxpts,
		        nypts, len_zfit)

real	coeff[ARB]		# 1D coefficient array
int	first_point		# offset of first data point
int	len_coeff		# row length of coeff 
real	x[nxpts]		# array of x values
real	y[nypts]		# array of y values
real	zfit[len_zfit,ARB]	# array of interpolatedvalues
int	nxpts			# number of x values
int	nypts			# number of y values
int	len_zfit		# row length of zfit

int	ny
int	index
int	i, j
pointer	sp, nx

errchk	smark, salloc, sfree

begin
	call smark (sp)
	call salloc (nx, nxpts, TY_INT)

	# calculate the nearest x
	do i = 1, nxpts
	    Memi[nx+i-1] = x[i] + 0.5

	# loop over the rows
	do j = 1, nypts {

	    # calculate pointer to the ny-th row of data
	    ny = y[j] + 0.5
	    index = first_point + (ny - 1) * len_coeff

	    # loop over the columns
	    do i = 1, nxpts
		zfit[i,j] = coeff[index + Memi[nx+i-1]]
	}

	call sfree (sp)
end


# II_GRLINEAR -- Procedure to evaluate the bilinear interpolant
# on a rectangular grid. The procedure assumes that 1 <= x <= nxpix and that
# 1 <= y <= nypix. The x and y vectors are assumed to be sorted in increasing
# order of x and y such that x[i] < x[i+1] and y[i] < y[i+1]. The routine 
# produces a grid of nxpix * nypix evaluated points using the coeff array
# where coeff[1+first_point] = datain[1,1].

procedure ii_grlinear (coeff, first_point, len_coeff, x, y, zfit, nxpts,
		       nypts, len_zfit)

real	coeff[ARB]		# 1D array of coefficients
int	first_point		# offset of first data point
int	len_coeff		# row length of coeff
real	x[nxpts]		# array of x values
real	y[nypts]		# array of y values
real	zfit[len_zfit,ARB]	# array of interpolated values
int	nxpts			# number of x values
int	nypts			# number of y values
int	len_zfit		# row length of zfit

int	i, j, ny
int	nymin, nymax, nylines
int	row_index, xindex
pointer	sp, nx, sx, tx, work, lbuf1, lbuf2
real	sy, ty

errchk	smark, salloc, sfree

begin
	# calculate the x and y limits
	nymin = y[1]
	nymax = int (y[nypts]) + 1
	nylines = nymax - nymin + 1

	# allocate storage for work array
	call smark (sp)
	call salloc (nx, nxpts, TY_INT)
	call salloc (sx, nxpts, TY_REAL)
	call salloc (tx, nxpts, TY_REAL)
	call salloc (work, nxpts * nylines, TY_REAL)

	# initialize
	call achtri (x, Memi[nx], nxpts)
	do i = 1, nxpts {
	    Memr[sx+i-1] = x[i] - Memi[nx+i-1]
	    Memr[tx+i-1] = 1. - Memr[sx+i-1]
	}

	# for each value of y interpolate in x and store in work array
	lbuf1 = work
	do j = 1, nylines {

	    # define pointer to appropriate row
	    row_index = first_point + (j + nymin - 2) * len_coeff

	    # interpolate in x at each y
	    do i = 1, nxpts {
		xindex = row_index + Memi[nx+i-1]
		Memr[lbuf1+i-1] = Memr[tx+i-1] * coeff[xindex] +
		    Memr[sx+i-1] * coeff[xindex+1]
	    }

	    lbuf1 = lbuf1 + nxpts
	}

	# at each x interpolate in y and store in temporary work array
	do j = 1, nypts {

	    ny = y[j]
	    sy = y[j] - ny
	    ty = 1. - sy

	    lbuf1 = work + nxpts * (ny - nymin)
	    lbuf2 = lbuf1 + nxpts

	    call awsur (Memr[lbuf1], Memr[lbuf2], zfit[1,j], nxpts,
		ty, sy)

	}

	# deallocate work space
	call sfree (sp)
end


# II_GRPOLY3 -- Procedure to evaluate the bicubic polynomial interpolant
# on a rectangular grid. The points to be evaluated are assumed
# to lie in the range 1 <= x <= nxpix and 1 <= y <= nypix. The x and y vectors
# are assumed to be sorted in increasing order of x and y such that
# x[i] < x[i+1] and y[i] < y[i+1]. The interpolation is done using
# Everett's central difference formula and separation of variables
# and assuming that coeff[1+first_point] = datain[1,1].

procedure ii_grpoly3 (coeff, first_point, len_coeff, x, y, zfit, nxpts, nypts,
		      len_zfit)

real	coeff[ARB]		# 1D array of coefficients
int	first_point		# offset of first data point
int	len_coeff		# length of row of coeffcient
real	x[nxpts]		# array of x values
real	y[nypts]		# array of y values
real	zfit[len_zfit,ARB]	# array of interpolatedvalues
int	nxpts			# number of x points
int	nypts			# number of y points
int	len_zfit		# row length of zfit

int	nymin, nymax, nylines
int	nxold, nyold
int	row_index, xindex
int	i, j, ny
pointer	sp, nx, sx, sx2m1, tx, tx2m1, work
pointer	lbuf, lbufp1, lbufp2, lbufm1
real	cd20x, cd21x, cd20y, cd21y
real	sy, ty, sy2m1, ty2m1

errchk smark, salloc, sfree

begin
	# find y limits
	nymin = int (y[1]) - 1
	nymax = int (y[nypts]) + 2
	nylines = nymax - nymin + 1

	# allocate work space
	call smark (sp)
	call salloc (nx, nxpts, TY_INT)
	call salloc (sx, nxpts, TY_REAL)
	call salloc (sx2m1, nxpts, TY_REAL)
	call salloc (tx, nxpts, TY_REAL)
	call salloc (tx2m1, nxpts, TY_REAL) 
	call salloc (work, nxpts * nylines, TY_REAL)

	# initialize
	call achtri (x, Memi[nx], nxpts)
	do i = 1, nxpts {
	    Memr[sx+i-1] = x[i] - Memi[nx+i-1]
	    Memr[sx2m1+i-1] = Memr[sx+i-1] * Memr[sx+i-1] - 1.
	    Memr[tx+i-1] = 1. - Memr[sx+i-1]
	    Memr[tx2m1+i-1] = Memr[tx+i-1] * Memr[tx+i-1] - 1.
	}

	# for each value of y interpolate in x
	lbuf = work
	do j = 1, nylines {

	    # calculate pointer to a row
	    row_index = first_point + (j + nymin - 2) * len_coeff

	    # interpolate in x at each y
	    nxold = -1
	    do i = 1, nxpts {

		xindex= row_index + Memi[nx+i-1]

		if (Memi[nx+i-1] != nxold) {
		    cd20x = 1./6. * (coeff[xindex+1] - 2. * coeff[xindex] +
		           coeff[xindex-1])
		    cd21x = 1./6. * (coeff[xindex+2] - 2. * coeff[xindex+1] +
			    coeff[xindex])
		}

		Memr[lbuf+i-1] = Memr[sx+i-1] * (coeff[xindex+1] +
		                Memr[sx2m1+i-1] * cd21x) +
			        Memr[tx+i-1] * (coeff[xindex] +
				Memr[tx2m1+i-1] * cd20x)

		nxold = Memi[nx+i-1]
	    }

	    lbuf = lbuf + nxpts
	}

	# interpolate in y at each x
	nyold = -1
	do j = 1, nypts {

	    ny = y[j]
	    sy = y[j] - ny
	    ty = 1. - sy
	    sy2m1 = sy ** 2 - 1.
	    ty2m1 = ty ** 2 - 1.

	    lbuf = work + nxpts * (ny - nymin) 
	    lbufm1 = lbuf - nxpts
	    lbufp1 = lbuf + nxpts
	    lbufp2 = lbufp1 + nxpts

	    do i = 1, nxpts {

		# calculate central differences in y
		if (nyold != ny) {
		    cd20y = 1./6. * (Memr[lbufp1+i-1] - 2. * Memr[lbuf+i-1] +
			    Memr[lbufm1+i-1])
		    cd21y = 1./6. * (Memr[lbufp2+i-1] - 2. *
		    	    Memr[lbufp1+i-1] + Memr[lbuf+i-1])
		}

		# interpolate in y
		zfit[i,j] =  sy * (Memr[lbufp1+i-1] + sy2m1 * cd21y) +
			     ty * (Memr[lbuf+i-1] + ty2m1 * cd20y)

	    }

	    nyold = ny
	}

	# free work space
	call sfree (sp)
end


# II_GRPOLY5 -- Procedure to evaluate the biquintic polynomial interpolant
# on a rectangular grid. The routine assumes that 1 <= x <= nxpix and
# 1 <= y <= nypix.  The vectors x and y are assumed to be sorted in
# increasing order such that x[i] < x[i+1] and y[i] < y[i+1]. The
# interpolation is done using Everett's interpolation formula and
# separation of variables and assuming that coeff[1+first_point] =
# datain[1,1].

procedure ii_grpoly5 (coeff, first_point, len_coeff, x, y, zfit, nxpts,
		      nypts, len_zfit)

real	coeff[ARB]		# 1D array of coefficients
int	first_point		# offset of first data point
int	len_coeff		# row length of coeff
real	x[nxpts]		# array of x values
real	y[nypts]		# array of y values
real	zfit[len_zfit,ARB]	# array of fitted values
int	nxpts			# number of x points
int	nypts			# number of y points
int	len_zfit		# row length of zfit

int	nymax, nymin, nylines, nxold, nyold
int	row_index, xindex
int	i, j, ny
pointer	sp, nx, sx, tx, sx2m1, sx2m4, tx2m1, tx2m4, work
pointer	lbuf, lbufp1, lbufp2, lbufp3, lbufm1, lbufm2
real	cd20x, cd21x, cd40x, cd41x
real	cd20y, cd21y, cd40y, cd41y
real	sy, ty, sy2m1, sy2m4, ty2m1, ty2m4

errchk	smark, salloc, sfree

begin
	# find the y limits
	nymin = int (y[1]) - 2
	nymax = int (y[nypts]) + 3
	nylines = nymax - nymin + 1

	# allocate space
	call smark (sp)
	call salloc (nx, nxpts, TY_INT)
	call salloc (sx, nxpts, TY_REAL)
	call salloc (sx2m1, nxpts, TY_REAL)
	call salloc (sx2m4, nxpts, TY_REAL)
	call salloc (tx, nxpts, TY_REAL)
	call salloc (tx2m1, nxpts, TY_REAL)
	call salloc (tx2m4, nxpts, TY_REAL)
	call salloc (work, nxpts * nylines, TY_REAL)

	# intialize
	call achtri (x, Memi[nx], nxpts)
	do i = 1, nxpts {
	    Memr[sx+i-1] = x[i] - Memi[nx+i-1]
	    Memr[sx2m1+i-1] = Memr[sx+i-1] ** 2 - 1.
	    Memr[sx2m4+i-1] = Memr[sx2m1+i-1] - 3.
	    Memr[tx+i-1] = 1. - Memr[sx+i-1]
	    Memr[tx2m1+i-1] = Memr[tx+i-1] ** 2 - 1.
	    Memr[tx2m4+i-1] = Memr[tx2m1+i-1] - 3.
	}


	# for each value of y interpolate in x
	lbuf = work
	do j = 1, nylines {

	    # calculate pointer to a row
	    row_index = first_point + (j + nymin - 2) * len_coeff

	    # interpolate in x at each y
	    nxold = -1
	    do i = 1, nxpts {

		xindex = row_index + Memi[nx+i-1]

		if (Memi[nx+i-1] != nxold) {
		    cd20x = 1./6. * (coeff[xindex+1] - 2. * coeff[xindex] +
		            coeff[xindex-1])
		    cd21x = 1./6. * (coeff[xindex+2] - 2. * coeff[xindex+1] +
			    coeff[xindex])
		    cd40x = 1./120. * (coeff[xindex-2] - 4. * coeff[xindex-1] +
			    6. * coeff[xindex] - 4. * coeff[xindex+1] +
			    coeff[xindex+2])
		    cd41x = 1./120. * (coeff[xindex-1] - 4. * coeff[xindex] +
			    6. * coeff[xindex+1] - 4. * coeff[xindex+2] +
			    coeff[xindex+3])
		}

		Memr[lbuf+i-1] = Memr[sx+i-1] * (coeff[xindex+1] +
				 Memr[sx2m1+i-1] * (cd21x +
				 Memr[sx2m4+i-1] * cd41x)) +
			         Memr[tx+i-1] * (coeff[xindex] +
				 Memr[tx2m1+i-1] * (cd20x +
				 Memr[tx2m4+i-1] * cd40x))


		nxold = Memi[nx+i-1]

	    }

	    lbuf = lbuf + nxpts
	}

	# at each x interpolate in y
	nyold = -1
	do j = 1, nypts {

	    ny = y[j]
	    sy = y[j] - ny
	    sy2m1 = sy ** 2 - 1.
	    sy2m4 = sy2m1 - 3.
	    ty = 1. - sy
	    ty2m1 = ty ** 2 - 1.
	    ty2m4 = ty2m1 - 3.

	    lbuf = work + nxpts * (ny - nymin)
	    lbufp1 = lbuf + nxpts
	    lbufp2 = lbufp1 + nxpts
	    lbufp3 = lbufp2 + nxpts
	    lbufm1 = lbuf - nxpts
	    lbufm2 = lbufm1 - nxpts

	    do i = 1, nxpts {

		# calculate central differences
		if (nyold != ny) {
		    cd20y = 1./6. * (Memr[lbufp1+i-1] - 2. * Memr[lbuf+i-1] +
			    Memr[lbufm1+i-1])
		    cd21y = 1./6. * (Memr[lbufp2+i-1] - 2. *
		    	    Memr[lbufp1+i-1] + Memr[lbuf+i-1])
		    cd40y = 1./120. * (Memr[lbufm2+i-1] -
		    	    4. * Memr[lbufm1+i-1] + 6. * Memr[lbuf+i-1] -
			    4. * Memr[lbufp1+i-1] + Memr[lbufp2+i-1])
		    cd41y = 1./120. * (Memr[lbufm1+i-1] - 4. * 
		    	    Memr[lbuf+i-1] + 6. * Memr[lbufp1+i-1] - 4. *
			    Memr[lbufp2+i-1] + Memr[lbufp3+i-1])
		}

		# interpolate in y
		zfit[i,j] = sy * (Memr[lbufp1+i-1] + sy2m1 *
			    (cd21y + sy2m4 * cd41y)) +
			    ty * (Memr[lbuf+i-1] + ty2m1 *
			    (cd20y + ty2m4 * cd40y))

	    }

	    nyold = ny
	}

	# release work space
	call sfree (sp)

end


# II_GRSPLINE3 -- Procedure to evaluate the bicubic spline interpolant
# on a rectangular grid.  The program assumes that 1 <= x <= nxpix and
# 1 <= y <= nypix. The routine assumes that x and y vectors are sorted
# such that x[i] < x[i+1] and y[i] < y[i+1]. The interpolant is evaluated
# by calculating the polynomial coefficients in x and y.

procedure ii_grspline3 (coeff, first_point, len_coeff, x, y, zfit, nxpts,
			nypts, len_zfit)

real	coeff[ARB]		# 1D array of coefficients
int	first_point		# offset of first data point
int	len_coeff		# row length of coeff
real	x[nxpts]		# array of x values
real	y[nypts]		# array of y values
real	zfit[len_zfit,ARB]	# array of interpolated values
int	nxpts			# number of x values
int	nypts			# number of y values
int	len_zfit		# row length of zfit

int	ny, nymin, nymax, nylines
int	row_index, xindex
int	i, j
pointer	sp, nx, sx, tx, sx3, tx3, work, lbuf, lbufp1, lbufp2, lbufm1
real	sy, ty, ty3, sy3

errchk	smark, salloc, sfree

begin
	# find the y limits
	nymin = int (y[1]) - 1
	nymax = int (y[nypts]) + 2
	nylines = nymax - nymin + 1

	# allocate space for work array
	call smark (sp)
	call salloc (nx, nxpts, TY_INT)
	call salloc (sx, nxpts, TY_REAL)
	call salloc (sx3, nxpts, TY_REAL)
	call salloc (tx, nxpts, TY_REAL)
	call salloc (tx3, nxpts, TY_REAL)
	call salloc (work, nylines * nxpts, TY_REAL)

	# intialize
	call achtri (x, Memi[nx], nxpts)
	do j = 1, nxpts {
	    Memr[sx+j-1] = x[j] - Memi[nx+j-1]
	    Memr[tx+j-1] = 1. - Memr[sx+j-1]
	}
	call apowkr (Memr[sx], 3, Memr[sx3], nxpts)
	call apowkr (Memr[tx], 3, Memr[tx3], nxpts)
	do j = 1, nxpts {
	    Memr[sx+j-1] = 1. + Memr[sx+j-1] * (3. + Memr[sx+j-1] *
			   (3. - 3. * Memr[sx+j-1])) 
	    Memr[tx+j-1] = 1. + Memr[tx+j-1] * (3. + Memr[tx+j-1] *
			   (3. -  3. * Memr[tx+j-1])) 
	}

	# interpolate in x for each y
	lbuf = work
	do i = 1, nylines {

	    # find appropriate row
	    row_index = first_point + (i + nymin - 2) * len_coeff

	    # x interpolation
	    do j = 1, nxpts {

		xindex = row_index + Memi[nx+j-1]
		Memr[lbuf+j-1] = Memr[tx3+j-1] * coeff[xindex-1] +
				 Memr[tx+j-1] * coeff[xindex] +
				 Memr[sx+j-1] * coeff[xindex+1] +
				 Memr[sx3+j-1] * coeff[xindex+2]
	    }
	    lbuf = lbuf + nxpts
	}

	# interpolate in y
	do i = 1, nypts {

	    ny = y[i]
	    sy = y[i] - ny
	    ty = 1. - sy
	    sy3 = sy ** 3
	    ty3 = ty ** 3
	    sy = 1. + sy * (3. + sy * (3. - 3. * sy))
	    ty = 1. + ty * (3. + ty * (3. - 3. * ty))

	    lbuf = work + nxpts * (ny - nymin)
	    lbufp1 = lbuf + nxpts
	    lbufp2 = lbufp1 + nxpts
	    lbufm1 = lbuf - nxpts

	    do j = 1, nxpts
		zfit[j,i] = ty3 * Memr[lbufm1+j-1] + ty * Memr[lbuf+j-1] +
			    sy * Memr[lbufp1+j-1] + sy3 * Memr[lbufp2+j-1]
	}

	# release working space
	call sfree (sp)
end
