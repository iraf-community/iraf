# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CV_EVCHEB -- Procedure to evaluate a Chebyshev polynomial assuming that
# the coefficients have been calculated. 

procedure cv_evcheb (coeff, x, yfit, npts, order, k1, k2)

real	coeff[ARB]		# EV array of coefficients
real	x[npts]			# x values of points to be evaluated
real	yfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	order			# order of the polynomial, 1 = constant
real	k1, k2			# normalizing constants

int	i
pointer	sx, pn, pnm1, pnm2
pointer sp
real	c1, c2

begin
	# fit a constant
	call amovkr (coeff[1], yfit, npts)
	if (order == 1)
	    return

	# fit a linear function
	c1 = k2 * coeff[2]
	c2 = c1 * k1 + coeff[1]
	call altmr (x, yfit, npts, c1, c2)
	if (order == 2)
	    return

	# allocate temporary space
	call smark (sp)
	call salloc (sx, npts, TY_REAL)
	call salloc (pn, npts, TY_REAL)
	call salloc (pnm1, npts, TY_REAL)
	call salloc (pnm2, npts, TY_REAL)

	# a higher order polynomial
	call amovkr (1., Memr[pnm2], npts)
	call altar (x, Memr[sx], npts, k1, k2)
	call amovr (Memr[sx], Memr[pnm1], npts)
	call amulkr (Memr[sx], 2., Memr[sx], npts)
	do i = 3, order {
	    call amulr (Memr[sx], Memr[pnm1], Memr[pn], npts)
	    call asubr (Memr[pn], Memr[pnm2], Memr[pn], npts)
	    if (i < order) {
	        call amovr (Memr[pnm1], Memr[pnm2], npts)
	        call amovr (Memr[pn], Memr[pnm1], npts)
	    }
	    call amulkr (Memr[pn], coeff[i], Memr[pn], npts)
	    call aaddr (yfit, Memr[pn], yfit, npts)
	}

	# free temporary space
	call sfree (sp)

end


# CV_EVLEG -- Procedure to evaluate a Legendre polynomial assuming that
# the coefficients have been calculated. 

procedure cv_evleg (coeff, x, yfit, npts, order, k1, k2)

real	coeff[ARB]		# EV array of coefficients
real	x[npts]			# x values of points to be evaluated
real	yfit[npts]		# the fitted points
int	npts			# number of data points
int	order			# order of the polynomial, 1 = constant
real	k1, k2			# normalizing constants

int	i
pointer	sx, pn, pnm1, pnm2
pointer	sp
real	ri, ri1, ri2

begin
	# fit a constant
	call amovkr (coeff[1], yfit, npts)
	if (order == 1)
	    return

	# fit a linear function
	ri1 = k2 * coeff[2]
	ri2 = ri1 * k1 + coeff[1]
	call altmr (x, yfit, npts, ri1, ri2)
	if (order == 2)
	    return

	# allocate temporary space
	call smark (sp)
	call salloc (sx, npts, TY_REAL)
	call salloc (pn, npts, TY_REAL)
	call salloc (pnm1, npts, TY_REAL)
	call salloc (pnm2, npts, TY_REAL)

	# a higher order polynomial
	call amovkr (1., Memr[pnm2], npts)
	call altar (x, Memr[sx], npts, k1, k2)
	call amovr (Memr[sx], Memr[pnm1], npts)
	do i = 3, order {
	    ri = i
	    ri1 = (2. * ri - 3.) / (ri - 1.)
	    ri2 = - (ri - 2.) / (ri - 1.)
	    call amulr (Memr[sx], Memr[pnm1], Memr[pn], npts)
	    call awsur (Memr[pn], Memr[pnm2], Memr[pn], npts, ri1, ri2)
	    if (i < order) {
	        call amovr (Memr[pnm1], Memr[pnm2], npts)
	        call amovr (Memr[pn], Memr[pnm1], npts)
	    }
	    call amulkr (Memr[pn], coeff[i], Memr[pn], npts)
	    call aaddr (yfit, Memr[pn], yfit, npts)
	}

	# free temporary space
	call sfree (sp)

end


# CV_EVSPLINE1 -- Procedure to evaluate a piecewise linear spline function
# assuming that the coefficients have been calculated.

procedure cv_evspline1 (coeff, x, yfit, npts, npieces, k1, k2)

real	coeff[ARB]		# array of coefficients
real	x[npts]			# array of x values
real	yfit[npts]		# array of fitted values
int	npts			# number of data points
int	npieces			# number of fitted points minus 1
real	k1, k2			# normalizing constants

int	j
pointer sx, tx, index
pointer	sp

begin
	# allocate the required space
	call smark (sp)
	call salloc (sx, npts, TY_REAL)
	call salloc (tx, npts, TY_REAL)
	call salloc (index, npts, TY_INT)

	# calculate the index of the first non-zero coefficient
	# for each point
	call altar (x, Memr[sx], npts, k1, k2)
	call achtri (Memr[sx], Memi[index], npts)
	call aminki (Memi[index], npieces, Memi[index], npts)

	# transform sx to range 0 to 1
	do j = 1, npts {
	    Memr[sx+j-1] = Memr[sx+j-1] - Memi[index+j-1]
	    Memr[tx+j-1] = 1. - Memr[sx+j-1]
	}

    	# calculate yfit using the two non-zero basis function
	call aclrr (yfit, npts)
	do j = 1, npts
	    yfit[j] = Memr[tx+j-1] * coeff[1+Memi[index+j-1]] +
		      Memr[sx+j-1] * coeff[2+Memi[index+j-1]]

        # free space
	call sfree (sp)

end


# CV_EVSPLINE3 -- Procedure to evaluate the cubic spline assuming that
# the coefficients of the fit are known.

procedure cv_evspline3 (coeff, x, yfit, npts, npieces, k1, k2)

real	coeff[ARB]	# array of coeffcients
real	x[npts]		# array of x values
real	yfit[npts]	# array of fitted values
int	npts		# number of data points
int	npieces		# number of polynomial pieces
real	k1, k2		# normalizing constants

int	i, j
pointer	sx, tx, temp, index, sp

begin

	# allocate the required space
	call smark (sp)
        call salloc (sx, npts, TY_REAL)
	call salloc (tx, npts, TY_REAL)
	call salloc (temp, npts, TY_REAL)
	call salloc (index, npts, TY_INT)

	# calculate to which coefficients the x values contribute to
        call altar (x, Memr[sx], npts, k1, k2)
        call achtri (Memr[sx], Memi[index], npts)
        call aminki (Memi[index], npieces, Memi[index], npts)

        # transform sx to range 0 to 1
	do j = 1, npts {
	    Memr[sx+j-1] = Memr[sx+j-1] - Memi[index+j-1]
	    Memr[tx+j-1] = 1. - Memr[sx+j-1]
	}

        # calculate yfit using the four non-zero basis function
	call aclrr (yfit, npts)
        do i = 1, 4 {

	    switch (i) {
	    case 1:
		call apowkr (Memr[tx], 3, Memr[temp], npts)
	    case 2:
		do j = 1, npts {
		    Memr[temp+j-1] = 1. + Memr[tx+j-1] * (3. + Memr[tx+j-1] *
			(3. - 3. * Memr[tx+j-1]))
		}
	    case 3:
		do j = 1, npts {
		    Memr[temp+j-1] = 1. + Memr[sx+j-1] * (3. + Memr[sx+j-1] *
			(3. - 3. * Memr[sx+j-1]))
		}
	    case 4:
		call apowkr (Memr[sx], 3, Memr[temp], npts)
	    }

	    do j = 1, npts
		Memr[temp+j-1] = Memr[temp+j-1] * coeff[i+Memi[index+j-1]]
	    call aaddr (yfit, Memr[temp], yfit, npts)
	}

	# free space
	call sfree (sp)
end
