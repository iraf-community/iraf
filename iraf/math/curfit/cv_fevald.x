# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CV_EVCHEB -- Procedure to evaluate a Chebyshev polynomial assuming that
# the coefficients have been calculated. 

procedure dcv_evcheb (coeff, x, yfit, npts, order, k1, k2)

double	coeff[ARB]		# 1D array of coefficients
double	x[npts]			# x values of points to be evaluated
double	yfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	order			# order of the polynomial, 1 = constant
double	k1, k2			# normalizing constants

int	i
pointer	sx, pn, pnm1, pnm2
pointer sp
double	c1, c2

begin
	# fit a constant
	if (order == 1) {
	    call amovkd (coeff[1], yfit, npts)
	    return
	}

	# fit a linear function
	c1 = k2 * coeff[2]
	c2 = c1 * k1 + coeff[1]
	call altmd (x, yfit, npts, c1, c2)
	if (order == 2)
	    return

	# allocate temporary space
	call smark (sp)
	call salloc (sx, npts, TY_DOUBLE)
	call salloc (pn, npts, TY_DOUBLE)
	call salloc (pnm1, npts, TY_DOUBLE)
	call salloc (pnm2, npts, TY_DOUBLE)

	# a higher order polynomial
	call amovkd (double(1.0), Memd[pnm2], npts)
	call altad (x, Memd[sx], npts, k1, k2)
	call amovd (Memd[sx], Memd[pnm1], npts)
	call amulkd (Memd[sx], double(2.0), Memd[sx], npts)
	do i = 3, order {
	    call amuld (Memd[sx], Memd[pnm1], Memd[pn], npts)
	    call asubd (Memd[pn], Memd[pnm2], Memd[pn], npts)
	    if (i < order) {
	        call amovd (Memd[pnm1], Memd[pnm2], npts)
	        call amovd (Memd[pn], Memd[pnm1], npts)
	    }
	    call amulkd (Memd[pn], coeff[i], Memd[pn], npts)
	    call aaddd (yfit, Memd[pn], yfit, npts)
	}

	# free temporary space
	call sfree (sp)

end

# CV_EVLEG -- Procedure to evaluate a Legendre polynomial assuming that
# the coefficients have been calculated. 

procedure dcv_evleg (coeff, x, yfit, npts, order, k1, k2)

double	coeff[ARB]		# 1D array of coefficients
double	x[npts]			# x values of points to be evaluated
double	yfit[npts]		# the fitted points
int	npts			# number of data points
int	order			# order of the polynomial, 1 = constant
double	k1, k2			# normalizing constants

int	i
pointer	sx, pn, pnm1, pnm2
pointer	sp
double	ri, ri1, ri2

begin

	# fit a constant
	if (order == 1) {
	    call amovkd (coeff[1], yfit, npts)
	    return
	}

	# fit a linear function
	ri1 = k2 * coeff[2]
	ri2 = ri1 * k1 + coeff[1]
	call altmd (x, yfit, npts, ri1, ri2)
	if (order == 2)
	    return

	# allocate temporary space
	call smark (sp)
	call salloc (sx, npts, TY_DOUBLE)
	call salloc (pn, npts, TY_DOUBLE)
	call salloc (pnm1, npts, TY_DOUBLE)
	call salloc (pnm2, npts, TY_DOUBLE)

	# a higher order polynomial
	call amovkd (double(1.0), Memd[pnm2], npts)
	call altad (x, Memd[sx], npts, k1, k2)
	call amovd (Memd[sx], Memd[pnm1], npts)
	do i = 3, order {
	    ri = i
	    ri1 = (double(2.0) * ri - double(3.0)) / (ri - double(1.0))
	    ri2 = - (ri - double(2.0)) / (ri - double(1.0))
	    call amuld (Memd[sx], Memd[pnm1], Memd[pn], npts)
	    call awsud (Memd[pn], Memd[pnm2], Memd[pn], npts, ri1, ri2)
	    if (i < order) {
	        call amovd (Memd[pnm1], Memd[pnm2], npts)
	        call amovd (Memd[pn], Memd[pnm1], npts)
	    }
	    call amulkd (Memd[pn], coeff[i], Memd[pn], npts)
	    call aaddd (yfit, Memd[pn], yfit, npts)
	}

	# free temporary space
	call sfree (sp)

end

# CV_EVSPLINE1 -- Procedure to evaluate a piecewise linear spline function
# assuming that the coefficients have been calculated.

procedure dcv_evspline1 (coeff, x, yfit, npts, npieces, k1, k2)

double	coeff[ARB]		# array of coefficients
double	x[npts]			# array of x values
double	yfit[npts]		# array of fitted values
int	npts			# number of data points
int	npieces			# number of fitted points minus 1
double	k1, k2			# normalizing constants

int	j
pointer sx, tx, azindex, aindex, index
pointer	sp

begin

	# allocate the required space
	call smark (sp)
	call salloc (sx, npts, TY_DOUBLE)
	call salloc (tx, npts, TY_DOUBLE)
	call salloc (index, npts, TY_INT)

	# calculate the index of the first non-zero coefficient
	# for each point
	call altad (x, Memd[sx], npts, k1, k2)
	call achtdi (Memd[sx], Memi[index], npts)
	call aminki (Memi[index], npieces, Memi[index], npts)

	# transform sx to range 0 to 1
	azindex = sx - 1
	do j = 1, npts {
	    aindex = azindex + j
	    Memd[aindex] = max (double(0.0), min (double(1.0), Memd[aindex] -
	        Memi[index+j-1]))
	    Memd[tx+j-1] = max (double(0.0), min (double(1.0), double(1.0) -
	        Memd[aindex]))
	}

    	# calculate yfit using the two non-zero basis function
	do j = 1, npts
	    yfit[j] = Memd[tx+j-1] * coeff[1+Memi[index+j-1]] +
		      Memd[sx+j-1] * coeff[2+Memi[index+j-1]]

        # free space
	call sfree (sp)

end

# CV_EVSPLINE3 -- Procedure to evaluate the cubic spline assuming that
# the coefficients of the fit are known.

procedure dcv_evspline3 (coeff, x, yfit, npts, npieces, k1, k2)

double	coeff[ARB]	# array of coeffcients
double	x[npts]		# array of x values
double	yfit[npts]	# array of fitted values
int	npts		# number of data points
int	npieces		# number of polynomial pieces
double	k1, k2		# normalizing constants

int	i, j
pointer	sx, tx, temp, index, sp

begin

	# allocate the required space
	call smark (sp)
        call salloc (sx, npts, TY_DOUBLE)
	call salloc (tx, npts, TY_DOUBLE)
	call salloc (temp, npts, TY_DOUBLE)
	call salloc (index, npts, TY_INT)

	# calculate to which coefficients the x values contribute to
        call altad (x, Memd[sx], npts, k1, k2)
        call achtdi (Memd[sx], Memi[index], npts)
        call aminki (Memi[index], npieces, Memi[index], npts)

        # transform sx to range 0 to 1
	do j = 1, npts {
	    Memd[sx+j-1] = max (double(0.0), min (double(1.0), Memd[sx+j-1] -
	        Memi[index+j-1]))
	    Memd[tx+j-1] = max (double(0.0), min (double(1.0), double(1.0) -
	        Memd[sx+j-1]))
	}

        # calculate yfit using the four non-zero basis function
	call aclrd (yfit, npts)
        do i = 1, 4 {

	    switch (i) {
	    case 1:
		call apowkd (Memd[tx], 3, Memd[temp], npts)
	    case 2:
		do j = 1, npts {
		    Memd[temp+j-1] = double(1.0) + Memd[tx+j-1] *
		        (double(3.0) + Memd[tx+j-1] * (double(3.0) -
			double(3.0) * Memd[tx+j-1]))
		}
	    case 3:
		do j = 1, npts {
		    Memd[temp+j-1] = double(1.0) + Memd[sx+j-1] *
		        (double(3.0) + Memd[sx+j-1] * (double(3.0) -
			double(3.0) * Memd[sx+j-1]))
		}
	    case 4:
		call apowkd (Memd[sx], 3, Memd[temp], npts)
	    }

	    do j = 1, npts
		Memd[temp+j-1] = Memd[temp+j-1] * coeff[i+Memi[index+j-1]]
	    call aaddd (yfit, Memd[temp], yfit, npts)
	}

	# free space
	call sfree (sp)

end
