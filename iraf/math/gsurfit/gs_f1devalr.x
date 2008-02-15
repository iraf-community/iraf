# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GS_1DEVPOLY -- Procedure to evaulate a 1D polynomial

procedure rgs_1devpoly (coeff, x, yfit, npts, order, k1, k2)

real	coeff[ARB]		# EV array of coefficients
real	x[npts]			# x values of points to be evaluated
real	yfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	order			# order of the polynomial, 1 = constant
real	k1, k2			# normalizing constants

int	i
pointer	sp, temp

begin
	# fit a constant
	call amovkr (coeff[1], yfit, npts)
	if (order == 1)
	    return

	# fit a linear function
	call altmr (x, yfit, npts, coeff[2], coeff[1])
	if (order == 2)
	    return

	call smark (sp)
	call salloc (temp, npts, TY_REAL)

	# accumulate the output vector
	call amovr (x, Memr[temp], npts)
	do i = 3, order {
	    call amulr (Memr[temp], x, Memr[temp], npts)
	    call awsur (yfit, Memr[temp], yfit, npts, 1.0, coeff[i])
	}

	call sfree (sp)

end

# GS_1DEVCHEB -- Procedure to evaluate a Chebyshev polynomial assuming that
# the coefficients have been calculated. 

procedure rgs_1devcheb (coeff, x, yfit, npts, order, k1, k2)

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
	call amulkr (Memr[sx], 2.0, Memr[sx], npts)
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


# GS_1DEVLEG -- Procedure to evaluate a Legendre polynomial assuming that
# the coefficients have been calculated. 

procedure rgs_1devleg (coeff, x, yfit, npts, order, k1, k2)

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
