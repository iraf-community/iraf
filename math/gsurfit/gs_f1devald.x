# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GS_1DEVPOLY -- Procedure to evaulate a 1D polynomial

procedure dgs_1devpoly (coeff, x, yfit, npts, order, k1, k2)

double	coeff[ARB]		# EV array of coefficients
double	x[npts]			# x values of points to be evaluated
double	yfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	order			# order of the polynomial, 1 = constant
double	k1, k2			# normalizing constants

int	i
pointer	sp, temp

begin
	# fit a constant
	call amovkd (coeff[1], yfit, npts)
	if (order == 1)
	    return

	# fit a linear function
	call altmd (x, yfit, npts, coeff[2], coeff[1])
	if (order == 2)
	    return

	call smark (sp)
	call salloc (temp, npts, TY_DOUBLE)

	# accumulate the output vector
	call amovd (x, Memd[temp], npts)
	do i = 3, order {
	    call amuld (Memd[temp], x, Memd[temp], npts)
	    call awsud (yfit, Memd[temp], yfit, npts, 1.0d0, coeff[i])
	}

	call sfree (sp)

end

# GS_1DEVCHEB -- Procedure to evaluate a Chebyshev polynomial assuming that
# the coefficients have been calculated. 

procedure dgs_1devcheb (coeff, x, yfit, npts, order, k1, k2)

double	coeff[ARB]		# EV array of coefficients
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
	call amovkd (coeff[1], yfit, npts)
	if (order == 1)
	    return

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
	call amovkd (1.0d0, Memd[pnm2], npts)
	call altad (x, Memd[sx], npts, k1, k2)
	call amovd (Memd[sx], Memd[pnm1], npts)
	call amulkd (Memd[sx], 2.0D0, Memd[sx], npts)
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


# GS_1DEVLEG -- Procedure to evaluate a Legendre polynomial assuming that
# the coefficients have been calculated. 

procedure dgs_1devleg (coeff, x, yfit, npts, order, k1, k2)

double	coeff[ARB]		# EV array of coefficients
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
	call amovkd (coeff[1], yfit, npts)
	if (order == 1)
	    return

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
	call amovkd (1.0d0, Memd[pnm2], npts)
	call altad (x, Memd[sx], npts, k1, k2)
	call amovd (Memd[sx], Memd[pnm1], npts)
	do i = 3, order {
	    ri = i
	    ri1 = (2. * ri - 3.) / (ri - 1.)
	    ri2 = - (ri - 2.) / (ri - 1.)
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
