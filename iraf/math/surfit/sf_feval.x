# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# SF_EVCHEB -- Procedure to evaluate a Chebyshev polynomial assuming that
# the coefficients have been calculated. 

procedure sf_evcheb (coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x,
	k1y, k2y)

real	coeff[ARB]		# 1D array of coefficients
real	x[npts]			# x values of points to be evaluated
real	y[npts]
real	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
real	k1x, k2x		# normalizing constants
real	k1y, k2y

int	i, k, j
int	ytorder, cptr
pointer	sp
pointer	xb, yb, accum
pointer ybzptr, ybptr, xbzptr

begin
	# fit a constant
	if (xorder == 1 && yorder == 1) {
	    call amovkr (coeff[1], zfit, npts)
	    return
	}

	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_REAL)
	call salloc (yb, yorder * npts, TY_REAL)
	call salloc (accum, npts, TY_REAL)

	# calculate basis functions
	call sf_bcheb (x, npts, xorder, k1x, k2x, Memr[xb])
	call sf_bcheb (y, npts, yorder, k1y, k2y, Memr[yb])

	# clear the accumulator
	call aclrr (zfit, npts)

	# accumulate the values
	cptr = 0
	ybzptr = yb - 1
	xbzptr = xb - 1
	ytorder = yorder

	do i = 1, xorder {
	    call aclrr (Memr[accum], npts)
	    ybptr = ybzptr
	    do k = 1, ytorder {
		do j = 1, npts
		    Memr[accum+j-1] = Memr[accum+j-1] + coeff[cptr+k] *
		        Memr[ybptr+j]
		ybptr = ybptr + npts
	    }
	    do j = 1, npts
		zfit[j] = zfit[j] + Memr[accum+j-1] * Memr[xbzptr+j]

	    if (xterms == NO)
		ytorder = 1

	    cptr = cptr + yorder
	    xbzptr = xbzptr + npts
	}

	# free temporary space
	call sfree (sp)
end


# SF_EVLEG -- Procedure to evaluate a Chebyshev polynomial assuming that
# the coefficients have been calculated. 

procedure sf_evleg (coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x,
	k1y, k2y)

real	coeff[ARB]		# 1D array of coefficients
real	x[npts]			# x values of points to be evaluated
real	y[npts]
real	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
real	k1x, k2x		# normalizing constants
real	k1y, k2y

int	i, k, j
int	ytorder, cptr
pointer	sp
pointer	xb, yb, accum
pointer ybzptr, ybptr, xbzptr

begin
	# fit a constant
	if (xorder == 1 && yorder == 1) {
	    call amovkr (coeff[1], zfit, npts)
	    return
	}

	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_REAL)
	call salloc (yb, yorder * npts, TY_REAL)
	call salloc (accum, npts, TY_REAL)

	# calculate basis functions
	call sf_bleg (x, npts, xorder, k1x, k2x, Memr[xb])
	call sf_bleg (y, npts, yorder, k1y, k2y, Memr[yb])

	# clear the accumulator
	call aclrr (zfit, npts)

	# accumulate the values
	cptr = 0
	ybzptr = yb - 1
	xbzptr = xb - 1
	ytorder = yorder

	do i = 1, xorder {
	    call aclrr (Memr[accum], npts)
	    ybptr = ybzptr
	    do k = 1, ytorder {
		do j = 1, npts
		    Memr[accum+j-1] = Memr[accum+j-1] + coeff[cptr+k] *
		        Memr[ybptr+j]
		ybptr = ybptr + npts
	    }
	    do j = 1, npts
		zfit[j] = zfit[j] + Memr[accum+j-1] * Memr[xbzptr+j]

	    if (xterms == NO)
		ytorder = 1

	    cptr = cptr + yorder
	    xbzptr = xbzptr + npts
	}

	# free temporary space
	call sfree (sp)
end


# SF_EVSPLINE3 -- Procedure to evaluate a piecewise linear spline function
# assuming that the coefficients have been calculated.

procedure sf_evspline3 (coeff, x, y, zfit, npts, nxpieces, nypieces, k1x, k2x,
	k1y, k2y)

real	coeff[ARB]		# array of coefficients
real	x[npts]			# array of x values
real	y[npts]			# array of y values
real	zfit[npts]		# array of fitted values
int	npts			# number of data points
int	nxpieces, nypieces	# number of fitted points minus 1
real	k1x, k2x		# normalizing constants
real	k1y, k2y

int	i, j, k, cindex
pointer	xb, xbzptr, yb, ybzptr, ybptr
pointer	accum, leftx, lefty
pointer	sp

begin
	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, 4 * npts, TY_REAL)
	call salloc (yb, 4 * npts, TY_REAL)
	call salloc (accum, npts, TY_REAL)
	call salloc (leftx, npts, TY_INT)
	call salloc (lefty, npts, TY_INT)

	# calculate basis functions
	call sf_bspline3 (x, npts, nxpieces, k1x, k2x, Memr[xb], Memi[leftx])
	call sf_bspline3 (y, npts, nypieces, k1y, k2y, Memr[yb], Memi[lefty])

	# set up the indexing
	call amulki (Memi[leftx], (nypieces+4), Memi[leftx], npts)
	call aaddi (Memi[leftx], Memi[lefty], Memi[lefty], npts)

	# clear the accumulator
	call aclrr (zfit, npts)

	# accumulate the values

	ybzptr = yb - 1
	xbzptr = xb - 1

	do i = 1, 4 {
	    call aclrr (Memr[accum], npts)
	    ybptr = ybzptr
	    do k = 1, 4 {
		do j = 1, npts {
		    cindex = k + Memi[lefty+j-1]
		    Memr[accum+j-1] = Memr[accum+j-1] + coeff[cindex] *
		        Memr[ybptr+j]
		}
		ybptr = ybptr + npts
	    }
	    do j = 1, npts
		zfit[j] = zfit[j] + Memr[accum+j-1] * Memr[xbzptr+j]

	    xbzptr = xbzptr + npts
	    call aaddki (Memi[lefty], (nypieces+4), Memi[lefty], npts)
	}

	# free temporary space
	call sfree (sp)
end


# SF_EVSPLINE1 -- Procedure to evaluate a piecewise linear spline function
# assuming that the coefficients have been calculated.

procedure sf_evspline1 (coeff, x, y, zfit, npts, nxpieces, nypieces, k1x, k2x,
	k1y, k2y)

real	coeff[ARB]		# array of coefficients
real	x[npts]			# array of x values
real	y[npts]			# array of y values
real	zfit[npts]		# array of fitted values
int	npts			# number of data points
int	nxpieces, nypieces	# number of fitted points minus 1
real	k1x, k2x		# normalizing constants
real	k1y, k2y

int	i, j, k, cindex
pointer	xb, xbzptr, yb, ybzptr, ybptr
pointer	accum, leftx, lefty
pointer	sp

begin
	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, 2 * npts, TY_REAL)
	call salloc (yb, 2 * npts, TY_REAL)
	call salloc (accum, npts, TY_REAL)
	call salloc (leftx, npts, TY_INT)
	call salloc (lefty, npts, TY_INT)

	# calculate basis functions
	call sf_bspline1 (x, npts, nxpieces, k1x, k2x, Memr[xb], Memi[leftx])
	call sf_bspline1 (y, npts, nypieces, k1y, k2y, Memr[yb], Memi[lefty])

	# set up the indexing
	call amulki (Memi[leftx], (nypieces+2), Memi[leftx], npts)
	call aaddi (Memi[leftx], Memi[lefty], Memi[lefty], npts)

	# clear the accumulator
	call aclrr (zfit, npts)

	# accumulate the values

	ybzptr = yb - 1
	xbzptr = xb - 1

	do i = 1, 2 {
	    call aclrr (Memr[accum], npts)
	    ybptr = ybzptr
	    do k = 1, 2 {
		do j = 1, npts {
		    cindex = k + Memi[lefty+j-1]
		    Memr[accum+j-1] = Memr[accum+j-1] + coeff[cindex] *
		        Memr[ybptr+j]
		}
		ybptr = ybptr + npts
	    }
	    do j = 1, npts
		zfit[j] = zfit[j] + Memr[accum+j-1] * Memr[xbzptr+j]

	    xbzptr = xbzptr + npts
	    call aaddki (Memi[lefty], (nypieces+2), Memi[lefty], npts)
	}

	# free temporary space
	call sfree (sp)
end
