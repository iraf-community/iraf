# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>

# GS_EVPOLY -- Procedure to evluate the polynomials

procedure rgs_evpoly (coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x,
    k2x, k1y, k2y)

real	coeff[ARB]		# 1D array of coefficients
real	x[npts]			# x values of points to be evaluated
real	y[npts]
real	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
real	k1x, k2x		# normalizing constants
real	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, xbptr, ybptr, accum

begin
	# fit a constant
	if (xorder == 1 && yorder == 1) {
	    call amovkr (coeff[1], zfit, npts)
	    return
	}

	# fit first order in x and y
	if (xorder == 2 && yorder == 1) {
	    call altmr (x, zfit, npts, coeff[2], coeff[1])
	    return
	}
	if (yorder == 2 && xorder == 1) {
	    call altmr (x, zfit, npts, coeff[2], coeff[1])
	    return
	}
	if (xorder == 2 && yorder == 2 && xterms == NO) {
	    do i = 1, npts
		zfit[i] = coeff[1] + x[i] * coeff[2] + y[i] * coeff[3]
	    return
	}

	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_REAL)
	call salloc (yb, yorder * npts, TY_REAL)
	call salloc (accum, npts, TY_REAL)

	# calculate basis functions
	call rgs_bpol (x, npts, xorder, k1x, k2x, Memr[xb])
	call rgs_bpol (y, npts, yorder, k1y, k2y, Memr[yb])

	# accumulate the output vector
	cptr = 0
	call aclrr (zfit, npts)
	if (xterms != GS_XNONE) {
	    maxorder = max (xorder + 1, yorder + 1)
	    xincr = xorder
	    ybptr = yb
	    do i = 1, yorder {
	        call aclrr (Memr[accum], npts)
	        xbptr = xb
	        do k = 1, xincr {
		    call awsur (Memr[accum], Memr[xbptr], Memr[accum], npts,
		        1.0, coeff[cptr+k])
		    xbptr = xbptr + npts
	        }
	        call gs_asumvpr (Memr[accum], Memr[ybptr], zfit, zfit, npts)
	        cptr = cptr + xincr
	        ybptr = ybptr + npts
		switch (xterms) {
		case GS_XHALF:
		    if ((i + xorder + 1) > maxorder)
			xincr = xincr - 1
		default:
		    ;
		}
	    }
	} else {
	    xbptr = xb
	    do k = 1, xorder {
		call awsur (zfit, Memr[xbptr], zfit, npts, 1.0, coeff[k])
		xbptr = xbptr + npts
	    }
	    ybptr = yb + npts
	    do k = 1, yorder - 1 {
		call awsur (zfit, Memr[ybptr], zfit, npts, 1.0, coeff[xorder+k])
		ybptr = ybptr + npts
	    }
	}


	call sfree (sp)
end

# GS_EVCHEB -- Procedure to evaluate a Chebyshev polynomial assuming that
# the coefficients have been calculated. 

procedure rgs_evcheb (coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x,
    k2x, k1y, k2y)

real	coeff[ARB]		# 1D array of coefficients
real	x[npts]			# x values of points to be evaluated
real	y[npts]
real	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
real	k1x, k2x		# normalizing constants
real	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, xbptr, ybptr, accum

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
	call rgs_bcheb (x, npts, xorder, k1x, k2x, Memr[xb])
	call rgs_bcheb (y, npts, yorder, k1y, k2y, Memr[yb])

	# accumulate thr output vector
	cptr = 0
	call aclrr (zfit, npts)
	if (xterms != GS_XNONE) {
	    maxorder = max (xorder + 1, yorder + 1)
	    xincr = xorder
	    ybptr = yb
	    do i = 1, yorder {
	        call aclrr (Memr[accum], npts)
	        xbptr = xb
	        do k = 1, xincr {
		    call awsur (Memr[accum], Memr[xbptr], Memr[accum], npts,
		        1.0, coeff[cptr+k])
		    xbptr = xbptr + npts
	        }
	        call gs_asumvpr (Memr[accum], Memr[ybptr], zfit, zfit, npts)
	        cptr = cptr + xincr
	        ybptr = ybptr + npts
		switch (xterms) {
		case GS_XHALF:
		    if ((i + xorder + 1) > maxorder)
			xincr = xincr - 1
		default:
		    ;
		}
	    }
	} else {
	    xbptr = xb
	    do k = 1, xorder {
		call awsur (zfit, Memr[xbptr], zfit, npts, 1.0, coeff[k])
		xbptr = xbptr + npts
	    }
	    ybptr = yb + npts
	    do k = 1, yorder - 1 {
		call awsur (zfit, Memr[ybptr], zfit, npts, 1.0, coeff[xorder+k])
		ybptr = ybptr + npts
	    }
	}

	# free temporary space
	call sfree (sp)
end


# GS_EVLEG -- Procedure to evaluate a Chebyshev polynomial assuming that
# the coefficients have been calculated. 

procedure rgs_evleg (coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x,
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

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, accum, xbptr, ybptr

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
	call rgs_bleg (x, npts, xorder, k1x, k2x, Memr[xb])
	call rgs_bleg (y, npts, yorder, k1y, k2y, Memr[yb])

	cptr = 0
	call aclrr (zfit, npts)
	if (xterms != GS_XNONE) {
	    maxorder = max (xorder + 1, yorder + 1)
	    xincr = xorder
	    ybptr = yb
	    do i = 1, yorder {
	        xbptr = xb
	        call aclrr (Memr[accum], npts)
	        do k = 1, xincr {
		    call awsur (Memr[accum], Memr[xbptr], Memr[accum], npts,
		        1.0, coeff[cptr+k])
		    xbptr = xbptr + npts
	        }
	        call gs_asumvpr (Memr[accum], Memr[ybptr], zfit, zfit, npts)
	        cptr = cptr + xincr
	        ybptr = ybptr + npts
		switch (xterms) {
		case GS_XHALF:
		    if ((i + xorder + 1) > maxorder)
			xincr = xincr - 1
		default:
		    ;
		}
	    }
	} else {
	    xbptr = xb
	    do k = 1, xorder {
		call awsur (zfit, Memr[xbptr], zfit, npts, 1.0, coeff[k])
		xbptr = xbptr + npts
	    }
	    ybptr = yb + npts
	    do k = 1, yorder - 1 {
		call awsur (zfit, Memr[ybptr], zfit, npts, 1.0, coeff[xorder+k])
		ybptr = ybptr + npts
	    }
	}

	# free temporary space
	call sfree (sp)
end

# GS_ASUMVP -- Procedure to add the product of two vectors to another vector

procedure gs_asumvpr (a, b, c, d, npts)

real	a[ARB]		# first input vector
real	b[ARB]		# second input vector
real	c[ARB]		# third vector
real	d[ARB]		# output vector
int	npts		# number of points

int	i

begin
	do i = 1, npts
	    d[i] = c[i] + a[i] * b[i]
end
