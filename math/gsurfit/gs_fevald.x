# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>

# GS_EVPOLY -- Procedure to evluate the polynomials

procedure dgs_evpoly (coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x,
    k2x, k1y, k2y)

double	coeff[ARB]		# 1D array of coefficients
double	x[npts]			# x values of points to be evaluated
double	y[npts]
double	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
double	k1x, k2x		# normalizing constants
double	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, xbptr, ybptr, accum

begin
	# fit a constant
	if (xorder == 1 && yorder == 1) {
	    call amovkd (coeff[1], zfit, npts)
	    return
	}

	# fit first order in x and y
	if (xorder == 2 && yorder == 1) {
	    call altmd (x, zfit, npts, coeff[2], coeff[1])
	    return
	}
	if (yorder == 2 && xorder == 1) {
	    call altmd (x, zfit, npts, coeff[2], coeff[1])
	    return
	}
	if (xorder == 2 && yorder == 2 && xterms == NO) {
	    do i = 1, npts
		zfit[i] = coeff[1] + x[i] * coeff[2] + y[i] * coeff[3]
	    return
	}

	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_DOUBLE)
	call salloc (yb, yorder * npts, TY_DOUBLE)
	call salloc (accum, npts, TY_DOUBLE)

	# calculate basis functions
	call dgs_bpol (x, npts, xorder, k1x, k2x, Memd[xb])
	call dgs_bpol (y, npts, yorder, k1y, k2y, Memd[yb])

	# accumulate the output vector
	cptr = 0
	call aclrd (zfit, npts)
	if (xterms != GS_XNONE) {
	    maxorder = max (xorder + 1, yorder + 1)
	    xincr = xorder
	    ybptr = yb
	    do i = 1, yorder {
	        call aclrd (Memd[accum], npts)
	        xbptr = xb
	        do k = 1, xincr {
		    call awsud (Memd[accum], Memd[xbptr], Memd[accum], npts,
		        1.0d0, coeff[cptr+k])
		    xbptr = xbptr + npts
	        }
	        call gs_asumvpd (Memd[accum], Memd[ybptr], zfit, zfit, npts)
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
		call awsud (zfit, Memd[xbptr], zfit, npts, 1.0d0, coeff[k])
		xbptr = xbptr + npts
	    }
	    ybptr = yb + npts
	    do k = 1, yorder - 1 {
		call awsud (zfit, Memd[ybptr], zfit, npts, 1.0d0,
		    coeff[xorder+k])
		ybptr = ybptr + npts
	    }
	}


	call sfree (sp)
end

# GS_EVCHEB -- Procedure to evaluate a Chebyshev polynomial assuming that
# the coefficients have been calculated. 

procedure dgs_evcheb (coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x,
    k2x, k1y, k2y)

double	coeff[ARB]		# 1D array of coefficients
double	x[npts]			# x values of points to be evaluated
double	y[npts]
double	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
double	k1x, k2x		# normalizing constants
double	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, xbptr, ybptr, accum

begin
	# fit a constant
	if (xorder == 1 && yorder == 1) {
	    call amovkd (coeff[1], zfit, npts)
	    return
	}

	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_DOUBLE)
	call salloc (yb, yorder * npts, TY_DOUBLE)
	call salloc (accum, npts, TY_DOUBLE)

	# calculate basis functions
	call dgs_bcheb (x, npts, xorder, k1x, k2x, Memd[xb])
	call dgs_bcheb (y, npts, yorder, k1y, k2y, Memd[yb])

	# accumulate thr output vector
	cptr = 0
	call aclrd (zfit, npts)
	if (xterms != GS_XNONE) {
	    maxorder = max (xorder + 1, yorder + 1)
	    xincr = xorder
	    ybptr = yb
	    do i = 1, yorder {
	        call aclrd (Memd[accum], npts)
	        xbptr = xb
	        do k = 1, xincr {
		    call awsud (Memd[accum], Memd[xbptr], Memd[accum], npts,
		        1.0d0, coeff[cptr+k])
		    xbptr = xbptr + npts
	        }
	        call gs_asumvpd (Memd[accum], Memd[ybptr], zfit, zfit, npts)
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
		call awsud (zfit, Memd[xbptr], zfit, npts, 1.0d0, coeff[k])
		xbptr = xbptr + npts
	    }
	    ybptr = yb + npts
	    do k = 1, yorder - 1 {
		call awsud (zfit, Memd[ybptr], zfit, npts, 1.0d0,
		    coeff[xorder+k])
		ybptr = ybptr + npts
	    }
	}

	# free temporary space
	call sfree (sp)
end


# GS_EVLEG -- Procedure to evaluate a Chebyshev polynomial assuming that
# the coefficients have been calculated. 

procedure dgs_evleg (coeff, x, y, zfit, npts, xterms, xorder, yorder, k1x, k2x,
    k1y, k2y)

double	coeff[ARB]		# 1D array of coefficients
double	x[npts]			# x values of points to be evaluated
double	y[npts]
double	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
double	k1x, k2x		# normalizing constants
double	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, accum, xbptr, ybptr

begin
	# fit a constant
	if (xorder == 1 && yorder == 1) {
	    call amovkd (coeff[1], zfit, npts)
	    return
	}

	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_DOUBLE)
	call salloc (yb, yorder * npts, TY_DOUBLE)
	call salloc (accum, npts, TY_DOUBLE)

	# calculate basis functions
	call dgs_bleg (x, npts, xorder, k1x, k2x, Memd[xb])
	call dgs_bleg (y, npts, yorder, k1y, k2y, Memd[yb])

	cptr = 0
	call aclrd (zfit, npts)
	if (xterms != GS_XNONE) {
	    maxorder = max (xorder + 1, yorder + 1)
	    xincr = xorder
	    ybptr = yb
	    do i = 1, yorder {
	        xbptr = xb
	        call aclrd (Memd[accum], npts)
	        do k = 1, xincr {
		    call awsud (Memd[accum], Memd[xbptr], Memd[accum], npts,
		        1.0d0, coeff[cptr+k])
		    xbptr = xbptr + npts
	        }
	        call gs_asumvpd (Memd[accum], Memd[ybptr], zfit, zfit, npts)
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
		call awsud (zfit, Memd[xbptr], zfit, npts, 1.0d0, coeff[k])
		xbptr = xbptr + npts
	    }
	    ybptr = yb + npts
	    do k = 1, yorder - 1 {
		call awsud (zfit, Memd[ybptr], zfit, npts, 1.0d0,
		    coeff[xorder+k])
		ybptr = ybptr + npts
	    }
	}

	# free temporary space
	call sfree (sp)
end

# GS_ASUMVP -- Procedure to add the product of two vectors to another vector

procedure gs_asumvpd (a, b, c, d, npts)

double	a[ARB]		# first input vector
double	b[ARB]		# second input vector
double	c[ARB]		# third vector
double	d[ARB]		# output vector
int	npts		# number of points

int	i

begin
	do i = 1, npts
	    d[i] = c[i] + a[i] * b[i]
end
