# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>

# GS_DERPOLY -- Evaluate the new polynomial derivative surface.

procedure dgs_derpoly (coeff, x, y, zfit, npts, xterms, xorder, yorder, nxder,
	nyder, k1x, k2x, k1y, k2y)

double	coeff[ARB]		# 1D array of coefficients
double	x[npts]			# x values of points to be evaluated
double	y[npts]
double	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
int	nxder,nyder		# order of the derivatives in x and y
double	k1x, k2x		# normalizing constants
double	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, xbptr, ybptr, accum

begin
	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_DOUBLE)
	call salloc (yb, yorder * npts, TY_DOUBLE)
	call salloc (accum, npts, TY_DOUBLE)

	# calculate basis functions
	call dgs_dpol (x, npts, xorder, nxder, k1x, k2x, Memd[xb])
	call dgs_dpol (y, npts, yorder, nyder, k1y, k2y, Memd[yb])

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
	    call amuld (Memd[xb], Memd[yb], zfit, npts)
	    call amulkd (zfit, coeff[1], zfit, npts)
	    xbptr = xb + npts
	    do k = 1, xorder - 1 {
		call awsud (zfit, Memd[xbptr], zfit, npts, 1.0d0, coeff[k+1])
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

# GS_DERCHEB -- Evaluate the new Chebyshev polynomial derivative surface.

procedure dgs_dercheb (coeff, x, y, zfit, npts, xterms, xorder, yorder,
	nxder, nyder, k1x, k2x, k1y, k2y)

double	coeff[ARB]		# 1D array of coefficients
double	x[npts]			# x values of points to be evaluated
double	y[npts]
double	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
int	nxder,nyder		# order of the derivatives in x and y
double	k1x, k2x		# normalizing constants
double	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, xbptr, ybptr, accum

begin
	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_DOUBLE)
	call salloc (yb, yorder * npts, TY_DOUBLE)
	call salloc (accum, npts, TY_DOUBLE)

	# calculate basis functions
	call dgs_dcheb (x, npts, xorder, nxder, k1x, k2x, Memd[xb])
	call dgs_dcheb (y, npts, yorder, nyder, k1y, k2y, Memd[yb])

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
	    call amuld (Memd[xb], Memd[yb], zfit, npts)
	    call amulkd (zfit, coeff[1], zfit, npts)
	    xbptr = xb + npts
	    do k = 1, xorder - 1 {
		call awsud (zfit, Memd[xbptr], zfit, npts, 1.0d0, coeff[k+1])
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


# GS_DERLEG -- Evaluate the new Legendre polynomial derivative surface.

procedure dgs_derleg (coeff, x, y, zfit, npts, xterms, xorder, yorder,
	nxder, nyder, k1x, k2x, k1y, k2y)

double	coeff[ARB]		# 1D array of coefficients
double	x[npts]			# x values of points to be evaluated
double	y[npts]
double	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
int	nxder,nyder		# order of the derivatives in x and y
double	k1x, k2x		# normalizing constants
double	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, accum, xbptr, ybptr

begin
	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_DOUBLE)
	call salloc (yb, yorder * npts, TY_DOUBLE)
	call salloc (accum, npts, TY_DOUBLE)

	# calculate basis functions
	call dgs_dleg (x, npts, xorder, nxder, k1x, k2x, Memd[xb])
	call dgs_dleg (y, npts, yorder, nyder, k1y, k2y, Memd[yb])

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
	    call amuld (Memd[xb], Memd[yb], zfit, npts)
	    call amulkd (zfit, coeff[1], zfit, npts)
	    xbptr = xb + npts
	    do k = 1, xorder - 1 {
		call awsud (zfit, Memd[xbptr], zfit, npts, 1.0d0, coeff[k+1])
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
